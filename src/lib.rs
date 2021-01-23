#[macro_use]
extern crate vst;

mod neighbor_pairs;
mod params;
mod sound_gen;
mod ui;

use std::{convert::TryFrom, sync::Arc};

use params::{
    ModulationType, OSCParameterType, OSCParams, ParameterType, Parameters, RawParameters,
};
use sound_gen::{
    normalize_U7, normalize_pitch_bend, to_pitch_envelope, to_pitch_multiplier,
    NormalizedPitchbend, NoteShape, Oscillator, SampleRate, ADSR,
};
use ui::UIFrontEnd;

use iced_baseview::Application;
use vst::{
    api::{Events, Supported},
    buffer::AudioBuffer,
    editor::Editor,
    plugin::{CanDo, Category, HostCallback, Info, Plugin, PluginParameters},
};
use wmidi::MidiMessage;

struct SoundGenerator {
    osc_1: OSCGroup,
    osc_2: OSCGroup,
    note: wmidi::Note,
}

impl SoundGenerator {
    fn is_alive(&self, sample_rate: SampleRate, params: &Parameters) -> bool {
        match params.osc_2_mod {
            // If we mix together the two sounds then we should only kill the note
            // after both oscillators have died.
            ModulationType::Mix => {
                self.osc_1
                    .osc
                    .is_alive(sample_rate, params.osc_1.vol_adsr.release)
                    || self
                        .osc_2
                        .osc
                        .is_alive(sample_rate, params.osc_2.vol_adsr.release)
            }
            _ => self
                .osc_1
                .osc
                .is_alive(sample_rate, params.osc_1.vol_adsr.release),
        }
    }

    fn note_on(&mut self, mix: ModulationType, frame_delta: i32, vel: f32) {
        if mix == ModulationType::Mix {
            self.osc_1.osc.note_on(frame_delta, vel);
            self.osc_2.osc.note_on(frame_delta, vel);
        } else {
            self.osc_1.osc.note_on(frame_delta, vel);
            self.osc_2.osc.note_on(frame_delta, 1.0);
        }
    }

    fn note_off(&mut self, frame_delta: i32) {
        self.osc_1.osc.note_off(frame_delta);
        self.osc_2.osc.note_off(frame_delta);
    }
}

struct OSCGroup {
    osc: Oscillator,
    volume_lfo: Oscillator,
    pitch_lfo: Oscillator,
}

impl OSCGroup {
    fn next_sample(
        &mut self,
        params: &OSCParams,
        sample_rate: f32,
        i: usize,
        pitch_bend: f32,
        (mod_type, modulation): (ModulationType, f32),
        apply_filter: bool,
    ) -> f32 {
        // Compute volume from parameters, ADSR, LFO, and AmpMod
        let vol_env = params
            .vol_adsr
            .get(self.osc.time, self.osc.note_state, sample_rate);

        let vol_lfo = self.volume_lfo.next_sample_raw(
            1.0 / params.vol_lfo.period,
            NoteShape::Sine,
            sample_rate,
        ) * params.vol_lfo.amplitude;

        let vol_mod = if mod_type == ModulationType::AmpMod {
            modulation
        } else {
            0.0
        };

        // Apply parameter, ADSR, LFO, and AmpMod for total volume
        // We clamp the LFO to positive values because negative values cause the
        // signal to be inverted, which isn't what we want (instead it should
        // just have zero volume). We don't do this for the AmpMod because inverting
        // the signal allows for more interesting audio.
        let total_volume = params.volume * vol_env * (1.0 + vol_lfo).max(0.0) * (1.0 + vol_mod);

        // Compute note pitch multiplier from ADSR and envelope
        let pitch_env = params
            .pitch_adsr
            .get(self.osc.time, self.osc.note_state, sample_rate);
        let pitch_lfo = self.pitch_lfo.next_sample_raw(
            1.0 / params.pitch_lfo.period,
            NoteShape::Sine,
            sample_rate,
        ) * params.pitch_lfo.amplitude;
        let pitch_coarse = to_pitch_multiplier(1.0, params.coarse_tune);
        let pitch_fine = to_pitch_multiplier(params.fine_tune, 1);
        let pitch_bend = to_pitch_multiplier(pitch_bend, 12);
        let note_pitch = to_pitch_multiplier(pitch_env + pitch_lfo, 24);

        let fm_mod = if mod_type == ModulationType::FreqMod {
            to_pitch_multiplier(modulation, 24)
        } else {
            1.0
        };

        // The final pitch multiplier, post-FM
        // Note pitch consists of the applied pitch bend, pitch ADSR, and pitch
        // LFOs applied to it, with a max range of 12 semis.
        // Fine and course pitchbend come from the parameters.
        // And the FM Mod comes from the modulation value.
        let pitch = note_pitch * pitch_bend * pitch_coarse * pitch_fine * fm_mod;

        let warp_mod = if mod_type == ModulationType::WarpMod {
            modulation
        } else {
            0.0
        };

        let phase_mod = if mod_type == ModulationType::PhaseMod {
            modulation
        } else {
            0.0
        };

        // Disable the filter when doing modulation (filtering the signal makes
        // it nearly impossible to get a nice modulation signal since it messes
        // with the phase a lot)
        let filter_params = if apply_filter {
            Some(params.filter_params)
        } else {
            None
        };

        // Get next sample
        self.osc.next_sample(
            i,
            sample_rate,
            params.shape.add(warp_mod),
            vol_env,
            pitch,
            phase_mod,
            filter_params,
        ) * total_volume
    }
}

/// The main plugin struct.
struct Revisit {
    /// All the notes to be played.
    notes: Vec<SoundGenerator>,
    /// The sample rate in Hz/sec (usually 44,100)
    sample_rate: SampleRate,
    /// The parameters which are shared with the VST host
    params: Arc<RawParameters>,
    /// Pitchbend messages. Format is (value, frame_delta) where
    /// value is a normalized f32 and frame_delta is the offset into the current
    /// frame for which the pitchbend value occurs
    pitch_bend: Vec<(NormalizedPitchbend, i32)>,
    /// The last pitch bend value from the previous frame.
    last_pitch_bend: NormalizedPitchbend,
    /// If true, then the GUI has been initalized and `get_editor()` will return
    /// None.
    gui_initialized: bool,
}

impl Plugin for Revisit {
    fn new(host: HostCallback) -> Self {
        Revisit {
            params: Arc::new(RawParameters {
                host,
                ..Default::default()
            }),
            ..Default::default()
        }
    }

    fn init(&mut self) {
        // let result = simple_logging::log_to_file("revisit.log", LevelFilter::Info);
        // if let Err(err) = result {
        //     println!("Couldn't start logging! {}", err);
        // }
        // info!("Begin VST log");
    }

    fn get_info(&self) -> Info {
        Info {
            name: "Revisit".to_string(),
            vendor: "a2aaron".to_string(),
            // Used by hosts to differentiate between plugins.
            // Don't worry much about this now - just fill in a random number.
            unique_id: 413612,
            version: 1,
            category: Category::Synth,
            // Subtract one here due to "error" type
            parameters: (ParameterType::VARIANT_COUNT - 2 + OSCParameterType::VARIANT_COUNT * 2)
                as i32,
            // No audio inputs
            inputs: 0,
            // Two channel audio!
            outputs: 2,
            // For now, fill in the rest of our fields with `Default` info.
            ..Default::default()
        }
    }

    fn can_do(&self, can_do: CanDo) -> Supported {
        match can_do {
            CanDo::ReceiveMidiEvent => Supported::Yes,
            _ => Supported::No,
        }
    }

    // Output audio given the current state of the VST
    fn process(&mut self, buffer: &mut AudioBuffer<f32>) {
        let num_samples = buffer.samples();

        let params = Parameters::from(self.params.as_ref());

        // Get the envelope from MIDI pitch bend
        let (pitch_bends, last_bend) =
            to_pitch_envelope(&self.pitch_bend, self.last_pitch_bend, num_samples);
        self.last_pitch_bend = last_bend;

        let pitch_bends: Vec<f32> = pitch_bends.collect();

        // Get sound for each note
        let (_, mut output_buffer) = buffer.split();

        let mut output = vec![0.0; num_samples];
        for gen in &mut self.notes {
            for i in 0..num_samples {
                let (osc_1, osc_2) = (&mut gen.osc_1, &mut gen.osc_2);

                let osc_2 = osc_2.next_sample(
                    &params.osc_2,
                    self.sample_rate,
                    i,
                    pitch_bends[i],
                    (ModulationType::Mix, 0.0),
                    params.osc_2_mod == ModulationType::Mix,
                );

                let osc_1 = osc_1.next_sample(
                    &params.osc_1,
                    self.sample_rate,
                    i,
                    pitch_bends[i],
                    (params.osc_2_mod, osc_2),
                    true,
                );

                if params.osc_2_mod == ModulationType::Mix {
                    output[i] += osc_1 + osc_2;
                } else {
                    output[i] += osc_1;
                }
            }
        }

        // Write sound
        for channel in output_buffer.into_iter() {
            for (i, sample) in channel.iter_mut().enumerate() {
                *sample = output[i] * params.master_vol;
            }
        }
    }

    fn process_events(&mut self, events: &Events) {
        let sample_rate = self.sample_rate;
        // remove "dead" notes
        // we do this in process_events _before_ processing any midi messages
        // because this is the start of a new frame, and we want to make sure
        // that midi messages do not apply to dead notes
        // ex: if we do this after processing midi messages, a bug occurs where
        // - frame 0 - note is in release state and is dead by end of frame
        // - frame 1 - process events send midi messages to dead note
        // - frame 1 - process removes dead note
        // - frame 1 - user is confused to why note does not play despite holding
        //             down key (the KeyOn event was "eaten" by the dead note!)
        let params = Parameters::from(self.params.as_ref());
        self.notes.retain(|gen| gen.is_alive(sample_rate, &params));

        // Clear pitch bend to get new messages
        self.pitch_bend.clear();
        for event in events.events() {
            match event {
                vst::event::Event::Midi(event) => {
                    let message = MidiMessage::try_from(&event.data as &[u8]);
                    if let Ok(message) = message {
                        match message {
                            MidiMessage::NoteOn(_, note, vel) => {
                                // On note on, either add or retrigger the note
                                let vel = normalize_U7(vel);
                                let gen = if let Some(osc) =
                                    self.notes.iter_mut().find(|gen| gen.note == note)
                                {
                                    osc
                                } else {
                                    self.notes.push(SoundGenerator {
                                        osc_1: OSCGroup {
                                            osc: Oscillator::new_from_note(
                                                note,
                                                vel,
                                                self.sample_rate,
                                            ),
                                            volume_lfo: Oscillator::new(1.0, 1.0, self.sample_rate),
                                            pitch_lfo: Oscillator::new(1.0, 1.0, self.sample_rate),
                                        },
                                        osc_2: OSCGroup {
                                            osc: Oscillator::new_from_note(
                                                note,
                                                vel,
                                                self.sample_rate,
                                            ),
                                            volume_lfo: Oscillator::new(1.0, 1.0, self.sample_rate),
                                            pitch_lfo: Oscillator::new(1.0, 1.0, self.sample_rate),
                                        },

                                        note,
                                    });
                                    self.notes.last_mut().unwrap()
                                };

                                gen.note_on(params.osc_2_mod, event.delta_frames, vel);
                            }
                            MidiMessage::NoteOff(_, note, _) => {
                                // On note off, send note off
                                if let Some(i) = self.notes.iter().position(|gen| gen.note == note)
                                {
                                    let gen = &mut self.notes[i];
                                    gen.note_off(event.delta_frames);
                                }
                            }
                            MidiMessage::PitchBendChange(_, pitch_bend) => {
                                self.pitch_bend
                                    .push((normalize_pitch_bend(pitch_bend), event.delta_frames));
                            }
                            _ => println!("shut up clippy"),
                        }
                    }
                }
                _ => println!("shut up clippy"),
            }
        }

        // Sort pitch bend changes by delta_frame.
        self.pitch_bend.sort_unstable_by(|a, b| a.1.cmp(&b.1));
    }

    fn stop_process(&mut self) {}

    fn set_sample_rate(&mut self, rate: f32) {
        self.sample_rate = rate;
    }

    // The raw parameters exposed to the host
    fn get_parameter_object(&mut self) -> Arc<dyn PluginParameters> {
        Arc::clone(&self.params) as Arc<dyn PluginParameters>
    }

    // The GUI exposed to the host
    fn get_editor(&mut self) -> Option<Box<dyn Editor>> {
        if self.gui_initialized {
            None
        } else {
            self.gui_initialized = true;
            let notify = self.params.notify.clone();
            let (editor, _) = UIFrontEnd::new((self.params.clone(), notify));
            Some(Box::new(editor))
        }
    }
}

impl Default for Revisit {
    fn default() -> Self {
        Revisit {
            notes: Vec::with_capacity(16),
            sample_rate: 44100.0,
            params: Arc::new(RawParameters::default()),
            pitch_bend: Vec::with_capacity(16),
            last_pitch_bend: 0.0,
            gui_initialized: false,
        }
    }
}

// Export symbols for main
plugin_main!(Revisit);
