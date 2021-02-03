#[macro_use]
extern crate vst;

mod neighbor_pairs;
mod params;
mod sound_gen;
mod ui;
mod ui_tabs;

use std::{convert::TryFrom, sync::Arc};

use biquad::{Biquad, DirectForm1, ToHertz, Q_BUTTERWORTH_F32};
use iced_graphics::canvas::Frame;
use params::{
    CountedEnum, Envelope, EnvelopeParams, ModBankEnvs, ModulationBank, ModulationSend,
    ModulationType, OSCParams, ParameterType, Parameters, RawParameters,
};
use sound_gen::{
    normalize_U7, normalize_pitch_bend, to_pitch_envelope, to_pitch_multiplier, FilterParams,
    FrameDelta, NormalizedPitchbend, NoteShape, NoteState, Oscillator, SampleRate, RETRIGGER_TIME,
};
use ui::UIFrontEnd;

use derive_more::Add;
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
    // The pitch of the note this SoundGenerator is playing, ignoring all coarse
    // detune and pitch bend effects. This is in hertz.
    note_pitch: f32,
    // The velocity of the note that this SoundGenerator is playing, ignoring all
    // amplitude modulation effects. This is a 0.0 - 1.0 normalized value.
    vel: f32,
    // The time, in samples, that this SoundGenerator has run since the last note on
    // event. This is NOT an interframe sample number!
    samples_since_note_on: usize,
    // The current state of the SoundGenerator (held, released, etc)
    note_state: NoteState,
    // The per-sample filter applied to the output
    // filter: DirectForm1<f32>,
    // If Some(frame_delta, vel), then a note on event occurs in the next frame
    // at sample `frame_delta` samples into the the frame, and the note has a
    // note velocity of `vel`
    next_note_on: Option<(FrameDelta, f32)>,
    // If Some(frame_delta), then the next note off event occurs in the next frame
    // at `frame_delta` samples into the frame
    next_note_off: Option<FrameDelta>,
}

impl SoundGenerator {
    pub fn new(note: wmidi::Note, vel: f32, sample_rate: SampleRate) -> SoundGenerator {
        SoundGenerator {
            note,
            note_pitch: wmidi::Note::to_freq_f32(note),
            vel,
            samples_since_note_on: 0,
            note_state: NoteState::None,
            osc_1: OSCGroup::new(sample_rate),
            osc_2: OSCGroup::new(sample_rate),
            next_note_on: None,
            next_note_off: None,
        }
    }

    /// Returns true if the note is "alive" (playing audio). A note is dead if
    /// it is in the release state and it is after the total release time.
    fn is_alive(&self, sample_rate: SampleRate, params: &Parameters) -> bool {
        match self.note_state {
            NoteState::None | NoteState::Held | NoteState::Retrigger { time: _ } => true,
            NoteState::Released { time: release_time } => {
                // The number of seconds it has been since release
                let time = (self.samples_since_note_on - release_time) as f32 / sample_rate;
                match params.osc_2_mod {
                    // If we mix together the two sounds then we should only kill the note
                    // after both oscillators have died.
                    ModulationType::Mix => {
                        time < params.osc_1.vol_adsr.release || time < params.osc_2.vol_adsr.release
                    }
                    _ => time < params.osc_1.vol_adsr.release,
                }
            }
        }
    }

    fn next_sample(
        &mut self,
        params: &Parameters,
        i: FrameDelta,
        sample_rate: SampleRate,
        pitch_bend: f32,
    ) -> (f32, f32) {
        // Only advance time if the note is being held down.
        match self.note_state {
            NoteState::None => (),
            _ => self.samples_since_note_on += 1,
        }

        // Handle note on event. If the note was previously not triggered (aka:
        // this is the first time the note has been triggered), then the note
        // transitions to the hold state. If this is a retrigger, then the note
        // transitions to the retrigger state, with volume `vol`.
        // Also, we set the note velocity to the appropriate new note velocity.
        match self.next_note_on {
            Some((note_on, note_on_vel)) if note_on == i => {
                self.osc_1.note_state_changed(
                    &params.osc_1,
                    &params.mod_bank,
                    sample_rate,
                    self.samples_since_note_on,
                    self.note_state,
                );
                self.osc_2.note_state_changed(
                    &params.osc_2,
                    &params.mod_bank,
                    sample_rate,
                    self.samples_since_note_on,
                    self.note_state,
                );

                self.note_state = match self.note_state {
                    NoteState::None => NoteState::Held,
                    _ => NoteState::Retrigger {
                        time: self.samples_since_note_on,
                    },
                };
                self.vel = note_on_vel;
                self.next_note_on = None;
            }
            _ => (),
        }

        // Trigger note off events
        match self.next_note_off {
            Some(note_off) if note_off == i => {
                self.osc_1.note_state_changed(
                    &params.osc_1,
                    &params.mod_bank,
                    sample_rate,
                    self.samples_since_note_on,
                    self.note_state,
                );
                self.osc_2.note_state_changed(
                    &params.osc_2,
                    &params.mod_bank,
                    sample_rate,
                    self.samples_since_note_on,
                    self.note_state,
                );

                self.note_state = NoteState::Released {
                    time: self.samples_since_note_on,
                };
                self.next_note_off = None;
            }
            _ => (),
        }

        // If it has been 10 samples in the retrigger state, switch back to
        // the held state. This also resets the time.
        if let NoteState::Retrigger {
            time: retrigger_time,
        } = self.note_state
        {
            if self.samples_since_note_on - retrigger_time > RETRIGGER_TIME {
                self.note_state = NoteState::Held;
                self.samples_since_note_on = 0;
            }
        }

        // When osc_2 is not in mix mode, the note velocity is ignored (to make
        // it easier to get a consistent sound)
        let osc_2_is_mix = params.osc_2_mod == ModulationType::Mix;
        let osc_2 = self.osc_2.next_sample(
            &params.osc_2,
            sample_rate,
            if osc_2_is_mix { self.vel } else { 1.0 },
            self.note_pitch,
            i,
            self.samples_since_note_on,
            self.note_state,
            pitch_bend,
            (ModulationType::Mix, 0.0),
            &params.mod_bank,
            osc_2_is_mix,
        );

        let osc_1 = self.osc_1.next_sample(
            &params.osc_1,
            sample_rate,
            self.vel,
            self.note_pitch,
            i,
            self.samples_since_note_on,
            self.note_state,
            pitch_bend,
            (params.osc_2_mod, osc_2),
            &params.mod_bank,
            true,
        );

        fn pan_split(pan: f32) -> (f32, f32) {
            let radians = (pan + 1.0) * std::f32::consts::PI / 4.0;
            (radians.cos(), radians.sin())
        }

        if params.osc_2_mod == ModulationType::Mix {
            let (osc_1_left, osc_1_right) = pan_split(params.osc_1.pan);
            let (osc_2_left, osc_2_right) = pan_split(params.osc_2.pan);
            let left = osc_1 * osc_1_left + osc_2 * osc_2_left;
            let right = osc_1 * osc_1_right + osc_2 * osc_2_right;
            (left, right)
        } else {
            let (pan_left, pan_right) = pan_split(params.osc_1.pan);
            (osc_1 * pan_left, osc_1 * pan_right)
        }
    }

    fn note_on(&mut self, frame_delta: i32, vel: f32) {
        self.next_note_on = Some((frame_delta as usize, vel));
    }

    fn note_off(&mut self, frame_delta: i32) {
        self.next_note_off = Some(frame_delta as usize);
    }
}

struct OSCGroup {
    osc: Oscillator,
    vol_env: Envelope,
    pitch_env: Envelope,
    mod_bank_envs: ModBankEnvs,
    volume_lfo: Oscillator,
    pitch_lfo: Oscillator,
    // The state for the EQ/filters, applied after the signal is generated
    filter: DirectForm1<f32>,
}

impl OSCGroup {
    fn new(sample_rate: f32) -> OSCGroup {
        OSCGroup {
            osc: Oscillator::new(),
            vol_env: Envelope::new(),
            pitch_env: Envelope::new(),
            mod_bank_envs: ModBankEnvs::new(),
            volume_lfo: Oscillator::new(),
            pitch_lfo: Oscillator::new(),
            filter: DirectForm1::<f32>::new(
                biquad::Coefficients::<f32>::from_params(
                    biquad::Type::LowPass,
                    sample_rate.hz(),
                    (10000).hz(),
                    Q_BUTTERWORTH_F32,
                )
                .unwrap(),
            ),
        }
    }

    /// Get the next sample from the osc group, applying modulation parameters as
    /// well.
    /// Note that the `i` argument is the interframe sample number (which sample
    /// within a frame we are at). This is used by `osc: Oscillator` to do
    /// interframe note event handling.
    fn next_sample(
        &mut self,
        params: &OSCParams,
        sample_rate: f32,
        base_vel: f32,
        base_note: f32,
        i: FrameDelta,
        samples_since_note_on: usize,
        note_state: NoteState,
        pitch_bend: f32,
        (mod_type, modulation): (ModulationType, f32),
        mod_bank: &ModulationBank,
        apply_filter: bool,
    ) -> f32 {
        // TODO: consider merging ModulationValues with the rest of the modulation
        // calculations in this block of code. Some notes
        // 1. You probably need to commit to storing either the post-multiplied
        //    or the pre-multiplied values (WITH the semi-tone amount) in the
        //    pitch variable. This probably means making more pitch field?
        //    Also, some modulations have additional weird offsets applied
        //    EX: AmpMod and VolLFO both are plus one'd and VolLFO is clamped at
        //    zero. Tis is easy to do but will be annoying to generalize.
        //    Also, how do we handle LFOs in the mod bank? (i think this should
        //    handled at from_mod_bank time)
        //    You need to probably store pre-multiplied values for each of the
        //    various modulation values.

        let mod_bank = ModulationValues::from_mod_bank(
            &self.mod_bank_envs,
            mod_bank,
            samples_since_note_on,
            note_state,
            sample_rate,
        );

        // Compute volume from parameters, ADSR, LFO, and AmpMod
        let vol_env = self.vol_env.get(
            &params.vol_adsr,
            samples_since_note_on,
            note_state,
            sample_rate,
        );

        let vol_lfo = self.volume_lfo.next_sample(
            sample_rate,
            NoteShape::Sine,
            1.0 / params.vol_lfo.period,
            0.0, // no phase mod
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
        let total_volume = base_vel
            * params.volume
            * vol_env
            * (1.0 + vol_lfo).max(0.0)
            * (1.0 + vol_mod)
            * (1.0 - mod_bank.amplitude);

        // Compute note pitch multiplier from ADSR and envelope
        let pitch_env = self.pitch_env.get(
            &params.pitch_adsr,
            samples_since_note_on,
            note_state,
            sample_rate,
        );
        let pitch_lfo = self.pitch_lfo.next_sample(
            sample_rate,
            NoteShape::Sine,
            1.0 / params.pitch_lfo.period,
            1.0,
        ) * params.pitch_lfo.amplitude;
        let pitch_coarse = to_pitch_multiplier(1.0, params.coarse_tune);
        let pitch_fine = to_pitch_multiplier(params.fine_tune, 1);
        let pitch_bend = to_pitch_multiplier(pitch_bend, 12);
        let pitch_mods = to_pitch_multiplier(pitch_env + pitch_lfo, 24);
        let mod_bank_pitch = to_pitch_multiplier(mod_bank.pitch, 24);

        let fm_mod = if mod_type == ModulationType::FreqMod {
            to_pitch_multiplier(modulation, 24)
        } else {
            1.0
        };

        // The final pitch multiplier, post-FM
        // Base note is the base note frequency, in hz
        // Pitch mods consists of the applied pitch bend, pitch ADSR, pitch LFOs
        // applied to it, with a max range of 12 semis.
        // Fine and course pitchbend come from the parameters.
        // The FM Mod comes from the modulation value.
        // Mod bank pitch comes from the mod bank.
        let pitch = base_note
            * pitch_mods
            * pitch_bend
            * pitch_coarse
            * pitch_fine
            * fm_mod
            * mod_bank_pitch;

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
        let value = self.osc.next_sample(
            sample_rate,
            params.shape.add(warp_mod).add(mod_bank.warp),
            pitch,
            phase_mod + params.phase + mod_bank.phase,
        );

        // Apply filter (if desired)
        let value = match filter_params {
            Some(params) => {
                let coefficents = FilterParams::into_coefficients(params, sample_rate);
                self.filter.update_coefficients(coefficents);
                let output = self.filter.run(value);
                if output.is_finite() {
                    output
                } else {
                    // If the output happens to be NaN or Infinity, output the
                    // original  signal instead. Hopefully, this will "reset"
                    // the filter on the next sample, instead of being filled
                    // with garbage values.
                    value
                }
            }
            None => value,
        };

        value * total_volume
    }

    /// Handle hold-to-release and release-to-retrigger state transitions
    fn note_state_changed(
        &mut self,
        params: &OSCParams,
        mod_bank_params: &ModulationBank,
        sample_rate: f32,
        samples_since_note_on: usize,
        note_state: NoteState,
    ) {
        self.vol_env.remember(
            &params.vol_adsr,
            samples_since_note_on,
            note_state,
            sample_rate,
        );
        self.pitch_env.remember(
            &params.pitch_adsr,
            samples_since_note_on,
            note_state,
            sample_rate,
        );
        self.mod_bank_envs.env_1.remember(
            &mod_bank_params.env_1,
            samples_since_note_on,
            note_state,
            sample_rate,
        );
        self.mod_bank_envs.env_2.remember(
            &mod_bank_params.env_2,
            samples_since_note_on,
            note_state,
            sample_rate,
        );
    }
}

#[derive(Debug, Default, Add)]
struct ModulationValues {
    amplitude: f32,
    pitch: f32, // pre-multiplied pitch bend value
    phase: f32,
    warp: f32,
    filter_freq: f32,
}

impl ModulationValues {
    fn from_value(value: f32, send: ModulationSend) -> ModulationValues {
        let (mut amplitude, mut pitch, mut phase, mut warp, mut filter_freq) =
            (0.0, 0.0, 0.0, 0.0, 0.0);
        match send {
            ModulationSend::Amplitude => amplitude = value,
            ModulationSend::Phase => phase = value,
            ModulationSend::Pitch => pitch = value,
            ModulationSend::Warp => warp = value,
            ModulationSend::FilterFreq => filter_freq = value,
        }
        ModulationValues {
            amplitude,
            pitch,
            phase,
            warp,
            filter_freq,
        }
    }

    fn from_mod_bank(
        mod_bank_envs: &ModBankEnvs,
        mod_bank: &ModulationBank,
        time: usize,
        note_state: NoteState,
        sample_rate: SampleRate,
    ) -> ModulationValues {
        let env_1 = mod_bank_envs
            .env_1
            .get(&mod_bank.env_1, time, note_state, sample_rate);
        let env_2 = mod_bank_envs
            .env_2
            .get(&mod_bank.env_2, time, note_state, sample_rate);

        ModulationValues::from_value(env_1, mod_bank.env_1_send)
            + ModulationValues::from_value(env_2, mod_bank.env_2_send)
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
        let result = simple_logging::log_to_file("revisit.log", log::LevelFilter::Info);
        if let Err(err) = result {
            println!("Couldn't start logging! {}", err);
        }
        log::info!("Begin VST log");
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
            parameters: ParameterType::COUNT as i32,
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

        let mut left_out = vec![0.0; num_samples];
        let mut right_out = vec![0.0; num_samples];

        for gen in &mut self.notes {
            for i in 0..num_samples {
                let (left, right) = gen.next_sample(&params, i, self.sample_rate, pitch_bends[i]);
                left_out[i] += left;
                right_out[i] += right;
            }
        }

        // Write sound
        for i in 0..num_samples {
            output_buffer[0][i] = left_out[i] * params.master_vol;
            output_buffer[1][i] = right_out[i] * params.master_vol;
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
                                    self.notes.push(SoundGenerator::new(note, vel, sample_rate));
                                    self.notes.last_mut().unwrap()
                                };

                                gen.note_on(event.delta_frames, vel);
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
