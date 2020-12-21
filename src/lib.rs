#[macro_use]
extern crate vst;
extern crate log;
extern crate rand;
extern crate simple_logging;
extern crate wmidi;

mod neighbor_pairs;
mod sound_gen;

use sound_gen::{
    normalize_pitch_bend, to_pitch_envelope, AmplitudeADSR, NoteShape, Oscillator, PitchADSR,
    SampleRate, ADSR,
};

use std::{convert::TryFrom, sync::Arc};

use log::{info, LevelFilter};
use vst::{
    api::{Events, Supported},
    buffer::AudioBuffer,
    plugin::{CanDo, Category, Info, Plugin, PluginParameters},
    util::AtomicFloat,
};
use wmidi::MidiMessage;

// Default values for parameters
const VOLUME: f32 = 0.33;
const SHAPE: f32 = 0.225; // (triangle)
const VOL_ATTACK: f32 = 0.1;
const VOL_DECAY: f32 = 0.2;
const VOL_SUSTAIN: f32 = 0.5;
const VOL_RELEASE: f32 = 0.3;
const PITCH_ATTACK: f32 = 1.0 / 10000.0;
const PITCH_DECAY: f32 = 0.2;
const PITCH_MULTIPLY: f32 = 0.5;
const PITCH_RELEASE: f32 = 1.0 / 10000.0;
const LOW_PASS_ALPHA: f32 = 0.5;

struct Revisit {
    notes: Vec<Oscillator>,
    sample_rate: SampleRate,
    params: Arc<RevisitParameters>,
    // (normalized pitchbend value, frame delta)
    pitch_bend: Vec<(f32, i32)>,
    last_pitch_bend: f32,
}

impl Plugin for Revisit {
    fn init(&mut self) {
        let result = simple_logging::log_to_file("revisit.log", LevelFilter::Info);
        if let Err(err) = result {
            println!("Couldn't start logging! {}", err);
        }
        info!("Begin VST log");
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
            // Volume + Note Shape + Volume ADSR + Pitch ADSR + LowPassAlpha
            parameters: 1 + 1 + 4 + 4 + 1,
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
        if !self.notes.is_empty() {
            info!("Processing frame, total notes: {}", self.notes.len());
        }

        // Get the relevant parameters
        let num_samples = buffer.samples();
        let vol_adsr = AmplitudeADSR::from_params(&self.params.vol_env);
        let volume = self.params.volume.get();

        let pitch_adsr = PitchADSR::from_params(&self.params.pitch_env);
        let shape = NoteShape::from(self.params.shape.get());

        let low_pass_alpha = self.params.low_pass_alpha.get();

        // Get the envelope from MIDI pitch bend
        let (pitch_bends, last_bend) =
            to_pitch_envelope(&self.pitch_bend, self.last_pitch_bend, num_samples);
        self.last_pitch_bend = last_bend;

        let pitch_bends: Vec<f32> = pitch_bends.collect();

        // Get sound for each note
        let (_, mut output_buffer) = buffer.split();

        let mut output = vec![0.0; num_samples];
        let mut osc_buffer = vec![0.0; num_samples];
        for note in &mut self.notes {
            note.values(
                &mut osc_buffer,
                self.sample_rate,
                vol_adsr,
                pitch_adsr,
                shape,
                &pitch_bends,
                low_pass_alpha,
            );

            output = output
                .iter()
                .zip(osc_buffer.iter())
                .map(|(&a, &b)| a + b)
                .collect();
        }

        // Write sound
        for channel in output_buffer.into_iter() {
            for (i, sample) in channel.iter_mut().enumerate() {
                *sample = output[i] * volume;
            }
        }
    }

    fn process_events(&mut self, events: &Events) {
        if events.num_events > 0 {
            info!(
                "Processing MIDI events. Total events: {}",
                events.num_events
            );
        }
        let envelope = AmplitudeADSR::from_params(&self.params.vol_env);
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
        self.notes
            .retain(|osc| osc.is_alive(sample_rate, envelope.release));

        // Clear pitch bend to get new messages
        self.pitch_bend.clear();
        for event in events.events() {
            match event {
                vst::event::Event::Midi(event) => {
                    let message = MidiMessage::try_from(&event.data as &[u8]);
                    if let Ok(message) = message {
                        match message {
                            MidiMessage::NoteOn(_, pitch, vel) => {
                                // On note on, either add or retrigger the note
                                let osc = if let Some(osc) =
                                    self.notes.iter_mut().find(|x| x.pitch == pitch)
                                {
                                    osc
                                } else {
                                    let osc = Oscillator::new(pitch, vel);
                                    self.notes.push(osc);
                                    self.notes.last_mut().unwrap()
                                };

                                osc.note_on(event.delta_frames);
                                info!(
                                    "NoteOn event! {:?} @ {:?}, frame delta: {}",
                                    pitch, vel, event.delta_frames
                                );
                            }
                            MidiMessage::NoteOff(_, pitch, _) => {
                                // On note off, send note off
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].note_off(event.delta_frames);
                                }
                                info!(
                                    "NoteOff event! {:?}, frame delta: {}",
                                    pitch, event.delta_frames
                                );
                            }
                            MidiMessage::PitchBendChange(_, pitch_bend) => {
                                self.pitch_bend
                                    .push((normalize_pitch_bend(pitch_bend), event.delta_frames));
                                info!(
                                    "Pitch Bend event! {:?} frame delta: {}",
                                    normalize_pitch_bend(pitch_bend),
                                    event.delta_frames
                                );
                            }
                            _ => info!("Unrecognized MidiMessage! {:?}", message),
                        }
                    }
                }
                _ => info!("Unrecognized event!"),
            }
        }

        // Sort pitch bend changes by delta_frame.
        self.pitch_bend.sort_unstable_by(|a, b| a.1.cmp(&b.1));
    }

    fn stop_process(&mut self) {
        info!("Stopping process...");
    }

    fn set_sample_rate(&mut self, rate: f32) {
        self.sample_rate = rate;
    }

    // Needed so that the host knows what parameters exist
    fn get_parameter_object(&mut self) -> Arc<dyn PluginParameters> {
        Arc::clone(&self.params) as Arc<dyn PluginParameters>
    }
}

impl Default for Revisit {
    fn default() -> Self {
        Revisit {
            notes: Vec::with_capacity(16),
            sample_rate: 44100.0,
            params: Arc::new(RevisitParameters {
                vol_env: RevisitParametersEnvelope {
                    attack: AtomicFloat::new(VOL_ATTACK),
                    decay: AtomicFloat::new(VOL_DECAY),
                    sustain: AtomicFloat::new(VOL_SUSTAIN),
                    release: AtomicFloat::new(VOL_RELEASE),
                },
                pitch_env: RevisitParametersEnvelope {
                    attack: AtomicFloat::new(PITCH_ATTACK),
                    decay: AtomicFloat::new(PITCH_DECAY),
                    sustain: AtomicFloat::new(PITCH_MULTIPLY),
                    release: AtomicFloat::new(PITCH_RELEASE),
                },
                volume: AtomicFloat::new(VOLUME),
                shape: AtomicFloat::new(SHAPE),
                low_pass_alpha: AtomicFloat::new(LOW_PASS_ALPHA),
            }),
            pitch_bend: Vec::with_capacity(16),
            last_pitch_bend: 0.0,
        }
    }
}

/// The raw parameter values that a host DAW will set and modify.
/// These are unscaled and are always in the [0.0, 1.0] range
struct RevisitParameters {
    vol_env: RevisitParametersEnvelope,
    pitch_env: RevisitParametersEnvelope,
    volume: AtomicFloat,
    shape: AtomicFloat,
    low_pass_alpha: AtomicFloat,
}

// Convience struct, represents parameters that are part of an envelope
pub struct RevisitParametersEnvelope {
    attack: AtomicFloat,
    decay: AtomicFloat,
    sustain: AtomicFloat,
    release: AtomicFloat,
}

/// The type of parameter. "Error" is included as a convience type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParameterType {
    Volume,
    Shape,
    VolAttack,
    VolDecay,
    VolSustain,
    VolRelease,
    PitchAttack,
    PitchDecay,
    PitchMultiply,
    PitchRelease,
    LowPassAlpha,
    Error,
}

impl From<i32> for ParameterType {
    fn from(i: i32) -> Self {
        use ParameterType::*;
        match i {
            0 => Volume,
            1 => Shape,
            2 => VolAttack,
            3 => VolDecay,
            4 => VolSustain,
            5 => VolRelease,
            6 => PitchAttack,
            7 => PitchDecay,
            8 => PitchMultiply,
            9 => PitchRelease,
            10 => LowPassAlpha,
            _ => Error,
        }
    }
}

impl PluginParameters for RevisitParameters {
    fn get_parameter_label(&self, index: i32) -> String {
        use ParameterType::*;
        match index.into() {
            VolAttack | VolDecay | VolRelease | PitchAttack | PitchDecay | PitchRelease => {
                " sec".to_string()
            }
            VolSustain | PitchMultiply | Volume => "%".to_string(),
            Shape | LowPassAlpha => "".to_string(),
            Error => "".to_string(),
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        let vol_env = AmplitudeADSR::from_params(&self.vol_env);
        let pitch_env = PitchADSR::from_params(&self.pitch_env);
        use ParameterType::*;
        match index.into() {
            VolAttack => format!("{:.2}", vol_env.attack),
            VolDecay => format!("{:.2}", vol_env.decay),
            VolSustain => format!("{:.2}", vol_env.sustain * 100.0),
            VolRelease => format!("{:.2}", vol_env.release),
            PitchAttack => format!("{:.2}", pitch_env.attack),
            PitchDecay => format!("{:.2}", pitch_env.decay),
            PitchMultiply => format!("{:.2}", pitch_env.multiply * 100.0),
            PitchRelease => format!("{:.2}", pitch_env.release),
            Volume => format!("{:.2}", self.volume.get() * 100.0),
            Shape => format!("{}", NoteShape::from(self.shape.get())),
            LowPassAlpha => format!("{}", self.low_pass_alpha.get()),
            Error => "".to_string(),
        }
    }

    fn get_parameter_name(&self, index: i32) -> String {
        use ParameterType::*;
        match index.into() {
            Volume => "Volume".to_string(),
            Shape => "Note Shape".to_string(),
            VolAttack => "Attack (Volume)".to_string(),
            VolDecay => "Decay (Volume)".to_string(),
            VolSustain => "Sustain (Volume)".to_string(),
            VolRelease => "Release (Volume)".to_string(),
            PitchAttack => "Attack (Pitch Bend)".to_string(),
            PitchDecay => "Decay (Pitch Bend)".to_string(),
            PitchMultiply => "Multiply (Pitch Bend)".to_string(),
            PitchRelease => "Release (Pitch Bend)".to_string(),
            LowPassAlpha => "Low Pass Alpha".to_string(),
            Error => "".to_string(),
        }
    }

    fn get_parameter(&self, index: i32) -> f32 {
        use ParameterType::*;
        match index.into() {
            Volume => self.volume.get(),
            Shape => self.shape.get(),
            VolAttack => self.vol_env.attack.get(),
            VolDecay => self.vol_env.decay.get(),
            VolSustain => self.vol_env.sustain.get(),
            VolRelease => self.vol_env.release.get(),
            PitchAttack => self.pitch_env.attack.get(),
            PitchDecay => self.pitch_env.decay.get(),
            PitchMultiply => self.pitch_env.sustain.get(),
            PitchRelease => self.pitch_env.release.get(),
            LowPassAlpha => self.low_pass_alpha.get(),
            Error => 0.0,
        }
    }

    fn set_parameter(&self, index: i32, value: f32) {
        use ParameterType::*;
        match index.into() {
            Volume => self.volume.set(value),
            Shape => self.shape.set(value),
            VolAttack => self.vol_env.attack.set(value),
            VolDecay => self.vol_env.decay.set(value),
            VolSustain => self.vol_env.sustain.set(value),
            VolRelease => self.vol_env.release.set(value),
            PitchAttack => self.pitch_env.attack.set(value),
            PitchDecay => self.pitch_env.decay.set(value),
            PitchMultiply => self.pitch_env.sustain.set(value),
            PitchRelease => self.pitch_env.release.set(value),
            LowPassAlpha => self.low_pass_alpha.set(value),
            Error => (),
        }
    }

    fn can_be_automated(&self, index: i32) -> bool {
        ParameterType::from(index) != ParameterType::Error
    }

    fn string_to_parameter(&self, _index: i32, _text: String) -> bool {
        false
    }
}

// Export symbols for main
plugin_main!(Revisit);
