#[macro_use]
extern crate vst;
extern crate log;
extern crate rand;
extern crate simple_logging;
extern crate wmidi;

use std::{convert::TryFrom, sync::Arc};

use log::{info, LevelFilter};
use vst::{
    api::{Events, Supported},
    buffer::AudioBuffer,
    plugin::{CanDo, Category, Info, Plugin, PluginParameters},
    util::AtomicFloat,
};
use wmidi::{MidiMessage, Note, PitchBend, U14, U7};

const TAU: f32 = std::f32::consts::TAU;

// Default values for parameters
const ATTACK: f32 = 0.1;
const DECAY: f32 = 0.2;
const SUSTAIN: f32 = 0.3;
const RELEASE: f32 = 0.3;
const VOLUME: f32 = 0.5;
const SHAPE: f32 = 0.225; // (triangle)
const RETRIGGER_TIME: usize = 88; // 88 samples is about 2 miliseconds
#[derive(Debug, Clone, Copy)]
enum NoteState {
    // The note is being held down
    Held,
    // The note has just been released. Time is in seconds and denotes how many
    // seconds since the oscillator has started. Vel is the velocity that the
    // note was at (will go down to zero)
    Released { time: usize, vel: f32 },
    // The note has just be retriggered during a release. Time is in samples
    // and denotes how many simples it has been since the oscillator has started.
    // Vel is the velocity that the note was (will go down to zero over 10 samples)
    // Notes exit this state after 10 samples.
    Retrigger { time: usize, vel: f32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NoteShape {
    Sine,
    Square,
    Sawtooth,
    Triangle,
}

impl From<f32> for NoteShape {
    fn from(x: f32) -> Self {
        if x < 0.25 {
            NoteShape::Sine
        } else if x < 0.5 {
            NoteShape::Triangle
        } else if x < 0.75 {
            NoteShape::Square
        } else {
            NoteShape::Sawtooth
        }
    }
}

impl std::fmt::Display for NoteShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use NoteShape::*;
        match self {
            Sine => write!(f, "Sine"),
            Square => write!(f, "Square"),
            Sawtooth => write!(f, "Sawtooth"),
            Triangle => write!(f, "Triangle"),
        }
    }
}

#[derive(Debug)]
struct Oscillator {
    pitch: Note,
    vel: U7,
    angle: f32,
    time: usize,
    note_state: NoteState,
}

impl Oscillator {
    fn new(pitch: Note, vel: U7) -> Oscillator {
        Oscillator {
            pitch,
            vel,
            angle: 0.0,
            time: 0,
            note_state: NoteState::Held,
        }
    }

    /// Fills dest with the signal of the oscillator
    fn values(
        &mut self,
        dest: &mut [f32],
        sample_rate: f32,
        envelope: Envelope,
        shape: NoteShape,
        pitch_bend: &[f32],
    ) {
        // What position through the waveform the oscillator is at.
        // Ranges from [0.0, 1.0]
        let mut angle = self.angle;

        for i in 0..dest.len() {
            // The pitch of the note, in hz
            let pitch = Note::to_freq_f32(self.pitch) * pitch_bend[i];

            // Get the raw signal
            let value = match shape {
                // See https://www.desmos.com/calculator/dqg8kdvung for visuals
                NoteShape::Sine => (angle * TAU).sin(),
                NoteShape::Sawtooth => (angle * 2.0) - 1.0,
                NoteShape::Square => {
                    if angle < 0.5 {
                        -1.0
                    } else {
                        1.0
                    }
                }
                NoteShape::Triangle => -2.0 * (2.0 * angle - 1.0).abs() + 1.0,
            };

            // Apply volume envelope
            let value = value * self.envelope(sample_rate, envelope);
            dest[i] = value;

            // Update the angle. Each sample is 1.0 / sample_rate apart for a
            // complete waveform. We multiply by the pitch to scale faster.
            // We also constrain the angle between 0 and 1, as this reduces
            // roundoff error.
            let angle_delta = pitch / sample_rate;
            angle = (angle + angle_delta) % 1.0;

            self.time += 1;

            // If it has been 10 samples in the retrigger state, switch back to
            // the held state. This also resets the time.
            if let NoteState::Retrigger {
                time: retrigger_time,
                vel: _,
            } = self.note_state
            {
                if self.time - retrigger_time > RETRIGGER_TIME {
                    self.note_state = NoteState::Held;
                    self.time = 0;
                }
            }
        }

        // Update the tracked angle. We do it like this instead of adding it all
        // at once as this reduces the error induced by multiplying the large
        // number of samples.
        self.angle = angle;
    }

    /// Get the current envelope multiplier
    fn envelope(&self, sample_rate: f32, envelope: Envelope) -> f32 {
        Oscillator::full_envelope(self.time, self.note_state, sample_rate, envelope)
    }

    /// Release the note
    fn note_off(&mut self, sample_rate: f32, envelope: Envelope) {
        self.note_state = NoteState::Released {
            time: self.time,
            vel: self.envelope(sample_rate, envelope),
        };
    }

    /// Retrigger the note
    fn retrigger(&mut self, sample_rate: f32, envelope: Envelope) {
        self.note_state = NoteState::Retrigger {
            time: self.time,
            vel: self.envelope(sample_rate, envelope),
        };
    }

    /// Returns true if the note is "alive" (playing audio). A note is dead if
    /// it is in the release state and it is after the total release time.
    fn is_alive(&self, sample_rate: f32, release: f32) -> bool {
        match self.note_state {
            NoteState::Held | NoteState::Retrigger { time: _, vel: _ } => true,
            NoteState::Released {
                time: release_time,
                vel: _,
            } => (self.time - release_time) as f32 / sample_rate < release,
        }
    }

    /// Computes the envelope multiplier. Time is the number of samples since the
    /// start of the note. This depends on the state of the note:
    /// Held - do normal attack/decay/sustain envelope
    /// Released - do release envelope, going from the released velocity to zero
    /// Retrigger - do short envelope from retrigger velocity to zero
    fn full_envelope(
        time: usize,
        note_state: NoteState,
        sample_rate: f32,
        envelope: Envelope,
    ) -> f32 {
        match note_state {
            NoteState::Held => ads_env(
                envelope.attack,
                envelope.decay,
                envelope.sustain,
                time as f32 / sample_rate,
            ),
            NoteState::Released {
                time: rel_time,
                vel,
            } => {
                let time = (time - rel_time) as f32 / sample_rate;
                lerp(vel, 0.0, time / envelope.release)
            }
            NoteState::Retrigger {
                time: retrig_time,
                vel,
            } => {
                // Forcibly decay over 10 seconds.
                let time = (time - retrig_time) as f32 / RETRIGGER_TIME as f32;
                lerp(vel, 0.0, time)
            }
        }
    }
}

/// An ADSR envelope.
#[derive(Debug, Clone, Copy)]
struct Envelope {
    // In seconds
    attack: f32,
    // In seconds
    decay: f32,
    // In percent (0.0 to 1.0)
    sustain: f32,
    // In seconds
    release: f32,
}

impl From<&RevisitParameters> for Envelope {
    fn from(params: &RevisitParameters) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack.get());
        let decay = ease_in_expo(params.decay.get());
        let sustain = params.sustain.get();
        let release = ease_in_expo(params.release.get());
        Envelope {
            // Clamp values to around 1 ms minimum.
            // This avoids division by zero problems.
            // Also prevents annoying clicking which is undesirable.
            attack: (attack * 2.0).max(1.0 / 1000.0),
            decay: (decay * 5.0).max(1.0 / 1000.0),
            sustain,
            release: (release * 5.0).max(1.0 / 1000.0),
        }
    }
}

/// The raw parameter values that a host DAW will set and modify.
/// These are unscaled and are always in the [0.0, 1.0] range
struct RevisitParameters {
    attack: AtomicFloat,
    decay: AtomicFloat,
    sustain: AtomicFloat,
    release: AtomicFloat,
    volume: AtomicFloat,
    shape: AtomicFloat,
}

/// The type of parameter. "Error" is included as a convience type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParameterType {
    Attack,
    Decay,
    Sustain,
    Release,
    Volume,
    Shape,
    Error,
}

impl From<i32> for ParameterType {
    fn from(i: i32) -> Self {
        use ParameterType::*;
        match i {
            0 => Attack,
            1 => Decay,
            2 => Sustain,
            3 => Release,
            4 => Volume,
            5 => Shape,
            _ => Error,
        }
    }
}

struct Revisit {
    notes: Vec<Oscillator>,
    sample_rate: f32,
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
            // ADSR + Volume + Note Shape
            parameters: 6,
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
        info!("Processing frame");

        // Get the relevant parameters
        let num_samples = buffer.samples();
        let sample_rate = self.sample_rate;
        let envelope = Envelope::from(self.params.as_ref());
        let volume = self.params.volume.get() * 0.25;
        let shape = NoteShape::from(self.params.shape.get());

        // Sort pitch bend changes by delta_frame.
        self.pitch_bend.sort_unstable_by(|a, b| a.1.cmp(&b.1));

        let pitch_envelope = to_pitch_envelope(&self.pitch_bend, self.last_pitch_bend, num_samples);
        self.last_pitch_bend = *pitch_envelope.last().expect("Pitch envelope was empty?");
        // convert the normalized pitch bends into pitch multipliers
        let pitch_envelope: Vec<f32> = pitch_envelope
            .iter()
            .map(|normalized| to_pitch_multiplier(*normalized, 12))
            .collect();

        self.pitch_bend.clear();

        // remove "dead" notes
        self.notes
            .retain(|osc| osc.is_alive(sample_rate, envelope.release));

        // Get sound for each note
        let (_, mut output_buffer) = buffer.split();

        let mut output = vec![0.0; num_samples];
        let mut osc_buffer = vec![0.0; num_samples];
        for note in &mut self.notes {
            note.values(
                &mut osc_buffer,
                self.sample_rate,
                envelope,
                shape,
                &pitch_envelope,
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
        info!(
            "Processing MIDI events. Total events: {}",
            events.num_events
        );
        let envelope = Envelope::from(self.params.as_ref());
        for event in events.events() {
            match event {
                vst::event::Event::Midi(event) => {
                    let message = MidiMessage::try_from(&event.data as &[u8]);
                    if let Ok(message) = message {
                        match message {
                            MidiMessage::NoteOn(_, pitch, vel) => {
                                // On note on, either add or retrigger the note
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].retrigger(self.sample_rate, envelope);
                                } else {
                                    self.notes.push(Oscillator::new(pitch, vel));
                                }
                                info!(
                                    "NoteOn event! {:?} @ {:?}, frame delta: {}",
                                    pitch, vel, event.delta_frames
                                );
                            }
                            MidiMessage::NoteOff(_, pitch, _) => {
                                // On note off, send note off
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].note_off(self.sample_rate, envelope);
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
    }

    fn stop_process(&mut self) {
        info!("Stopping process...");
    }

    // Needed so that the host knows what parameters exist
    fn get_parameter_object(&mut self) -> Arc<dyn PluginParameters> {
        Arc::clone(&self.params) as Arc<dyn PluginParameters>
    }
}

impl PluginParameters for RevisitParameters {
    fn get_parameter_label(&self, index: i32) -> String {
        use ParameterType::*;
        match index.into() {
            Attack | Decay | Release => " sec".to_string(),
            Sustain | Volume => "%".to_string(),
            Shape => "".to_string(),
            Error => "".to_string(),
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        let envelope = Envelope::from(self);
        use ParameterType::*;
        match index.into() {
            Attack => format!("{:.2}", envelope.attack),
            Decay => format!("{:.2}", envelope.decay),
            Sustain => format!("{:.2}", envelope.sustain * 100.0),
            Release => format!("{:.2}", envelope.release),
            Volume => format!("{:.2}", self.volume.get() * 100.0),
            Shape => format!("{}", NoteShape::from(self.shape.get())),
            Error => "".to_string(),
        }
    }

    fn get_parameter_name(&self, index: i32) -> String {
        use ParameterType::*;
        match index.into() {
            Attack => "Attack".to_string(),
            Decay => "Decay".to_string(),
            Sustain => "Sustain".to_string(),
            Release => "Release".to_string(),
            Volume => "Volume".to_string(),
            Shape => "Note Shape".to_string(),
            Error => "".to_string(),
        }
    }

    fn get_parameter(&self, index: i32) -> f32 {
        use ParameterType::*;
        match index.into() {
            Attack => self.attack.get(),
            Decay => self.decay.get(),
            Sustain => self.sustain.get(),
            Release => self.release.get(),
            Volume => self.volume.get(),
            Shape => self.shape.get(),
            Error => 0.0,
        }
    }

    fn set_parameter(&self, index: i32, value: f32) {
        use ParameterType::*;
        match index.into() {
            Attack => self.attack.set(value),
            Decay => self.decay.set(value),
            Sustain => self.sustain.set(value),
            Release => self.release.set(value),
            Volume => self.volume.set(value),
            Shape => self.shape.set(value),
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

impl Default for Revisit {
    fn default() -> Self {
        Revisit {
            notes: Vec::with_capacity(16),
            sample_rate: 44100.0,
            params: Arc::new(RevisitParameters {
                attack: AtomicFloat::new(ATTACK),
                decay: AtomicFloat::new(DECAY),
                sustain: AtomicFloat::new(SUSTAIN),
                release: AtomicFloat::new(RELEASE),
                volume: AtomicFloat::new(VOLUME),
                shape: AtomicFloat::new(SHAPE),
            }),
            pitch_bend: Vec::with_capacity(16),
            last_pitch_bend: 0.0,
        }
    }
}

// Returns a vector of size num_samples which linearly interpolates between the
// points specified by pitch_bend. last_pitch_bend is assumed to be the "-1th"
// value and is used as the starting point.
fn to_pitch_envelope(
    pitch_bend: &[(f32, i32)],
    last_pitch_bend: f32,
    num_samples: usize,
) -> Vec<f32> {
    fn interpolate_n(start: f32, end: f32, num: usize) -> Vec<f32> {
        (0..num)
            .map(|i| lerp(start, end, i as f32 / num as f32))
            .collect()
    }

    let mut buf = Vec::with_capacity(num_samples);
    let mut start = last_pitch_bend;
    let (mut end, mut num) = match pitch_bend.first() {
        None => (last_pitch_bend, num_samples),
        Some((pitch, num)) => (*pitch, *num as usize),
    };

    buf.append(&mut interpolate_n(start, end, num));
    for i in 0..pitch_bend.len() {
        start = end;
        match pitch_bend.get(i + 1) {
            None => {
                end = start;
                num = num_samples - buf.len();
            }
            Some((pitch, next_i)) => {
                end = *pitch;
                num = *next_i as usize - buf.len();
            }
        }

        buf.append(&mut interpolate_n(start, end, num));
    }
    buf
}

fn normalize_pitch_bend(pitch_bend: PitchBend) -> f32 {
    // A pitchbend is a U14 in range [0, 0x3FFF] with 0x2000 meaning "no bend",
    // 0x0 meaning "max down bend" and 0x3FFF meaning "max up bend".
    // convert to u16 - range [0, 0x3FFF]
    let pitch_bend = U14::data_to_slice(&[pitch_bend])[0];
    // convert to i16 - range [-0x2000, 0x1FFF]
    let pitch_bend = pitch_bend as i16 - 0x2000;
    // convert to f32 - range [-1.0, 1.0]
    pitch_bend as f32 * (1.0 / 0x2000 as f32)
}

// Converts a normalized pitch bend (range [-1.0, 1.0]) to the appropriate pitch
// multiplier.
fn to_pitch_multiplier(normalized_pitch_bend: f32, semitones: i32) -> f32 {
    // Given any note, the note a single semitone away is 2^1/12 times the original note
    // So (2^1/12)^n is n semitones away
    let exponent = 2.0f32.powf(semitones as f32 / 12.0);
    // We take an exponential here because frequency is exponential with respect
    // to note value
    exponent.powf(normalized_pitch_bend)
}

fn ads_env(attack: f32, decay: f32, sustain: f32, time: f32) -> f32 {
    if time < attack {
        // Attack
        time / attack
    } else if time < attack + decay {
        // Decay
        let time = time - attack;
        lerp(1.0, sustain, time / decay)
    } else {
        // Sustain
        sustain
    }
}

fn lerp(start: f32, end: f32, x: f32) -> f32 {
    (end - start) * x.clamp(0.0, 1.0) + start
}

fn ease_in_expo(x: f32) -> f32 {
    if x <= 0.0 {
        0.0
    } else {
        2.0f32.powf(10.0 * x - 10.0)
    }
}

// Make sure you call this, or nothing will happen.
plugin_main!(Revisit);
