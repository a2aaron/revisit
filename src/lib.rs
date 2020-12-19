#![feature(clamp)]

#[macro_use]
extern crate vst;
extern crate rand;
extern crate wmidi;

use std::{convert::TryFrom, sync::Arc};

use vst::{
    api::{Events, Supported},
    buffer::{AudioBuffer, Outputs},
    plugin::{CanDo, Category, Info, Plugin, PluginParameters},
    util::AtomicFloat,
};
use wmidi::{MidiMessage, Note, U7};

const TAU: f32 = std::f32::consts::TAU;

const ATTACK: f32 = 10.0 / 1000.0;
const DECAY: f32 = 50.0 / 1000.0;
const SUSTAIN: f32 = 0.3;
const RELEASE: f32 = 0.3;
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

    /// Get num_samples worth of samples from the Oscillator
    fn values(&mut self, num_samples: usize, sample_rate: f32, envelope: Envelope) -> Vec<f32> {
        let mut buf = Vec::with_capacity(num_samples);
        let mut angle = self.angle;
        let pitch = Note::to_freq_f32(self.pitch);
        for _ in 0..num_samples {
            // Get the sine signal
            let value = (angle * TAU).sin();
            let value = value * self.envelope(sample_rate, envelope);
            buf.push(value);

            // Constrain the angle between 0 and 1, reduces roundoff error
            let angle_delta = pitch / sample_rate;
            angle = (angle + angle_delta) % 1.0;

            self.time += 1;

            // If it has been 10 samples in the retrigger state, switch back to
            // the held state.
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

        self.angle = angle;
        buf
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
    /// start of the note.
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

struct RevisitParameters {
    attack: AtomicFloat,
    decay: AtomicFloat,
    sustain: AtomicFloat,
    release: AtomicFloat,
}

struct Revisit {
    notes: Vec<Oscillator>,
    sample_rate: f32,
    envelope: Arc<RevisitParameters>,
}

impl Plugin for Revisit {
    fn get_info(&self) -> Info {
        Info {
            name: "Revisit".to_string(),
            vendor: "a2aaron".to_string(),
            // Used by hosts to differentiate between plugins.
            // Don't worry much about this now - just fill in a random number.
            unique_id: 413612,
            version: 1,
            category: Category::Synth,
            // 4 parameters -- ADSR
            parameters: 4,
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
        let envelope = Envelope::from(self.envelope.as_ref());

        // remove "dead" notes
        let sample_rate = self.sample_rate;
        self.notes
            .retain(|osc| osc.is_alive(sample_rate, envelope.release));

        let num_samples = buffer.samples();
        let (_, mut output_buffer) = buffer.split();

        let mut output: Vec<f32> = (0..num_samples).map(|_| 0.0).collect();

        for note in &mut self.notes {
            let osc_buffer = note.values(num_samples, self.sample_rate, envelope);
            output = output
                .iter()
                .zip(osc_buffer.iter())
                .map(|(&a, &b)| a + b)
                .collect();
        }

        for channel in output_buffer.into_iter() {
            for (i, sample) in channel.iter_mut().enumerate() {
                let volume = 1.0;
                *sample = output[i] * volume;
            }
        }
    }

    fn process_events(&mut self, events: &Events) {
        let envelope = Envelope::from(self.envelope.as_ref());
        for event in events.events() {
            match event {
                vst::event::Event::Midi(event) => {
                    let message = MidiMessage::try_from(&event.data as &[u8]);
                    if let Ok(message) = message {
                        match message {
                            MidiMessage::NoteOn(_, pitch, vel) => {
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].retrigger(self.sample_rate, envelope);
                                } else {
                                    self.notes.push(Oscillator::new(pitch, vel));
                                }
                            }
                            MidiMessage::NoteOff(_, pitch, _) => {
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].note_off(self.sample_rate, envelope);
                                }
                            }
                            _ => println!("Unrecognized MidiMessage! {:?}", message),
                        }
                    }
                }
                _ => println!("Unrecognized event!"),
            }
        }
    }

    fn get_parameter_object(&mut self) -> Arc<dyn PluginParameters> {
        Arc::clone(&self.envelope) as Arc<dyn PluginParameters>
    }
}

impl PluginParameters for RevisitParameters {
    fn get_parameter_label(&self, index: i32) -> String {
        match index {
            0 | 1 | 3 => " sec".to_string(),
            2 => "%".to_string(),
            _ => "".to_string(),
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        let envelope = Envelope::from(self);
        match index {
            0 => format!("{:.2}", envelope.attack),
            1 => format!("{:.2}", envelope.decay),
            2 => format!("{:.2}", envelope.sustain * 100.0),
            3 => format!("{:.2}", envelope.release),
            _ => "".to_string(),
        }
    }

    fn get_parameter_name(&self, index: i32) -> String {
        match index {
            0 => "Attack".to_string(),
            1 => "Decay".to_string(),
            2 => "Sustain".to_string(),
            3 => "Release".to_string(),
            _ => "".to_string(),
        }
    }

    fn get_parameter(&self, index: i32) -> f32 {
        match index {
            0 => self.attack.get(),
            1 => self.decay.get(),
            2 => self.sustain.get(),
            3 => self.release.get(),
            _ => 0.0,
        }
    }

    fn set_parameter(&self, index: i32, value: f32) {
        match index {
            0 => self.attack.set(value),
            1 => self.decay.set(value),
            2 => self.sustain.set(value),
            3 => self.release.set(value),
            _ => (),
        }
    }

    fn can_be_automated(&self, index: i32) -> bool {
        matches!(index, 0 | 1 | 2 | 3)
    }

    fn string_to_parameter(&self, index: i32, text: String) -> bool {
        false
    }
}

impl Default for Revisit {
    fn default() -> Self {
        Revisit {
            notes: Vec::with_capacity(16),
            sample_rate: 44100.0,
            envelope: Arc::new(RevisitParameters {
                attack: AtomicFloat::new(ATTACK),
                decay: AtomicFloat::new(DECAY),
                sustain: AtomicFloat::new(SUSTAIN),
                release: AtomicFloat::new(RELEASE),
            }),
        }
    }
}

fn sample_time_to_f32(sample_time: usize, sample_rate: f32) -> f32 {
    sample_time as f32 / sample_rate
}

fn clear_output_buffer(output: &mut Outputs<f32>) {
    for channel in output.into_iter() {
        for sample in channel {
            *sample = 0.0;
        }
    }
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
