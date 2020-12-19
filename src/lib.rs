// `vst` uses macros, so we'll need to specify that we're using them!
#[macro_use]
extern crate vst;
extern crate rand;
extern crate wmidi;

use std::convert::TryFrom;

use vst::{
    api::{Events, Supported},
    buffer::{AudioBuffer, Outputs},
    plugin::{CanDo, Category, Info, Plugin},
};
use wmidi::{MidiMessage, Note, U7};

const TAU: f32 = std::f32::consts::TAU;

struct Oscillator {
    pitch: Note,
    vel: U7,
    angle: f32,
    // time (in samples) since start of note
    time: usize,
    release_state: Option<(usize, f32)>,
}

impl Oscillator {
    fn new(pitch: Note, vel: U7) -> Oscillator {
        Oscillator {
            pitch,
            vel,
            angle: 0.0,
            time: 0,
            release_state: None,
        }
    }

    fn tick_angle(&mut self, num_samples: usize, sample_rate: f32) {
        // Constrain the angle between 0 and 1, reduces roundoff error
        let angle_delta = num_samples as f32 / sample_rate;
        self.angle = (self.angle + angle_delta) % 1.0;
    }

    fn values(&mut self, num_samples: usize, sample_rate: f32) -> Vec<f32> {
        let mut buf = Vec::with_capacity(num_samples);
        let mut angle = self.angle;
        let pitch = Note::to_freq_f32(self.pitch);
        for _ in 0..num_samples {
            // Get the sine signal
            let value = (angle * TAU).sin();
            let value = value * self.envelope(sample_rate);
            buf.push(value);

            // Constrain the angle between 0 and 1, reduces roundoff error
            let angle_delta = pitch / sample_rate;
            angle = (angle + angle_delta) % 1.0;

            self.time += 1;
        }

        self.angle = angle;
        buf
    }

    fn envelope(&self, sample_rate: f32) -> f32 {
        let time = self.time as f32 / sample_rate;
        let attack = 1.0;
        let decay = 1.0;
        let sustain = 0.5;
        let release = 1.0;
        if let Some((release_time, release_vel)) = self.release_state {
            // Release
            let time = (self.time - release_time) as f32 / sample_rate;
            lerp(release_vel, 0.0, time / release)
        } else {
            ads_env(attack, decay, sustain, time)
        }
    }

    fn value(&self) -> f32 {
        (Note::to_freq_f32(self.pitch) * self.angle * TAU).sin()
    }

    fn note_off(&mut self, sample_rate: f32) {
        let vel = ads_env(1.0, 1.0, 0.5, self.time as f32 / sample_rate);
        self.release_state = Some((self.time, vel));
    }

    fn retrigger(&mut self) {
        self.release_state = None;
        self.time = 0;
    }

    fn is_alive(&self, sample_rate: f32) -> bool {
        match self.release_state {
            None => true,
            Some((release_time, _)) => (self.time - release_time) as f32 / sample_rate < 1.0,
        }
    }
}

struct Revisit {
    notes: Vec<Oscillator>,
    sample_rate: f32,
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
        // remove "dead" notes
        let sample_rate = self.sample_rate;
        self.notes.retain(|osc| osc.is_alive(sample_rate));

        let num_samples = buffer.samples();
        let (_, mut output_buffer) = buffer.split();

        let mut output: Vec<f32> = (0..num_samples).map(|_| 0.0).collect();

        for note in &mut self.notes {
            let osc_buffer = note.values(num_samples, self.sample_rate);
            output = output
                .iter()
                .zip(osc_buffer.iter())
                .map(|(&a, &b)| a + b)
                .collect();
        }

        for channel in output_buffer.into_iter() {
            for (i, sample) in channel.iter_mut().enumerate() {
                let volume = 0.25;
                *sample = output[i] * volume;
            }
        }
    }

    fn process_events(&mut self, events: &Events) {
        for event in events.events() {
            match event {
                vst::event::Event::Midi(event) => {
                    let message = MidiMessage::try_from(&event.data as &[u8]);
                    if let Ok(message) = message {
                        match message {
                            MidiMessage::NoteOn(_, pitch, vel) => {
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].retrigger();
                                } else {
                                    self.notes.push(Oscillator::new(pitch, vel));
                                }
                            }
                            MidiMessage::NoteOff(_, pitch, _) => {
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes[i].note_off(self.sample_rate);
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
}

impl Default for Revisit {
    fn default() -> Self {
        Revisit {
            notes: Vec::with_capacity(16),
            sample_rate: 44100.0,
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
    (end - start) * x + start
}

// Make sure you call this, or nothing will happen.
plugin_main!(Revisit);
