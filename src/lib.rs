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
}

impl Oscillator {
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
            let value = (angle * TAU).sin();
            buf.push(value);

            // Constrain the angle between 0 and 1, reduces roundoff error
            let angle_delta = pitch / sample_rate;
            angle = (angle + angle_delta) % 1.0;
        }

        self.angle = angle;
        buf
    }

    fn value(&self) -> f32 {
        (Note::to_freq_f32(self.pitch) * self.angle * TAU).sin()
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
                                self.notes.push(Oscillator {
                                    pitch,
                                    vel,
                                    angle: 0.0,
                                });
                            }
                            MidiMessage::NoteOff(_, pitch, vel) => {
                                if let Some(i) = self.notes.iter().position(|x| x.pitch == pitch) {
                                    self.notes.remove(i);
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

// Make sure you call this, or nothing will happen.
plugin_main!(Revisit);
