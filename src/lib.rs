// `vst` uses macros, so we'll need to specify that we're using them!
#[macro_use]
extern crate vst;
extern crate rand;
extern crate wmidi;

use std::convert::TryFrom;

use vst::{api::{Events, Supported}, buffer::{AudioBuffer, Outputs}, plugin::{CanDo, Category, Info, Plugin}};
use wmidi::{MidiMessage, Note};

const TAU: f32 = std::f32::consts::TAU;

struct Revisit {
    // Whether or not the VST should play sound
    note: Option<Note>,
    sample_rate: f32,
    // What time the VST is at, in samples
    angle: f32,
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
        let (_, mut output_buffer) = buffer.split();

        // Reset the timer if there isn't any audio being played
        // This avoids issues with round off errors when dealing with high
        // float values.
        // if self.note.is_none() {
            // self.sample_time = 0;
        // }

        // Use a seperate time value for each channel
        // We don't want to use self.time for each channel because we want each
        // channel to use the same time.
        let mut angle = self.angle;
        for channel in output_buffer.into_iter() {
            angle = self.angle;
            for sample in channel {
                let volume = 0.25;
                let signal = match self.note {
                    Some(note) => {
                        let angular_frequency = Note::to_freq_f32(note) / self.sample_rate;
                        // Constrain the angle between 0 and 1, reduces roundoff error
                        angle = (angle + angular_frequency) % 1.0;

                        (angle * TAU).sin()
                    },
                    None => 0.0,
                };
                *sample = signal * volume;
            }
        }
        self.angle = angle;
    }

    fn process_events(&mut self, events: &Events) {
        for event in events.events() {
            match event {
                vst::event::Event::Midi(event) => {
                    let message = MidiMessage::try_from(&event.data as &[u8]);
                    if let Ok(message) = message {
                        match message {
                            MidiMessage::NoteOn(_, note, vel) => {
                                self.note = Some(note);
                            }
                            MidiMessage::NoteOff(_, note, vel) => {
                                self.note = None;
                            }
                            _ => {
                                println!("Unrecognized MidiMessage! {:?}", message)
                            }
                        }
                    }
                }
                _ => {
                    println!("Unrecognized event!")
                }
            }
        }
    }
}


impl Default for Revisit {
    fn default() -> Self {
        Revisit {
            note: None,
            sample_rate: 44100.0,
            angle: 0.0,
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
