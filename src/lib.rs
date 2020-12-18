// `vst` uses macros, so we'll need to specify that we're using them!
#[macro_use]
extern crate vst;
extern crate rand;
extern crate wmidi;

use std::convert::TryFrom;

use vst::{api::{Events, Supported}, buffer::{AudioBuffer, Outputs}, plugin::{CanDo, Category, Info, Plugin}};
use wmidi::{MidiMessage, Note};

#[derive(Default)]
struct Revisit {
    play: bool,
}

// We're implementing a trait `Plugin` that does all the VST-y stuff for us.
impl Plugin for Revisit {
    fn init(&mut self) {
        self.play = false;
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

    fn process(&mut self, buffer: &mut AudioBuffer<f32>) {
        let (_, mut output_buffer) = buffer.split();

        if !self.play {
            clear_output_buffer(&mut output_buffer);
            return;
        }

        for channel in output_buffer.into_iter() {
            for sample in channel {
                *sample = (rand::random::<f32>() - 0.5) * 0.1;
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
                            MidiMessage::NoteOn(_, note, vel) => {
                                self.play = true;
                            }
                            MidiMessage::NoteOff(_, note, vel) => {
                                self.play = false;
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

fn clear_output_buffer(output: &mut Outputs<f32>) {
    for channel in output.into_iter() {
        for sample in channel {
            *sample = 0.0;
        }
    }
}

// Make sure you call this, or nothing will happen.
plugin_main!(Revisit); 
