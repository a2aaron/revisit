// `vst` uses macros, so we'll need to specify that we're using them!
#[macro_use]
extern crate vst;
extern crate rand;

use vst::{buffer::AudioBuffer, plugin::{Category, Info, Plugin}};

#[derive(Default)]
struct Revisit;

// We're implementing a trait `Plugin` that does all the VST-y stuff for us.
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

    fn process(&mut self, buffer: &mut AudioBuffer<f32>) {
        let (_, mut output_buffer) = buffer.split();
        for channel in output_buffer.into_iter() {
            for sample in channel {
                *sample = (rand::random::<f32>() - 0.5) * 0.1;
            }
        }
    }
}

// Make sure you call this, or nothing will happen.
plugin_main!(Revisit); 
