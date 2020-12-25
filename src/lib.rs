#[macro_use]
extern crate vst;
extern crate log;
extern crate rand;
extern crate raw_window_handle;
extern crate simple_logging;
extern crate tokio;
extern crate variant_count;
extern crate wmidi;

mod neighbor_pairs;
mod sound_gen;
mod ui;

use std::{convert::TryFrom, sync::Arc};

use iced_baseview::Application;
use sound_gen::{
    ease_in_expo, ease_in_poly, normalize_U7, normalize_pitch_bend, to_pitch_envelope,
    to_pitch_multiplier, AmplitudeADSR, NoteShape, Oscillator, PitchADSR, SampleRate, ADSR,
};

use log::{info, LevelFilter};
use ui::UIFrontEnd;
use variant_count::VariantCount;
use vst::{
    api::{Events, Supported},
    buffer::AudioBuffer,
    editor::Editor,
    host::Host,
    plugin::{CanDo, Category, HostCallback, Info, Plugin, PluginParameters},
    util::AtomicFloat,
};
use wmidi::MidiMessage;

struct SoundGenerator {
    osc: Oscillator,
    fm: Oscillator,
}

struct Revisit {
    notes: Vec<SoundGenerator>,
    sample_rate: SampleRate,
    params: Arc<RawParameters>,
    // (normalized pitchbend value, frame delta)
    pitch_bend: Vec<(f32, i32)>,
    last_pitch_bend: f32,
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
            // Subtract one here due to "error" type
            parameters: (ParameterType::VARIANT_COUNT - 1) as i32,
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
                let (osc, fm) = (&mut gen.osc, &mut gen.fm);
                let vel = params
                    .vol_adsr
                    .get(osc.time, osc.note_state, self.sample_rate);

                let pitch = params
                    .pitch_adsr
                    .get(osc.time, osc.note_state, self.sample_rate)
                    + pitch_bends[i];

                let pitch = to_pitch_multiplier(pitch, 12);

                let fm_pitch = if params.fm_on_off {
                    let pitch = pitch * params.fm_pitch_mult;
                    fm.next_sample(
                        i,
                        self.sample_rate,
                        params.fm_shape,
                        params.fm_vol,
                        pitch,
                        None,
                    )
                } else {
                    0.0
                };

                let pitch = pitch * to_pitch_multiplier(fm_pitch, 24);

                let sample = osc.next_sample(
                    i,
                    self.sample_rate,
                    params.shape,
                    vel,
                    pitch,
                    Some(params.low_pass_alpha),
                );

                output[i] += sample;
            }
        }

        // Write sound
        for channel in output_buffer.into_iter() {
            for (i, sample) in channel.iter_mut().enumerate() {
                *sample = output[i] * params.volume;
            }
        }
    }

    fn process_events(&mut self, events: &Events) {
        let envelope = AmplitudeADSR::from(&self.params.vol_adsr);
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
            .retain(|gen| gen.osc.is_alive(sample_rate, envelope.release));

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
                                let gen = if let Some(osc) =
                                    self.notes.iter_mut().find(|gen| gen.osc.pitch == pitch)
                                {
                                    osc
                                } else {
                                    let fm = Oscillator::new(pitch, 1.0);
                                    let osc = Oscillator::new(pitch, normalize_U7(vel));
                                    self.notes.push(SoundGenerator { osc, fm });
                                    self.notes.last_mut().unwrap()
                                };

                                gen.osc.note_on(event.delta_frames);
                                gen.fm.note_on(event.delta_frames);
                            }
                            MidiMessage::NoteOff(_, pitch, _) => {
                                // On note off, send note off
                                if let Some(i) =
                                    self.notes.iter().position(|gen| gen.osc.pitch == pitch)
                                {
                                    let gen = &mut self.notes[i];
                                    gen.osc.note_off(event.delta_frames);
                                    gen.fm.note_off(event.delta_frames);
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

    fn stop_process(&mut self) {
        info!("Stopping process...");
    }

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

struct Parameters {
    vol_adsr: AmplitudeADSR,
    pitch_adsr: PitchADSR,
    volume: f32,
    shape: NoteShape,
    low_pass_alpha: f32,
    fm_on_off: bool,
    fm_vol: f32,
    fm_pitch_mult: f32,
    fm_shape: NoteShape,
}

impl From<&RawParameters> for Parameters {
    fn from(params: &RawParameters) -> Self {
        Parameters {
            vol_adsr: AmplitudeADSR::from(&params.vol_adsr),
            pitch_adsr: PitchADSR::from(&params.pitch_adsr),
            volume: params.volume.get(),
            shape: NoteShape::from_warp(params.shape.get(), params.warp.get()),
            low_pass_alpha: ease_in_poly(params.low_pass_alpha.get(), 4).clamp(0.0, 1.0),
            fm_on_off: params.fm_on_off.get() > 0.5,
            fm_vol: params.fm_vol.get() * 2.0,
            fm_pitch_mult: to_pitch_multiplier((params.fm_pitch_mult.get() - 0.5) * 2.0, 24),
            fm_shape: NoteShape::from_warp(params.fm_shape.get(), 0.5),
        }
    }
}

impl From<&RawParametersEnvelope> for AmplitudeADSR {
    fn from(params: &RawParametersEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack.get());
        let hold = ease_in_expo(params.hold.get());
        let decay = ease_in_expo(params.decay.get());
        let sustain = params.sustain.get();
        let release = ease_in_expo(params.release.get());
        AmplitudeADSR {
            // Clamp values to around 1 ms minimum.
            // This avoids division by zero problems.
            // Also prevents annoying clicking which is undesirable.
            attack: (attack * 2.0).max(1.0 / 1000.0),
            hold: hold * 5.0,
            decay: (decay * 5.0).max(1.0 / 1000.0),
            sustain,
            release: (release * 5.0).max(1.0 / 1000.0),
        }
    }
}

impl From<&RawParametersEnvelope> for PitchADSR {
    fn from(params: &RawParametersEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack.get());
        let hold = ease_in_expo(params.hold.get());
        let decay = ease_in_expo(params.decay.get());
        let multiply = params.sustain.get();
        let release = ease_in_expo(params.release.get());
        PitchADSR {
            attack: (attack * 2.0).max(1.0 / 10000.0),
            hold: hold * 5.0,
            decay: (decay * 5.0).max(1.0 / 10000.0),
            multiply: (multiply - 0.5) * 2.0,
            release: (release * 5.0).max(1.0 / 10000.0),
        }
    }
}

/// The raw parameter values that a host DAW will set and modify.
/// These are unscaled and are always in the [0.0, 1.0] range
pub struct RawParameters {
    vol_adsr: RawParametersEnvelope,
    pitch_adsr: RawParametersEnvelope,
    volume: AtomicFloat,
    shape: AtomicFloat,
    warp: AtomicFloat,
    low_pass_alpha: AtomicFloat,
    fm_on_off: AtomicFloat,
    fm_vol: AtomicFloat,
    fm_pitch_mult: AtomicFloat,
    fm_shape: AtomicFloat,
    host: HostCallback,
    notify: Arc<tokio::sync::Notify>,
}

impl RawParameters {
    fn get(&self, parameter: ParameterType) -> f32 {
        use ParameterType::*;
        match parameter {
            MasterVolume => self.volume.get(),
            Shape => self.shape.get(),
            Warp => self.warp.get(),
            VolAttack => self.vol_adsr.attack.get(),
            VolHold => self.vol_adsr.hold.get(),
            VolDecay => self.vol_adsr.decay.get(),
            VolSustain => self.vol_adsr.sustain.get(),
            VolRelease => self.vol_adsr.release.get(),
            PitchAttack => self.pitch_adsr.attack.get(),
            PitchHold => self.pitch_adsr.hold.get(),
            PitchDecay => self.pitch_adsr.decay.get(),
            PitchMultiply => self.pitch_adsr.sustain.get(),
            PitchRelease => self.pitch_adsr.release.get(),
            LowPassAlpha => self.low_pass_alpha.get(),
            FMOnOff => self.fm_on_off.get(),
            FMVolume => self.fm_vol.get(),
            FMPitchMultiplier => self.fm_pitch_mult.get(),
            FMShape => self.fm_shape.get(),
            Error => 0.0,
        }
    }

    fn set(&self, value: f32, parameter: ParameterType) {
        use ParameterType::*;
        match parameter {
            MasterVolume => self.volume.set(value),
            Shape => self.shape.set(value),
            Warp => {
                // TODO: this is stupid
                let old_value = format!(
                    "{}",
                    NoteShape::from_warp(self.shape.get(), self.warp.get())
                );
                let new_value = format!("{}", NoteShape::from_warp(self.shape.get(), value));

                self.warp.set(value);

                if old_value != new_value {
                    self.host.update_display();
                }
            }
            VolAttack => self.vol_adsr.attack.set(value),
            VolHold => self.vol_adsr.hold.set(value),
            VolDecay => self.vol_adsr.decay.set(value),
            VolSustain => self.vol_adsr.sustain.set(value),
            VolRelease => self.vol_adsr.release.set(value),
            PitchAttack => self.pitch_adsr.attack.set(value),
            PitchHold => self.pitch_adsr.hold.set(value),
            PitchDecay => self.pitch_adsr.decay.set(value),
            PitchMultiply => self.pitch_adsr.sustain.set(value),
            PitchRelease => self.pitch_adsr.release.set(value),
            LowPassAlpha => self.low_pass_alpha.set(value),
            FMOnOff => self.fm_on_off.set(value),
            FMVolume => self.fm_vol.set(value),
            FMPitchMultiplier => self.fm_pitch_mult.set(value),
            FMShape => self.fm_shape.set(value),
            Error => (),
        }
    }
}

impl PluginParameters for RawParameters {
    fn get_parameter_label(&self, index: i32) -> String {
        use ParameterType::*;
        match index.into() {
            VolAttack | VolHold | VolDecay | VolRelease | PitchAttack | PitchHold | PitchDecay
            | PitchRelease => " sec".to_string(),
            VolSustain | PitchMultiply | MasterVolume | FMVolume => "%".to_string(),
            FMPitchMultiplier => "x".to_string(),
            Shape | Warp | LowPassAlpha | FMOnOff | FMShape => "".to_string(),
            Error => "".to_string(),
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        let params = Parameters::from(self);
        use ParameterType::*;
        match index.into() {
            VolAttack => format!("{:.2}", params.vol_adsr.attack),
            VolHold => format!("{:.2}", params.vol_adsr.hold),
            VolDecay => format!("{:.2}", params.vol_adsr.decay),
            VolSustain => format!("{:.2}", params.vol_adsr.sustain * 100.0),
            VolRelease => format!("{:.2}", params.vol_adsr.release),
            PitchAttack => format!("{:.2}", params.pitch_adsr.attack),
            PitchHold => format!("{:.2}", params.pitch_adsr.hold),
            PitchDecay => format!("{:.2}", params.pitch_adsr.decay),
            PitchMultiply => format!("{:.2}", params.pitch_adsr.multiply * 100.0),
            PitchRelease => format!("{:.2}", params.pitch_adsr.release),
            MasterVolume => format!("{:.2}", params.volume * 100.0),
            Shape => format!("{}", params.shape),
            Warp => format!("{:.2}", self.warp.get()),
            LowPassAlpha => format!("{:.5}", params.low_pass_alpha),
            FMOnOff => if params.fm_on_off { "On" } else { "Off" }.to_string(),
            FMVolume => format!("{:.2}", params.fm_vol * 100.0),
            FMPitchMultiplier => format!("{:.2}", params.fm_pitch_mult),
            FMShape => format!("{}", params.fm_shape),
            Error => "".to_string(),
        }
    }

    fn get_parameter_name(&self, index: i32) -> String {
        use ParameterType::*;
        match index.into() {
            MasterVolume => "Master Volume".to_string(),
            Shape => "Note Shape".to_string(),
            Warp => "Note Warp".to_string(),
            VolAttack => "Attack (Volume)".to_string(),
            VolHold => "Hold (Volume)".to_string(),
            VolDecay => "Decay (Volume)".to_string(),
            VolSustain => "Sustain (Volume)".to_string(),
            VolRelease => "Release (Volume)".to_string(),
            PitchAttack => "Attack (Pitch Bend)".to_string(),
            PitchHold => "Hold (Pitch Bend)".to_string(),
            PitchDecay => "Decay (Pitch Bend)".to_string(),
            PitchMultiply => "Multiply (Pitch Bend)".to_string(),
            PitchRelease => "Release (Pitch Bend)".to_string(),
            LowPassAlpha => "Low Pass Alpha".to_string(),
            FMOnOff => "FM On/Off".to_string(),
            FMVolume => "FM Volume".to_string(),
            FMPitchMultiplier => "FM Pitch Multiplier".to_string(),
            FMShape => "FM Note Shape".to_string(),
            Error => "".to_string(),
        }
    }

    fn get_parameter(&self, index: i32) -> f32 {
        self.get(index.into())
    }

    fn set_parameter(&self, index: i32, value: f32) {
        self.set(value, index.into());
        self.notify.as_ref().notify_one();
    }

    fn can_be_automated(&self, index: i32) -> bool {
        ParameterType::from(index) != ParameterType::Error
    }

    fn string_to_parameter(&self, _index: i32, _text: String) -> bool {
        false
    }
}

impl Default for RawParameters {
    fn default() -> Self {
        RawParameters {
            volume: AtomicFloat::new(0.33),
            shape: AtomicFloat::new(0.225), // Triangle
            warp: AtomicFloat::new(0.5),
            vol_adsr: RawParametersEnvelope {
                attack: AtomicFloat::new(0.1),
                hold: AtomicFloat::new(0.0),
                decay: AtomicFloat::new(0.2),
                sustain: AtomicFloat::new(0.5),
                release: AtomicFloat::new(0.3),
            },
            pitch_adsr: RawParametersEnvelope {
                attack: AtomicFloat::new(1.0 / 10000.0),
                hold: AtomicFloat::new(0.0),
                decay: AtomicFloat::new(0.2),
                sustain: AtomicFloat::new(0.5),
                release: AtomicFloat::new(1.0 / 10000.0),
            },
            low_pass_alpha: AtomicFloat::new(0.5),
            fm_on_off: AtomicFloat::new(0.0),     // Off
            fm_vol: AtomicFloat::new(0.5),        // 100%
            fm_pitch_mult: AtomicFloat::new(0.5), // 1x
            fm_shape: AtomicFloat::new(0.0),      // Sine
            host: Default::default(),
            notify: Arc::new(tokio::sync::Notify::new()),
        }
    }
}

// Convience struct, represents parameters that are part of an envelope
pub struct RawParametersEnvelope {
    attack: AtomicFloat,
    hold: AtomicFloat,
    decay: AtomicFloat,
    sustain: AtomicFloat,
    release: AtomicFloat,
}

/// The type of parameter. "Error" is included as a convience type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, VariantCount)]
pub enum ParameterType {
    MasterVolume,
    Shape,
    Warp,
    VolAttack,
    VolHold,
    VolDecay,
    VolSustain,
    VolRelease,
    PitchAttack,
    PitchHold,
    PitchDecay,
    PitchMultiply,
    PitchRelease,
    LowPassAlpha,
    FMOnOff,
    FMVolume,
    FMPitchMultiplier,
    FMShape,
    Error,
}

impl From<i32> for ParameterType {
    fn from(i: i32) -> Self {
        use ParameterType::*;
        match i {
            0 => MasterVolume,
            1 => Shape,
            2 => Warp,
            3 => VolAttack,
            4 => VolHold,
            5 => VolDecay,
            6 => VolSustain,
            7 => VolRelease,
            8 => PitchAttack,
            9 => PitchHold,
            10 => PitchDecay,
            11 => PitchMultiply,
            12 => PitchRelease,
            13 => LowPassAlpha,
            14 => FMOnOff,
            15 => FMVolume,
            16 => FMPitchMultiplier,
            17 => FMShape,
            _ => Error,
        }
    }
}

// Export symbols for main
plugin_main!(Revisit);
