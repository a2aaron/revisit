use std::convert::TryFrom;

use crate::{
    ease::{ease_in_expo, DiscreteLinear, Easing},
    generate_raw_params, impl_display, impl_from_i32, impl_get_default, impl_get_ref,
    impl_into_i32, impl_new, impl_set_by_preset,
    presets::{I32Divable, PresetData},
    sound_gen::{Decibel, Envelope, FilterParams, NoteShape, NoteShapeDiscrim},
};

use derive_more::Display;
use variant_count::VariantCount;
use vst::{host::Host, plugin::PluginParameters, util::AtomicFloat};

// Low Pass, High Pass, Bandpass
// All Pass, Notch Filter
// Low Shelf, High Shelf, Peaking EQ
pub const FILTER_TYPE_VARIANT_COUNT: usize = 8;

// Default values for master volume
pub const DEFAULT_MASTER_VOL: f32 = 0.6875; // -3 dB

// Default values for individual oscillator volume
pub const DEFAULT_OSC_VOL: f32 = 0.5833; // -3 dB

// Min and maximum ranges for volume knobs.
pub const MASTER_VOL_MIN_DB: f32 = -36.0;
pub const MASTER_VOL_MAX_DB: f32 = 12.0;
pub const OSC_VOL_MIN_DB: f32 = -24.0;
pub const OSC_VOL_MAX_DB: f32 = 12.0;
pub const SUSTAIN_MIN_DB: f32 = -24.0;

// Default values for volume envelope
pub const DEFAULT_VOL_ATTACK: f32 = 0.1;
pub const DEFAULT_VOL_HOLD: f32 = 0.0;
pub const DEFAULT_VOL_DECAY: f32 = 0.5; // ~200 ms
pub const DEFAULT_VOL_SUSTAIN: f32 = 0.75;
pub const DEFAULT_VOL_RELEASE: f32 = 0.3;

// Default values for modulation envelopes
pub const DEFAULT_ATTACK: f32 = 0.00001;
pub const DEFAULT_HOLD: f32 = 0.2;
pub const DEFAULT_DECAY: f32 = 0.0;
pub const DEFAULT_SUSTAIN: f32 = 0.0;
pub const DEFAULT_RELEASE: f32 = 0.00001;
pub const DEFAULT_MULTIPLY: f32 = 0.5; // +0%

pub const IDENTITY: Easing<f32> = Easing::Linear {
    start: 0.0,
    end: 1.0,
};

pub const BIPOLAR: Easing<f32> = Easing::Linear {
    start: -1.0,
    end: 1.0,
};

pub const EASER: ParamEaser = ParamEaser {
    master_vol: Decibel::ease_db(MASTER_VOL_MIN_DB, MASTER_VOL_MAX_DB),
    osc_vol: Decibel::ease_db(OSC_VOL_MIN_DB, OSC_VOL_MAX_DB),
    vol_sustain: Decibel::ease_db(SUSTAIN_MIN_DB, 0.0),
    phase: IDENTITY,
    pan: BIPOLAR,
    warp: IDENTITY,
    shape: DiscreteLinear {
        values: [
            NoteShapeDiscrim::Sine,
            NoteShapeDiscrim::Skewtooth,
            NoteShapeDiscrim::Square,
            NoteShapeDiscrim::Noise,
        ],
    },
    fine_tune: Easing::Linear {
        start: -100.0,
        end: 100.0,
    },
    coarse_tune: Easing::SteppedLinear {
        start: I32Divable::new(-24),
        end: I32Divable::new(24),
        steps: 24 * 2 + 1,
    },
};

pub struct ParamEaser {
    master_vol: Easing<Decibel>,
    osc_vol: Easing<Decibel>,
    pan: Easing<f32>,
    phase: Easing<f32>,
    shape: DiscreteLinear<NoteShapeDiscrim, { NoteShapeDiscrim::VARIANT_COUNT }>,
    warp: Easing<f32>,
    fine_tune: Easing<f32>,
    coarse_tune: Easing<I32Divable>,
    vol_sustain: Easing<Decibel>,
}

pub struct Parameters {
    pub osc_1: OSCParams,
    pub osc_2: OSCParams,
    pub master_vol: Decibel,
    pub osc_2_mod: ModulationType,
    pub mod_bank: ModulationBank,
}

impl From<&RawParameters> for Parameters {
    fn from(params: &RawParameters) -> Self {
        Parameters {
            osc_1: OSCParams::from(&params.get_osc_1()),
            osc_2: OSCParams::from(&params.get_osc_2()),
            master_vol: EASER.master_vol.ease(params.master_vol.get()),
            osc_2_mod: params.osc_2_mod.get().into(),
            mod_bank: ModulationBank::from(&params.get_mod_bank()),
        }
    }
}

pub struct OSCParams {
    pub volume: Decibel,
    pub shape: NoteShape,
    // A value in [-1.0, 1.0] range. -1.0 means hard pan left. 1.0 means hard
    // pan right. 0.0 means center.
    pub pan: f32,
    // A normalized angle in [0.0, 1.0] range
    pub phase: f32,
    // In semi-tones
    pub coarse_tune: i32,
    // In cents
    pub fine_tune: f32,
    pub vol_adsr: VolEnvParams,
    pub vol_lfo: LFO,
    pub pitch_adsr: GeneralEnvParams,
    pub pitch_lfo: LFO,
    pub filter_params: FilterParams,
}

impl From<&RawOSC> for OSCParams {
    fn from(params: &RawOSC) -> Self {
        OSCParams {
            volume: EASER.osc_vol.ease(params.volume),
            shape: NoteShape::new(EASER.shape.ease(params.shape), EASER.warp.ease(params.warp)),
            pan: EASER.pan.ease(params.pan),
            phase: EASER.phase.ease(params.phase),
            // In semi-tones
            coarse_tune: ((params.coarse_tune - 0.5) * 2.0 * 24.0) as i32,
            // In [-1.0, 1.0] range
            fine_tune: EASER.fine_tune.ease(params.fine_tune),
            vol_adsr: VolEnvParams::from(&params.vol_adsr),
            vol_lfo: LFO {
                amplitude: ease_in_expo(params.vol_lfo.amount),
                period: (ease_in_expo(params.vol_lfo.period) * 10.0).max(0.001),
            },
            pitch_adsr: GeneralEnvParams::from(&params.pitch_adsr),
            pitch_lfo: LFO {
                amplitude: ease_in_expo(params.pitch_lfo.amount) * 0.1,
                period: (ease_in_expo(params.pitch_lfo.period) * 10.0).max(0.001),
            },
            filter_params: FilterParams {
                filter: to_filter_type(params.filter_type, (params.filter_gain - 0.5) * 36.0),
                q_value: (params.filter_q * 10.0).max(0.01),
                freq: params.filter_freq,
            },
        }
    }
}

pub struct ModBankEnvs {
    pub env_1: Envelope<f32>,
    pub env_2: Envelope<f32>,
}

impl ModBankEnvs {
    pub fn new() -> ModBankEnvs {
        ModBankEnvs {
            env_1: Envelope::<f32>::new(),
            env_2: Envelope::<f32>::new(),
        }
    }
}

pub struct ModulationBank {
    pub env_1: GeneralEnvParams,
    pub env_1_send: ModBankSend,
    pub env_2: GeneralEnvParams,
    pub env_2_send: ModBankSend,
}

impl From<&RawModBank> for ModulationBank {
    fn from(params: &RawModBank) -> Self {
        ModulationBank {
            env_1: GeneralEnvParams::from(&params.env_1),
            env_1_send: ModBankSend::from((params.env_1_send, params.env_1_send_to)),
            env_2: GeneralEnvParams::from(&params.env_2),
            env_2_send: ModBankSend::from((params.env_2_send, params.env_2_send_to)),
        }
    }
}

pub struct ModBankSend {
    pub mod_type: ModulationSend,
    pub osc: OSCType,
}

impl From<(f32, f32)> for ModBankSend {
    fn from(x: (f32, f32)) -> Self {
        ModBankSend {
            mod_type: ModulationSend::from(x.0),
            osc: OSCType::from(x.1),
        }
    }
}

// A set of immutable envelope parameters. The envelope is defined as follows:
// - In the attack phase, the envelope value goes from the `zero` value to the
//   `max` value.
// - In the decay phase, the envelope value goes from the `max` value to the
//   `sustain` value.
// - In the sustain phase, the envelope value is constant at the `sustain` value.
// - In the release phase, the envelope value goes from the `sustain` value to
//   `zero` value.
// The envelope value is then scaled by the `multiply` value
pub trait EnvelopeParams<T> {
    // In seconds, how long attack phase is
    fn attack(&self) -> f32;
    // In seconds, how long hold phase is
    fn hold(&self) -> f32;
    // In seconds, how long decay phase is
    fn decay(&self) -> f32;
    // The value to go to during sustain phase
    fn sustain(&self) -> T;
    // In seconds, how long release phase is
    fn release(&self) -> f32;
    // In -1.0 to 1.0 range usually. Multiplied by the value given by the ADSR
    fn multiply(&self) -> f32 {
        1.0
    }
}

/// An ADSR envelope.
#[derive(Debug, Clone, Copy)]
pub struct VolEnvParams {
    pub attack: f32,
    pub hold: f32,
    pub decay: f32,
    pub sustain: Decibel,
    pub release: f32,
}

impl EnvelopeParams<Decibel> for VolEnvParams {
    fn attack(&self) -> f32 {
        self.attack
    }
    fn hold(&self) -> f32 {
        self.hold
    }
    fn decay(&self) -> f32 {
        self.decay
    }
    fn sustain(&self) -> Decibel {
        self.sustain
    }
    fn release(&self) -> f32 {
        self.release
    }
}

impl VolEnvParams {
    fn from(params: &RawEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack);
        let hold = ease_in_expo(params.hold);
        let decay = ease_in_expo(params.decay);
        let release = ease_in_expo(params.release);
        VolEnvParams {
            // Clamp values to around 1 ms minimum.
            // This avoids division by zero problems.
            // Also prevents annoying clicking which is undesirable.
            attack: (attack * 2.0).max(1.0 / 1000.0),
            hold: hold * 5.0,
            decay: (decay * 5.0).max(1.0 / 1000.0),
            sustain: EASER.vol_sustain.ease(params.sustain),
            release: (release * 5.0).max(1.0 / 1000.0),
        }
    }
}

/// An ADSR envelope.
#[derive(Debug, Clone, Copy)]
pub struct GeneralEnvParams {
    // In seconds
    pub attack: f32,
    // In seconds
    pub hold: f32,
    // In seconds
    pub decay: f32,
    // In percent (0.0 to 1.0)
    pub sustain: f32,
    // In seconds
    pub release: f32,
    // In percent (0.0 to 1.0)
    pub multiply: f32,
}

impl EnvelopeParams<f32> for GeneralEnvParams {
    fn attack(&self) -> f32 {
        self.attack
    }
    fn hold(&self) -> f32 {
        self.hold
    }
    fn decay(&self) -> f32 {
        self.decay
    }
    fn sustain(&self) -> f32 {
        self.sustain
    }
    fn release(&self) -> f32 {
        self.release
    }
    fn multiply(&self) -> f32 {
        self.multiply
    }
}

impl From<&RawEnvelope> for GeneralEnvParams {
    fn from(params: &RawEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack);
        let hold = ease_in_expo(params.hold);
        let decay = ease_in_expo(params.decay);
        let sustain = params.sustain;
        let release = ease_in_expo(params.release);
        let multiply = (params.multiply - 0.5) * 2.0;
        GeneralEnvParams {
            // Clamp values to around 1 ms minimum.
            // This avoids division by zero problems.
            // Also prevents annoying clicking which is undesirable.
            attack: (attack * 2.0).max(1.0 / 1000.0),
            hold: hold * 5.0,
            decay: (decay * 5.0).max(1.0 / 1000.0),
            sustain,
            release: (release * 5.0).max(1.0 / 1000.0),
            multiply,
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
pub struct LFO {
    pub amplitude: f32,
    // In seconds
    pub period: f32,
}

impl RawParameters {
    /// Set a parameter, but do not send any GUI knob update. You usually want to
    /// call this if you are working in UI code since the knobs will already update
    /// themselves properly.
    pub fn set(&self, value: f32, parameter: ParameterType) {
        // These are needed so Ableton will notice parameter changes in the
        // "Configure" window.
        // TODO: investigate if I should send this only on mouseup/mousedown
        self.host.begin_edit(parameter.into());

        // Check if warp + shape changes cause the parameter text to change
        let update = match parameter {
            ParameterType::OSC1(inner @ (OSCParameterType::Warp | OSCParameterType::Shape))
            | ParameterType::OSC2(inner @ (OSCParameterType::Warp | OSCParameterType::Shape)) => {
                let osc = match parameter {
                    ParameterType::OSC1(_) => ParameterType::OSC1,
                    ParameterType::OSC2(_) => ParameterType::OSC2,
                    _ => unreachable!(),
                };
                let old_shape = self.get(osc(OSCParameterType::Shape));
                let old_warp = self.get(osc(OSCParameterType::Warp));
                let (new_shape, new_warp) = match inner {
                    OSCParameterType::Shape => (value, old_warp),
                    OSCParameterType::Warp => (old_shape, value),
                    _ => unreachable!(),
                };

                let old_value = format!("{}", NoteShape::from_warp(old_shape, old_warp));
                let new_value = format!("{}", NoteShape::from_warp(new_shape, new_warp));
                old_value != new_value
            }
            _ => false,
        };
        self.get_ref(parameter).set(value);
        if update {
            self.host.update_display();
        }

        self.host.end_edit(parameter.into());
    }

    /// Set a parameter, updating the GUI with it. This is usually only called
    /// when setting a parameter from the host side or internally, such as when
    /// loading a preset. (basically any time that we set a parameter not through
    /// a GUI Message/knob interaction).
    pub fn set_and_update_knob(&self, value: f32, parameter: ParameterType) {
        self.set(value, parameter);

        // Notify the GUI to update its view
        // TODO: Is it really okay to just ignore errors?
        let _ = self.sender.send((parameter, value));
    }

    pub fn get(&self, parameter: ParameterType) -> f32 {
        self.get_ref(parameter).get()
    }

    /// Returns a user-facing text output for the given parameter. This is broken
    /// into a tuple consisting of (`value`, `units`)
    fn get_strings(&self, parameter: ParameterType) -> (String, String) {
        use EnvelopeParam::*;
        use OSCParameterType::*;
        let params = Parameters::from(self);

        fn make_strings(value: f32, label: &str) -> (String, String) {
            (format!("{:.2}", value), label.to_string())
        }

        fn duration_strings(duration: f32) -> (String, String) {
            if duration < 1.0 {
                (format!("{:.1}", duration * 1000.0), " ms".to_string())
            } else {
                (format!("{:.2}", duration), " sec".to_string())
            }
        }

        fn make_pan(pan: f32) -> (String, String) {
            if pan < 0.0 {
                make_strings(-pan * 100.0, "% L")
            } else if pan > 0.0 {
                make_strings(pan * 100.0, "% R")
            } else {
                ("".to_string(), "% C".to_string())
            }
        }

        fn envelope_strings(envelope: GeneralEnvParams, param: EnvelopeParam) -> (String, String) {
            match param {
                Attack => duration_strings(envelope.attack),
                Hold => duration_strings(envelope.hold),
                Decay => duration_strings(envelope.decay),
                Sustain => make_strings(envelope.sustain * 100.0, "%"),
                Release => duration_strings(envelope.release),
                Multiply => make_strings(envelope.multiply * 100.0, "%"),
            }
        }

        fn vol_env_strings(envelope: VolEnvParams, param: EnvelopeParam) -> (String, String) {
            match param {
                Attack => duration_strings(envelope.attack),
                Hold => duration_strings(envelope.hold),
                Decay => duration_strings(envelope.decay),
                Sustain => volume_string(envelope.sustain),
                Release => duration_strings(envelope.release),
                Multiply => make_strings(-9999.0, "IMPOSSIBLE"),
            }
        }

        fn volume_string(decibel: Decibel) -> (String, String) {
            if decibel.get_db() <= crate::sound_gen::NEG_INF_DB_THRESHOLD {
                ("-inf".to_string(), "dB".to_string())
            } else if decibel.get_db() < 0.0 {
                (format!("{:.2}", decibel.get_db()), "dB".to_string())
            } else {
                (format!("+{:.2}", decibel.get_db()), "dB".to_string())
            }
        }

        match parameter {
            ParameterType::MasterVolume => volume_string(params.master_vol),
            ParameterType::OSC1(osc_param) | ParameterType::OSC2(osc_param) => {
                let osc = match parameter {
                    ParameterType::OSC1(_) => &params.osc_1,
                    ParameterType::OSC2(_) => &params.osc_2,
                    _ => unreachable!(),
                };
                match osc_param {
                    Volume => volume_string(osc.volume),
                    Pan => make_pan(osc.pan),
                    Phase => make_strings(osc.phase * 360.0, " deg"),
                    Shape => (format!("{:.2}", osc.shape), "".to_string()),
                    Warp => match osc.shape {
                        NoteShape::Skewtooth(warp) | NoteShape::Square(warp) => {
                            (format!("{:.2}", warp), "".to_string())
                        }
                        NoteShape::Sine | NoteShape::Noise => ("N/A".to_string(), "".to_string()),
                    },
                    CoarseTune => (format!("{}", osc.coarse_tune), " semis".to_string()),
                    FineTune => make_strings(osc.fine_tune, " cents"),
                    VolumeEnv(param) => vol_env_strings(osc.vol_adsr, param),
                    VolLFOAmplitude => make_strings(osc.vol_lfo.amplitude * 100.0, "%"),
                    VolLFOPeriod => duration_strings(osc.vol_lfo.period),
                    PitchEnv(param) => envelope_strings(osc.pitch_adsr, param),
                    PitchLFOAmplitude => make_strings(osc.pitch_lfo.amplitude * 100.0, "%"),
                    PitchLFOPeriod => duration_strings(osc.pitch_lfo.period),
                    FilterType => (biquad_to_string(osc.filter_params.filter), "".to_string()),
                    FilterFreq => make_strings(osc.filter_params.freq, " Hz"),
                    FilterQ => make_strings(osc.filter_params.q_value, ""),
                    FilterGain => match osc.filter_params.filter {
                        biquad::Type::LowShelf(db_gain)
                        | biquad::Type::HighShelf(db_gain)
                        | biquad::Type::PeakingEQ(db_gain) => make_strings(db_gain, " dB"),
                        _ => ("N/A".to_string(), "".to_string()),
                    },
                }
            }
            ParameterType::OSC2Mod => (format!("{}", params.osc_2_mod), "".to_string()),
            ParameterType::ModBank(ModBankParameter::Env1(param)) => {
                envelope_strings(params.mod_bank.env_1, param)
            }
            ParameterType::ModBank(ModBankParameter::Env2(param)) => {
                envelope_strings(params.mod_bank.env_2, param)
            }
            ParameterType::ModBankSend(ModBankType::Env1) => (
                format!(
                    "{} to {}",
                    params.mod_bank.env_1_send.mod_type, params.mod_bank.env_1_send.osc
                ),
                "".to_string(),
            ),
            ParameterType::ModBankSend(ModBankType::Env2) => (
                format!(
                    "{} to {}",
                    params.mod_bank.env_2_send.mod_type, params.mod_bank.env_2_send.osc
                ),
                "".to_string(),
            ),
        }
    }
}

impl PluginParameters for RawParameters {
    fn get_parameter_label(&self, index: i32) -> String {
        if let Ok(parameter) = ParameterType::try_from(index) {
            self.get_strings(parameter).1
        } else {
            "".to_string()
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        if let Ok(parameter) = ParameterType::try_from(index) {
            self.get_strings(parameter).0
        } else {
            "".to_string()
        }
    }

    fn get_parameter_name(&self, index: i32) -> String {
        if let Ok(param) = ParameterType::try_from(index) {
            param.to_string()
        } else {
            "".to_string()
        }
    }

    fn get_parameter(&self, index: i32) -> f32 {
        if let Ok(parameter) = ParameterType::try_from(index) {
            self.get(parameter)
        } else {
            0.0
        }
    }

    fn set_parameter(&self, index: i32, value: f32) {
        if let Ok(parameter) = ParameterType::try_from(index) {
            // This is needed because some VST hosts, such as Ableton, echo a
            // parameter change back to the plugin. This causes issues such as
            // weird knob behavior where the knob "flickers" because the user tries
            // to change the knob value, but ableton keeps sending back old, echoed
            // values.
            #[allow(clippy::float_cmp)]
            if self.get(parameter) == value {
                return;
            }

            // We need to update the GUI since we set the parameter via the host side.
            self.set_and_update_knob(value, parameter);
        }
    }

    fn can_be_automated(&self, index: i32) -> bool {
        ParameterType::try_from(index).is_ok()
    }

    fn string_to_parameter(&self, _index: i32, _text: String) -> bool {
        false
    }
}

/// Oscillator specific parameters. These are normalized f32 which should be
/// baked by calling OSCParams::from()
pub struct RawOSC {
    pub volume: f32,
    pub shape: f32,
    pub pan: f32,
    pub phase: f32,
    pub warp: f32,
    pub coarse_tune: f32,
    pub fine_tune: f32,
    pub vol_adsr: RawEnvelope,
    pub vol_lfo: RawLFO,
    pub pitch_adsr: RawEnvelope,
    pub pitch_lfo: RawLFO,
    pub filter_type: f32,
    pub filter_freq: f32,
    pub filter_q: f32,
    pub filter_gain: f32,
}

// Represents a bank of LFO and envelope modulators.
#[derive(Debug)]
pub struct RawModBank {
    pub env_1: RawEnvelope,
    pub env_1_send: f32,
    pub env_1_send_to: f32,
    pub env_2: RawEnvelope,
    pub env_2_send: f32,
    pub env_2_send_to: f32,
}

/// An enum which represents particular modulator, and then a particular
/// parameter within that modulator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModBankParameter {
    Env1(EnvelopeParam),
    Env2(EnvelopeParam),
}

/// A fieldless version of the ModBankParameter enum. This is used to refer to
/// an entire modulator (independent of the specific parameters). Also, you need
/// to use this in the ParameterType enum, otherwise from_into_int generates too
/// many parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModBankType {
    Env1,
    Env2,
}

/// The location to send a modulation value in the ModulationBank
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, VariantCount)]
pub enum ModulationSend {
    Amplitude,
    Phase,
    Pitch,
    Warp,
    #[display(fmt = "Filter Frequency")]
    FilterFreq,
}

impl From<f32> for ModulationSend {
    fn from(x: f32) -> Self {
        if x < 1.0 / 5.0 {
            ModulationSend::Amplitude
        } else if x < 2.0 / 5.0 {
            ModulationSend::Phase
        } else if x < 3.0 / 5.0 {
            ModulationSend::Pitch
        } else if x < 4.0 / 5.0 {
            ModulationSend::Warp
        } else {
            ModulationSend::FilterFreq
        }
    }
}

impl From<ModulationSend> for f32 {
    fn from(x: ModulationSend) -> Self {
        match x {
            ModulationSend::Amplitude => 0.0 / 4.0,
            ModulationSend::Phase => 1.0 / 4.0,
            ModulationSend::Pitch => 2.0 / 4.0,
            ModulationSend::Warp => 3.0 / 4.0,
            ModulationSend::FilterFreq => 1.0,
        }
    }
}

// Convience struct, represents parameters that are part of an envelope
#[derive(Debug)]
pub struct RawEnvelope {
    pub attack: f32,
    pub hold: f32,
    pub decay: f32,
    pub sustain: f32,
    pub release: f32,
    pub multiply: f32,
}

impl RawEnvelope {
    pub fn get(&self, param: EnvelopeParam) -> f32 {
        match param {
            EnvelopeParam::Attack => self.attack,
            EnvelopeParam::Hold => self.hold,
            EnvelopeParam::Decay => self.decay,
            EnvelopeParam::Sustain => self.sustain,
            EnvelopeParam::Release => self.release,
            EnvelopeParam::Multiply => self.multiply,
        }
    }
}

pub struct RawLFO {
    period: f32,
    amount: f32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OSCParameterType {
    Volume,
    Phase,
    Pan,
    Shape,
    Warp,
    FineTune,
    CoarseTune,
    VolumeEnv(EnvelopeParam),
    VolLFOAmplitude,
    VolLFOPeriod,
    PitchEnv(EnvelopeParam),
    PitchLFOAmplitude,
    PitchLFOPeriod,
    FilterType,
    FilterFreq,
    FilterQ,
    FilterGain,
}

impl std::fmt::Display for OSCParameterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OSCParameterType::*;
        match self {
            Volume => write!(f, "Volume"),
            Phase => write!(f, "Phase"),
            Pan => write!(f, "Pan"),
            Shape => write!(f, "Shape"),
            Warp => write!(f, "Warp"),
            CoarseTune => write!(f, "Coarse Tune"),
            FineTune => write!(f, "Fine Tune"),
            VolumeEnv(param) => write!(f, "{} (Volume)", param),
            VolLFOAmplitude => write!(f, "LFO Amplitude (Volume)"),
            VolLFOPeriod => write!(f, "LFO Period (Volume)"),
            PitchEnv(param) => write!(f, "{} (Pitch)", param),
            PitchLFOAmplitude => write!(f, "LFO Amplitude (Pitch)"),
            PitchLFOPeriod => write!(f, "LFO Period (Pitch)"),
            FilterType => write!(f, "Filter Type"),
            FilterFreq => write!(f, "Filter Frequency"),
            FilterQ => write!(f, "Q-Factor"),
            FilterGain => write!(f, "Filter Gain"),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum EnvelopeParam {
    Attack,
    Decay,
    Hold,
    Sustain,
    Release,
    Multiply,
}

impl EnvelopeParam {
    pub fn get_default(param: EnvelopeParam) -> f32 {
        match param {
            EnvelopeParam::Attack => DEFAULT_ATTACK,
            EnvelopeParam::Decay => DEFAULT_DECAY,
            EnvelopeParam::Hold => DEFAULT_HOLD,
            EnvelopeParam::Sustain => DEFAULT_SUSTAIN,
            EnvelopeParam::Release => DEFAULT_RELEASE,
            EnvelopeParam::Multiply => DEFAULT_MULTIPLY,
        }
    }
}
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
pub enum OSCType {
    OSC1,
    OSC2,
}

impl From<f32> for OSCType {
    fn from(x: f32) -> Self {
        if x < 0.5 {
            OSCType::OSC1
        } else {
            OSCType::OSC2
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, VariantCount)]
pub enum ModulationType {
    Mix,
    #[display(fmt = "Amp. Mod")]
    AmpMod,
    #[display(fmt = "Freq. Mod")]
    FreqMod,
    #[display(fmt = "Phase Mod")]
    PhaseMod,
    #[display(fmt = "Warp Mod")]
    WarpMod,
}

impl From<ModulationType> for f32 {
    fn from(x: ModulationType) -> Self {
        match x {
            ModulationType::Mix => 0.0 / 4.0,
            ModulationType::AmpMod => 1.0 / 4.0,
            ModulationType::FreqMod => 2.0 / 4.0,
            ModulationType::PhaseMod => 3.0 / 4.0,
            ModulationType::WarpMod => 1.0,
        }
    }
}

impl From<f32> for ModulationType {
    fn from(x: f32) -> Self {
        use ModulationType::*;
        if x < 1.0 / 5.0 {
            Mix
        } else if x < 2.0 / 5.0 {
            AmpMod
        } else if x < 3.0 / 5.0 {
            FreqMod
        } else if x < 4.0 / 5.0 {
            PhaseMod
        } else {
            WarpMod
        }
    }
}

/// The type of parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParameterType {
    MasterVolume,
    OSC1(OSCParameterType),
    OSC2Mod,
    OSC2(OSCParameterType),
    ModBankSend(ModBankType),
    ModBank(ModBankParameter),
}

impl From<(OSCParameterType, OSCType)> for ParameterType {
    fn from((param, osc): (OSCParameterType, OSCType)) -> Self {
        match osc {
            OSCType::OSC1 => ParameterType::OSC1(param),
            OSCType::OSC2 => ParameterType::OSC2(param),
        }
    }
}

pub fn to_filter_type(x: f32, db_gain: f32) -> biquad::Type<f32> {
    let x = (x * 8.0) as i32;
    match x {
        0 => biquad::Type::LowPass,
        1 => biquad::Type::HighPass,
        2 => biquad::Type::PeakingEQ(db_gain),
        3 => biquad::Type::LowShelf(db_gain),
        4 => biquad::Type::HighShelf(db_gain),
        5 => biquad::Type::BandPass,
        6 => biquad::Type::Notch,
        _ => biquad::Type::AllPass,
    }
}

pub fn biquad_to_string(x: biquad::Type<f32>) -> String {
    match x {
        biquad::Type::SinglePoleLowPass => "Single Pole Low Pass".to_string(),
        biquad::Type::LowPass => "Low Pass".to_string(),
        biquad::Type::HighPass => "High Pass".to_string(),
        biquad::Type::BandPass => "Band Pass".to_string(),
        biquad::Type::AllPass => "All Pass".to_string(),
        biquad::Type::Notch => "Notch Filter".to_string(),
        biquad::Type::LowShelf(_) => "Low Shelf".to_string(),
        biquad::Type::HighShelf(_) => "High Shelf".to_string(),
        biquad::Type::PeakingEQ(_) => "Peaking EQ".to_string(),
    }
}

macro_rules! table {
    ($macro:ident) => {
        $macro! {
        //  RawParameter identifier, ParameterType identifier
            RawParameters,          ParameterType;
            //  dummy parameters, included only because EnvelopeParam has a multiply
            //  variant but this doesn't make sense for volume envelopes.
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Multiply)),      osc_1_vol_env_multiply,         "OSC 1 Volume Multiply",      -1,   1.0,    NONE,     NONE;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Multiply)),      osc_2_vol_env_multiply,         "OSC 2 Volume Multiply",      -2,   1.0,    NONE,     NONE;
            //  real parameters
            //  variant                                                                     field_name                      name                          idx   default                 easer_field         preset_field
            ParameterType::MasterVolume,                                                    master_vol,                     "Master Volume",              0,    DEFAULT_MASTER_VOL,     master_vol,         master_vol;
            ParameterType::OSC1(OSCParameterType::Volume),                                  osc_1_volume,                   "OSC 1 Volume",               1,    DEFAULT_OSC_VOL,        osc_vol,            osc_1.volume;
            ParameterType::OSC1(OSCParameterType::Phase),                                   osc_1_phase,                    "OSC 1 Phase",                2,    0.0,                    phase,              osc_1.phase;
            ParameterType::OSC1(OSCParameterType::Pan),                                     osc_1_pan,                      "OSC 1 Pan",                  3,    0.5,                    pan,                osc_1.pan;
            ParameterType::OSC1(OSCParameterType::Shape),                                   osc_1_shape,                    "OSC 1 Shape",                4,    0.0,                    shape,              osc_1.shape;
            ParameterType::OSC1(OSCParameterType::Warp),                                    osc_1_warp,                     "OSC 1 Warp",                 5,    0.5,                    warp,               osc_1.warp;
            ParameterType::OSC1(OSCParameterType::FineTune),                                osc_1_fine_tune,                "OSC 1 Fine Tune",            6,    0.5,                    fine_tune,          osc_1.fine_tune;
            ParameterType::OSC1(OSCParameterType::CoarseTune),                              osc_1_coarse_tune,              "OSC 1 Coarse Tune",          7,    0.5,                    coarse_tune,        osc_1.coarse_tune;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Attack)),        osc_1_vol_env_attack,           "OSC 1 Volume Attack",        8,    DEFAULT_VOL_ATTACK,     NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Hold)),          osc_1_vol_env_hold,             "OSC 1 Volume Hold",          9,    DEFAULT_VOL_HOLD,       NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Decay)),         osc_1_vol_env_decay,            "OSC 1 Volume Decay",         10,   DEFAULT_VOL_DECAY,      NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Sustain)),       osc_1_vol_env_sustain,          "OSC 1 Volume Sustain",       11,   DEFAULT_VOL_SUSTAIN,    vol_sustain,        osc_1.vol_sustain;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Release)),       osc_1_vol_env_release,          "OSC 1 Volume Release",       12,   DEFAULT_VOL_RELEASE,    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::VolLFOAmplitude),                         osc_1_vol_lfo_amplitude,        "OSC 1 Vol LFO Amplitude",    13,   0.0,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::VolLFOPeriod),                            osc_1_vol_lfo_period,           "OSC 1 Vol LFO Period",       14,   0.5,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Attack)),         osc_1_pitch_env_attack,         "OSC 1 Pitch Attack",         15,   DEFAULT_ATTACK,         NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Hold)),           osc_1_pitch_env_hold,           "OSC 1 Pitch Hold",           16,   DEFAULT_HOLD,           NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Decay)),          osc_1_pitch_env_decay,          "OSC 1 Pitch Decay",          17,   DEFAULT_DECAY,          NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Sustain)),        osc_1_pitch_env_sustain,        "OSC 1 Pitch Sustain",        18,   DEFAULT_SUSTAIN,        NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Release)),        osc_1_pitch_env_release,        "OSC 1 Pitch Release",        19,   DEFAULT_RELEASE,        NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Multiply)),       osc_1_pitch_env_multiply,       "OSC 1 Pitch Multiply",       20,   DEFAULT_MULTIPLY,       NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchLFOAmplitude),                       osc_1_pitch_lfo_amplitude,      "OSC 1 Pitch LFO Amplitude",  21,   0.0,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::PitchLFOPeriod),                          osc_1_pitch_lfo_period,         "OSC 1 Pitch LFO Period",     22,   0.5,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::FilterType),                              osc_1_filter_type,              "OSC 1 Filter Type",          23,   0.0,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::FilterFreq),                              osc_1_filter_freq,              "OSC 1 Filter Freq",          24,   1.0,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::FilterQ),                                 osc_1_filter_q,                 "OSC 1 Filter Q",             25,   0.1,                    NONE,               NONE;
            ParameterType::OSC1(OSCParameterType::FilterGain),                              osc_1_filter_gain,              "OSC 1 Filter Gain",          26,   0.5,                    NONE,               NONE;
            ParameterType::OSC2Mod,                                                         osc_2_mod,                      "OSC 2 Mod",                  27,   0.0,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::Volume),                                  osc_2_volume,                   "OSC 2 Volume",               28,   DEFAULT_OSC_VOL,        osc_vol,            osc_2.volume;
            ParameterType::OSC2(OSCParameterType::Phase),                                   osc_2_phase,                    "OSC 2 Phase",                29,   0.0,                    phase,              osc_2.phase;
            ParameterType::OSC2(OSCParameterType::Pan),                                     osc_2_pan,                      "OSC 2 Pan",                  30,   0.5,                    pan,                osc_2.pan;
            ParameterType::OSC2(OSCParameterType::Shape),                                   osc_2_shape,                    "OSC 2 Shape",                31,   0.0,                    shape,              osc_2.shape;
            ParameterType::OSC2(OSCParameterType::Warp),                                    osc_2_warp,                     "OSC 2 Warp",                 32,   0.5,                    warp,               osc_2.warp;
            ParameterType::OSC2(OSCParameterType::FineTune),                                osc_2_fine_tune,                "OSC 2 Fine Tune",            33,   0.5,                    fine_tune,          osc_2.fine_tune;
            ParameterType::OSC2(OSCParameterType::CoarseTune),                              osc_2_coarse_tune,              "OSC 2 Coarse Tune",          34,   0.5,                    coarse_tune,        osc_2.coarse_tune;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Attack)),        osc_2_vol_env_attack,           "OSC 2 Volume Attack",        35,   DEFAULT_VOL_ATTACK,     NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Hold)),          osc_2_vol_env_hold,             "OSC 2 Volume Hold",          36,   DEFAULT_VOL_HOLD,       NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Decay)),         osc_2_vol_env_decay,            "OSC 2 Volume Decay",         37,   DEFAULT_VOL_DECAY,      NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Sustain)),       osc_2_vol_env_sustain,          "OSC 2 Volume Sustain",       38,   DEFAULT_VOL_SUSTAIN,    vol_sustain,        osc_2.vol_sustain;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Release)),       osc_2_vol_env_release,          "OSC 2 Volume Release",       39,   DEFAULT_VOL_RELEASE,    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::VolLFOAmplitude),                         osc_2_vol_lfo_amplitude,        "OSC 2 Vol LFO Amplitude",    40,   0.0,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::VolLFOPeriod),                            osc_2_vol_lfo_period,           "OSC 2 Vol LFO Period",       41,   0.5,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Attack)),         osc_2_pitch_env_attack,         "OSC 2 Pitch Attack",         42,   DEFAULT_ATTACK,         NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Hold)),           osc_2_pitch_env_hold,           "OSC 2 Pitch Hold",           43,   DEFAULT_HOLD,           NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Decay)),          osc_2_pitch_env_decay,          "OSC 2 Pitch Decay",          44,   DEFAULT_DECAY,          NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Sustain)),        osc_2_pitch_env_sustain,        "OSC 2 Pitch Sustain",        45,   DEFAULT_SUSTAIN,        NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Release)),        osc_2_pitch_env_release,        "OSC 2 Pitch Release",        46,   DEFAULT_RELEASE,        NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Multiply)),       osc_2_pitch_env_multiply,       "OSC 2 Pitch Multiply",       47,   DEFAULT_MULTIPLY,       NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchLFOAmplitude),                       osc_2_pitch_lfo_amplitude,      "OSC 2 Pitch LFO Amplitude",  48,   0.0,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::PitchLFOPeriod),                          osc_2_pitch_lfo_period,         "OSC 2 Pitch LFO Period",     49,   0.5,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::FilterType),                              osc_2_filter_type,              "OSC 2 Filter Type",          50,   0.0,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::FilterFreq),                              osc_2_filter_freq,              "OSC 2 Filter Freq",          51,   1.0,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::FilterQ),                                 osc_2_filter_q,                 "OSC 2 Filter Q",             52,   0.1,                    NONE,               NONE;
            ParameterType::OSC2(OSCParameterType::FilterGain),                              osc_2_filter_gain,              "OSC 2 Filter Gain",          53,   0.5,                    NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Attack)),          mod_bank_1_attack,              "Mod Bank Env 1 Attack",      54,   DEFAULT_ATTACK,         NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Hold)),            mod_bank_1_hold,                "Mod Bank Env 1 Hold",        55,   DEFAULT_HOLD,           NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Decay)),           mod_bank_1_decay,               "Mod Bank Env 1 Decay",       56,   DEFAULT_DECAY,          NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Sustain)),         mod_bank_1_sustain,             "Mod Bank Env 1 Sustain",     57,   DEFAULT_SUSTAIN,        NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Release)),         mod_bank_1_release,             "Mod Bank Env 1 Release",     58,   DEFAULT_RELEASE,        NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Multiply)),        mod_bank_1_multiply,            "Mod Bank Env 1 Multiply",    59,   DEFAULT_MULTIPLY,       NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Attack)),          mod_bank_2_attack,              "Mod Bank Env 2 Attack",      60,   DEFAULT_ATTACK,         NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Hold)),            mod_bank_2_hold,                "Mod Bank Env 2 Hold",        61,   DEFAULT_HOLD,           NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Decay)),           mod_bank_2_decay,               "Mod Bank Env 2 Decay",       62,   DEFAULT_DECAY,          NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Sustain)),         mod_bank_2_sustain,             "Mod Bank Env 2 Sustain",     63,   DEFAULT_SUSTAIN,        NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Release)),         mod_bank_2_release,             "Mod Bank Env 2 Release",     64,   DEFAULT_RELEASE,        NONE,               NONE;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Multiply)),        mod_bank_2_multiply,            "Mod Bank Env 2 Multiply",    65,   DEFAULT_MULTIPLY,       NONE,               NONE;
            ParameterType::ModBankSend(ModBankType::Env1),                                  mod_bank_1_send,                "Mod Bank Env 1 Send",        66,   0.0,                    NONE,               NONE;
            ParameterType::ModBankSend(ModBankType::Env2),                                  mod_bank_2_send,                "Mod Bank Env 2 Send",        67,   0.0,                    NONE,               NONE;
        }
    };
}

impl ParameterType {
    pub const COUNT: usize = 68;
}

impl RawParameters {
    pub fn get_osc_1(&self) -> RawOSC {
        RawOSC {
            volume: self.osc_1_volume.get(),
            phase: self.osc_1_phase.get(),
            pan: self.osc_1_pan.get(),
            shape: self.osc_1_shape.get(),
            warp: self.osc_1_warp.get(),
            fine_tune: self.osc_1_fine_tune.get(),
            coarse_tune: self.osc_1_coarse_tune.get(),
            vol_adsr: RawEnvelope {
                attack: self.osc_1_vol_env_attack.get(),
                hold: self.osc_1_vol_env_hold.get(),
                decay: self.osc_1_vol_env_decay.get(),
                sustain: self.osc_1_vol_env_sustain.get(),
                release: self.osc_1_vol_env_release.get(),
                multiply: self.osc_1_vol_env_multiply.get(),
            },
            vol_lfo: RawLFO {
                amount: self.osc_1_vol_lfo_amplitude.get(),
                period: self.osc_1_vol_lfo_period.get(),
            },
            pitch_adsr: RawEnvelope {
                attack: self.osc_1_pitch_env_attack.get(),
                hold: self.osc_1_pitch_env_hold.get(),
                decay: self.osc_1_pitch_env_decay.get(),
                sustain: self.osc_1_pitch_env_sustain.get(),
                release: self.osc_1_pitch_env_release.get(),
                multiply: self.osc_1_pitch_env_multiply.get(),
            },
            pitch_lfo: RawLFO {
                amount: self.osc_1_pitch_lfo_amplitude.get(),
                period: self.osc_1_pitch_lfo_period.get(),
            },
            filter_type: self.osc_1_filter_type.get(),
            filter_freq: self.osc_1_filter_freq.get(),
            filter_q: self.osc_1_filter_q.get(),
            filter_gain: self.osc_1_filter_gain.get(),
        }
    }

    pub fn get_osc_2(&self) -> RawOSC {
        RawOSC {
            volume: self.osc_2_volume.get(),
            phase: self.osc_2_phase.get(),
            pan: self.osc_2_pan.get(),
            shape: self.osc_2_shape.get(),
            warp: self.osc_2_warp.get(),
            fine_tune: self.osc_2_fine_tune.get(),
            coarse_tune: self.osc_2_coarse_tune.get(),
            vol_adsr: RawEnvelope {
                attack: self.osc_2_vol_env_attack.get(),
                hold: self.osc_2_vol_env_hold.get(),
                decay: self.osc_2_vol_env_decay.get(),
                sustain: self.osc_2_vol_env_sustain.get(),
                release: self.osc_2_vol_env_release.get(),
                multiply: self.osc_2_vol_env_multiply.get(),
            },
            vol_lfo: RawLFO {
                amount: self.osc_2_vol_lfo_amplitude.get(),
                period: self.osc_2_vol_lfo_period.get(),
            },
            pitch_adsr: RawEnvelope {
                attack: self.osc_2_pitch_env_attack.get(),
                hold: self.osc_2_pitch_env_hold.get(),
                decay: self.osc_2_pitch_env_decay.get(),
                sustain: self.osc_2_pitch_env_sustain.get(),
                release: self.osc_2_pitch_env_release.get(),
                multiply: self.osc_2_pitch_env_multiply.get(),
            },
            pitch_lfo: RawLFO {
                amount: self.osc_2_pitch_lfo_amplitude.get(),
                period: self.osc_2_pitch_lfo_period.get(),
            },
            filter_type: self.osc_2_filter_type.get(),
            filter_freq: self.osc_2_filter_freq.get(),
            filter_q: self.osc_2_filter_q.get(),
            filter_gain: self.osc_2_filter_gain.get(),
        }
    }

    pub fn get_mod_bank(&self) -> RawModBank {
        RawModBank {
            env_1: RawEnvelope {
                attack: self.mod_bank_1_attack.get(),
                hold: self.mod_bank_1_hold.get(),
                decay: self.mod_bank_1_decay.get(),
                sustain: self.mod_bank_1_sustain.get(),
                release: self.mod_bank_1_release.get(),
                multiply: self.mod_bank_1_multiply.get(),
            },
            env_1_send: self.mod_bank_1_send.get(),
            env_1_send_to: 0.0,
            env_2: RawEnvelope {
                attack: self.mod_bank_2_attack.get(),
                hold: self.mod_bank_2_hold.get(),
                decay: self.mod_bank_2_decay.get(),
                sustain: self.mod_bank_2_sustain.get(),
                release: self.mod_bank_2_release.get(),
                multiply: self.mod_bank_2_multiply.get(),
            },
            env_2_send: self.mod_bank_2_send.get(),
            env_2_send_to: 1.0,
        }
    }
}

table! {generate_raw_params}
table! {impl_new}
table! {impl_get_ref}
table! {impl_get_default}
table! {impl_from_i32}
table! {impl_into_i32}
table! {impl_display}
table! {impl_set_by_preset}
