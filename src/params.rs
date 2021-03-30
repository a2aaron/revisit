use std::convert::TryFrom;

use crate::{
    impl_display, impl_from_i32, impl_get_default, impl_into_i32,
    sound_gen::{ease_in_expo, Envelope, FilterParams, NoteShape},
};

use derive_more::Display;
use variant_count::VariantCount;
use vst::{
    host::Host,
    plugin::{HostCallback, PluginParameters},
    util::AtomicFloat,
};

// Low Pass, High Pass, Bandpass
// All Pass, Notch Filter
// Low Shelf, High Shelf, Peaking EQ
pub const FILTER_TYPE_VARIANT_COUNT: usize = 8;

// Default values for volume envelope
pub const DEFAULT_VOL_ATTACK: f32 = 0.1;
pub const DEFAULT_VOL_HOLD: f32 = 0.0;
pub const DEFAULT_VOL_DECAY: f32 = 0.2;
pub const DEFAULT_VOL_SUSTAIN: f32 = 0.5;
pub const DEFAULT_VOL_RELEASE: f32 = 0.3;
pub const DEFAULT_VOL_MULTIPLY: f32 = 1.0; // +100%

// Default values for modulation envelopes
pub const DEFAULT_ATTACK: f32 = 0.00001;
pub const DEFAULT_HOLD: f32 = 0.2;
pub const DEFAULT_DECAY: f32 = 0.0;
pub const DEFAULT_SUSTAIN: f32 = 0.0;
pub const DEFAULT_RELEASE: f32 = 0.00001;
pub const DEFAULT_MULTIPLY: f32 = 0.5; // +0%

pub struct Parameters {
    pub osc_1: OSCParams,
    pub osc_2: OSCParams,
    pub master_vol: f32,
    pub osc_2_mod: ModulationType,
    pub mod_bank: ModulationBank,
}

impl From<&RawParameters> for Parameters {
    fn from(params: &RawParameters) -> Self {
        Parameters {
            osc_1: OSCParams::from(&params.osc_1),
            osc_2: OSCParams::from(&params.osc_2),
            master_vol: params.master_vol.get(),
            osc_2_mod: params.osc_2_mod.get().into(),
            mod_bank: ModulationBank::from(&params.mod_bank),
        }
    }
}

pub struct OSCParams {
    pub volume: f32,
    pub shape: NoteShape,
    // A value in [-1.0, 1.0] range. -1.0 means hard pan left. 1.0 means hard
    // pan right. 0.0 means center.
    pub pan: f32,
    // A normalized angle in [0.0, 1.0] range
    pub phase: f32,
    // In semi-tones
    pub coarse_tune: i32,
    // In [-1.0, 1.0] range
    pub fine_tune: f32,
    pub vol_adsr: EnvelopeParams,
    pub vol_lfo: LFO,
    pub pitch_adsr: EnvelopeParams,
    pub pitch_lfo: LFO,
    pub filter_params: FilterParams,
}

impl From<&RawOSC> for OSCParams {
    fn from(params: &RawOSC) -> Self {
        OSCParams {
            volume: params.volume.get() * 2.0,
            shape: NoteShape::from_warp(params.shape.get(), params.warp.get()),
            pan: (params.pan.get() - 0.5) * 2.0,
            phase: params.phase.get(),
            // In semi-tones
            coarse_tune: ((params.coarse_tune.get() - 0.5) * 2.0 * 24.0) as i32,
            // In [-1.0, 1.0] range
            fine_tune: (params.fine_tune.get() - 0.5) * 2.0,
            vol_adsr: EnvelopeParams::from(&params.vol_adsr),
            vol_lfo: LFO {
                amplitude: ease_in_expo(params.vol_lfo.amount.get()),
                period: (ease_in_expo(params.vol_lfo.period.get()) * 10.0).max(0.001),
            },
            pitch_adsr: EnvelopeParams::from(&params.pitch_adsr),
            pitch_lfo: LFO {
                amplitude: ease_in_expo(params.pitch_lfo.amount.get()) * 0.1,
                period: (ease_in_expo(params.pitch_lfo.period.get()) * 10.0).max(0.001),
            },
            filter_params: FilterParams {
                filter: to_filter_type(
                    params.filter_type.get(),
                    (params.filter_gain.get() - 0.5) * 36.0,
                ),
                q_value: (params.filter_q.get() * 10.0).max(0.01),
                freq: params.filter_freq.get(),
            },
        }
    }
}

pub struct ModBankEnvs {
    pub env_1: Envelope,
    pub env_2: Envelope,
}

impl ModBankEnvs {
    pub fn new() -> ModBankEnvs {
        ModBankEnvs {
            env_1: Envelope::new(),
            env_2: Envelope::new(),
        }
    }
}

pub struct ModulationBank {
    pub env_1: EnvelopeParams,
    pub env_1_send: ModBankSend,
    pub env_2: EnvelopeParams,
    pub env_2_send: ModBankSend,
}

impl From<&RawModBank> for ModulationBank {
    fn from(params: &RawModBank) -> Self {
        ModulationBank {
            env_1: EnvelopeParams::from(&params.env_1),
            env_1_send: ModBankSend::from((params.env_1_send.get(), params.env_1_send_to.get())),
            env_2: EnvelopeParams::from(&params.env_2),
            env_2_send: ModBankSend::from((params.env_2_send.get(), params.env_2_send_to.get())),
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

/// An ADSR envelope.
/// TODO: You need to add some kind of state tracking for this. This is because
/// you will get unpleasent clicks in the envelope wave as the note state transitions
/// from Held to Release (the value will instantly jump back to 1.0 and go to zero)
#[derive(Debug, Clone, Copy)]
pub struct EnvelopeParams {
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

impl From<&RawEnvelope> for EnvelopeParams {
    fn from(params: &RawEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack.get());
        let hold = ease_in_expo(params.hold.get());
        let decay = ease_in_expo(params.decay.get());
        let sustain = params.sustain.get();
        let release = ease_in_expo(params.release.get());
        let multiply = (params.multiply.get() - 0.5) * 2.0;
        EnvelopeParams {
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

/// The raw parameter values that a host DAW will set and modify.
/// These are unscaled and are always in the [0.0, 1.0] range
pub struct RawParameters {
    pub osc_1: RawOSC,
    pub osc_2: RawOSC,
    pub master_vol: AtomicFloat,
    pub osc_2_mod: AtomicFloat,
    pub mod_bank: RawModBank,
    /// The host callback, used for communicating with the VST host
    pub host: HostCallback,
    /// The sender that notifies the GUI thread to update due to the host
    /// modifying a value. If this is None, then the GUI is closed/does not exist
    pub sender: tokio::sync::broadcast::Sender<(ParameterType, f32)>,
}

impl RawParameters {
    pub fn new(host: HostCallback) -> Self {
        RawParameters {
            osc_1: RawOSC::default(OSCType::OSC1),
            osc_2: RawOSC::default(OSCType::OSC2),
            master_vol: RawParameters::get_default(ParameterType::MasterVolume).into(),
            osc_2_mod: RawParameters::get_default(ParameterType::OSC2Mod).into(),
            mod_bank: RawModBank::default(),
            host,
            sender: tokio::sync::broadcast::channel(128).0, // TODO: what size of channel should this be?
        }
    }

    pub fn get_ref(&self, parameter: ParameterType) -> &AtomicFloat {
        match parameter {
            ParameterType::MasterVolume => &self.master_vol,
            ParameterType::OSC2Mod => &self.osc_2_mod,
            ParameterType::OSC1(param) => self.osc_1.get_ref(param),
            ParameterType::OSC2(param) => self.osc_2.get_ref(param),
            ParameterType::ModBank(ModBankParameter::Env1(param)) => {
                self.mod_bank.env_1.get_ref(param)
            }
            ParameterType::ModBank(ModBankParameter::Env2(param)) => {
                self.mod_bank.env_2.get_ref(param)
            }
            ParameterType::ModBankSend(ModBankType::Env1) => &self.mod_bank.env_1_send,
            ParameterType::ModBankSend(ModBankType::Env2) => &self.mod_bank.env_2_send,
        }
    }

    pub fn set(&self, value: f32, parameter: ParameterType) {
        // These are needed so Ableton will notice parameter changes in the
        // "Configure" window.
        // TODO: investigate if I should send this only on mouseup/mousedown
        self.host.begin_edit(parameter.into());

        let update = match parameter {
            parameter if parameter == ParameterType::OSC1(OSCParameterType::Warp) => {
                let shape = self.get(parameter);
                let old_value = format!("{}", NoteShape::from_warp(shape, self.get(parameter)));
                let new_value = format!("{}", NoteShape::from_warp(shape, value));
                old_value != new_value
            }
            parameter if parameter == ParameterType::OSC2(OSCParameterType::Warp) => {
                let shape = self.get(parameter);
                let old_value = format!("{}", NoteShape::from_warp(shape, self.get(parameter)));
                let new_value = format!("{}", NoteShape::from_warp(shape, value));
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

        fn envelope_strings(envelope: EnvelopeParams, param: EnvelopeParam) -> (String, String) {
            match param {
                Attack => duration_strings(envelope.attack),
                Hold => duration_strings(envelope.hold),
                Decay => duration_strings(envelope.decay),
                Sustain => make_strings(envelope.sustain * 100.0, "%"),
                Release => duration_strings(envelope.release),
                Multiply => make_strings(envelope.multiply * 100.0, "%"),
            }
        }

        match parameter {
            ParameterType::MasterVolume => make_strings(self.master_vol.get() * 100.0, "%"),
            ParameterType::OSC1(osc_param) | ParameterType::OSC2(osc_param) => {
                let osc = match parameter {
                    ParameterType::OSC1(_) => &params.osc_1,
                    ParameterType::OSC2(_) => &params.osc_2,
                    _ => unreachable!(),
                };
                match osc_param {
                    Volume => make_strings(osc.volume * 100.0, "%"),
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
                    FineTune => make_strings(osc.fine_tune * 100.0, " cents"),
                    VolumeEnv(param) => envelope_strings(osc.vol_adsr, param),
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

            self.set(value, parameter);
            // Notify the GUI to update its view
            // TODO: Is it really okay to just ignore errors?
            let _ = self.sender.send((parameter, value));
        }
    }

    fn can_be_automated(&self, index: i32) -> bool {
        ParameterType::try_from(index).is_ok()
    }

    fn string_to_parameter(&self, _index: i32, _text: String) -> bool {
        false
    }
}

pub struct RawOSC {
    pub volume: AtomicFloat,
    pub shape: AtomicFloat,
    pub pan: AtomicFloat,
    pub phase: AtomicFloat,
    pub warp: AtomicFloat,
    pub coarse_tune: AtomicFloat,
    pub fine_tune: AtomicFloat,
    pub vol_adsr: RawEnvelope,
    pub vol_lfo: RawLFO,
    pub pitch_adsr: RawEnvelope,
    pub pitch_lfo: RawLFO,
    pub filter_type: AtomicFloat,
    pub filter_freq: AtomicFloat,
    pub filter_q: AtomicFloat,
    pub filter_gain: AtomicFloat,
}

impl RawOSC {
    pub fn get_ref(&self, param: OSCParameterType) -> &AtomicFloat {
        use OSCParameterType::*;
        match param {
            Volume => &self.volume,
            Phase => &self.phase,
            Pan => &self.pan,
            Shape => &self.shape,
            Warp => &self.warp,
            CoarseTune => &self.coarse_tune,
            FineTune => &self.fine_tune,
            VolumeEnv(param) => &self.vol_adsr.get_ref(param),
            VolLFOAmplitude => &self.vol_lfo.amount,
            VolLFOPeriod => &self.vol_lfo.period,
            PitchEnv(param) => &self.pitch_adsr.get_ref(param),
            PitchLFOAmplitude => &self.pitch_lfo.amount,
            PitchLFOPeriod => &self.pitch_lfo.period,
            FilterType => &self.filter_type,
            FilterFreq => &self.filter_freq,
            FilterQ => &self.filter_q,
            FilterGain => &self.filter_gain,
        }
    }

    fn get_default(param: OSCParameterType, _osc: OSCType) -> f32 {
        use OSCParameterType::*;
        match param {
            Volume => 0.5, // 100%
            Phase => 0.0,
            Pan => 0.5,   // Center
            Shape => 0.0, // Sine
            Warp => 0.5,
            CoarseTune => 0.5, // 0 semitones
            FineTune => 0.5,   // 0 cents
            VolumeEnv(param) => EnvelopeParam::get_default_vol(param),
            VolLFOAmplitude => 0.0,
            VolLFOPeriod => 0.5,
            PitchEnv(param) => EnvelopeParam::get_default(param),
            PitchLFOAmplitude => 0.0,
            PitchLFOPeriod => 0.5,
            FilterType => 0.0, // Low Pass
            FilterFreq => 1.0, // 22khz
            FilterQ => 0.1,
            FilterGain => 0.5, // 0 dB
        }
    }

    fn default(osc: OSCType) -> RawOSC {
        use OSCParameterType::*;
        RawOSC {
            volume: RawOSC::get_default(Volume, osc).into(),
            phase: RawOSC::get_default(Phase, osc).into(),
            pan: RawOSC::get_default(Pan, osc).into(),
            shape: RawOSC::get_default(Shape, osc).into(),
            warp: RawOSC::get_default(Warp, osc).into(),
            coarse_tune: RawOSC::get_default(CoarseTune, osc).into(),
            fine_tune: RawOSC::get_default(FineTune, osc).into(),
            vol_adsr: RawEnvelope::default_vol(),
            vol_lfo: RawLFO {
                period: RawOSC::get_default(VolLFOPeriod, osc).into(),
                amount: RawOSC::get_default(VolLFOAmplitude, osc).into(),
            },
            pitch_adsr: RawEnvelope::default(),
            pitch_lfo: RawLFO {
                period: RawOSC::get_default(PitchLFOPeriod, osc).into(),
                amount: RawOSC::get_default(PitchLFOAmplitude, osc).into(),
            },
            filter_type: RawOSC::get_default(FilterType, osc).into(),
            filter_freq: RawOSC::get_default(FilterFreq, osc).into(),
            filter_q: RawOSC::get_default(FilterQ, osc).into(),
            filter_gain: RawOSC::get_default(FilterGain, osc).into(),
        }
    }
}

// Represents a bank of LFO and envelope modulators.
#[derive(Debug)]
pub struct RawModBank {
    pub env_1: RawEnvelope,
    pub env_1_send: AtomicFloat,
    pub env_1_send_to: AtomicFloat,
    pub env_2: RawEnvelope,
    pub env_2_send: AtomicFloat,
    pub env_2_send_to: AtomicFloat,
}

impl Default for RawModBank {
    fn default() -> Self {
        RawModBank {
            env_1: RawEnvelope::default(),
            env_1_send: AtomicFloat::default(),
            env_1_send_to: AtomicFloat::default(),
            env_2: RawEnvelope::default(),
            env_2_send: AtomicFloat::default(),
            env_2_send_to: AtomicFloat::from(1.0),
        }
    }
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
    pub attack: AtomicFloat,
    pub hold: AtomicFloat,
    pub decay: AtomicFloat,
    pub sustain: AtomicFloat,
    pub release: AtomicFloat,
    pub multiply: AtomicFloat,
}

impl RawEnvelope {
    pub fn get_ref(&self, param: EnvelopeParam) -> &AtomicFloat {
        match param {
            EnvelopeParam::Attack => &self.attack,
            EnvelopeParam::Hold => &self.hold,
            EnvelopeParam::Decay => &self.decay,
            EnvelopeParam::Sustain => &self.sustain,
            EnvelopeParam::Release => &self.release,
            EnvelopeParam::Multiply => &self.multiply,
        }
    }
    fn default_vol() -> RawEnvelope {
        RawEnvelope {
            attack: EnvelopeParam::get_default_vol(EnvelopeParam::Attack).into(),
            hold: EnvelopeParam::get_default_vol(EnvelopeParam::Hold).into(),
            decay: EnvelopeParam::get_default_vol(EnvelopeParam::Decay).into(),
            sustain: EnvelopeParam::get_default_vol(EnvelopeParam::Sustain).into(),
            release: EnvelopeParam::get_default_vol(EnvelopeParam::Release).into(),
            multiply: EnvelopeParam::get_default_vol(EnvelopeParam::Multiply).into(),
        }
    }
}

impl Default for RawEnvelope {
    fn default() -> Self {
        RawEnvelope {
            attack: EnvelopeParam::get_default(EnvelopeParam::Attack).into(),
            hold: EnvelopeParam::get_default(EnvelopeParam::Hold).into(),
            decay: EnvelopeParam::get_default(EnvelopeParam::Decay).into(),
            sustain: EnvelopeParam::get_default(EnvelopeParam::Sustain).into(),
            release: EnvelopeParam::get_default(EnvelopeParam::Release).into(),
            multiply: EnvelopeParam::get_default(EnvelopeParam::Multiply).into(),
        }
    }
}

pub struct RawLFO {
    period: AtomicFloat,
    amount: AtomicFloat,
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
            EnvelopeParam::Attack => 1.0 / 10000.0,
            EnvelopeParam::Decay => 0.2,
            EnvelopeParam::Hold => 0.0,
            EnvelopeParam::Sustain => 0.0, // 0%
            EnvelopeParam::Release => 1.0 / 10000.0,
            EnvelopeParam::Multiply => 0.5, // 0%
        }
    }

    pub fn get_default_vol(param: EnvelopeParam) -> f32 {
        match param {
            EnvelopeParam::Attack => 0.1,
            EnvelopeParam::Hold => 0.0,
            EnvelopeParam::Decay => 0.2,
            EnvelopeParam::Sustain => 0.5,
            EnvelopeParam::Release => 0.3,
            EnvelopeParam::Multiply => 1.0, // +100%
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
        //  variant                                                                         name                          idx   default
            ParameterType::MasterVolume,                                                    "Master Volume",              0,    0.33                    ;
            ParameterType::OSC1(OSCParameterType::Volume),                                  "OSC 1 Volume",               1,    0.5                     ;
            ParameterType::OSC1(OSCParameterType::Phase),                                   "OSC 1 Phase",                2,    0.0                     ;
            ParameterType::OSC1(OSCParameterType::Pan),                                     "OSC 1 Pan",                  3,    0.5                     ;
            ParameterType::OSC1(OSCParameterType::Shape),                                   "OSC 1 Shape",                4,    0.0                     ;
            ParameterType::OSC1(OSCParameterType::Warp),                                    "OSC 1 Warp",                 5,    0.5                     ;
            ParameterType::OSC1(OSCParameterType::FineTune),                                "OSC 1 Fine Tune",            6,    0.5                     ;
            ParameterType::OSC1(OSCParameterType::CoarseTune),                              "OSC 1 Coarse Tune",          7,    0.5                     ;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Attack)),        "OSC 1 Volume Attack",        8,    DEFAULT_VOL_ATTACK      ;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Hold)),          "OSC 1 Volume Hold",          9,    DEFAULT_VOL_HOLD        ;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Decay)),         "OSC 1 Volume Decay",         10,   DEFAULT_VOL_DECAY       ;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Sustain)),       "OSC 1 Volume Sustain",       11,   DEFAULT_VOL_SUSTAIN     ;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Release)),       "OSC 1 Volume Release",       12,   DEFAULT_VOL_RELEASE     ;
            ParameterType::OSC1(OSCParameterType::VolumeEnv(EnvelopeParam::Multiply)),      "OSC 1 Volume Multiply",      13,   DEFAULT_VOL_MULTIPLY    ;
            ParameterType::OSC1(OSCParameterType::VolLFOAmplitude),                         "OSC 1 VolLFOAmplitude",      14,   0.0                     ;
            ParameterType::OSC1(OSCParameterType::VolLFOPeriod),                            "OSC 1 VolLFOPeriod",         15,   0.5                     ;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Attack)),         "OSC 1 Pitch Attack",         16,   DEFAULT_ATTACK          ;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Hold)),           "OSC 1 Pitch Hold",           17,   DEFAULT_HOLD            ;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Decay)),          "OSC 1 Pitch Decay",          18,   DEFAULT_DECAY           ;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Sustain)),        "OSC 1 Pitch Sustain",        19,   DEFAULT_SUSTAIN         ;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Release)),        "OSC 1 Pitch Release",        20,   DEFAULT_RELEASE         ;
            ParameterType::OSC1(OSCParameterType::PitchEnv(EnvelopeParam::Multiply)),       "OSC 1 Pitch Multiply",       21,   DEFAULT_MULTIPLY        ;
            ParameterType::OSC1(OSCParameterType::PitchLFOAmplitude),                       "OSC 1 PitchLFOAmplitude",    22,   0.0                     ;
            ParameterType::OSC1(OSCParameterType::PitchLFOPeriod),                          "OSC 1 PitchLFOPeriod",       23,   0.5                     ;
            ParameterType::OSC1(OSCParameterType::FilterType),                              "OSC 1 FilterType",           24,   0.0                     ;
            ParameterType::OSC1(OSCParameterType::FilterFreq),                              "OSC 1 FilterFreq",           25,   1.0                     ;
            ParameterType::OSC1(OSCParameterType::FilterQ),                                 "OSC 1 FilterQ",              26,   0.1                     ;
            ParameterType::OSC1(OSCParameterType::FilterGain),                              "OSC 1 FilterGain",           27,   0.5                     ;

            ParameterType::OSC2Mod,                                                         "OSC 2 Mod",                  28,   0.0                     ;

            ParameterType::OSC2(OSCParameterType::Volume),                                  "OSC 2 Volume",               29,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::Phase),                                   "OSC 2 Phase",                30,   0.0                     ;
            ParameterType::OSC2(OSCParameterType::Pan),                                     "OSC 2 Pan",                  31,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::Shape),                                   "OSC 2 Shape",                32,   0.0                     ;
            ParameterType::OSC2(OSCParameterType::Warp),                                    "OSC 2 Warp",                 33,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::FineTune),                                "OSC 2 Fine Tune",            34,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::CoarseTune),                              "OSC 2 Coarse Tune",          35,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Attack)),        "OSC 2 Volume Attack",        36,   DEFAULT_VOL_ATTACK      ;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Hold)),          "OSC 2 Volume Hold",          37,   DEFAULT_VOL_HOLD        ;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Decay)),         "OSC 2 Volume Decay",         38,   DEFAULT_VOL_DECAY       ;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Sustain)),       "OSC 2 Volume Sustain",       39,   DEFAULT_VOL_SUSTAIN     ;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Release)),       "OSC 2 Volume Release",       40,   DEFAULT_VOL_RELEASE     ;
            ParameterType::OSC2(OSCParameterType::VolumeEnv(EnvelopeParam::Multiply)),      "OSC 2 Volume Multiply",      41,   DEFAULT_VOL_MULTIPLY    ;
            ParameterType::OSC2(OSCParameterType::VolLFOAmplitude),                         "OSC 2 VolLFOAmplitude",      42,   0.0                     ;
            ParameterType::OSC2(OSCParameterType::VolLFOPeriod),                            "OSC 2 VolLFOPeriod",         43,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Attack)),         "OSC 2 Pitch Attack",         44,   DEFAULT_ATTACK          ;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Hold)),           "OSC 2 Pitch Hold",           45,   DEFAULT_HOLD            ;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Decay)),          "OSC 2 Pitch Decay",          46,   DEFAULT_DECAY           ;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Sustain)),        "OSC 2 Pitch Sustain",        47,   DEFAULT_SUSTAIN         ;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Release)),        "OSC 2 Pitch Release",        48,   DEFAULT_RELEASE         ;
            ParameterType::OSC2(OSCParameterType::PitchEnv(EnvelopeParam::Multiply)),       "OSC 2 Pitch Multiply",       49,   DEFAULT_MULTIPLY        ;
            ParameterType::OSC2(OSCParameterType::PitchLFOAmplitude),                       "OSC 2 PitchLFOAmplitude",    50,   0.0                     ;
            ParameterType::OSC2(OSCParameterType::PitchLFOPeriod),                          "OSC 2 PitchLFOPeriod",       51,   0.5                     ;
            ParameterType::OSC2(OSCParameterType::FilterType),                              "OSC 2 FilterType",           52,   0.0                     ;
            ParameterType::OSC2(OSCParameterType::FilterFreq),                              "OSC 2 FilterFreq",           53,   1.0                     ;
            ParameterType::OSC2(OSCParameterType::FilterQ),                                 "OSC 2 FilterQ",              54,   0.1                     ;
            ParameterType::OSC2(OSCParameterType::FilterGain),                              "OSC 2 FilterGain",           55,   0.5                     ;

            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Attack)),          "Mod Bank Env 1 Attack",      56,   DEFAULT_ATTACK          ;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Hold)),            "Mod Bank Env 1 Hold",        57,   DEFAULT_HOLD            ;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Decay)),           "Mod Bank Env 1 Decay",       58,   DEFAULT_DECAY           ;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Sustain)),         "Mod Bank Env 1 Sustain",     59,   DEFAULT_SUSTAIN         ;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Release)),         "Mod Bank Env 1 Release",     60,   DEFAULT_RELEASE         ;
            ParameterType::ModBank(ModBankParameter::Env1(EnvelopeParam::Multiply)),        "Mod Bank Env 1 Multiply",    61,   DEFAULT_MULTIPLY        ;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Attack)),          "Mod Bank Env 2 Attack",      62,   DEFAULT_ATTACK          ;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Hold)),            "Mod Bank Env 2 Hold",        63,   DEFAULT_HOLD            ;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Decay)),           "Mod Bank Env 2 Decay",       64,   DEFAULT_DECAY           ;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Sustain)),         "Mod Bank Env 2 Sustain",     65,   DEFAULT_SUSTAIN         ;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Release)),         "Mod Bank Env 2 Release",     66,   DEFAULT_RELEASE         ;
            ParameterType::ModBank(ModBankParameter::Env2(EnvelopeParam::Multiply)),        "Mod Bank Env 2 Multiply",    67,   DEFAULT_MULTIPLY        ;
            ParameterType::ModBankSend(ModBankType::Env1),                                  "Mod Bank Env 1 Send",        68,   0.0                     ;
            ParameterType::ModBankSend(ModBankType::Env2),                                  "Mod Bank Env 2 Send",        69,   0.0                     ;
        }
    };
}

impl ParameterType {
    pub const COUNT: usize = 70;
}

table! {impl_from_i32}
table! {impl_into_i32}
table! {impl_display}
table! {impl_get_default}
