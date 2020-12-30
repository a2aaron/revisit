use std::{convert::TryFrom, sync::Arc};

use crate::sound_gen::{
    ease_in_expo, ease_in_poly, envelope, NoteShape, NoteState, SampleRate, ADSR,
};

use variant_count::VariantCount;
use vst::{
    host::Host,
    plugin::{HostCallback, PluginParameters},
    util::AtomicFloat,
};

pub struct Parameters {
    pub osc_1: OSCParams,
    pub osc_2: OSCParams,
    pub master_vol: f32,
    pub osc_2_on_off: bool,
}

impl From<&RawParameters> for Parameters {
    fn from(params: &RawParameters) -> Self {
        Parameters {
            osc_1: OSCParams::from(&params.osc_1),
            osc_2: OSCParams::from(&params.osc_2),
            master_vol: params.master_vol.get(),
            osc_2_on_off: params.osc_2_on_off.get() > 0.5,
        }
    }
}

pub struct OSCParams {
    pub volume: f32,
    pub shape: NoteShape,
    // In semi-tones
    pub coarse_tune: i32,
    // In [-1.0, 1.0] range
    pub fine_tune: f32,
    pub vol_adsr: AmplitudeADSR,
    pub vol_lfo: LFO,
    pub pitch_adsr: PitchADSR,
    pub pitch_lfo: LFO,
    pub low_pass_alpha: f32,
}

impl From<&RawOSC> for OSCParams {
    fn from(params: &RawOSC) -> Self {
        OSCParams {
            volume: params.volume.get() * 2.0,
            shape: NoteShape::from_warp(params.shape.get(), params.warp.get()),
            // In semi-tones
            coarse_tune: ((params.coarse_tune.get() - 0.5) * 2.0 * 24.0) as i32,
            // In [-1.0, 1.0] range
            fine_tune: (params.fine_tune.get() - 0.5) * 2.0,
            vol_adsr: AmplitudeADSR::from(&params.vol_adsr),
            vol_lfo: LFO {
                amplitude: ease_in_expo(params.vol_lfo.amount.get()),
                period: (ease_in_expo(params.vol_lfo.period.get()) * 10.0).max(0.001),
            },
            pitch_adsr: PitchADSR::from(&params.pitch_adsr),
            pitch_lfo: LFO {
                amplitude: ease_in_expo(params.pitch_lfo.amount.get()) * 0.1,
                period: (ease_in_expo(params.pitch_lfo.period.get()) * 10.0).max(0.001),
            },
            low_pass_alpha: ease_in_poly(params.low_pass_alpha.get(), 4).clamp(0.0, 1.0),
        }
    }
}
/// An ADSR envelope.
#[derive(Debug, Clone, Copy)]
pub struct AmplitudeADSR {
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
}

impl ADSR for AmplitudeADSR {
    fn get(&self, time: usize, note_state: NoteState, sample_rate: SampleRate) -> f32 {
        envelope(
            (
                self.attack,
                self.hold,
                self.decay,
                self.sustain,
                self.release,
            ),
            time,
            note_state,
            sample_rate,
        )
    }
}

impl From<&RawEnvelope> for AmplitudeADSR {
    fn from(params: &RawEnvelope) -> Self {
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

#[derive(Debug, Clone, Copy)]
pub struct PitchADSR {
    // In seconds
    pub attack: f32,
    // In seconds
    pub hold: f32,
    // In seconds
    pub decay: f32,
    // In percent (-1.0 to 1.0)
    pub multiply: f32,
    // In seconds
    pub release: f32,
}

impl ADSR for PitchADSR {
    fn get(&self, time: usize, note_state: NoteState, sample_rate: SampleRate) -> f32 {
        envelope(
            (self.attack, self.hold, self.decay, 0.0, self.release),
            time,
            note_state,
            sample_rate,
        ) * self.multiply
    }
}

impl From<&RawEnvelope> for PitchADSR {
    fn from(params: &RawEnvelope) -> Self {
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
    pub osc_2_on_off: AtomicFloat,
    pub host: HostCallback,
    pub notify: Arc<tokio::sync::Notify>,
}

impl RawParameters {
    fn get_ref(&self, parameter: ParameterType) -> &AtomicFloat {
        use OSCParameterType::*;
        use ParameterType::*;
        match parameter {
            MasterVolume => &self.master_vol,
            OSC1(Volume) => &self.osc_1.volume,
            OSC1(Shape) => &self.osc_1.shape,
            OSC1(Warp) => &self.osc_1.warp,
            OSC1(CoarseTune) => &self.osc_1.coarse_tune,
            OSC1(FineTune) => &self.osc_1.fine_tune,
            OSC1(VolAttack) => &self.osc_1.vol_adsr.attack,
            OSC1(VolHold) => &self.osc_1.vol_adsr.hold,
            OSC1(VolDecay) => &self.osc_1.vol_adsr.decay,
            OSC1(VolSustain) => &self.osc_1.vol_adsr.sustain,
            OSC1(VolRelease) => &self.osc_1.vol_adsr.release,
            OSC1(VolLFOAmplitude) => &self.osc_1.vol_lfo.amount,
            OSC1(VolLFOPeriod) => &self.osc_1.vol_lfo.period,
            OSC1(PitchAttack) => &self.osc_1.pitch_adsr.attack,
            OSC1(PitchHold) => &self.osc_1.pitch_adsr.hold,
            OSC1(PitchDecay) => &self.osc_1.pitch_adsr.decay,
            OSC1(PitchMultiply) => &self.osc_1.pitch_adsr.sustain,
            OSC1(PitchRelease) => &self.osc_1.pitch_adsr.release,
            OSC1(PitchLFOAmplitude) => &self.osc_1.pitch_lfo.amount,
            OSC1(PitchLFOPeriod) => &self.osc_1.pitch_lfo.period,
            OSC1(LowPassAlpha) => &self.osc_1.low_pass_alpha,
            OSC2OnOff => &self.osc_2_on_off,
            OSC2(Volume) => &self.osc_2.volume,
            OSC2(Shape) => &self.osc_2.shape,
            OSC2(Warp) => &self.osc_2.warp,
            OSC2(CoarseTune) => &self.osc_2.coarse_tune,
            OSC2(FineTune) => &self.osc_2.fine_tune,
            OSC2(VolAttack) => &self.osc_2.vol_adsr.attack,
            OSC2(VolHold) => &self.osc_2.vol_adsr.hold,
            OSC2(VolDecay) => &self.osc_2.vol_adsr.decay,
            OSC2(VolSustain) => &self.osc_2.vol_adsr.sustain,
            OSC2(VolRelease) => &self.osc_2.vol_adsr.release,
            OSC2(VolLFOAmplitude) => &self.osc_2.vol_lfo.amount,
            OSC2(VolLFOPeriod) => &self.osc_2.vol_lfo.period,
            OSC2(PitchAttack) => &self.osc_2.pitch_adsr.attack,
            OSC2(PitchHold) => &self.osc_2.pitch_adsr.hold,
            OSC2(PitchDecay) => &self.osc_2.pitch_adsr.decay,
            OSC2(PitchMultiply) => &self.osc_2.pitch_adsr.sustain,
            OSC2(PitchRelease) => &self.osc_2.pitch_adsr.release,
            OSC2(PitchLFOAmplitude) => &self.osc_2.pitch_lfo.amount,
            OSC2(PitchLFOPeriod) => &self.osc_2.pitch_lfo.period,
            OSC2(LowPassAlpha) => &self.osc_2.low_pass_alpha,
        }
    }

    pub fn get_default(parameter: ParameterType) -> f32 {
        use OSCParameterType::*;
        use ParameterType::*;
        match parameter {
            MasterVolume => 0.33,
            OSC1(Volume) => 0.5,  // 100%
            OSC1(Shape) => 0.225, // Triangle
            OSC1(Warp) => 0.5,
            OSC1(CoarseTune) => 0.5, // 0 semitones
            OSC1(FineTune) => 0.5,   // 0 cents
            OSC1(VolAttack) => 0.1,
            OSC1(VolHold) => 0.0,
            OSC1(VolDecay) => 0.2,
            OSC1(VolSustain) => 0.5,
            OSC1(VolRelease) => 0.3,
            OSC1(VolLFOAmplitude) => 0.0,
            OSC1(VolLFOPeriod) => 0.5,
            OSC1(PitchAttack) => 1.0 / 10000.0,
            OSC1(PitchHold) => 0.0,
            OSC1(PitchDecay) => 0.2,
            OSC1(PitchMultiply) => 0.5,
            OSC1(PitchRelease) => 1.0 / 10000.0,
            OSC1(PitchLFOAmplitude) => 0.0,
            OSC1(PitchLFOPeriod) => 0.5,
            OSC1(LowPassAlpha) => 1.0,
            OSC2OnOff => 0.0, // Off
            OSC2(Volume) => 0.5,
            OSC2(Shape) => 0.0, // Sine
            OSC2(Warp) => 0.5,
            OSC2(CoarseTune) => 0.5, // 0 semitones
            OSC2(FineTune) => 0.5,   // 0 cents
            OSC2(VolAttack) => 0.1,
            OSC2(VolHold) => 0.0,
            OSC2(VolDecay) => 0.2,
            OSC2(VolSustain) => 0.5,
            OSC2(VolRelease) => 0.3,
            OSC2(VolLFOAmplitude) => 0.0,
            OSC2(VolLFOPeriod) => 0.5,
            OSC2(PitchAttack) => 1.0 / 10000.0,
            OSC2(PitchHold) => 0.0,
            OSC2(PitchDecay) => 0.2,
            OSC2(PitchMultiply) => 0.5,
            OSC2(PitchRelease) => 1.0 / 10000.0,
            OSC2(PitchLFOAmplitude) => 0.0,
            OSC2(PitchLFOPeriod) => 0.5,
            OSC2(LowPassAlpha) => 1.0,
        }
    }

    pub fn get(&self, parameter: ParameterType) -> f32 {
        self.get_ref(parameter).get()
    }

    pub fn set(&self, value: f32, parameter: ParameterType) {
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
    }
}

impl PluginParameters for RawParameters {
    fn get_parameter_label(&self, index: i32) -> String {
        if let Ok(param) = ParameterType::try_from(index) {
            use OSCParameterType::*;
            match param {
                ParameterType::MasterVolume => "%".to_string(),
                ParameterType::OSC1(param) | ParameterType::OSC2(param) => match param {
                    Volume => "%".to_string(),
                    Shape | Warp => "".to_string(),
                    VolAttack | VolHold | VolDecay | VolRelease => " sec".to_string(),
                    PitchAttack | PitchHold | PitchDecay | PitchRelease => " sec".to_string(),
                    VolSustain | PitchMultiply => "%".to_string(),
                    VolLFOAmplitude | PitchLFOAmplitude => "%".to_string(),
                    VolLFOPeriod | PitchLFOPeriod => " sec".to_string(),
                    LowPassAlpha => "".to_string(),
                    CoarseTune => " semis".to_string(),
                    FineTune => " cents".to_string(),
                },
                ParameterType::OSC2OnOff => "".to_string(),
            }
        } else {
            "".to_string()
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        let params = Parameters::from(self);
        if let Ok(param) = ParameterType::try_from(index) {
            use OSCParameterType::*;
            match param {
                ParameterType::MasterVolume => "%".to_string(),
                ParameterType::OSC1(osc_param) | ParameterType::OSC2(osc_param) => {
                    let (osc, warp) = match param {
                        ParameterType::OSC1(_) => (&params.osc_1, self.osc_1.warp.get()),
                        ParameterType::OSC2(_) => (&params.osc_2, self.osc_2.warp.get()),
                        _ => unreachable!(),
                    };
                    match osc_param {
                        Volume => format!("{:.2}", osc.volume * 100.0),
                        Shape => format!("{:.2}", osc.shape),
                        Warp => format!("{:.2}", warp),
                        CoarseTune => format!("{}", osc.coarse_tune),
                        FineTune => format!("{:.2}", osc.fine_tune * 100.0),
                        VolAttack => format!("{:.2}", osc.vol_adsr.attack),
                        VolHold => format!("{:.2}", osc.vol_adsr.hold),
                        VolDecay => format!("{:.2}", osc.vol_adsr.decay),
                        VolSustain => format!("{:.2}", osc.vol_adsr.sustain * 100.0),
                        VolRelease => format!("{:.2}", osc.vol_adsr.release),
                        VolLFOAmplitude => format!("{:.2}", osc.vol_lfo.amplitude * 100.0),
                        VolLFOPeriod => format!("{:.2}", osc.vol_lfo.period),
                        PitchAttack => format!("{:.2}", osc.pitch_adsr.attack),
                        PitchHold => format!("{:.2}", osc.pitch_adsr.hold),
                        PitchDecay => format!("{:.2}", osc.pitch_adsr.decay),
                        PitchMultiply => format!("{:.2}", osc.pitch_adsr.multiply * 100.0),
                        PitchRelease => format!("{:.2}", osc.pitch_adsr.release),
                        PitchLFOAmplitude => format!("{:.2}", osc.pitch_lfo.amplitude * 100.0),
                        PitchLFOPeriod => format!("{:.2}", osc.pitch_lfo.period),
                        LowPassAlpha => format!("{:.2}", osc.low_pass_alpha),
                    }
                }
                ParameterType::OSC2OnOff => {
                    if params.osc_2_on_off { "On" } else { "Off" }.to_string()
                }
            }
        } else {
            "".to_string()
        }
    }

    fn get_parameter_name(&self, index: i32) -> String {
        if let Ok(param) = ParameterType::try_from(index) {
            match param {
                ParameterType::MasterVolume => "Master Volume".to_string(),
                ParameterType::OSC1(param) => format!("OSC 1 {}", param),
                ParameterType::OSC2(param) => format!("OSC 2 {}", param),
                ParameterType::OSC2OnOff => "OSC 2 ON/OFF".to_string(),
            }
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
            self.set(value, parameter);
            self.notify.as_ref().notify_one();
        }
    }

    fn can_be_automated(&self, index: i32) -> bool {
        ParameterType::try_from(index).is_ok()
    }

    fn string_to_parameter(&self, _index: i32, _text: String) -> bool {
        false
    }
}

impl Default for RawParameters {
    fn default() -> Self {
        use OSCParameterType::*;
        use ParameterType::*;
        RawParameters {
            osc_1: RawOSC {
                volume: RawParameters::get_default(OSC1(Volume)).into(),
                shape: RawParameters::get_default(OSC1(Shape)).into(),
                warp: RawParameters::get_default(OSC1(Warp)).into(),
                coarse_tune: RawParameters::get_default(OSC1(CoarseTune)).into(),
                fine_tune: RawParameters::get_default(OSC1(FineTune)).into(),
                vol_adsr: RawEnvelope {
                    attack: RawParameters::get_default(OSC1(VolAttack)).into(),
                    hold: RawParameters::get_default(OSC1(VolHold)).into(),
                    decay: RawParameters::get_default(OSC1(VolDecay)).into(),
                    sustain: RawParameters::get_default(OSC1(VolSustain)).into(),
                    release: RawParameters::get_default(OSC1(VolRelease)).into(),
                },
                vol_lfo: RawLFO {
                    period: RawParameters::get_default(OSC1(VolLFOPeriod)).into(),
                    amount: RawParameters::get_default(OSC1(VolLFOAmplitude)).into(),
                },
                pitch_adsr: RawEnvelope {
                    attack: RawParameters::get_default(OSC1(PitchAttack)).into(),
                    hold: RawParameters::get_default(OSC1(PitchHold)).into(),
                    decay: RawParameters::get_default(OSC1(PitchDecay)).into(),
                    sustain: RawParameters::get_default(OSC1(PitchMultiply)).into(),
                    release: RawParameters::get_default(OSC1(PitchRelease)).into(),
                },
                pitch_lfo: RawLFO {
                    period: RawParameters::get_default(OSC1(PitchLFOPeriod)).into(),
                    amount: RawParameters::get_default(OSC1(PitchLFOAmplitude)).into(),
                },
                low_pass_alpha: RawParameters::get_default(OSC1(LowPassAlpha)).into(),
            },
            osc_2: RawOSC {
                volume: RawParameters::get_default(OSC2(Volume)).into(),
                shape: RawParameters::get_default(OSC2(Shape)).into(),
                warp: RawParameters::get_default(OSC2(Warp)).into(),
                coarse_tune: RawParameters::get_default(OSC2(CoarseTune)).into(),
                fine_tune: RawParameters::get_default(OSC2(FineTune)).into(),
                vol_adsr: RawEnvelope {
                    attack: RawParameters::get_default(OSC2(VolAttack)).into(),
                    hold: RawParameters::get_default(OSC2(VolHold)).into(),
                    decay: RawParameters::get_default(OSC2(VolDecay)).into(),
                    sustain: RawParameters::get_default(OSC2(VolSustain)).into(),
                    release: RawParameters::get_default(OSC2(VolRelease)).into(),
                },
                vol_lfo: RawLFO {
                    period: RawParameters::get_default(OSC2(VolLFOPeriod)).into(),
                    amount: RawParameters::get_default(OSC2(VolLFOAmplitude)).into(),
                },
                pitch_adsr: RawEnvelope {
                    attack: RawParameters::get_default(OSC2(PitchAttack)).into(),
                    hold: RawParameters::get_default(OSC2(PitchHold)).into(),
                    decay: RawParameters::get_default(OSC2(PitchDecay)).into(),
                    sustain: RawParameters::get_default(OSC2(PitchMultiply)).into(),
                    release: RawParameters::get_default(OSC2(PitchRelease)).into(),
                },
                pitch_lfo: RawLFO {
                    period: RawParameters::get_default(OSC2(PitchLFOPeriod)).into(),
                    amount: RawParameters::get_default(OSC2(PitchLFOAmplitude)).into(),
                },
                low_pass_alpha: RawParameters::get_default(OSC2(LowPassAlpha)).into(),
            },
            master_vol: RawParameters::get_default(MasterVolume).into(),
            osc_2_on_off: RawParameters::get_default(OSC2OnOff).into(),
            host: Default::default(),
            notify: Arc::new(tokio::sync::Notify::new()),
        }
    }
}

pub struct RawOSC {
    pub volume: AtomicFloat,
    pub shape: AtomicFloat,
    pub warp: AtomicFloat,
    pub coarse_tune: AtomicFloat,
    pub fine_tune: AtomicFloat,
    pub vol_adsr: RawEnvelope,
    pub vol_lfo: RawLFO,
    pub pitch_adsr: RawEnvelope,
    pub pitch_lfo: RawLFO,
    pub low_pass_alpha: AtomicFloat,
}

// Convience struct, represents parameters that are part of an envelope
pub struct RawEnvelope {
    attack: AtomicFloat,
    hold: AtomicFloat,
    decay: AtomicFloat,
    sustain: AtomicFloat,
    release: AtomicFloat,
}

pub struct RawLFO {
    period: AtomicFloat,
    amount: AtomicFloat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, VariantCount)]
pub enum OSCParameterType {
    Volume,
    Shape,
    Warp,
    FineTune,
    CoarseTune,
    VolAttack,
    VolHold,
    VolDecay,
    VolSustain,
    VolRelease,
    VolLFOAmplitude,
    VolLFOPeriod,
    PitchAttack,
    PitchHold,
    PitchDecay,
    PitchMultiply,
    PitchRelease,
    PitchLFOAmplitude,
    PitchLFOPeriod,
    LowPassAlpha,
}

impl std::fmt::Display for OSCParameterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OSCParameterType::*;
        match self {
            Volume => write!(f, "Volume"),
            Shape => write!(f, "Shape"),
            Warp => write!(f, "Warp"),
            CoarseTune => write!(f, "Coarse Tune"),
            FineTune => write!(f, "Fine Tune"),
            VolAttack => write!(f, "Attack (Volume)"),
            VolHold => write!(f, "Hold (Volume)"),
            VolDecay => write!(f, "Decay (Volume)"),
            VolSustain => write!(f, "Sustain (Volume)"),
            VolRelease => write!(f, "Release (Volume)"),
            VolLFOAmplitude => write!(f, "LFO Amplitude (Volume)"),
            VolLFOPeriod => write!(f, "LFO Period (Volume)"),
            PitchAttack => write!(f, "Attack (Pitch)"),
            PitchHold => write!(f, "Hold (Pitch)"),
            PitchDecay => write!(f, "Decay (Pitch)"),
            PitchMultiply => write!(f, "Multiply (Pitch)"),
            PitchRelease => write!(f, "Release (Pitch)"),
            PitchLFOAmplitude => write!(f, "LFO Amplitude (Pitch)"),
            PitchLFOPeriod => write!(f, "LFO Period (Pitch)"),
            LowPassAlpha => write!(f, "Low Pass Alpha"),
        }
    }
}

/// The type of parameter. "Error" is included as a convience type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, VariantCount)]
pub enum ParameterType {
    MasterVolume,
    OSC1(OSCParameterType),
    OSC2OnOff,
    OSC2(OSCParameterType),
}

impl TryFrom<i32> for ParameterType {
    type Error = ();

    fn try_from(i: i32) -> Result<Self, ()> {
        use OSCParameterType::*;
        use ParameterType::*;
        match i {
            0 => Ok(MasterVolume),
            1 => Ok(OSC1(Volume)),
            2 => Ok(OSC1(Shape)),
            3 => Ok(OSC1(Warp)),
            4 => Ok(OSC1(FineTune)),
            5 => Ok(OSC1(CoarseTune)),
            6 => Ok(OSC1(VolAttack)),
            7 => Ok(OSC1(VolHold)),
            8 => Ok(OSC1(VolDecay)),
            9 => Ok(OSC1(VolSustain)),
            10 => Ok(OSC1(VolRelease)),
            11 => Ok(OSC1(VolLFOAmplitude)),
            12 => Ok(OSC1(VolLFOPeriod)),
            13 => Ok(OSC1(PitchAttack)),
            14 => Ok(OSC1(PitchHold)),
            15 => Ok(OSC1(PitchDecay)),
            16 => Ok(OSC1(PitchMultiply)),
            17 => Ok(OSC1(PitchRelease)),
            18 => Ok(OSC1(PitchLFOAmplitude)),
            19 => Ok(OSC1(PitchLFOPeriod)),
            20 => Ok(OSC1(LowPassAlpha)),
            21 => Ok(OSC2OnOff),
            22 => Ok(OSC2(Volume)),
            23 => Ok(OSC2(Shape)),
            24 => Ok(OSC2(Warp)),
            25 => Ok(OSC2(FineTune)),
            26 => Ok(OSC2(CoarseTune)),
            27 => Ok(OSC2(VolAttack)),
            28 => Ok(OSC2(VolHold)),
            29 => Ok(OSC2(VolDecay)),
            30 => Ok(OSC2(VolSustain)),
            31 => Ok(OSC2(VolRelease)),
            32 => Ok(OSC2(VolLFOAmplitude)),
            33 => Ok(OSC2(VolLFOPeriod)),
            34 => Ok(OSC2(PitchAttack)),
            35 => Ok(OSC2(PitchHold)),
            36 => Ok(OSC2(PitchDecay)),
            37 => Ok(OSC2(PitchMultiply)),
            38 => Ok(OSC2(PitchRelease)),
            39 => Ok(OSC2(PitchLFOAmplitude)),
            40 => Ok(OSC2(PitchLFOPeriod)),
            41 => Ok(OSC2(LowPassAlpha)),
            _ => Err(()),
        }
    }
}
