use std::{convert::TryFrom, sync::Arc};

use crate::sound_gen::{
    ahds_env, ease_in_expo, ease_in_poly, lerp, FilterParams, NoteShape, NoteState, SampleRate,
    RETRIGGER_TIME,
};

use derive_more::Display;
use log::info;
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

/// Trait which counts how many possible enum variants can exist. This includes
/// recursively
pub trait CountedEnum {
    const COUNT: usize;
}

/// Implement TryFrom<i32> for the given enum
/// SYNTAX:
/// ```
/// from_into_int! {
///     #[derive(Copy, Clone)]
///     pub enum MyEnum {
///         A,
///         B,
///         C,
///     }
/// }
/// ```
/// This works with nested enums, so long as the leaf enums are simple and
/// have no fields.
/// Thank you to Cassie for providing this code!!!
macro_rules! from_into_int {
    // use of @count and @from allows the macro to use only certain rules without
    // accidentally polluting the public namespace. @count gives how many
    // variants of an enum exist, for a specific variant (either 1 if a variant
    // has no fields, and $name::COUNT where $name is the name of the field)
    // Simple case (variant with no field)
    (@count) => { 1 };
    // Complicated case (variant with subenum)
    (@count $name:ident) => { $name::COUNT };
    // @from handles generating the TryFrom code.
    // $Name is the name of the enum, $i is the variable i: i32 as given by the
    // TryFrom trait, and $base is the "current index" of this enum
    // Base case -- No more tokens to consume
    (@from $Name:ident $i:expr; $base:expr ;) => { Err($i) };
    // Recursive case -- variant with no subenum.
    (@from $Name:ident $i:expr; $base:expr ; $case:ident, $($rest:tt)*) => {
        if $i < $base + 1 {
            Ok($Name::$case)
        } else {
            from_into_int!(@from $Name $i; $base + 1; $($rest)*)
        }
    };
    // Recursive case -- variant with a subenum
    (@from $Name:ident $i:expr; $base:expr; $case:ident $inner:ident, $($rest:tt)*) => {
        if $i < ($base + $inner::COUNT) {
            Ok($Name::$case($inner::try_from($i - $base)?))
        } else {
            from_into_int!(@from $Name $i; $base + $inner::COUNT; $($rest)*)
        }
    };
    // "Public" facing case--get the enum + any derives and relist them
    // then implement the traits.
    (
        $(
            #[$($attr:meta)+]
        )*
        pub enum $Name:ident {
            $(
                $case:ident $(($nested:ident))?,
            )*
        }
    ) => {
        $(#[$($attr)*])*
        pub enum $Name {
            $(
                $case $(($nested))?,
            )*
        }

        impl CountedEnum for $Name {
            const COUNT: usize = 0 $(
                + from_into_int!(@count $($nested)?)
            )*;
        }

        impl std::convert::TryFrom<usize> for $Name {
            type Error = usize;
            fn try_from(i: usize) -> Result<Self, usize> {
                from_into_int!(@from $Name i; 0 ; $($case $($nested)?,)*)
            }
        }
    }
}

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
                freq: ease_in_poly(params.filter_freq.get(), 4).clamp(0.0, 1.0) * 22100.0,
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

// TODO: This is slightly wrong! ModulationSends also need to specify which
// oscillator they are going to send to! Otherwise this will just apply to all
// of the oscillators, which is not what you want! (likely just use a tuple
// of ModulationSend and OSCType)
pub struct ModulationBank {
    pub env_1: EnvelopeParams,
    pub env_1_send: ModulationSend,
    pub env_2: EnvelopeParams,
    pub env_2_send: ModulationSend,
}

impl From<&RawModBank> for ModulationBank {
    fn from(params: &RawModBank) -> Self {
        ModulationBank {
            env_1: EnvelopeParams::from(&params.env_1),
            env_1_send: ModulationSend::from(params.env_1_send.get()),
            env_2: EnvelopeParams::from(&params.env_2),
            env_2_send: ModulationSend::from(params.env_2_send.get()),
        }
    }
}

pub struct Envelope {
    // The value to lerp from when in Retrigger or Release state
    pub lerp_from: f32,
}

impl Envelope {
    pub fn new() -> Envelope {
        Envelope { lerp_from: 0.0 }
    }

    /// Get the current envelope value. `time` is how many samples it has been
    /// since the start of the note
    pub fn get(
        &self,
        params: &EnvelopeParams,
        time: usize,
        note_state: NoteState,
        sample_rate: SampleRate,
    ) -> f32 {
        let attack = params.attack;
        let hold = params.hold;
        let decay = params.decay;
        let sustain = params.sustain;
        let release = params.release;

        let value = match note_state {
            NoteState::None => 0.0,
            NoteState::Held => ahds_env(attack, hold, decay, sustain, time as f32 / sample_rate),
            NoteState::Released { time: rel_time } => {
                let time = (time - rel_time) as f32 / sample_rate;
                lerp(self.lerp_from, 0.0, time / release)
            }
            NoteState::Retrigger { time: retrig_time } => {
                // Forcibly decay over RETRIGGER_TIME.
                let time = (time - retrig_time) as f32 / RETRIGGER_TIME as f32;
                lerp(self.lerp_from, 0.0, time)
            }
        };
        value * params.multiply
    }

    // Set self.lerp_from to the specified value using the arguments
    pub fn remember(
        &mut self,
        params: &EnvelopeParams,
        time: usize,
        note_state: NoteState,
        sample_rate: SampleRate,
    ) {
        self.lerp_from = self.get(params, time, note_state, sample_rate);
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
    pub host: HostCallback,
    pub notify: Arc<tokio::sync::Notify>,
}

impl RawParameters {
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

    pub fn get_default(parameter: ParameterType) -> f32 {
        match parameter {
            ParameterType::MasterVolume => 0.33,
            ParameterType::OSC2Mod => 0.0, // Mix
            ParameterType::OSC1(param) => RawOSC::get_default(param, OSCType::OSC1),
            ParameterType::OSC2(param) => RawOSC::get_default(param, OSCType::OSC2),
            ParameterType::ModBank(ModBankParameter::Env1(param)) => {
                EnvelopeParam::get_default(param)
            }
            ParameterType::ModBank(ModBankParameter::Env2(param)) => {
                EnvelopeParam::get_default(param)
            }
            ParameterType::ModBankSend(ModBankType::Env1) => 0.0, // Amplitude
            ParameterType::ModBankSend(ModBankType::Env2) => 0.0, // Amplitude
        }
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
            ParameterType::ModBankSend(ModBankType::Env1) => {
                (format!("{}", params.mod_bank.env_1_send), "".to_string())
            }
            ParameterType::ModBankSend(ModBankType::Env2) => {
                (format!("{}", params.mod_bank.env_2_send), "".to_string())
            }
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
            match param {
                ParameterType::MasterVolume => "Master Volume".to_string(),
                ParameterType::OSC1(param) => format!("OSC 1 {}", param),
                ParameterType::OSC2(param) => format!("OSC 2 {}", param),
                ParameterType::OSC2Mod => "OSC 2 ON/OFF".to_string(),
                ParameterType::ModBank(ModBankParameter::Env1(param)) => {
                    format!("ADSR 1 {}", param)
                }
                ParameterType::ModBank(ModBankParameter::Env2(param)) => {
                    format!("ADSR 2 {}", param)
                }
                ParameterType::ModBankSend(ModBankType::Env1) => "ADSR 1 Send".to_string(),
                ParameterType::ModBankSend(ModBankType::Env2) => "ADSR 2 Send".to_string(),
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
        RawParameters {
            osc_1: RawOSC::default(OSCType::OSC1),
            osc_2: RawOSC::default(OSCType::OSC2),
            master_vol: RawParameters::get_default(ParameterType::MasterVolume).into(),
            osc_2_mod: RawParameters::get_default(ParameterType::OSC2Mod).into(),
            mod_bank: RawModBank::default(),
            host: Default::default(),
            notify: Arc::new(tokio::sync::Notify::new()),
        }
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

    pub fn get(&self, param: OSCParameterType) -> f32 {
        self.get_ref(param).get()
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
    pub env_2: RawEnvelope,
    pub env_2_send: AtomicFloat,
}

impl Default for RawModBank {
    fn default() -> Self {
        RawModBank {
            env_1: RawEnvelope::default(),
            env_2: RawEnvelope::default(),
            env_1_send: AtomicFloat::default(),
            env_2_send: AtomicFloat::default(),
        }
    }
}

from_into_int! {
    /// An enum which represents particular modulator, and then a particular
    /// parameter within that modulator.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ModBankParameter {
        Env1(EnvelopeParam),
        Env2(EnvelopeParam),
    }
}

from_into_int! {
    /// A fieldless version of the ModBankParameter enum. This is used to refer to
    /// an entire modulator (independent of the specific parameters). Also, you need
    /// to use this in the ParameterType enum, otherwise from_into_int generates too
    /// many parameters.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum ModBankType {
        Env1,
        Env2,
    }
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

from_into_int! {
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

from_into_int! {
    #[derive(Debug, Display, Clone, Copy, PartialEq, Eq)]
    pub enum EnvelopeParam {
        Attack,
        Decay,
        Hold,
        Sustain,
        Release,
        Multiply,
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OSCType {
    OSC1,
    OSC2,
}

from_into_int! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, VariantCount)]
    pub enum ModulationType {
        Mix,
        AmpMod,
        FreqMod,
        PhaseMod,
        WarpMod,
    }
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

impl std::fmt::Display for ModulationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ModulationType::*;
        match self {
            Mix => write!(f, "Mix"),
            AmpMod => write!(f, "Amp. Mod"),
            FreqMod => write!(f, "Freq. Mod"),
            PhaseMod => write!(f, "Phase Mod"),
            WarpMod => write!(f, "Warp Mod"),
        }
    }
}

from_into_int! {
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
}

impl TryFrom<i32> for ParameterType {
    type Error = usize;
    fn try_from(i: i32) -> Result<Self, Self::Error> {
        if i < 0 {
            Err(413)
        } else {
            ParameterType::try_from(i as usize)
        }
    }
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
