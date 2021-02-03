use biquad::{Biquad, DirectForm1, ToHertz, Q_BUTTERWORTH_F32};
use variant_count::VariantCount;
use wmidi::{Note, PitchBend, U14, U7};

use crate::neighbor_pairs::NeighborPairsIter;

const TAU: f32 = std::f32::consts::TAU;

pub const RETRIGGER_TIME: usize = 88; // 88 samples is about 2 miliseconds.

/// An offset, in samples, from the start of the frame.
pub type FrameDelta = usize;
/// A value in range [0.0, 1.0] which denotes the position wihtin a wave cycle.
pub type Angle = f32;
/// The sample rate in Hz/seconds.
pub type SampleRate = f32;
/// A pitchbend value in [-1.0, +1.0] range, where +1.0 means "max upward bend"
/// and -1.0 means "max downward bend"
pub type NormalizedPitchbend = f32;

#[derive(Debug)]
pub struct Oscillator {
    angle: Angle,
}

impl Oscillator {
    pub fn new() -> Oscillator {
        Oscillator { angle: 0.0 }
    }

    /// Return the next sample from the oscillator
    /// sample_num - which sample within the frame it is. This is used to do
    ///              proper sub-frame handling of note events.
    /// sample_rate - the sample rate of the note. This is used to ensure that
    ///               the pitch of a note stays the same across sample rates
    /// shape - what noteshape to use for the signal
    /// vol_env - the raw volume of the ADSR envelope. This is used so that
    ///           notes properly transition to and from release states (ex: if
    ///           the volume ADSR was previous at +0.5 volume, the release
    ///           state should carry the note from +0.5 to 0.0 volume. This
    ///           value is NOT used for the overal volume modifier. This value is
    ///           needed because of how it interacts with the note state to allow
    ///           for continious ADSR.
    ///             
    ///           The actual volume multiplier, should be seperated out because
    ///           the volume multiplier includes more than just the vol ADSR.
    ///           EX: amplitude LFO, amplitude modulation, etc, but these
    ///           values should not be included in the note state. Note that
    ///           the real multiplier is mainly applied in SoundGenerator
    /// pitch - the pitch multiplier to be applied to the base frequency of the
    ///         oscillator. This is a unitless value.
    /// phase_mod - how much to add to the current angle value to produce a
    ///             a phase offset. Units are 0.0-1.0 normalized angles (so
    ///             0.0 is zero radians, 1.0 is 2pi radians.)
    /// filter_info - the filter parameters for the filter to be used. If this
    ///               is None, the filter is bypassed.
    pub fn next_sample(
        &mut self,
        sample_rate: SampleRate,
        shape: NoteShape,
        pitch: f32,
        phase_mod: f32,
    ) -> f32 {
        // Get the raw signal
        let mut value = shape.get((self.angle + phase_mod) % 1.0);

        // Update the angle. Each sample is 1.0 / sample_rate apart for a
        // complete waveform. We also multiply by pitch to advance the right amount
        // We also constrain the angle between 0 and 1, as this reduces
        // roundoff error.
        let angle_delta = pitch / sample_rate;
        self.angle = (self.angle + angle_delta) % 1.0;

        value
    }

    // Return the next sample from the oscillator, but without applying any
    // envelopes or filters. This also ignores the set frequency of the oscillator
    // and just uses pitch.
    pub fn next_sample_raw(&mut self, pitch: f32, shape: NoteShape, sample_rate: f32) -> f32 {
        // Get the raw signal
        let mut value = shape.get(self.angle);

        // Update the angle.
        let angle_delta = pitch / sample_rate;
        self.angle = (self.angle + angle_delta) % 1.0;

        value
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FilterParams {
    pub filter: biquad::Type<f32>,
    /// in Hz. This value is clamped between 20 and 99% of the Nyquist frequency
    /// in order to prevent numerical instability at extremely high or low values
    pub freq: f32,
    /// Must be non-negative. If it is negative, it will be clamped to zero
    pub q_value: f32,
}

impl FilterParams {
    fn into_coefficients(
        params: FilterParams,
        sample_rate: SampleRate,
    ) -> biquad::Coefficients<f32> {
        // avoid numerical instability encountered at very low
        // or high frequencies. Clamping at around 20 Hz also
        // avoids blowing out the speakers.
        let freq = params.freq.clamp(20.0, sample_rate * 0.99 / 2.0).hz();
        let q_value = params.q_value.max(0.0);
        biquad::Coefficients::<f32>::from_params(params.filter, sample_rate.hz(), freq, q_value)
            .unwrap()
    }
}

/// The state of a note, along with the time and velocity that note has, if
/// relevant. The typical life cycle of a note is as follows:
/// None -> Held -> Released -> [removed] or Retrigger -> Held
/// Notes start at None, then become Held. When they are released, they become
/// Released, and are either removed if the release time on the note expires or
/// become Retrigger if the note is retriggered during the release time.
/// Retriggered notes become Held after a few samples automatically.
/// TODO: Note states should really not track the volume of the note. That should
/// be tracked on a per envelope basis, I think.
#[derive(Debug, Clone, Copy)]
pub enum NoteState {
    /// The note is not being held down, but no previous NoteOn or NoteOff exists
    /// for the note. This state indicates that a note was triggered this frame
    /// but the sample for when the note was triggered has not yet been reached.
    None,
    /// The note is being held down
    Held,
    /// The note has just been released. Time is in samples and denotes how many
    /// samples since the oscillator has started.
    Released { time: usize },
    /// The note has just be retriggered during a release. Time is in samples
    /// since the oscillator has retriggered.
    Retrigger { time: usize },
}

/// The shape of a note. The associated f32 indicates the "warp" of the note.
/// The warp is a value between 0.0 and 1.0.
#[derive(Debug, Clone, Copy, VariantCount)]
pub enum NoteShape {
    /// A sine wave
    Sine,
    /// A duty-cycle wave. The note is a square wave when the warp is 0.5.
    /// The warp for this NoteShape is clamped between 0.001 and 0.999.
    Square(f32),
    /// A NoteShape which warps between a sawtooth and triangle wave.
    /// Sawtooths: 0.0 and 1.0
    /// Triangle: 0.5
    Skewtooth(f32),
    /// White noise
    Noise,
}

impl NoteShape {
    /// Return the raw waveform using the given angle
    fn get(&self, angle: Angle) -> f32 {
        match self {
            // See https://www.desmos.com/calculator/dqg8kdvung for visuals
            // and https://www.desmos.com/calculator/hs8zd0sfkh for more visuals
            NoteShape::Sine => (angle * TAU).sin(),
            NoteShape::Square(warp) => {
                // This clamp is used to prevent the note from being completely
                // silent, which would occur at 0.0 and 1.0.
                if angle < (*warp).clamp(0.001, 0.999) {
                    -1.0
                } else {
                    1.0
                }
            }
            NoteShape::Skewtooth(warp) => {
                let warp = *warp;
                // Check if the warp makes the note a sawtooth and directly calculate
                // it. This avoids potential divide by zero issues.
                // Clippy lint complains about floating point compares but this
                // is ok to do since 1.0 is exactly representible in floating
                // point and also warp is always in range [0.0, 1.0].
                #[allow(clippy::float_cmp)]
                if warp == 0.0 {
                    return -2.0 * angle + 1.0;
                } else if warp == 1.0 {
                    return 2.0 * angle - 1.0;
                }

                // Otherwise, compute a triangle/skewed triangle shape.
                if angle < warp {
                    (2.0 * angle / warp) - 1.0
                } else {
                    -(2.0 * (angle - warp)) / (1.0 - warp) + 1.0
                }
            }
            NoteShape::Noise => rand::Rng::gen_range(&mut rand::thread_rng(), -1.0, 1.0),
        }
    }
}

impl NoteShape {
    /// Create a NoteShape using the given shape and warp. This is used for
    /// RawParameters mainly.
    pub fn from_warp(shape: f32, warp: f32) -> Self {
        if shape < 0.25 {
            NoteShape::Sine
        } else if shape < 0.5 {
            NoteShape::Skewtooth(warp)
        } else if shape < 0.75 {
            NoteShape::Square(warp)
        } else {
            NoteShape::Noise
        }
    }

    /// Add the warp of the given NoteShape with the modulation parameter. This
    /// is used for note shape modulation.
    pub fn add(&self, modulate: f32) -> Self {
        use NoteShape::*;
        match self {
            Sine => Sine,
            Square(warp) => Square((warp + modulate).clamp(0.0, 1.0)),
            Skewtooth(warp) => Skewtooth((warp + modulate).clamp(0.0, 1.0)),
            Noise => Noise,
        }
    }
}

impl std::fmt::Display for NoteShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use NoteShape::*;
        let string = match self {
            Sine => "Sine",
            Square(warp) => {
                if (warp - 0.5).abs() < 0.1 {
                    "Square"
                } else {
                    "Pulse"
                }
            }
            Skewtooth(warp) => {
                if (warp - 0.0).abs() < 0.1 || (warp - 1.0).abs() < 0.1 {
                    "Sawtooth"
                } else if (warp - 0.5).abs() < 0.1 {
                    "Triangle"
                } else {
                    "Skewtooth"
                }
            }
            Noise => "Noise",
        };

        write!(f, "{}", string)
    }
}

/// Returns an iterator of size num_samples which linearly interpolates between the
/// points specified by pitch_bend. last_pitch_bend is assumed to be the "-1th"
/// value and is used as the starting point.
/// Thank you to Cassie for this code!
pub fn to_pitch_envelope(
    pitch_bend: &[(f32, i32)],
    prev_pitch_bend: f32,
    num_samples: usize,
) -> (impl Iterator<Item = f32> + '_, f32) {
    // Linearly interpolate over num values
    fn interpolate_n(start: f32, end: f32, num: usize) -> impl Iterator<Item = f32> {
        (0..num).map(move |i| lerp(start, end, i as f32 / num as f32))
    }

    // We first make the first and last points to interpolate over. The first
    // point is just prev_pitch_bend, and the last point either gets the value
    // of the last point in pitch_bend, or just prev_pitch_bend if pitch_bend
    // is empty. If pitch_bend is nonempty, this means that the last "segment"
    // is constant value, which is okay since we can't see into the future
    // TODO: Use linear extrapolation for the last segment.
    let first = Some((prev_pitch_bend, 0));

    let last_bend = pitch_bend
        .last()
        .map(|&(bend, _)| bend)
        .unwrap_or(prev_pitch_bend);
    let last = Some((last_bend, num_samples as i32));

    // Now we make a list of points, starting with the first point, then all of
    // pitch_bend, then the last point
    let iter = first
        .into_iter()
        .chain(pitch_bend.iter().copied())
        .chain(last)
        // Make it a NeighborPairs so we can get the current point and the next point
        .neighbor_pairs()
        // Then interpolate the elements.
        .flat_map(|((start, a), (end, b))| {
            let num = b - a;
            interpolate_n(start, end, num as usize)
        });

    (iter, last_bend)
}

// rustc doesn't think "U7" is good snake case style, but its also the name of
// the type, so oh well.
#[allow(non_snake_case)]
/// Convert a U7 value into a normalized [0.0, 1.0] float.
pub fn normalize_U7(num: U7) -> f32 {
    // A U7 in is in range [0, 127]
    let num = U7::data_to_bytes(&[num])[0];
    // convert to f32 - range [0.0, 1.0]
    num as f32 / 127.0
}

/// Convert a PitchBend U14 value into a normalized [-1.0, 1.0] float
pub fn normalize_pitch_bend(pitch_bend: PitchBend) -> NormalizedPitchbend {
    // A pitchbend is a U14 in range [0, 0x3FFF] with 0x2000 meaning "no bend",
    // 0x0 meaning "max down bend" and 0x3FFF meaning "max up bend".
    // convert to u16 - range [0, 0x3FFF]
    let pitch_bend = U14::data_to_slice(&[pitch_bend])[0];
    // convert to i16 - range [-0x2000, 0x1FFF]
    let pitch_bend = pitch_bend as i16 - 0x2000;
    // convert to f32 - range [-1.0, 1.0]
    pitch_bend as f32 * (1.0 / 0x2000 as f32)
}

/// Convert a NormalizedPitchbend into a pitch multiplier. The multiplier is such
/// that a `pitch_bend` of +1.0 will bend up by `semitones` semitones and a value
/// of -1.0 will bend down by `semitones` semitones.
pub fn to_pitch_multiplier(pitch_bend: NormalizedPitchbend, semitones: i32) -> f32 {
    // Given any note, the note a single semitone away is 2^1/12 times the original note
    // So (2^1/12)^n is n semitones away
    let exponent = 2.0f32.powf(semitones as f32 / 12.0);
    // We take an exponential here because frequency is exponential with respect
    // to note value
    exponent.powf(pitch_bend)
}

// Get the envelope value given attack, decay, and sustain values.
// Attack, decay, and time are all in the same units (seconds usually)
// Sustain is a value in range [0.0, 1.0]
// Returned value is also in range [0.0, 1.0]
pub fn ahds_env(attack: f32, hold: f32, decay: f32, sustain: f32, time: f32) -> f32 {
    if time < attack {
        // Attack
        time / attack
    } else if time < attack + hold {
        // Hold
        1.0
    } else if time < attack + hold + decay {
        // Decay
        let time = time - attack - hold;
        lerp(1.0, sustain, time / decay)
    } else {
        // Sustain
        sustain
    }
}

pub fn lerp(start: f32, end: f32, x: f32) -> f32 {
    (end - start) * x.clamp(0.0, 1.0) + start
}

pub fn ease_in_expo(x: f32) -> f32 {
    if x <= 0.0 {
        0.0
    } else {
        (2.0f32.powf(10.0 * x) - 1.0) / (2.0f32.powf(10.0) - 1.0)
    }
}

pub fn ease_in_poly(x: f32, i: i32) -> f32 {
    x.powi(i)
}
