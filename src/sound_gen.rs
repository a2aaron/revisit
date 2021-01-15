use biquad::{Biquad, DirectForm1, ToHertz, Q_BUTTERWORTH_F32};
use wmidi::{Note, PitchBend, U14, U7};

use crate::neighbor_pairs::NeighborPairsIter;

const TAU: f32 = std::f32::consts::TAU;

const RETRIGGER_TIME: usize = 88; // 88 samples is about 2 miliseconds.

/// An offset, in samples, from the start of the frame.
pub type FrameDelta = i32;
/// A value in range [0.0, 1.0] which denotes the position wihtin a wave cycle.
pub type Angle = f32;
/// The sample rate in Hz/seconds.
pub type SampleRate = f32;
/// A pitchbend value in [-1.0, +1.0] range, where +1.0 means "max upward bend"
/// and -1.0 means "max downward bend"
pub type NormalizedPitchbend = f32;

#[derive(Debug)]
pub struct Oscillator {
    // The base pitch of the oscillator, in Hz
    pitch: f32,
    // The velocity of the oscillator
    vel: f32,
    // The current angle of the oscillator. This should be updated every sample
    angle: Angle,
    // The time, in samples, that this oscillator has run since the last note on
    // event
    pub time: usize,
    // The current state of the oscillator (held, released, etc)
    pub note_state: NoteState,
    // The per-sample filter applied to the output
    filter: DirectForm1<f32>,
    // If Some(frame_delta, vel), then a note on event occurs at `frame_delta`
    // with note velocity `vel`
    note_on: Option<(FrameDelta, f32)>,
    // If Some(frame_delta), then a note off event occurs at `frame_delta`
    note_off: Option<FrameDelta>,
}

impl Oscillator {
    pub fn new(pitch: f32, vel: f32, sample_rate: SampleRate) -> Oscillator {
        Oscillator {
            pitch,
            vel,
            angle: 0.0,
            time: 0,
            note_state: NoteState::None,
            filter: DirectForm1::<f32>::new(
                biquad::Coefficients::<f32>::from_params(
                    biquad::Type::LowPass,
                    sample_rate.hz(),
                    (10000).hz(),
                    Q_BUTTERWORTH_F32,
                )
                .unwrap(),
            ),
            note_on: None,
            note_off: None,
        }
    }

    pub fn new_from_note(note: Note, vel: f32, sample_rate: SampleRate) -> Oscillator {
        Oscillator::new(Note::to_freq_f32(note), vel, sample_rate)
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
    /// filter_info - the filter parameters for the filter to be used. If this
    ///               is None, the filter is bypassed.
    pub fn next_sample(
        &mut self,
        sample_num: usize,
        sample_rate: SampleRate,
        shape: NoteShape,
        vol_env: f32,
        pitch: f32,
        filter_params: Option<FilterParams>,
    ) -> f32 {
        // Only advance time if the note is being held down!
        match self.note_state {
            NoteState::None => (),
            _ => self.time += 1,
        }

        // Handle note on event. If the note was previously not triggered (aka:
        // this is the first time the note has been triggered), then the note
        // transitions to the hold state. If this is a retrigger, then the note
        // transitions to the retrigger state, with volume `vol`.
        // Also, we set the note velocity to the appropriate new note velocity.
        match self.note_on {
            Some((note_on, note_on_vel)) if note_on as usize == sample_num => {
                self.note_state = match self.note_state {
                    NoteState::None => NoteState::Held,
                    _ => NoteState::Retrigger {
                        time: self.time,
                        vol: vol_env,
                    },
                };
                self.vel = note_on_vel;
                self.note_on = None;
            }
            _ => (),
        }

        // Trigger note off events
        match self.note_off {
            Some(note_off) if note_off as usize == sample_num => {
                self.note_state = NoteState::Released {
                    time: self.time,
                    vol: vol_env,
                };
                self.note_off = None;
            }
            _ => (),
        }

        // If it has been 10 samples in the retrigger state, switch back to
        // the held state. This also resets the time.
        if let NoteState::Retrigger {
            time: retrigger_time,
            vol: _,
        } = self.note_state
        {
            if self.time - retrigger_time > RETRIGGER_TIME {
                self.note_state = NoteState::Held;
                self.time = 0;
            }
        }

        // Get the raw signal
        let mut value = shape.get(self.angle);

        // Apply volume envelope and note velocity
        value *= self.vel;

        // The pitch of the note after applying pitch multipliers
        let pitch = self.pitch * pitch;

        // Update the angle. Each sample is 1.0 / sample_rate apart for a
        // complete waveform. We also multiply by pitch to advance the right amount
        // We also constrain the angle between 0 and 1, as this reduces
        // roundoff error.
        let angle_delta = pitch / sample_rate;
        self.angle = (self.angle + angle_delta) % 1.0;

        // Apply low pass filter if it exists
        match filter_params {
            Some(params) => {
                let coefficents = FilterParams::into_coefficients(params, sample_rate);
                self.filter.update_coefficients(coefficents);
                let output = self.filter.run(value);
                if output.is_finite() {
                    output
                } else {
                    // If the output happens to be NaN or Infinity, output the
                    // original  signal instead. Hopefully, this will "reset"
                    // the filter on the next sample, instead of being filled
                    // with garbage values.
                    value
                }
            }
            None => value,
        }
    }

    // Return the next sample from the oscillator, but without applying any
    // envelopes or filters. This also ignores the set frequency of the oscillator
    // and just uses pitch.
    pub fn next_sample_raw(&mut self, pitch: f32, shape: NoteShape, sample_rate: f32) -> f32 {
        // Get the raw signal
        let mut value = shape.get(self.angle);

        // Apply note velocity
        value *= self.vel;

        // Update the angle.
        let angle_delta = pitch / sample_rate;
        self.angle = (self.angle + angle_delta) % 1.0;

        value
    }

    /// Release the note
    pub fn note_off(&mut self, frame_delta: i32) {
        self.note_off = Some(frame_delta);
    }

    /// Trigger or Retrigger the note
    pub fn note_on(&mut self, frame_delta: i32, vel: f32) {
        self.note_on = Some((frame_delta, vel));
    }

    /// Returns true if the note is "alive" (playing audio). A note is dead if
    /// it is in the release state and it is after the total release time.
    pub fn is_alive(&self, sample_rate: SampleRate, release: f32) -> bool {
        match self.note_state {
            NoteState::None | NoteState::Held | NoteState::Retrigger { time: _, vol: _ } => true,
            NoteState::Released {
                time: release_time,
                vol: _,
            } => (self.time - release_time) as f32 / sample_rate < release,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FilterParams {
    pub filter: biquad::Type,
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

/// An envelope trait.
pub trait ADSR {
    /// Get the current envelope value. `time` is how many samples it has been
    /// since the start of the note
    fn get(&self, time: usize, note_state: NoteState, sample_rate: SampleRate) -> f32;
}

/// The state of a note, along with the time and velocity that note has, if
/// relevant. The typical life cycle of a note is as follows:
/// None -> Held -> Released -> [removed] or Retrigger -> Held
/// Notes start at None, then become Held. When they are released, they become
/// Released, and are either removed if the release time on the note expires or
/// become Retrigger if the note is retriggered during the release time.
/// Retriggered notes become Held after a few samples automatically.
#[derive(Debug, Clone, Copy)]
pub enum NoteState {
    /// The note is not being held down, but no previous NoteOn or NoteOff exists
    /// for the note. This state indicates that a note was triggered this frame
    /// but the sample for when the note was triggered has not yet been reached.
    None,
    /// The note is being held down
    Held,
    /// The note has just been released. Time is in seconds and denotes how many
    /// samples since the oscillator has started. Vol is the volume that the
    /// note was at when it was released (used for envelope purposes).
    Released { time: usize, vol: f32 },
    /// The note has just be retriggered during a release. Time is in samples
    /// since the oscillator has retriggered. Vol is the volume that the
    /// note was at when it was retriggered (used for envelope purposes).
    Retrigger { time: usize, vol: f32 },
}

/// The shape of a note. The associated f32 indicates the "warp" of the note.
/// The warp is a value between 0.0 and 1.0.
#[derive(Debug, Clone, Copy)]
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
                if (warp - 0.0).abs() < 0.1 {
                    "Reverse Sawtooth"
                } else if (warp - 1.0).abs() < 0.1 {
                    "Sawtooth"
                } else if (warp - 0.5).abs() < 0.1 {
                    "Triangle"
                } else {
                    "Skewed Triangle"
                }
            }
            Noise => "White Noise",
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

/// Return the envelope value that the given ADSR would have at `time`.
/// `time`        - number of samples since the start of the note
/// `sample_rate` - in hz/second
/// `note_state`  - affects where in the envelope the note is at
/// Held - do normal attack/decay/sustain envelope
/// Released - do release envelope, going from the released velocity to zero
/// Retrigger - do short envelope from retrigger velocity to zero
pub fn envelope(
    ahdsr: (f32, f32, f32, f32, f32),
    time: usize,
    note_state: NoteState,
    sample_rate: SampleRate,
) -> f32 {
    let attack = ahdsr.0;
    let hold = ahdsr.1;
    let decay = ahdsr.2;
    let sustain = ahdsr.3;
    let release = ahdsr.4;

    match note_state {
        NoteState::None => 0.0,
        NoteState::Held => ahds_env(attack, hold, decay, sustain, time as f32 / sample_rate),
        NoteState::Released {
            time: rel_time,
            vol,
        } => {
            let time = (time - rel_time) as f32 / sample_rate;
            lerp(vol, 0.0, time / release)
        }
        NoteState::Retrigger {
            time: retrig_time,
            vol,
        } => {
            // Forcibly decay over RETRIGGER_TIME.
            let time = (time - retrig_time) as f32 / RETRIGGER_TIME as f32;
            lerp(vol, 0.0, time)
        }
    }
}

// Get the envelope value given attack, decay, and sustain values.
// Attack, decay, and time are all in the same units (seconds usually)
// Sustain is a value in range [0.0, 1.0]
// Returned value is also in range [0.0, 1.0]
fn ahds_env(attack: f32, hold: f32, decay: f32, sustain: f32, time: f32) -> f32 {
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

fn lerp(start: f32, end: f32, x: f32) -> f32 {
    (end - start) * x.clamp(0.0, 1.0) + start
}

pub fn ease_in_expo(x: f32) -> f32 {
    if x <= 0.0 {
        0.0
    } else {
        2.0f32.powf(10.0 * x - 10.0)
    }
}

pub fn ease_in_poly(x: f32, i: i32) -> f32 {
    x.powi(i)
}
