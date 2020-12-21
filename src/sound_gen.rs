use wmidi::{Note, PitchBend, U14, U7};

use crate::{neighbor_pairs::NeighborPairsIter, RevisitParametersEnvelope};

const TAU: f32 = std::f32::consts::TAU;

const RETRIGGER_TIME: usize = 88; // 88 samples is about 2 miliseconds.

/// A value in range [0.0, 1.0] which denotes the position wihtin a wave cycle.
pub type Angle = f32;
/// The sample rate in Hz/seconds.
pub type SampleRate = f32;

#[derive(Debug)]
pub struct Oscillator {
    pub pitch: Note,
    vel: f32,
    angle: f32,
    time: usize,
    note_state: NoteState,
    low_pass_output: Option<f32>,
    note_on: Option<i32>,
    note_off: Option<i32>,
}

impl Oscillator {
    pub fn new(pitch: Note, vel: U7) -> Oscillator {
        Oscillator {
            pitch,
            vel: normalize_U7(vel),
            angle: 0.0,
            time: 0,
            note_state: NoteState::None,
            low_pass_output: None,
            note_on: None,
            note_off: None,
        }
    }

    /// Fills dest with the signal of the oscillator
    pub fn values(
        &mut self,
        dest: &mut [f32],
        sample_rate: SampleRate,
        vol_adsr: AmplitudeADSR,
        pitch_adsr: PitchADSR,
        shape: NoteShape,
        pitch_bend: &[f32],
        low_pass_alpha: f32,
    ) {
        for i in 0..dest.len() {
            // Get volume envelope
            let vel = vol_adsr.get(self.time, self.note_state, sample_rate);

            // Get pitch envelope
            let pitch = pitch_bend[i] + pitch_adsr.get(self.time, self.note_state, sample_rate);

            // Only advance time if the note is being held down!
            match self.note_state {
                NoteState::None => (),
                _ => self.time += 1,
            }

            // Trigger note on events
            match self.note_on {
                Some(note_on) if note_on as usize == i => {
                    self.note_state = match self.note_state {
                        NoteState::None => NoteState::Held,
                        _ => NoteState::Retrigger {
                            time: self.time,
                            vel,
                        },
                    };
                    self.note_on = None;
                }
                _ => (),
            }

            // Trigger note off events
            match self.note_off {
                Some(note_off) if note_off as usize == i => {
                    self.note_state = NoteState::Released {
                        time: self.time,
                        vel,
                    };
                    self.note_off = None;
                }
                _ => (),
            }

            // If it has been 10 samples in the retrigger state, switch back to
            // the held state. This also resets the time.
            if let NoteState::Retrigger {
                time: retrigger_time,
                vel: _,
            } = self.note_state
            {
                if self.time - retrigger_time > RETRIGGER_TIME {
                    self.note_state = NoteState::Held;
                    self.time = 0;
                }
            }

            // Get the raw signal
            let value = shape.get(self.angle);

            // Apply volume envelope and note velocity
            let value = value * vel * self.vel;

            // Apply low pass filter
            let value = if let Some(low) = self.low_pass_output {
                // If low is NaN or Infinity, reset it.
                let low = if low.is_finite() { low } else { value };

                let value = low + low_pass_alpha * (value - low);
                self.low_pass_output = Some(value);
                value
            } else {
                self.low_pass_output = Some(value);
                value
            };

            dest[i] = value;

            // The pitch of the note after applying pitch multipliers
            let pitch = Note::to_freq_f32(self.pitch) * to_pitch_multiplier(pitch, 12);

            // Update the angle. Each sample is 1.0 / sample_rate apart for a
            // complete waveform. We also multiply by pitch to advance the right amount
            // We also constrain the angle between 0 and 1, as this reduces
            // roundoff error.
            let angle_delta = pitch / sample_rate;
            self.angle = (self.angle + angle_delta) % 1.0;
        }
    }

    /// Release the note
    pub fn note_off(&mut self, frame_delta: i32) {
        self.note_off = Some(frame_delta);
    }

    /// Trigger or Retrigger the note
    pub fn note_on(&mut self, frame_delta: i32) {
        self.note_on = Some(frame_delta);
    }

    /// Returns true if the note is "alive" (playing audio). A note is dead if
    /// it is in the release state and it is after the total release time.
    pub fn is_alive(&self, sample_rate: SampleRate, release: f32) -> bool {
        match self.note_state {
            NoteState::None | NoteState::Held | NoteState::Retrigger { time: _, vel: _ } => true,
            NoteState::Released {
                time: release_time,
                vel: _,
            } => (self.time - release_time) as f32 / sample_rate < release,
        }
    }
}

/// An ADSR envelope.
#[derive(Debug, Clone, Copy)]
pub struct AmplitudeADSR {
    // In seconds
    pub attack: f32,
    // In seconds
    pub decay: f32,
    // In percent (0.0 to 1.0)
    pub sustain: f32,
    // In seconds
    pub release: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct PitchADSR {
    // In seconds
    pub attack: f32,
    // In seconds
    pub decay: f32,
    // In percent (-1.0 to 1.0)
    pub multiply: f32,
    // In seconds
    pub release: f32,
}

pub trait ADSR {
    // Convert [0.0, 1.0] normalized parameters into an envelope.
    fn from_params(params: &RevisitParametersEnvelope) -> Self;
    // Get the current envelope value.
    // time is how many samples since the start of the note
    // note_state is what state the note is in
    // sample rate is in hz/second
    fn get(&self, time: usize, note_state: NoteState, sample_rate: SampleRate) -> f32;
}

impl ADSR for AmplitudeADSR {
    fn from_params(params: &RevisitParametersEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack.get());
        let decay = ease_in_expo(params.decay.get());
        let sustain = params.sustain.get();
        let release = ease_in_expo(params.release.get());
        AmplitudeADSR {
            // Clamp values to around 1 ms minimum.
            // This avoids division by zero problems.
            // Also prevents annoying clicking which is undesirable.
            attack: (attack * 2.0).max(1.0 / 1000.0),
            decay: (decay * 5.0).max(1.0 / 1000.0),
            sustain,
            release: (release * 5.0).max(1.0 / 1000.0),
        }
    }
    fn get(&self, time: usize, note_state: NoteState, sample_rate: SampleRate) -> f32 {
        envelope(
            (self.attack, self.decay, self.sustain, self.release),
            time,
            note_state,
            sample_rate,
        )
    }
}

impl ADSR for PitchADSR {
    fn from_params(params: &RevisitParametersEnvelope) -> Self {
        // Apply exponetial scaling to input values.
        // This makes it easier to select small envelope lengths.
        let attack = ease_in_expo(params.attack.get());
        let decay = ease_in_expo(params.decay.get());
        let multiply = params.sustain.get();
        let release = ease_in_expo(params.release.get());
        PitchADSR {
            attack: (attack * 2.0).max(1.0 / 10000.0),
            decay: (decay * 5.0).max(1.0 / 10000.0),
            multiply: (multiply - 0.5) * 2.0,
            release: (release * 5.0).max(1.0 / 10000.0),
        }
    }

    fn get(&self, time: usize, note_state: NoteState, sample_rate: SampleRate) -> f32 {
        envelope(
            (self.attack, self.decay, 0.0, self.release),
            time,
            note_state,
            sample_rate,
        ) * self.multiply
    }
}

#[derive(Debug, Clone, Copy)]
pub enum NoteState {
    // The note is not being held down, but no previous NoteOn or NoteOff exists
    // for the note.
    None,
    // The note is being held down
    Held,
    // The note has just been released. Time is in seconds and denotes how many
    // seconds since the oscillator has started. Vel is the velocity that the
    // note was at (will go down to zero)
    Released { time: usize, vel: f32 },
    // The note has just be retriggered during a release. Time is in samples
    // and denotes how many simples it has been since the oscillator has started.
    // Vel is the velocity that the note was (will go down to zero over 10 samples)
    // Notes exit this state after 10 samples.
    Retrigger { time: usize, vel: f32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NoteShape {
    Sine,
    Square,
    Sawtooth,
    Triangle,
}

impl NoteShape {
    // Return the raw waveform using the given angle
    fn get(&self, angle: Angle) -> f32 {
        match self {
            // See https://www.desmos.com/calculator/dqg8kdvung for visuals
            NoteShape::Sine => (angle * TAU).sin(),
            NoteShape::Sawtooth => (angle * 2.0) - 1.0,
            NoteShape::Square => {
                if angle < 0.5 {
                    -1.0
                } else {
                    1.0
                }
            }
            NoteShape::Triangle => -2.0 * (2.0 * angle - 1.0).abs() + 1.0,
        }
    }
}

impl From<f32> for NoteShape {
    fn from(x: f32) -> Self {
        if x < 0.25 {
            NoteShape::Sine
        } else if x < 0.5 {
            NoteShape::Triangle
        } else if x < 0.75 {
            NoteShape::Square
        } else {
            NoteShape::Sawtooth
        }
    }
}

impl std::fmt::Display for NoteShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use NoteShape::*;
        match self {
            Sine => write!(f, "Sine"),
            Square => write!(f, "Square"),
            Sawtooth => write!(f, "Sawtooth"),
            Triangle => write!(f, "Triangle"),
        }
    }
}

// Returns an iterator of size num_samples which linearly interpolates between the
// points specified by pitch_bend. last_pitch_bend is assumed to be the "-1th"
// value and is used as the starting point.
// Thank you to Cassie for this code!
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

pub fn normalize_U7(num: U7) -> f32 {
    // A U7 in is in range [0, 127]
    let num = U7::data_to_bytes(&[num])[0];
    // convert to f32 - range [0.0, 1.0]
    num as f32 / 127.0
}

pub fn normalize_pitch_bend(pitch_bend: PitchBend) -> f32 {
    // A pitchbend is a U14 in range [0, 0x3FFF] with 0x2000 meaning "no bend",
    // 0x0 meaning "max down bend" and 0x3FFF meaning "max up bend".
    // convert to u16 - range [0, 0x3FFF]
    let pitch_bend = U14::data_to_slice(&[pitch_bend])[0];
    // convert to i16 - range [-0x2000, 0x1FFF]
    let pitch_bend = pitch_bend as i16 - 0x2000;
    // convert to f32 - range [-1.0, 1.0]
    pitch_bend as f32 * (1.0 / 0x2000 as f32)
}

// Converts a normalized pitch bend (range [-1.0, 1.0]) to the appropriate pitch
// multiplier.
fn to_pitch_multiplier(normalized_pitch_bend: f32, semitones: i32) -> f32 {
    // Given any note, the note a single semitone away is 2^1/12 times the original note
    // So (2^1/12)^n is n semitones away
    let exponent = 2.0f32.powf(semitones as f32 / 12.0);
    // We take an exponential here because frequency is exponential with respect
    // to note value
    exponent.powf(normalized_pitch_bend)
}

/// Return the envelope value that the given ADSR would have at `time`.
/// `time`        - number of samples since the start of the note
/// `sample_rate` - in hz/second
/// `note_state`  - affects where in the envelope the note is at
/// Held - do normal attack/decay/sustain envelope
/// Released - do release envelope, going from the released velocity to zero
/// Retrigger - do short envelope from retrigger velocity to zero
fn envelope(
    adsr: (f32, f32, f32, f32),
    time: usize,
    note_state: NoteState,
    sample_rate: SampleRate,
) -> f32 {
    let attack = adsr.0;
    let decay = adsr.1;
    let sustain = adsr.2;
    let release = adsr.3;

    match note_state {
        NoteState::None => 0.0,
        NoteState::Held => ads_env(attack, decay, sustain, time as f32 / sample_rate),
        NoteState::Released {
            time: rel_time,
            vel,
        } => {
            let time = (time - rel_time) as f32 / sample_rate;
            lerp(vel, 0.0, time / release)
        }
        NoteState::Retrigger {
            time: retrig_time,
            vel,
        } => {
            // Forcibly decay over RETRIGGER_TIME.
            let time = (time - retrig_time) as f32 / RETRIGGER_TIME as f32;
            lerp(vel, 0.0, time)
        }
    }
}

// Get the envelope value given attack, decay, and sustain values.
// Attack, decay, and time are all in the same units (seconds usually)
// Sustain is a value in range [0.0, 1.0]
// Returned value is also in range [0.0, 1.0]
fn ads_env(attack: f32, decay: f32, sustain: f32, time: f32) -> f32 {
    if time < attack {
        // Attack
        time / attack
    } else if time < attack + decay {
        // Decay
        let time = time - attack;
        lerp(1.0, sustain, time / decay)
    } else {
        // Sustain
        sustain
    }
}

fn lerp(start: f32, end: f32, x: f32) -> f32 {
    (end - start) * x.clamp(0.0, 1.0) + start
}

fn ease_in_expo(x: f32) -> f32 {
    if x <= 0.0 {
        0.0
    } else {
        2.0f32.powf(10.0 * x - 10.0)
    }
}