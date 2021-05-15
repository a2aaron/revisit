use std::ops::{Add, Div, Mul, Sub};

pub trait Lerpable = Add<Self, Output = Self>
    + Sub<Self, Output = Self>
    + Mul<f32, Output = Self>
    + Sized
    + Copy
    + Clone;

pub trait InvLerpable = Sub<Self, Output = Self> + Div<Self, Output = f32> + Sized + Copy + Clone;

pub enum Easing<T> {
    Linear {
        start: T,
        end: T,
    },
    SplitLinear {
        start: T,
        mid: T,
        end: T,
        split_at: f32,
    },
}

impl<T: Lerpable + InvLerpable> Easing<T> {
    // `t` must be in 0.0 to 1.0 range
    pub fn ease(&self, t: f32) -> T {
        match *self {
            Easing::Linear { start, end } => lerp(start, end, t),
            Easing::SplitLinear {
                start,
                mid,
                end,
                split_at,
            } => {
                if t < split_at {
                    // Map [0.0, split_at] to the [start, mid] range
                    remap(0.0, split_at, t, start, mid)
                } else {
                    // Map [split_at, 1.0] to the [mid, end] range
                    remap(split_at, 1.0, t, mid, end)
                }
            }
        }
    }

    // inv_ease is not guarenteed to return within 0.0-1.0 range.
    pub fn inv_ease(&self, val: T) -> f32 {
        match *self {
            Easing::Linear { start, end } => inv_lerp::<T>(start, end, val),
            Easing::SplitLinear {
                start,
                mid,
                end,
                split_at,
            } => {
                // First determine if the value fits into the lower half of the function
                // Map [start, end] to [0.0, split_at]
                let lower_val = remap(start, mid, val, 0.0, split_at);
                if lower_val < split_at {
                    lower_val
                } else {
                    // Otherwise the value is in the upper half
                    // Map [mid, end] to [split_at, 1.0]
                    remap(mid, end, val, split_at, 1.0)
                }
            }
        }
    }
}

/// Lerp between two values. This function is not clamped
pub fn lerp<T: Lerpable>(start: T, end: T, t: f32) -> T {
    (end - start) * t + start
}

/// Returns the "inverse lerp" of a value. The returned value is zero if val == start
/// and is 1.0 if val == end. This function can return outside the 0.0-1.0 range.
pub fn inv_lerp<T: InvLerpable>(start: T, end: T, val: T) -> f32 {
    (val - start) / (end - start)
}

/// Map the range [old_start, old_end] to [new_start, new_end]. Note that
/// lerp(start, end, t) == remap(0.0, 1.0, t, start, end)
/// inv_lerp(start, end, val) == remap(start, end, val, 0.0, 1.0)
pub fn remap<T: InvLerpable, U: Lerpable>(
    old_start: T,
    old_end: T,
    val: T,
    new_start: U,
    new_end: U,
) -> U {
    let t = inv_lerp(old_start, old_end, val);
    lerp(new_start, new_end, t)
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
