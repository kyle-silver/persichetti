//! This crate provides basic functionality for working with and reasoning about the harmony common in western
//! classical and contemporary music. Included are basic building blocks such as notes and intervals, and tools
//! for identifying and generating more advanced structures.
//!
//! # Example: Interval Naming
//! ```rust
//! use persichetti::primitives::*;
//! # fn main() -> Result<(), Error> {
//! let b_flat = Note::from_str("Bb")?;
//! let g_sharp = Note::from_str("G#")?;
//! assert_eq!(Interval::from_str("A6")?, Interval::from_notes(&b_flat, &g_sharp));
//! # Ok(())
//! # }
//! ```

pub mod primitives;
pub mod serialism;

/// The number of distinct pitches in a chromatic scale spanning one octave
pub const CHROMATIC_SCALE: usize = 12;

/// The number of notes in a diatonic scale
pub const DIATONIC_SCALE: usize = 7;

/// In MIDI's encoding scheme, this number represents the pitch corresponding to C<sub>0</sub>
/// in scientific pitch notation
pub const C_ZERO_MIDI: isize = 12;