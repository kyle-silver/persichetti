pub mod primitives;
pub mod serialism;

/// The number of distinct pitches in a chromatic scale spanning one octave
pub const CHROMATIC_SCALE: usize = 12;

/// The number of notes in a diatonic scale
pub const DIATONIC_SCALE: usize = 7;

/// In MIDI's encoding scheme, this number represents the pitch corresponding to C<sub>0</sub>
/// in scientific pitch notation
pub const C_ZERO_MIDI: isize = 12;