//! This module provides notes and intervals for use in more complex applications, as well as macros for
//! quicky initializing them.
//! 
//! # Note Shorthand
//! Notes can be represented compactly as strings. [`Note`] names are not case-sensitive, but flat symbols and double-sharps are.
//! 
//! | Symbol | Meaning | Example |
//! | --- | --- | --- |
//! | `A-G` | Note Name | `"a", "C", "f"` |
//! | `nat` | Natural | `"bnat", "Gnat"` |
//! | `b...` | One or more flats | `"Bb", "Abb"` |
//! | `#...` | One or more sharps | `"f#", "C#"` |
//! | `x...` | One or more double-sharps | `"gx", "Dx"` |
//! | `#x..` | A triple+ sharp | `"C#x", "E#xx"` |
//! 
//! When initializing a [`PitchedNote`], the octave number can be appended to the end of the note name. Negative numbers are supported
//! 
//! ```rust
//! # use persichetti::primitives::*;
//! let b_flat_2 = PitchedNote::from_str("Bb2").unwrap();
//! let g_sharp_negative_one = PitchedNote::from_str("g#-1").unwrap();
//! ```
//!
//! # Interval Shorthand
//! Intervals have a string representation as well. Capital and lowercase M&rsquo;s are case-sensitive, but all other qualities are not.
//! When an interval is double diminished or doubly augmented, it can be represented as `"aa5"` (doubly-augmented fifth) or `"dd3"
//! (doubly-diminished third). This can be extended to an arbitrary number of augmentations or diminutions.
//!
//! | Symbol | Meaning | Example |
//! | --- | --- | --- |
//! | `1-7` | Unison through Seventh| `"m6", "A4"` |
//! | `0` | Unison. Both 0 and 1 represent Unisons | `"p0"` |
//! | `U` | Unison | `"au", "pU"` |
//! | `P` | Perfect | `"p4", "P5"` |
//! | `A...` | Augmented by one or more degrees | `"A4", "aa6"` |
//! | `D...` | Diminished by one or more degrees | `"D3", "dd2"` |
//! | `M` | Major | `"M3", "M7"` |
//! | `m` | Minor | `"m2", "m6` |

use std::{cmp::Ordering, ops::{Add, Sub}, usize};

use derive_more::From;
use regex::Regex;
use lazy_static::lazy_static;

use crate::{CHROMATIC_SCALE, C_ZERO_MIDI, DIATONIC_SCALE};

/// Initialize a [`Note`] from either the [`Note::from_str`] constructor or the [`Note::new`] constructor.
/// In either case, the the return type is a [`Result`] of [`Note`] or [`NoteError`].
#[macro_export]
macro_rules! note {
    ($token:expr) => {
        Note::from_str($token)
    };
    ($name:expr, $accidental:expr) => {
        Note::new($name, $accidental)
    };
}

/// Initialize an [`Interval`] from either the [`Interval::from_str`] constructor or the [`Interval::new`] constructor.
/// In either case, the return type is a [`Result`] of [`Interval`] or [`IntervalError`].
#[macro_export]
macro_rules! ivl {
    ($token:expr) => {
        Interval::from_str($token)
    };
    ($size:expr, $quality:expr) => {
        Interval::new($size, $quality)
    };
}

#[derive(Debug, From)]
pub enum Error {
    Note(NoteError),
    Interval(IntervalError),
}

#[derive(Debug)]
pub enum NoteError {
    InvalidNoteName,
}

/// The &ldquo;white keys&rdquo; of the piano; the seven note names used in western harmony.
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum NoteName {
    C, D, E, F, G, A, B,
}

impl NoteName {
    /// Instantiate from a string representing the note name; case-insensitive.
    pub fn new(name: &str) -> Result<NoteName, NoteError> {
        match name.to_lowercase().as_str() {
            "a" => Ok(NoteName::A),
            "b" => Ok(NoteName::B),
            "c" => Ok(NoteName::C),
            "d" => Ok(NoteName::D),
            "e" => Ok(NoteName::E),
            "f" => Ok(NoteName::F),
            "g" => Ok(NoteName::G),
            _ => Err(NoteError::InvalidNoteName)
        }
    }

    /// The placement of the note name in a C Major scale. C is zero and B is seven.
    pub fn diatonic_scale_degree(&self) -> usize {
        match self {
            NoteName::C => 0,
            NoteName::D => 1,
            NoteName::E => 2,
            NoteName::F => 3,
            NoteName::G => 4,
            NoteName::A => 5,
            NoteName::B => 6,
        }
    }

    /// The placement of the &ldquo;white key&rdquo; in a chromatic scale. C is zero and B is eleven.
    pub fn chromatic_scale_degree(&self) -> usize {
        match self {
            NoteName::C => 0,
            NoteName::D => 2,
            NoteName::E => 4,
            NoteName::F => 5,
            NoteName::G => 7,
            NoteName::A => 9,
            NoteName::B => 11,
        }
    }

    /// Get the corresponding &ldquo;white key&rdquo; for a given number. Supports values outside of the range 0&ndash;6.
    /// ```rust
    /// # use persichetti::primitives::*;
    /// assert_eq!(NoteName::D, NoteName::from_diatonic_scale_degree(1));
    /// assert_eq!(NoteName::E, NoteName::from_diatonic_scale_degree(9));
    /// assert_eq!(NoteName::B, NoteName::from_diatonic_scale_degree(-1));
    /// ```
    pub fn from_diatonic_scale_degree(degree: isize) -> NoteName {
        match degree.rem_euclid(DIATONIC_SCALE as isize) {
            0 => NoteName::C,
            1 => NoteName::D,
            2 => NoteName::E,
            3 => NoteName::F,
            4 => NoteName::G,
            5 => NoteName::A,
            6 => NoteName::B,
            _ => panic!("Rust's Euclidian remainder function is broken")
        }
    }

    /// Calculate the [`IntervalSize`] between two note names
    pub fn interval_size(&self, note_above: &Self) -> IntervalSize {
        let size = note_above.diatonic_scale_degree() as isize - self.diatonic_scale_degree() as isize;
        let size = size.rem_euclid(DIATONIC_SCALE as isize);
        IntervalSize::from_diatonic_size(size as usize)
    }
}

impl Add<&IntervalSize> for &NoteName {
    type Output = NoteName;

    /// Simple, &ldquo;white key only&rdquo; computations without any consideration for accidentals.
    /// This is analogous to traversing a C Major scale.
    fn add(self, size: &IntervalSize) -> Self::Output {
        let degree = self.diatonic_scale_degree() + size.diatonic_size();
        NoteName::from_diatonic_scale_degree(degree as isize)
    }
}

impl Add<IntervalSize> for NoteName {
    type Output = NoteName;

    fn add(self, rhs: IntervalSize) -> Self::Output {
        &self + &rhs
    }
}

/// An accidental represents the degree of chromatic alteration applied to a [`NoteName`]. Please note
/// that `Flat(1)` represents a diminution of a single half-step, and `Sharp(1)` represents an augmentation
/// of a single half-step. `Flat(0)` is equivalent to `Natural` and probably not useful to you. Double flats
/// and double sharps are represented as `Flat(2)` and `Sharp(2)`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Accidental {
    Flat(usize),
    Natural,
    Sharp(usize)
}

impl Accidental {
    /// Converts a degree of chromatic alteration applied to a note into an accidental.
    pub fn from_isize(degree: isize) -> Accidental {
        match degree {
            isize::MIN..=-1 => Accidental::Flat(-degree as usize),
            0 => Accidental::Natural,
            _ => Accidental::Sharp(degree as usize)
        }
    }

    /// The number of half-steps up or down that an accidental represents
    pub fn chromatic_offset(&self) -> isize {
        match self {
            Accidental::Flat(degree) => -1 * (*degree as isize),
            Accidental::Natural => 0,
            Accidental::Sharp(degree) => *degree as isize,
        }
    }
}

/// A note is represented as a pairing of a [`NoteName`] and an [`Accidental`]. These notes have no concept of
/// absolute pitch or octave equivalence; if you need a note with octave information, use [`PitchedNote`].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Note {
    name: NoteName,
    accidental: Accidental,
}

impl Note {
    pub fn new(name: NoteName, accidental: Accidental) -> Note {
        Note { name, accidental }
    }

    pub fn name(&self) -> &NoteName {
        &self.name
    }

    pub fn accidental(&self) -> &Accidental {
        &self.accidental
    }

    /// Instantiate a `Note` using the shorthand documented in [`crate::primitives`]
    pub fn from_str(note: &str) -> Result<Note, NoteError> {
        lazy_static! {
            static ref RE: Regex =  Regex::new("^(?P<note>[A-Ga-g])((?P<sharp>(#+|x+|#x+))|(?P<flat>b+)|(?P<nat>nat))?$").unwrap();
        }
        if RE.is_match(note) == false {
            return Err(NoteError::InvalidNoteName);
        }
        let caps = RE.captures(note).unwrap();
        if let Some(note_name) = caps.name("note") {
            let note_name = NoteName::new(note_name.as_str())?;
            if let Some(sharp) = caps.name("sharp") {
                let degree: usize = sharp.as_str().chars().map(|c| match c {
                    '#' => 1,
                    'x' => 2,
                    _ => panic!("invalid sharp character")
                }).sum();
                return Ok(Note::new(note_name, Accidental::Sharp(degree)));
            }
            if let Some(flat) = caps.name("flat") {
                let degree = flat.as_str().len();
                return Ok(Note::new(note_name, Accidental::Flat(degree)));
            }
            Ok(Note::new(note_name, Accidental::Natural))
        } else {
            Err(NoteError::InvalidNoteName)
        }
    }

    pub fn chromatic_scale_degree(&self) -> usize {
        let chromatic_degree = self.name.chromatic_scale_degree() as isize;
        let signed_position = chromatic_degree + self.accidental.chromatic_offset();
        signed_position.rem_euclid(CHROMATIC_SCALE as isize) as usize
    }
}

impl Add<&Interval> for &Note {
    type Output = Note;

    /// Adding an [`Interval`] I to a [`Note`] N will yeild a new note whose distance N is exactly I.
    fn add(self, interval: &Interval) -> Self::Output {
        // get the "white key" of the output
        let name = &self.name + &interval.size;
        // get the pitch (without accidental information) of the output
        let pitch = (self.chromatic_scale_degree() as isize + interval.chromatic_size()) % 12;
        // how many degrees does the accdental alter the white key
        let degree = pitch as isize - name.chromatic_scale_degree() as isize;
        let accidental = Accidental::from_isize(degree);
        Note::new(name, accidental)
    }
}

impl Add<Interval> for Note {
    type Output = Note;

    fn add(self, rhs: Interval) -> Self::Output {
        &self + &rhs
    }
}

impl Sub<&Interval> for &Note {
    type Output = Note;

    fn sub(self, interval: &Interval) -> Self::Output {
        self + &interval.inverse()
    }
}

impl Sub<Interval> for Note {
    type Output = Note;

    fn sub(self, rhs: Interval) -> Self::Output {
        &self - &rhs
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntervalError {
    InvalidQualityAndSizeCombination,
    InvalidToken,
    UnsupportedCompoundInterval,
}

/// Represents the pure diatonic intervals with no chromatic alterations
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum IntervalSize {
    Unison,
    Second,
    Third,
    Fourth,
    Fifth,
    Sixth,
    Seventh,
}

impl IntervalSize {
    /// The diatonic distance between the top note and the bottom note in an interval pair. A `Unison` has a 
    /// distance of zero and a `Seventh` has a distance of 6.
    pub fn diatonic_size(&self) -> usize {
        match self {
            IntervalSize::Unison => 0,
            IntervalSize::Second => 1,
            IntervalSize::Third => 2,
            IntervalSize::Fourth => 3,
            IntervalSize::Fifth => 4,
            IntervalSize::Sixth => 5,
            IntervalSize::Seventh => 6,
        }
    }

    /// The number of half-steps between the top note and the bottom note. This assumes that the intervals are
    /// from a major scale, i.e. all intervals are either Major or Perfect
    pub fn chromatic_size(&self) -> usize {
        match self {
            IntervalSize::Unison => 0,
            IntervalSize::Second => 2,
            IntervalSize::Third => 4,
            IntervalSize::Fourth => 5,
            IntervalSize::Fifth => 7,
            IntervalSize::Sixth => 9,
            IntervalSize::Seventh => 11,
        }
    }

    /// Construct an `IntervalSize` from a number representing a position in a diatonic scale. A unison is zero
    /// and a seventh is six. Numbers outside of the range 0&ndash;6 are supported.
    pub fn from_diatonic_size(size: usize) -> IntervalSize {
        match size % 7 {
            0 => IntervalSize::Unison,
            1 => IntervalSize::Second,
            2 => IntervalSize::Third,
            3 => IntervalSize::Fourth,
            4 => IntervalSize::Fifth,
            5 => IntervalSize::Sixth,
            6 => IntervalSize::Seventh,
            _ => panic!("Modulo operator is broke"),
        }
    }
}



impl Add<&IntervalSize> for &IntervalSize {
    type Output = IntervalSize;

    fn add(self, rhs: &IntervalSize) -> Self::Output {
        IntervalSize::from_diatonic_size(self.diatonic_size() + rhs.diatonic_size())
    }
}

impl Add<IntervalSize> for IntervalSize {
    type Output = IntervalSize;

    fn add(self, rhs: IntervalSize) -> Self::Output {
        &self + &rhs
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IntervalQuality {
    Diminished(usize),
    Augmented(usize),
    Minor,
    Major,
    Perfect,
}

impl IntervalQuality {
    fn from_chromatic_span(size: &IntervalSize, span: isize) -> IntervalQuality {
        use IntervalSize::*;
        let delta = span - size.chromatic_size() as isize;
        match size {
            Unison | Fourth | Fifth => {
                match delta {
                    isize::MIN..=-1 => IntervalQuality::Diminished(-delta as usize),
                    0 => IntervalQuality::Perfect,
                    _ => IntervalQuality::Augmented(delta as usize),
                }
            },
            Second | Third | Sixth | Seventh => {
                match delta {
                    isize::MIN..=-2 => IntervalQuality::Diminished((-delta - 1) as usize),
                    -1 => IntervalQuality::Minor,
                    0 => IntervalQuality::Major,
                    _ => IntervalQuality::Augmented(delta as usize)
                }
            }
        }
    } 
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Interval {
    size: IntervalSize,
    quality: IntervalQuality,
}

impl Interval {  
    pub fn new(size: IntervalSize, quality: IntervalQuality) -> Result<Interval, IntervalError> {
        use IntervalSize::*;
        use IntervalQuality::*;
        match quality {
            Major | Minor => match size {
                Unison | Fourth | Fifth => return Err(IntervalError::InvalidQualityAndSizeCombination),
                _ => {}
            },
            Perfect => match size {
                Second | Third | Sixth | Seventh => return Err(IntervalError::InvalidQualityAndSizeCombination),
                _ => {},
            },
            _ => {}
        };
        Ok(Interval { size, quality })
    }

    pub fn from_str(input: &str) -> Result<Interval, IntervalError> {
        lazy_static! {
            static ref RE: Regex = Regex::new("^(?P<quality>([MmPpDd]|[Aa]+|[Dd]+))(?P<size>([Uu]|\\d+))$").unwrap();
        }
        // let RE = Regex::new("^(?P<quality>([MmPpDd]|[Aa]+|[Dd]+))(?P<size>([Uu]|\\d+))$").unwrap();
        let caps = RE.captures(input).unwrap();
        if let (Some(quality), Some(size)) = (caps.name("quality"), caps.name("size")) {
            let quality = quality.as_str();
            let quality = match quality.chars().next().unwrap() {
                'M' => IntervalQuality::Major,
                'm' => IntervalQuality::Minor,
                'P' | 'p' => IntervalQuality::Perfect,
                'A' | 'a' => IntervalQuality::Augmented(quality.len()),
                'D' | 'd' => IntervalQuality::Diminished(quality.len()),
                _ => panic!("Interval parsing regex is broken"),
            };
            let size = match size.as_str() {
                "U" | "u" => Ok(IntervalSize::Unison),
                numeric => {
                    let interval_size = numeric.parse::<usize>().map_err(|_| IntervalError::InvalidToken)?;
                    let interval_size = if interval_size == 0 { 0 } else { interval_size - 1 };
                    match interval_size {
                        0..=6 => Ok(IntervalSize::from_diatonic_size(interval_size)),
                        _ => Err(IntervalError::UnsupportedCompoundInterval),
                    }
                },
            }?;
            Interval::new(size, quality)
        } else {
            Err(IntervalError::InvalidQualityAndSizeCombination)
        }
    }

    pub fn size(&self) -> &IntervalSize {
        &self.size
    }

    pub fn quality(&self) -> &IntervalQuality {
        &self.quality
    }

    fn chromatic_alteration(&self) -> isize {
        use IntervalSize::*;
        // the panic conditions will be unreachable given the public 
        // constructors for these classes
        match self.size {
            Unison | Fourth | Fifth => match self.quality {
                IntervalQuality::Diminished(degree) => -1 * degree as isize,
                IntervalQuality::Augmented(degree) => degree as isize,
                IntervalQuality::Perfect => 0,
                _ => panic!("Major or Minor quality applied to a perfect interval"),
            },
            _ => match self.quality {
                IntervalQuality::Diminished(degree) => -1 - (degree as isize),
                IntervalQuality::Augmented(degree) => degree as isize,
                IntervalQuality::Minor => -1,
                IntervalQuality::Major => 0,
                _ => panic!("Perfect quality applied to non-perfect interval"),
            }
        }
    }

    fn chromatic_size(&self) -> isize {
        self.size.chromatic_size() as isize + self.chromatic_alteration()
    }

    pub fn from_notes(lower: &Note, higher: &Note) -> Interval {
        let size = NoteName::interval_size(&lower.name, &higher.name);
        // white key distance
        let wk_distance = higher.name.chromatic_scale_degree() as isize - lower.name.chromatic_scale_degree() as isize;
        let wk_distance = wk_distance.rem_euclid(CHROMATIC_SCALE as isize) as isize;
        let chromatic_delta = -lower.accidental.chromatic_offset() + wk_distance + higher.accidental.chromatic_offset();
        let quality = IntervalQuality::from_chromatic_span(&size, chromatic_delta);
        Interval { size, quality }
    }

    /// The inverse of an interval I can be defined as the interval I&prime; such that the sum of I and I&prime; is an octave
    /// (or unison, in this case)
    pub fn inverse(&self) -> Interval {
        use IntervalSize::*;
        use IntervalQuality::*;
        let size = match self.size {
            Unison => Unison,
            Second => Seventh,
            Third => Sixth,
            Fourth => Fifth,
            Fifth => Fourth,
            Sixth => Third,
            Seventh => Second,
        };
        let quality = match self.quality {
            Diminished(degree) => Augmented(degree),
            Augmented(degree) => Diminished(degree),
            Minor => Major,
            Major => Minor,
            Perfect => Perfect,
        };
        // we can bypass the validity checks in the constructor because an inverse will always be valid
        // so long as the original interval is valid
        Interval { size, quality }
    }
}

impl Add<&Interval> for &Interval {
    type Output = Interval;

    fn add(self, rhs: &Interval) -> Self::Output {
        let size = &self.size + &rhs.size;
        let span = (self.chromatic_size() + rhs.chromatic_size()).rem_euclid(CHROMATIC_SCALE as isize);
        let quality = IntervalQuality::from_chromatic_span(&size, span);
        Interval { size, quality }
    }
}

impl Add<Interval> for Interval {
    type Output = Interval;

    fn add(self, rhs: Interval) -> Self::Output {
       &self + &rhs
    }
}

#[cfg(test)]
mod test_note_names {
    use super::*;
    use IntervalSize::*;
    use IntervalQuality::*;
    use NoteName::*;
    use Accidental::*;

    #[test]
    fn test_chromatic_scale_degree_conversion() -> Result<(), NoteError> {
        assert_eq!(10, Note::from_str("Cbb")?.chromatic_scale_degree());
        assert_eq!(10, Note::from_str("g#x")?.chromatic_scale_degree());
        assert_eq!(4, Note::from_str("Dx")?.chromatic_scale_degree());
        assert_eq!(5, Note::from_str("Fnat")?.chromatic_scale_degree());
        assert_eq!(9, Note::from_str("A")?.chromatic_scale_degree());
        Ok(())
    }

    #[test]
    fn note_plus_interval_trait() -> Result<(), Error> {
        assert_eq!(Note::from_str("G")?,&Note::from_str("C")? + &Interval::new(Fifth, Perfect)?);
        assert_eq!(Note::from_str("G#")?, &Note::from_str("C")? + &Interval::new(Fifth, Augmented(1))?);
        assert_eq!(Note::from_str("Gx")?, &Note::from_str("C")? + &Interval::new(Fifth, Augmented(2))?);
        assert_eq!(Note::from_str("F#")?, &Note::from_str("Bb")? + &Interval::new(Fifth, Augmented(1))?);
        assert_eq!(Note::from_str("Fx")?, &Note::from_str("B")? + &Interval::new(Fifth, Augmented(1))?);
        assert_eq!(Note::from_str("G#")?, &Note::from_str("Bb")? + &Interval::new(Sixth, Augmented(1))?);
        assert_eq!(Note::from_str("Gx")?, &Note::from_str("B")? + &Interval::new(Sixth, Augmented(1))?);
        assert_eq!(Note::from_str("Dbb")?, &Note::from_str("C")? + &Interval::new(Second, Diminished(1))?);
        assert_eq!(Note::from_str("D")?, &Note::from_str("B")? + &Interval::new(Third, Minor)?);
        assert_eq!(Note::from_str("Db")?, &Note::from_str("B")? + &Interval::new(Third, Diminished(1))?);
        assert_eq!(Note::from_str("D#")?, &Note::from_str("B")? + &Interval::new(Third, Major)?);
        assert_eq!(Note::from_str("Dx")?, &Note::from_str("B")? + &Interval::new(Third, Augmented(1))?);
        assert_eq!(Note::from_str("D#x")?, &Note::from_str("B")? + &Interval::new(Third, Augmented(2))?);
        assert_eq!(Note::from_str("C#")?, &Note::from_str("C")? + &Interval::new(Unison, Augmented(1))?);
        assert_eq!(Note::from_str("Cb")?, &Note::from_str("C")? + &Interval::new(Unison, Diminished(1))?);
        assert_eq!(Note::from_str("G")?, &Note::from_str("G#")? + &Interval::new(Unison, Diminished(1))?);
        Ok(())
    }

    #[test]
    fn interval_naming() -> Result<(), Error> {
        assert_eq!(Interval::new(Fifth, Perfect)?, Interval::from_notes(&Note::from_str("C")?, &Note::from_str("G")?));
        assert_eq!(Interval::new(Fifth, Perfect)?, Interval::from_notes(&Note::from_str("G")?, &Note::from_str("D")?));
        assert_eq!(Interval::new(Third, Major)?, Interval::from_notes(&Note::from_str("Bb")?, &Note::from_str("D")?));
        assert_eq!(Interval::new(Unison, Augmented(1))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("B#")?));
        assert_eq!(Interval::new(Third, Minor)?, Interval::from_notes(&Note::from_str("A")?, &Note::from_str("C")?));
        assert_eq!(Interval::new(Third, Diminished(1))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("Db")?));
        assert_eq!(Interval::new(Third, Diminished(2))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("Dbb")?));
        assert_eq!(Interval::new(Fourth, Augmented(1))?, Interval::from_notes(&Note::from_str("A")?, &Note::from_str("D#")?));
        assert_eq!(Interval::new(Fifth, Diminished(1))?, Interval::from_notes(&Note::from_str("A")?, &Note::from_str("Eb")?));
        assert_eq!(Interval::new(Fourth, Augmented(1))?, Interval::from_notes(&Note::from_str("Eb")?, &Note::from_str("A")?));
        assert_eq!(Interval::new(Fifth, Diminished(1))?, Interval::from_notes(&Note::from_str("Eb")?, &Note::from_str("Bbb")?));
        assert_eq!(Interval::new(Unison, Diminished(1))?, Interval::from_notes(&Note::from_str("C")?, &Note::from_str("Cb")?));
        assert_eq!(Interval::new(Unison, Diminished(2))?, Interval::from_notes(&Note::from_str("C")?, &Note::from_str("Cbb")?));
        assert_eq!(Interval::new(Unison, Diminished(1))?, Interval::from_notes(&Note::from_str("G#")?, &Note::from_str("G")?));
        assert_eq!(Interval::new(Third, Diminished(7))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("Dbbbbbbb")?));
        Ok(())
    }

    #[test]
    fn intervals_from_strings() -> Result<(), Error> {
        // valid
        assert_eq!(ivl!(Fifth, Perfect)?, Interval::from_str("p5")?);
        assert_eq!(ivl!(Third, Augmented(1))?, Interval::from_str("a3")?);
        assert_eq!(ivl!(Seventh, Diminished(2))?, Interval::from_str("dd7")?);
        // invalid
        assert_eq!(Err(IntervalError::InvalidQualityAndSizeCombination), Interval::from_str("mu"));
        Ok(())
    }

    #[test]
    fn inversion() -> Result<(), Error> {
        // identity
        assert_eq!(ivl!(Unison, Perfect)?, ivl!(Unison, Perfect)?.inverse());
        // other stuff...
        assert_eq!(ivl!(Fourth, Perfect)?, ivl!(Fifth, Perfect)?.inverse());
        assert_eq!(ivl!(Third, Diminished(1))?, ivl!(Sixth, Augmented(1))?.inverse());
        assert_eq!(ivl!(Unison, Augmented(1))?, ivl!(Unison, Diminished(1))?.inverse());
        assert_eq!(ivl!(Second, Minor)?, ivl!(Seventh, Major)?.inverse());
        Ok(())
    }

    #[test]
    fn interval_addition() -> Result<(), Error> {
        // identity
        assert_eq!(ivl!(Unison, Perfect)?, ivl!(Unison, Perfect)? + ivl!(Unison, Perfect)?);
        // other things
        assert_eq!(ivl!(Second, Major)?, ivl!(Unison, Perfect)? + ivl!(Second, Major)?);
        assert_eq!(ivl!(Third, Major)?, ivl!(Second, Major)? + ivl!(Second, Major)?);
        assert_eq!(ivl!(Fourth, Perfect)?, ivl!(Third, Major)? + ivl!(Second, Minor)?);
        assert_eq!(ivl!(Fourth, Augmented(1))?, ivl!(Third, Major)? + ivl!(Second, Major)?);
        assert_eq!(ivl!(Unison, Perfect)?, ivl!(Fourth, Augmented(1))? + ivl!(Fifth, Diminished(1))?);
        assert_eq!(ivl!(Unison, Perfect)?, ivl!(Unison, Augmented(1))? + ivl!(Unison, Diminished(1))?);
        assert_eq!(ivl!(Sixth, Augmented(1))?, ivl!(Seventh, Major)? + ivl!(Seventh, Major)?);
        // test
        assert_eq!(ivl!("pU")?, ivl!("M2")? + ivl!("M2")? + ivl!("m2")? + ivl!("M2")? + ivl!("M2")? + ivl!("M2")? + ivl!("m2")?);
        // augmented triad
        assert_eq!(ivl!(Fifth, Augmented(1))?, ivl!("M3")? + ivl!("M3")?);
        // diminished seventh
        assert_eq!(ivl!(Seventh, Diminished(1))?, ivl!("m3")? +  ivl!("m3")? + ivl!("m3")?);
        Ok(())
    }

    #[test]
    fn readme_example() -> Result<(), Error> {
        // note naming
        let b_flat = Note::new(B, Flat(1));
        let g_sharp = note!("G#")?;
        assert_eq!(Interval::new(Sixth, Augmented(1))?, Interval::from_notes(&b_flat, &g_sharp));

        // adding intervals
        let major_second = Interval::new(Second, Major)?;
        let perfect_fourth = Interval::from_str("p4")?;
        assert_eq!(Interval::new(Fifth, Perfect)?, major_second + perfect_fourth);

        // combine notes and intervals
        let f_sharp = note!("f#")?;
        let major_third = ivl!("M3")?;
        assert_eq!(note!("a#")?, f_sharp + major_third);
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PitchedNote {
    note: Note,
    octave: isize
}

/// Represents a [`Note`] in absolute pitch space, known as [Scientific Pitch](https://en.wikipedia.org/wiki/Scientific_pitch_notation).
/// Each instance of the struct will contain a note and its corresponding octave. Middle C is C<sub>4</sub> in this implementation.
/// Keep in mind that the letter name of the note is what determines its octave, i.e. C<sub>5</sub> and C&#9837;<sub>5</sub> are a 
/// half step apart. Functions are also provided to convert between scientific pitch and MIDI numbering.
impl PitchedNote {
    pub fn from_note(note: Note, octave: isize) -> PitchedNote {
        PitchedNote { note, octave }
    }

    pub fn new(name: NoteName, accidental: Accidental, octave: isize) -> PitchedNote {
        PitchedNote {
            note: Note::new(name, accidental),
            octave
        }
    }

    pub fn from_str(input: &str) -> Result<PitchedNote, NoteError> {
        // I don't feel like making a second regex...
        let pitch_index = input.find(|c: char| c.is_ascii_digit() || c == '-').ok_or(NoteError::InvalidNoteName)?;
        let note = Note::from_str(&input[..pitch_index])?;
        let octave = input[pitch_index..].parse().map_err(|_| NoteError::InvalidNoteName)?;
        Ok(PitchedNote { note, octave })
    }

    /// Converts a [`PitchedNote`] to its corresponding number in MIDI, where A<sub>0</sub> (the lowest note
    /// on a standard piano) is 21. The octave of the note is determined by its letter name, meaning for
    /// example that C<sub>1</sub> is 24 and C&#9837;<sub>1</sub> is 23, even though B<sub>0</sub> is also 23.
    pub fn midi_number(&self) -> isize {
        let base = self.note.name.chromatic_scale_degree() as isize;
        let offset = self.note.accidental.chromatic_offset();
        C_ZERO_MIDI + (CHROMATIC_SCALE as isize * self.octave) + base + offset
    }

    /// The number of half-steps (semitones) separating two pitches
    pub fn chromatic_distance(&self, other: &Self) -> usize {
        (self.midi_number() - other.midi_number()).abs() as usize
    }

    /// The interval between one [`PitchedNote`] and another, without any octave information
    pub fn simple_interval(&self, other: &Self) -> Interval {
        let (lower, higher) = if let Ordering::Less = self.cmp(other) {
            (self, other)
        } else {
            (other, self)
        };
        Interval::from_notes(&lower.note, &higher.note)
    }

    /// The interval between one `PitchedNote` and another, including how many octaves separate them. Notes in the
    /// same octave (less than 12 half-steps apart) will have an octave of zero. Of note is that there is **no**
    /// diminished octave in this naming system. The distance between, say, G&sharp;<sub>5</sub> and 
    /// G&natural;<sub>6</sub> is indicated as a diminished unison with an octave of one. This was an intentional
    /// choice, made because an octave is equivalent to a unison for analytical purposes.
    pub fn compound_interval(&self, other: &Self) -> CompoundInterval {
        let interval = self.simple_interval(other);
        // calculate the octave using only the white keys
        let white_key_chromatic_distance = {
            let lower_wk_midi = self.note.name.chromatic_scale_degree() as isize + (CHROMATIC_SCALE as isize * self.octave);
            let higher_wk_midi = other.note.name.chromatic_scale_degree() as isize + (CHROMATIC_SCALE as isize * other.octave);
            (higher_wk_midi - lower_wk_midi).abs() as usize
        };
        let compound_octaves = white_key_chromatic_distance / CHROMATIC_SCALE;
        CompoundInterval::new(interval, compound_octaves)
    }
}

impl PartialOrd for PitchedNote {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.midi_number().cmp(&other.midi_number()))
    }
}

impl Ord for PitchedNote {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.midi_number().cmp(&other.midi_number())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompoundInterval {
    interval: Interval,
    compound_octaves: usize,
}

impl CompoundInterval {
    pub fn new(interval: Interval, compound_octaves: usize) -> CompoundInterval {
        CompoundInterval { interval, compound_octaves }
    }

    pub fn from_str(input: &str) -> Result<CompoundInterval, IntervalError> {
        // again avoiding making a regex...
        let pitch_index = input.find(|c: char| c.is_ascii_digit() || c == '-').ok_or(IntervalError::InvalidToken)?;
        let interval = Interval::from_str(&input[..pitch_index])?;
        let compound_octaves = input[pitch_index..].parse().map_err(|_| IntervalError::InvalidToken)?;
        Ok(CompoundInterval::new(interval, compound_octaves))
    }

    pub fn simple_interval(&self) -> Interval {
        self.interval.clone()
    }
}

#[cfg(test)]
mod pitched_note_tests {
    use super::*;
    use NoteName::*;
    use Accidental::*;

    #[test]
    fn midi_conversions() -> Result<(), Error> {
        assert_eq!(21, PitchedNote::new(A, Natural, 0).midi_number());
        assert_eq!(24, PitchedNote::new(C, Natural, 1).midi_number());
        assert_eq!(25, PitchedNote::new(C, Sharp(1), 1).midi_number());
        assert_eq!(25, PitchedNote::new(D, Flat(1), 1).midi_number());
        Ok(())
    }

    #[test]
    fn interval_naming() -> Result<(), Error> {
        let b4 = PitchedNote::from_str("B4")?;
        let c5 = PitchedNote::from_str("C5")?;
        assert_eq!(Interval::from_str("m2")?, b4.simple_interval(&c5));
        assert_eq!(Interval::from_str("m2")?, c5.simple_interval(&b4));
        Ok(())
    }

    #[test]
    fn compound_interval() -> Result<(), Error> {
        assert_eq!(CompoundInterval::from_str("dU1")?, PitchedNote::from_str("G#5")?.compound_interval(&PitchedNote::from_str("G6")?));
        println!("{} vs {}", PitchedNote::from_str("B5")?.midi_number(), PitchedNote::from_str("B#5")?.midi_number());
        assert_eq!(CompoundInterval::from_str("aU0")?, PitchedNote::from_str("B5")?.compound_interval(&PitchedNote::from_str("B#5")?));
        Ok(())
    }
}