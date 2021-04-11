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
//! [`Interval`]s have a string representation as well. Capital and lowercase M&rsquo;s are case-sensitive, but all other qualities are not.
//! When an interval is doubly augmented or doubly diminished, it can be represented as `"aa5"` (doubly-augmented fifth) or `"dd3"
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

use std::{cmp::Ordering, fmt::Display, ops::{Add, Sub}, usize};

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

    /// Simple &ldquo;white key only&rdquo; computations without any consideration for accidentals.
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

impl Display for NoteName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            NoteName::C => "C",
            NoteName::D => "D",
            NoteName::E => "E",
            NoteName::F => "F",
            NoteName::G => "G",
            NoteName::A => "A",
            NoteName::B => "B",
        };
        f.write_str(name)
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

impl Display for Accidental {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Accidental::Flat(d) => {
                let flats: String = std::iter::repeat("b").take(*d).collect();
                f.write_str(&flats)
            },
            Accidental::Natural => {
                f.write_str("nat")
            },
            Accidental::Sharp(d) => {
                let prefix = if d % 2 == 1 { "#" } else { "" };
                let remainder = std::iter::repeat("x").take(d/2).collect::<String>();
                write!(f, "{}{}", prefix, remainder)
            }
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

impl Display for Note {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.accidental {
            Accidental::Natural => write!(f, "{}", self.name),
            _ => write!(f, "{}{}", self.name, self.accidental),
        }
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

impl Display for IntervalSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token = match self {
            IntervalSize::Unison => 'U',
            IntervalSize::Second => '2',
            IntervalSize::Third => '3',
            IntervalSize::Fourth => '4',
            IntervalSize::Fifth => '5',
            IntervalSize::Sixth => '6',
            IntervalSize::Seventh => '7',
        };
        write!(f, "{}", token)
    }
}

/// Represents a chromatic alteration up or down that is applied to a pure [`IntervalSize`]. Similar to 
/// [`Accidental`], using a zero with `Diminished` or `Augmented` is a no-op and probably not useful.
///
/// Due to a quirk of nomenclature, the value of `Diminished(n)` will vary depending on whether an interval
/// is `Perfect` or `Major`. For example, a diminished fifth has an alteration of &minus;1, but a diminished
/// third has an alteration of &minus;2.
/// 
/// | Quality | Size | Chromatic Alteration |
/// | --- | --- | --- |
/// | Major | Second, Third, Sixth, Seventh | 0 |
/// | Minor | Second, Third, Sixth, Seventh | &minus;1 |
/// | Perfect | Unison, Fourth, Fifth | 0 |
/// | Diminished(N) | Second, Third, Sixth, Seventh | &minus;1 &times; (N&plus;1) |
/// | Diminished(N) | Unison, Fourth, Fifth | &minus;N |
/// | Augmented(N) | All Intervals | (&plus;N)
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IntervalQuality {
    Diminished(usize),
    Augmented(usize),
    Minor,
    Major,
    Perfect,
}

impl IntervalQuality {
    /// Given the interval size, (how many note names are between the top and bottom) calculate an interval
    /// quality that will make the [`Interval`] span the correct number of whole steps.
    pub fn from_chromatic_span(size: &IntervalSize, span: isize) -> IntervalQuality {
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

impl Display for IntervalQuality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token: String = match self {
            IntervalQuality::Diminished(d) => std::iter::repeat("D").take(*d).collect(),
            IntervalQuality::Augmented(d) => std::iter::repeat("A").take(*d).collect(),
            IntervalQuality::Minor => "m".to_string(),
            IntervalQuality::Major => "M".to_string(),
            IntervalQuality::Perfect => "P".to_string(),
        };
        write!(f, "{}", token)
    }
}

/// An Interval is a combination of an [`IntervalSize`] and an [`IntervalQuality`]. It represents the relationship
/// between two [`Note`]s.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Interval {
    size: IntervalSize,
    quality: IntervalQuality,
}

impl Interval {  
    /// Instantiate a new `Interval`. If an invalid combination is entered, such as a major fourth or a perfect third,
    /// the function will return an error.
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

    /// Instantiate an `Interval` using the shorthand documented in [`crate::primitives`]. If the token cannot be
    /// parsed or the desired interval is invalid, an error will be returned.
    pub fn from_str(input: &str) -> Result<Interval, IntervalError> {
        lazy_static! {
            static ref RE: Regex = Regex::new("^(?P<quality>([MmPpDd]|[Aa]+|[Dd]+))(?P<size>([Uu]|\\d+))$").unwrap();
        }
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

    /// The number of half-steps up or down that the [`IntervalQuality`] adjusts the [`IntervalSize`] by.
    /// Keep in mind that the value of a `Diminished` interval depends on whether the size is Perfect or Major.
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

    /// The number of half steps between the top and bottom note in the interval. In the case of a diminished
    /// unison, the output could be negative
    /// ```rust
    /// # use persichetti::primitives::*;
    /// # use persichetti::primitives::{IntervalSize::*, IntervalQuality::*};
    /// # fn main() -> Result<(), IntervalError> {
    /// assert_eq!(7, Interval::new(Fifth, Perfect)?.chromatic_size());
    /// assert_eq!(-1, Interval::new(Unison, Diminished(1))?.chromatic_size());
    /// assert_eq!(-3, Interval::new(Second, Diminished(4))?.chromatic_size());
    /// # Ok(())
    /// # }
    /// ```
    pub fn chromatic_size(&self) -> isize {
        self.size.chromatic_size() as isize + self.chromatic_alteration()
    }

    /// Given two [`Note`]s, calculate the interval that describes their relationship. While there is discourse around
    /// the existence of a [diminished unison](https://www.youtube.com/watch?v=y5DxegJ5Hmw), this library acknowledges
    /// its existence since there is no concept of an octave.
    /// ```rust
    /// # use persichetti::{ivl, note};
    /// # use persichetti::primitives::*;
    /// # fn main() -> Result<(), Error> {
    /// assert_eq!(ivl!("A6")?, Interval::from_notes(&note!("Bb")?, &note!("G#")?));
    /// assert_eq!(ivl!("dU")?, Interval::from_notes(&note!("F#")?, &note!("F")?));
    /// # Ok(())
    /// # }
    /// ```
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

impl Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.quality, self.size)
    }
}

/// A pitched note represents a [`Note`] in a definite octave. C<sub>4</sub> is &ldquo;middle C&rdquo; 
/// and A<sub>0</sub> is the lowest note on a standard piano
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

    /// Instantiate using the same shorthand as a normal [`Note`] documented in [`crate::primitives`], but 
    /// using an additional octave indicator.
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

impl Display for PitchedNote {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.note, self.octave)
    }
}

/// A compound interval represents an [`Interval`] that may span more than one octave.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompoundInterval {
    interval: Interval,
    compound_octaves: usize,
}

impl CompoundInterval {
    pub fn new(interval: Interval, compound_octaves: usize) -> CompoundInterval {
        CompoundInterval { interval, compound_octaves }
    }

    pub fn simple_interval(&self) -> Interval {
        self.interval.clone()
    }
}

impl Display for CompoundInterval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}+{}", self.interval, self.compound_octaves)
    }
}
