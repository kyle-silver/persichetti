use std::collections::{HashMap, HashSet};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::primitives::{Interval, Note, consts::*};

#[derive(Debug, PartialEq, Eq, Clone, Hash, EnumIter)]
pub enum ChordType {
    Major,
    Minor,
    Diminished,
    Augmented,
    Sus2,
    Sus4,
    Seventh,
    MinorSeventh,
    MajorSeventh,
    MinorMajorSeventh,
    SuspendedSeventh,
    DiminishedSeventh,
    ItalianSixth,
    FrenchSixth,
    GermanSixth,
}

impl ChordType {
    /// Given a collection of [`Note`] values, returns a list of potential chord qualities.
    /// This does not take into account any harmonic context or voicing, which may be crucial
    /// to correctly identifying the chord.
    pub fn subchords(notes: HashSet<&Note>) -> HashMap<&Note, HashSet<ChordType>> {
        notes.iter().map(|n| {
            let intervals = notes.iter().map(|other| {
                Interval::from_notes(n, other)
            }).collect();
            (*n, ChordType::from_interval_set(&intervals))
        }).collect()
    }

    fn from_interval_set(intervals: &HashSet<Interval>) -> HashSet<ChordType> {
        Self::iter()
            .filter(|chord_type| chord_type.is_subchord(intervals))
            .collect()
    }

    pub fn intervals_from_root(&self) -> &[Interval] {
        use ChordType::*;
        match self {
            Major => &[MAJOR_THIRD, PERFECT_FIFTH],
            Minor => &[MINOR_THIRD, PERFECT_FIFTH],
            Diminished => &[MINOR_THIRD, DIMINISHED_FIFTH],
            Augmented => &[MAJOR_THIRD, AUGMENTED_FIFTH],
            Sus2 => &[MAJOR_SECOND, PERFECT_FIFTH],
            Sus4 => &[PERFECT_FOURTH, PERFECT_FIFTH],
            Seventh => &[MAJOR_THIRD, MINOR_SEVENTH],
            MinorSeventh => &[MINOR_THIRD, MINOR_SEVENTH],
            MajorSeventh => &[MAJOR_THIRD, MAJOR_SEVENTH],
            MinorMajorSeventh =>  &[MINOR_THIRD, MAJOR_SEVENTH],
            SuspendedSeventh => &[PERFECT_FOURTH, MINOR_SEVENTH],
            DiminishedSeventh => &[DIMINISHED_SEVENTH],
            ItalianSixth => &[MAJOR_THIRD, AUGMENTED_SIXTH],
            FrenchSixth => &[MAJOR_THIRD, AUGMENTED_FOURTH, AUGMENTED_SIXTH],
            GermanSixth => &[MAJOR_THIRD, PERFECT_FIFTH, AUGMENTED_SIXTH],
        }
    }

    pub fn is_subchord(&self, of: &HashSet<Interval>) -> bool {
        let intervals = of;
        self.intervals_from_root().iter().all(|interval| intervals.contains(interval))
    }
}
