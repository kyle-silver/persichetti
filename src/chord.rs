use std::collections::{HashMap, HashSet};

use crate::primitives::{Interval, Note, consts::*};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    pub fn possible_chords(notes: HashSet<&Note>) -> HashMap<&Note, HashSet<ChordType>> {
        notes.iter().map(|n| {
            let intervals = notes.iter().map(|other| {
                Interval::from_notes(n, other)
            }).collect();
            (*n, ChordType::from_interval_set(&intervals))
        }).collect()
    }

    fn from_interval_set(intervals: &HashSet<Interval>) -> HashSet<ChordType> {
        let mut subchords = HashSet::new();
        if intervals.contains(&MAJOR_THIRD) && intervals.contains(&PERFECT_FIFTH) {
            subchords.insert(ChordType::Major);
        }
        if intervals.contains(&MINOR_THIRD) && intervals.contains(&PERFECT_FIFTH) {
            subchords.insert(ChordType::Minor);
        }
        if intervals.contains(&MINOR_THIRD) && intervals.contains(&DIMINISHED_FIFTH) {
            subchords.insert(ChordType::Diminished);
        }
        if intervals.contains(&MAJOR_THIRD) && intervals.contains(&AUGMENTED_FIFTH) {
            subchords.insert(ChordType::Augmented);
        }
        if intervals.contains(&MAJOR_SECOND) && intervals.contains(&PERFECT_FIFTH) {
            subchords.insert(ChordType::Sus2);
        }
        if intervals.contains(&PERFECT_FOURTH) && intervals.contains(&PERFECT_FIFTH) {
            subchords.insert(ChordType::Sus4);
        }
        if intervals.contains(&MINOR_SEVENTH) && intervals.contains(&MAJOR_THIRD) {
            subchords.insert(ChordType::Seventh);
        }
        if intervals.contains(&MINOR_SEVENTH) && intervals.contains(&MINOR_THIRD) {
            subchords.insert(ChordType::MinorSeventh);
        }
        if intervals.contains(&MAJOR_SEVENTH) && intervals.contains(&MINOR_THIRD) {
            subchords.insert(ChordType::MinorMajorSeventh);
        }
        if intervals.contains(&MINOR_SEVENTH) && !intervals.contains(&MAJOR_THIRD) && !intervals.contains(&MINOR_THIRD) {
            subchords.insert(ChordType::SuspendedSeventh);
        }
        if intervals.contains(&DIMINISHED_SEVENTH) {
            subchords.insert(ChordType::DiminishedSeventh);
        }
        if intervals.contains(&MAJOR_THIRD) && intervals.contains(&AUGMENTED_SIXTH) {
            subchords.insert(ChordType::ItalianSixth);
        }
        if intervals.contains(&MAJOR_THIRD) && intervals.contains(&AUGMENTED_FOURTH) && intervals.contains(&AUGMENTED_SIXTH) {
            subchords.insert(ChordType::FrenchSixth);
        }
        if intervals.contains(&MAJOR_THIRD) && intervals.contains(&PERFECT_FIFTH) && intervals.contains(&AUGMENTED_SIXTH) {
            subchords.insert(ChordType::GermanSixth);
        }
        subchords
    }
}
