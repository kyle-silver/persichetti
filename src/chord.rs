use std::{collections::{HashMap, HashSet}};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use derive_more::Constructor;

use crate::primitives::{Accidental, Interval, Note, consts::*};

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
    pub fn subchords<'a>(notes: &HashSet<&'a Note>) -> HashMap<&'a Note, HashSet<ChordType>> {
        notes.iter().map(|&n| {
            let intervals = notes.iter().map(|other| {
                Interval::from_notes(n, other)
            }).collect();
            (n, ChordType::from_interval_set(&intervals))
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

    fn tensions(&self, root: &Note, notes: &HashSet<&Note>) -> HashSet<Extension> {
        let intervals = self.intervals_from_root();
        notes.iter()
            .filter(|&&note| note != root)
            .filter_map(|&note| {
                let interval = Interval::from_notes(root, &note);
                if intervals.contains(&interval) {
                    None
                } else {
                    Some(interval)
                }
            })
            .map(|interval| Extension::from(interval))
            .collect()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub enum TensionName {
    Root,
    Third,
    Fifth,
    Seventh,
    Ninth,
    Eleventh,
    Thirteenth,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Extension {
    alteration: Accidental,
    name: TensionName
}

impl Extension {
    fn new(alteration: Accidental, name: TensionName) -> Extension {
        Extension { alteration, name }
    }
}

impl From<Interval> for Extension {
    fn from(interval: Interval) -> Self {
        use crate::primitives::IntervalSize;
        let alteration = Accidental::from_isize(interval.chromatic_alteration());
        let name = match interval.size() {
            IntervalSize::Unison => TensionName::Root,
            IntervalSize::Second => TensionName::Ninth,
            IntervalSize::Third => TensionName::Third,
            IntervalSize::Fourth => TensionName::Eleventh,
            IntervalSize::Fifth => TensionName::Fifth,
            IntervalSize::Sixth => TensionName::Thirteenth,
            IntervalSize::Seventh => TensionName::Seventh,
        };
        Extension::new(alteration, name)
    }
}

#[derive(Debug, Constructor, PartialEq, Eq, Clone)]
pub struct ExtendedHarmonyChord<'a> {
    root: &'a Note,
    chord_type: ChordType,
    extensions: HashSet<Extension>
}

impl<'a> ExtendedHarmonyChord<'a> {
    pub fn candidates(notes: &HashSet<&'a Note>) -> Vec<ExtendedHarmonyChord<'a>> {
        let mut candidates: Vec<ExtendedHarmonyChord> = ChordType::subchords(notes).iter()
            .map(|(&root, subchords)| {
                let chords: Vec<_> = subchords.iter().map(|chord_type| {
                    let extensions = chord_type.tensions(root, notes);
                    ExtendedHarmonyChord::new(root, chord_type.clone(), extensions)
                }).collect();
                chords
            })
            .flat_map(|v| v)
            .collect();
        candidates.sort_by(|a, b| {
            a.extensions.len().cmp(&b.extensions.len()).reverse()
        });
        candidates
    }
}