use persichetti::{ivl, note, primitives::{Accidental::*, CompoundInterval, Error, Interval, IntervalQuality::*, IntervalSize::*, Note, NoteName::*, PitchedNote}};

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
    assert_eq!(
        CompoundInterval::new(Interval::new(Unison, Diminished(1))?, 1), 
        PitchedNote::from_str("G#5")?.compound_interval(&PitchedNote::from_str("G6")?)
    );
    println!("{} vs {}", PitchedNote::from_str("B5")?.midi_number(), PitchedNote::from_str("B#5")?.midi_number());
    assert_eq!(
        CompoundInterval::new(Interval::new(Unison, Augmented(1))?, 0), 
        PitchedNote::from_str("B5")?.compound_interval(&PitchedNote::from_str("B#5")?)
    );
    Ok(())
}

#[test]
fn pitched_note_display() -> Result<(), Error> {
    assert_eq!("G#x-1", format!("{}", PitchedNote::from_note(note!("G###")?, -1)).as_str());
    Ok(())
}

#[test]
fn compound_interval_display() -> Result<(), Error> {
    assert_eq!("DU+0", format!("{}", CompoundInterval::new(ivl!("du")?, 0)).as_str());
    assert_eq!("M3+1", format!("{}", CompoundInterval::new(ivl!("M3")?, 1)).as_str());
    Ok(())
}