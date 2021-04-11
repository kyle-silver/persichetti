use persichetti::{civl, ivl, note, pnote, primitives::{Accidental::*, CompoundInterval, Error, Interval, IntervalError, IntervalQuality::*, IntervalSize::*, Note, NoteName::*, PitchedNote}};

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
        CompoundInterval::from_interval(Interval::new(Unison, Diminished(1))?, 1), 
        PitchedNote::from_str("G#5")?.compound_interval(&PitchedNote::from_str("G6")?)
    );
    println!("{} vs {}", PitchedNote::from_str("B5")?.midi_number(), PitchedNote::from_str("B#5")?.midi_number());
    assert_eq!(
        CompoundInterval::from_interval(Interval::new(Unison, Augmented(1))?, 0), 
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
    assert_eq!("DU+0", format!("{}", CompoundInterval::from_interval(ivl!("du")?, 0)).as_str());
    assert_eq!("M3+1", format!("{}", CompoundInterval::from_interval(ivl!("M3")?, 1)).as_str());
    Ok(())
}

#[test]
fn compound_interval_shorthand() -> Result<(), Error> {
    assert_eq!(CompoundInterval::from_interval(ivl!("M2")?, 0), CompoundInterval::from_str("M2+0")?);
    assert_eq!(CompoundInterval::from_interval(ivl!("D5")?, 1), CompoundInterval::from_str("d5+1")?);
    assert_eq!(CompoundInterval::from_interval(ivl!("a6")?, 30), CompoundInterval::from_str("A6+30")?);
    assert_eq!(IntervalError::InvalidToken, CompoundInterval::from_str("p5").unwrap_err());
    Ok(())
}

#[test]
fn pitched_note_macro() -> Result<(), Error> {
    assert_eq!(PitchedNote::new(C, Natural, 4), pnote!("C4")?);
    assert_eq!(PitchedNote::from_str("Dbb7")?, pnote!(D, Flat(2), 7));
    Ok(())
}

#[test]
fn compound_interval_macro() -> Result<(), Error> {
    assert_eq!(CompoundInterval::new(Fifth, Perfect, 2)?, civl!("P5+2")?);
    assert_eq!(CompoundInterval::from_str("DDU+1")?, civl!(Unison, Diminished(2), 1)?);
    Ok(())
}