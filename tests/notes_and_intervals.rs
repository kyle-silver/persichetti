use persichetti::{ivl, note, primitives::{Accidental::*, Error, Interval, IntervalError, IntervalQuality::*, IntervalSize::*, Note, NoteError, NoteName::*}};

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
    assert_eq!(Note::from_str("G#")?, &Note::from_str("C")? + &Interval::new(Fifth, Augmented(0))?);
    assert_eq!(Note::from_str("Gx")?, &Note::from_str("C")? + &Interval::new(Fifth, Augmented(1))?);
    assert_eq!(Note::from_str("F#")?, &Note::from_str("Bb")? + &Interval::new(Fifth, Augmented(0))?);
    assert_eq!(Note::from_str("Fx")?, &Note::from_str("B")? + &Interval::new(Fifth, Augmented(0))?);
    assert_eq!(Note::from_str("G#")?, &Note::from_str("Bb")? + &Interval::new(Sixth, Augmented(0))?);
    assert_eq!(Note::from_str("Gx")?, &Note::from_str("B")? + &Interval::new(Sixth, Augmented(0))?);
    assert_eq!(Note::from_str("Dbb")?, &Note::from_str("C")? + &Interval::new(Second, Diminished(0))?);
    assert_eq!(Note::from_str("D")?, &Note::from_str("B")? + &Interval::new(Third, Minor)?);
    assert_eq!(Note::from_str("Db")?, &Note::from_str("B")? + &Interval::new(Third, Diminished(0))?);
    assert_eq!(Note::from_str("D#")?, &Note::from_str("B")? + &Interval::new(Third, Major)?);
    assert_eq!(Note::from_str("Dx")?, &Note::from_str("B")? + &Interval::new(Third, Augmented(0))?);
    assert_eq!(Note::from_str("D#x")?, &Note::from_str("B")? + &Interval::new(Third, Augmented(1))?);
    assert_eq!(Note::from_str("C#")?, &Note::from_str("C")? + &Interval::new(Unison, Augmented(0))?);
    assert_eq!(Note::from_str("Cb")?, &Note::from_str("C")? + &Interval::new(Unison, Diminished(0))?);
    assert_eq!(Note::from_str("G")?, &Note::from_str("G#")? + &Interval::new(Unison, Diminished(0))?);
    Ok(())
}

#[test]
fn interval_naming() -> Result<(), Error> {
    assert_eq!(Interval::new(Fifth, Perfect)?, Interval::from_notes(&Note::from_str("C")?, &Note::from_str("G")?));
    assert_eq!(Interval::new(Fifth, Perfect)?, Interval::from_notes(&Note::from_str("G")?, &Note::from_str("D")?));
    assert_eq!(Interval::new(Third, Major)?, Interval::from_notes(&Note::from_str("Bb")?, &Note::from_str("D")?));
    assert_eq!(Interval::new(Unison, Augmented(0))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("B#")?));
    assert_eq!(Interval::new(Third, Minor)?, Interval::from_notes(&Note::from_str("A")?, &Note::from_str("C")?));
    assert_eq!(Interval::new(Third, Diminished(0))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("Db")?));
    assert_eq!(Interval::new(Third, Diminished(1))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("Dbb")?));
    assert_eq!(Interval::new(Fourth, Augmented(0))?, Interval::from_notes(&Note::from_str("A")?, &Note::from_str("D#")?));
    assert_eq!(Interval::new(Fifth, Diminished(0))?, Interval::from_notes(&Note::from_str("A")?, &Note::from_str("Eb")?));
    assert_eq!(Interval::new(Fourth, Augmented(0))?, Interval::from_notes(&Note::from_str("Eb")?, &Note::from_str("A")?));
    assert_eq!(Interval::new(Fifth, Diminished(0))?, Interval::from_notes(&Note::from_str("Eb")?, &Note::from_str("Bbb")?));
    assert_eq!(Interval::new(Unison, Diminished(0))?, Interval::from_notes(&Note::from_str("C")?, &Note::from_str("Cb")?));
    assert_eq!(Interval::new(Unison, Diminished(1))?, Interval::from_notes(&Note::from_str("C")?, &Note::from_str("Cbb")?));
    assert_eq!(Interval::new(Unison, Diminished(0))?, Interval::from_notes(&Note::from_str("G#")?, &Note::from_str("G")?));
    assert_eq!(Interval::new(Third, Diminished(6))?, Interval::from_notes(&Note::from_str("B")?, &Note::from_str("Dbbbbbbb")?));
    Ok(())
}

#[test]
fn intervals_from_strings() -> Result<(), Error> {
    // valid
    assert_eq!(ivl!(Fifth, Perfect)?, Interval::from_str("p5")?);
    assert_eq!(ivl!(Third, Augmented(0))?, Interval::from_str("a3")?);
    assert_eq!(ivl!(Seventh, Diminished(1))?, Interval::from_str("dd7")?);
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
    assert_eq!(ivl!(Third, Diminished(0))?, ivl!(Sixth, Augmented(0))?.inverse());
    assert_eq!(ivl!(Unison, Augmented(0))?, ivl!(Unison, Diminished(0))?.inverse());
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
    assert_eq!(ivl!(Fourth, Augmented(0))?, ivl!(Third, Major)? + ivl!(Second, Major)?);
    assert_eq!(ivl!(Unison, Perfect)?, ivl!(Fourth, Augmented(0))? + ivl!(Fifth, Diminished(0))?);
    assert_eq!(ivl!(Unison, Perfect)?, ivl!(Unison, Augmented(0))? + ivl!(Unison, Diminished(0))?);
    assert_eq!(ivl!(Sixth, Augmented(0))?, ivl!(Seventh, Major)? + ivl!(Seventh, Major)?);
    // test
    assert_eq!(ivl!("pU")?, ivl!("M2")? + ivl!("M2")? + ivl!("m2")? + ivl!("M2")? + ivl!("M2")? + ivl!("M2")? + ivl!("m2")?);
    // augmented triad
    assert_eq!(ivl!(Fifth, Augmented(0))?, ivl!("M3")? + ivl!("M3")?);
    // diminished seventh
    assert_eq!(ivl!(Seventh, Diminished(0))?, ivl!("m3")? +  ivl!("m3")? + ivl!("m3")?);
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

#[test]
fn note_display() -> Result<(), Error> {
    assert_eq!("C", format!("{}", note!("C")?).as_str());
    assert_eq!("E", format!("{}", note!("Enat")?).as_str());
    assert_eq!("Bb", format!("{}", note!("Bb")?).as_str());
    assert_eq!("G#xx", format!("{}", note!("G#####")?).as_str());
    assert_eq!("Fx", format!("{}", note!("Fx")?).as_str());
    assert_eq!("Abb", format!("{}", note!("Abb")?).as_str());
    assert_eq!("D#", format!("{}", note!("D#")?).as_str());
    Ok(())
}

#[test]
fn interval_display() -> Result<(), Error> {
    assert_eq!("P5", format!("{}", ivl!("p5")?).as_str());
    assert_eq!("AAAA3", format!("{}", ivl!("aaaa3")?).as_str());
    assert_eq!("m6", format!("{}", ivl!("m6")?).as_str());
    assert_eq!("PU", format!("{}", ivl!("pu")?).as_str());
    assert_eq!("D7", format!("{}", ivl!("d7")?).as_str());
    assert_eq!("M2", format!("{}", ivl!("M2")?).as_str());
    Ok(())
}
