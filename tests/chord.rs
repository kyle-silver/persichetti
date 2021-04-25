use persichetti::{chord::{ChordType, ExtendedHarmonyChord}, note, primitives::{Error, Note}};

#[test]
fn chord_test() -> Result<(), Error> {
    let notes = vec![
        // note!("Bb")?, note!("C")?, note!("E")?, note!("G")?,
        note!("B")?, note!("A")?, note!("C#")?, note!("E")?, note!("G#")?
    ];
    let notes = &notes.iter().collect();
    let possible = ChordType::subchords(notes);
    for (k, v) in possible {
        println!("{}: {:?}", k, v);
    }
    for candidate in ExtendedHarmonyChord::candidates(notes) {
        println!("{:#?}", candidate);
    }
    Ok(())
}