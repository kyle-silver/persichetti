use persichetti::{chord::{ChordType, ExtendedHarmonyChord}, note, primitives::{Error, Note}};

#[test]
fn chord_test() -> Result<(), Error> {
    let notes = vec![
        note!("C")?, note!("F")?, note!("Bb")?, note!("D")?,
        // note!("Bb")?, note!("C")?, note!("E")?, note!("Gb")?,
        // note!("B")?, note!("A")?, note!("C#")?, note!("E")?, note!("G#")?
    ];
    let notes = &notes.iter().collect();
    let possible = ChordType::subchords_ranked(notes);
    // for (k, v) in possible {
    //     println!("{}: {:?}", k, v);
    // }

    for candidate in ExtendedHarmonyChord::candidates2(notes) {
        println!("{:?}", candidate);
    }
    Ok(())
}