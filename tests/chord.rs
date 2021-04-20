use persichetti::{chord::ChordType, note, primitives::{Error, Note}};

#[test]
fn chord_test() -> Result<(), Error> {
    let notes = vec![
        // note!("Bb")?, note!("C")?, note!("E")?, note!("G")?,
        note!("Bb")?, note!("D")?, note!("E")?, note!("G#")?,
    ];
    let notes = notes.iter().collect();
    let possible = ChordType::possible_chords(notes);
    for (k, v) in possible {
        println!("{}: {:?}", k, v);
    }
    Ok(())
}