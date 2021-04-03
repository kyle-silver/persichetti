# Persichetti

Persichetti is a crate that provides primitives for reasoning about harmonic concepts in western classical and contemporary music. It is named after [Vincent Persichetti](https://en.wikipedia.org/wiki/Vincent_Persichetti), composer and author of _20th Century Harmony_. Some of the concepts in that book are realized here.

## Features

Name an interval, given the two notes that make it up

```rust
let b_flat = note!("Bb")?;
let g_sharp = note!("G#")?;
assert_eq!(Interval::new(Sixth, Augmented(1))?, Interval::from_notes(&b_flat, &g_sharp));
```

Add intervals together

```rust
let major_second = Interval::new(Second, Major)?;
let perfect_fourth = Interval::from_str("p4")?;
assert_eq!(Interval::new(Fifth, Perfect)?, major_second + perfect_fourth);
```

Combine notes and intervals

```rust
let f_sharp = note!("f#")?;
let major_third = ivl!("M3")?;
assert_eq!(note!("a#")?, f_sharp + major_third);
```

Other tools include:

* Inverting intervals
* Scientific pitch and MIDI note conversion
* Generating serial tone matrixes

Work-in-progress tools include:

* Chord identification
* Extended / jazz harmony
* Neo-Remannian transformations (PLR / NSH)
* Slonimsky melodic sequences (Infrapulation, Interpolation, Ultrapolation)
* Modes of limited transposition
