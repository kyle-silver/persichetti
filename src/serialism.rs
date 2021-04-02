use std::{array::IntoIter, convert::TryInto, usize};

use itertools::Itertools;

#[derive(Debug)]
pub enum ToneRowError{
    DuplicateElements,
    TooBig(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToneRow<const D: usize> {
    row: [[usize; D]; D],
}

impl<const D: usize> ToneRow<D> {
    pub fn new(p0: [usize; D]) -> Result<ToneRow<D>, ToneRowError> {
        // a very silly edge case, hopefully this gets optimized away...
        if D == 0 {
            return Ok(ToneRow { row: [[0usize; D]; D] })
        }
        // error checking
        if let Some(n) = IntoIter::new(p0).find(|n| *n >= D) {
            return Err(ToneRowError::TooBig(n));
        }
        if D != IntoIter::new(p0).unique().count() {
            return Err(ToneRowError::DuplicateElements)
        }
        // calculae matrix
        let intervals: Vec<_> = p0.windows(2).map(|w| w[1] as isize - w[0] as isize)
            .map(|i| i.rem_euclid(D as isize))
            .collect();
        println!("{:?}", intervals);

        // populate prime row
        let mut row = [[0usize; D]; D];
        row[0] = p0;
        for (i, interval) in intervals.iter().enumerate() {
            row[i+1][0] = (row[i][0] as isize - *interval).rem_euclid(D as isize) as usize;
            for j in 1..D {
                row[i+1][j] = (row[i+1][j-1] as isize + intervals[j-1]).rem_euclid(D as isize) as usize;
            }
        }
        Ok(ToneRow { row })
    }

    /// Retrieve a prime row as a numeric slice. If you need the retrograde, call [`rev`] on it.
    ///
    /// [`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn p(&self, index: usize) -> &[usize; D] {
        let initial = self.row[0][0];
        for i in 0..D {
            if self.row[i][0] == (initial + index) % 12 {
                return &self.row[i];
            }
        }
        panic!("Could not find desired prime row; The index may be out of bounds.");
    }

    /// Retrieve a prime row as a numeric array. If you need the retrograde, call [`rev`] on it.
    ///
    /// [`rev`]: https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.rev
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn i(&self, index: usize) -> [usize; D] {
        let initial = self.row[0][0];
        for i in 0..D {
            if self.row[0][i] == (initial + index) % 12 {
                let v: Vec<_> = IntoIter::new(self.row).map(|r| r[i]).collect();
                return v.try_into().expect("Could not convert vec to array");
            }
        }
        panic!("Could not find desired prime row; The index may be out of bounds.");
    }

}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn basic() -> Result<(), ToneRowError> {
        let tr = ToneRow::new([5,7,9,11,3,2,6,8,10,1,0,4])?;
        for r in 0..12 {
            for c in 0..12 {
                print!("{}\t", tr.row[r][c]);
            }
            println!("");
        }
        Ok(())
    }

    #[test]
    pub fn test_p() -> Result<(), ToneRowError> {
        let tr = ToneRow::new([5,7,9,11,3,2,6,8,10,1,0,4])?;
        assert_eq!(tr.p(1), &[6,8,10,0,4,3,7,9,11,2,1,5]);
        Ok(())
    }

    #[test]
    pub fn test_i() -> Result<(), ToneRowError> {
        let tr = ToneRow::new([5,7,9,11,3,2,6,8,10,1,0,4])?;
        assert_eq!(tr.i(11), [4,2,0,10,6,7,3,1,11,8,9,5]);
        Ok(())
    }
}