use itertools::Itertools;
use std::ops::Range;
use std::iter::Rev;

pub fn for_permutation_indexes<F: FnMut(&[usize])>(n: usize, r: usize, mut func: F) {
    if r > n { return }
    let mut indexes = (0..n).collect_vec();
    let mut cycles = ((n - r + 1)..=n).rev().collect_vec();
    func(&indexes[..r]);
    'permLoop: while n > 0 {
        for i in (0..r).rev() {
            cycles[i] -= 1;
            if cycles[i] == 0 {
                let replace = indexes[i + 1..].iter().chain(&indexes[i..i + 1])
                    .cloned().collect_vec();
                indexes.truncate(i);
                indexes.extend(replace);
                cycles[i] = n - i;
            } else {
                let j = cycles[i];
                let len = indexes.len();
                indexes.swap(i, len - j);
                func(&indexes[..r]);
                continue 'permLoop;
            }
        }
        return
    }
}

pub fn permutation_indexes(n: usize, r: usize) -> IterPermutationIndexes {
    // Iterator version of for_permuation_indexes
    IterPermutationIndexes {
        r, n, inner: (0..r).rev(),
        initial: true,
        indexes: (0..n).collect_vec(),
        cycles: ((n - r + 1)..=n).rev().collect_vec()
    }
}

pub struct IterPermutationIndexes {
    r: usize,
    n: usize,
    inner: Rev<Range<usize>>,
    initial: bool,
    indexes: Vec<usize>,
    cycles: Vec<usize>,
}
impl Iterator for IterPermutationIndexes {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.r > self.n { return None }
        if self.initial {
            self.initial = false;
            return Some(Vec::from(&self.indexes[..self.r]));
        }
        while self.n > 0 {
            if let Some(i) = self.inner.next() {
                self.cycles[i] -= 1;
                if self.cycles[i] == 0 {
                    let replace = self.indexes[i + 1..].iter().chain(&self.indexes[i..i+1])
                        .cloned().collect_vec();
                    self.indexes.truncate(i);
                    self.indexes.extend(replace);
                    self.cycles[i] = self.n - i;
                } else {
                    let j = self.cycles[i];
                    let len = self.indexes.len();
                    self.indexes.swap(i, len - j);
                    let val = Vec::from(&self.indexes[..self.r]);
                    // Restart the inner loop
                    self.inner = (0..self.r).rev();
                    return Some(val);
                }
            } else {
                break
            }
        }
        return None
    }
}

#[cfg(test)]
mod test {
    use crate::collect::permutations::{permutation_indexes, for_permutation_indexes};
    use itertools::Itertools;

    #[test]
    fn test_for_permutation_indexes() {
        for &(n, r) in &[(2, 2), (3, 3), (3, 2)] {
            let mut actual = Vec::new();
            for_permutation_indexes(n, r, |s| actual.push(Vec::from(s)));
            assert_eq!(permutation_indexes(n, r).collect_vec(), actual);
        }
    }

    #[test]
    fn test_permutation_indexes() {
        assert_eq!(
            permutation_indexes(2, 2).collect_vec(),
            vec![vec![0, 1], vec![1, 0]]
        );
        assert_eq!(
            permutation_indexes(3, 3).collect_vec(),
            vec![
                vec![0, 1, 2], vec![0, 2, 1], vec![1, 0, 2],
                vec![1, 2, 0], vec![2, 0, 1], vec![2, 1, 0]
            ]
        );
        assert_eq!(
            permutation_indexes(3, 2).collect_vec(),
            vec![
                vec![0, 1], vec![0, 2], vec![1, 0],
                vec![1, 2], vec![2, 0], vec![2, 1]
            ]
        );
    }
}