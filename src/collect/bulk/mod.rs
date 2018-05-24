//! Performs bulk insertions
use std::ops::Range;
use std::iter::{ExactSizeIterator, FromIterator, TrustedLen};
use std::fmt::{self, Debug, Formatter};

use super::insertion_sort_by_key;

mod shift;

use self::shift::BulkShifter;

pub struct Insertion<T> {
    pub index: usize,
    pub element: T
}
impl<T> Insertion<T> {
    #[inline]
    pub fn new(index: usize, element: T) -> Self {
        Insertion { index, element }
    }
}
impl<T> From<(usize, T)> for Insertion<T> {
    #[inline]
    fn from(tuple: (usize, T)) -> Self {
        Insertion::new(tuple.0, tuple.1)
    }
}
impl<T: Debug> Debug for Insertion<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Insertion")
            .field(&self.index)
            .field(&self.element)
            .finish()
    }
}
pub struct InsertionSet<T> {
    insertions: Vec<Insertion<T>>
}
impl<T> InsertionSet<T> {
    #[inline]
    pub fn new() -> Self {
        InsertionSet { insertions: Vec::new() }
    }
    #[inline]
    pub fn push(&mut self, insertion: Insertion<T>) {
        self.insertions.push(insertion)
    }
    #[inline]
    pub fn insert(&mut self, index: usize, element: T) {
        self.push(Insertion { index, element })
    }
    #[inline]
    pub fn applied(mut self, mut target: Vec<T>) -> Vec<T> {
        self.apply(&mut target);
        target
    }
    #[inline]
    pub fn desired_insertions(&self) -> usize {
        self.insertions.len()
    }
    pub fn list_updated_locations(&mut self, target: &Vec<T>) -> Vec<(OriginalLocation, usize)> {
        let mut result = Vec::with_capacity(target.len() + self.desired_insertions());
        self.compute_updated_locations(
            target,
            |original, updated| result.push((original, updated))
        );
        result.sort_by_key(|&(_, updated)| updated);
        result
    }
    /// Computes all the updated locations
    pub fn compute_updated_locations<F>(&mut self, target: &Vec<T>, mut func: F)
        where F: FnMut(OriginalLocation, usize) {
        self.sort();
        compute_updated_locations(
            target,
            self.insertions.iter().rev().map(|insertion| insertion.index),
            |original, updated| {
                func(match original {
                    OriginalLocation::Original(_) => original,
                    OriginalLocation::Insertion(reversed_index) => {
                        // Convert the reversed insertion index back to the original one
                        OriginalLocation::Insertion(self.insertions.len() - (reversed_index + 1))
                    }
                }, updated)
            }
        )
    }
    /// Applies all the insertions to the specified target vector.
    ///
    /// The average runtime of this function is `O(n + m)`,
    /// where `n` is the number of existing elements and `m` is the number of insertions.
    pub fn apply(&mut self, target: &mut Vec<T>) {
        self.sort();
        apply_bulk_insertions(target, PoppingIter(&mut self.insertions));
    }
    fn sort(&mut self) {
        /*
         * Why would we possibly want to use insertion sort here?
         * First of all,
         * we need to maintain a stable sort to preserve the original order of the `Insertion`s.
         * Insertion sort has many other advantages over mergesort and quicksort,
         * and can be significantly faster in some scenarios.
         *
         * When the array is already mostly sorted, insertion sort has average running time `O(nk)`,
         * where `k` is the average distance of each element from its proper position.
         * In a randomly sorted array `k == n` giving `O(n^2)` worst case performance,
         * this isn't true in all scenarios as `k` may be significantly smaller.
         * We expect the `InsertionSet` to be mostly sorted already,
         * with only a few slightly out of place elements,
         * giving a very low average `k` value and very good running time.
         *
         * This is inspired by WebKit's choice to use bubble sort for their insertion set,
         * except that bubble sort is a terrible algorithm and insertion sort is much better.
         */
        insertion_sort_by_key(&mut *self.insertions, |insertion| insertion.index);
    }
}
impl<T> FromIterator<Insertion<T>> for InsertionSet<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item=Insertion<T>>>(iter: I) -> Self {
        InsertionSet { insertions: iter.into_iter().collect() }
    }
}
impl<T> FromIterator<(usize, T)> for InsertionSet<T> {
    #[inline]
    fn from_iter<I: IntoIterator<Item=(usize, T)>>(iter: I) -> Self {
        iter.into_iter().map(Insertion::from).collect()
    }
}
impl<T> Default for InsertionSet<T> {
    #[inline]
    fn default() -> Self {
        InsertionSet::new()
    }
}

struct PoppingIter<'a, T: 'a>(&'a mut Vec<T>);
impl<'a, T> Iterator for PoppingIter<'a, T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        self.0.pop()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.0.len(), Some(self.0.len()))
    }
}
impl<'a, T> ExactSizeIterator for PoppingIter<'a, T> {}
unsafe impl<'a, T> TrustedLen for PoppingIter<'a, T> {}

/// Applies all the specified insertions into the target vector.
///
/// The insertion iterator must be sorted in reverse order and give the proper size for its `ExactSizeIterator`.
/// Violating these constraints will never cause undefined behavior,
/// since internally we use the completely safe `BulkShifter` abstraction.
pub fn apply_bulk_insertions<T, I>(target: &mut Vec<T>, mut insertions: I)
    where I: Iterator<Item=Insertion<T>>,
          I: ExactSizeIterator,
{
    let mut shifter = BulkShifter::new(target, insertions.len());
    /*
     * We perform insertions in reverse order to reduce moving memory,
     * and ensure that the function is panic safe.
     *
     * For example, given the vector
     * and the InsertionSet `[(0, 0), (1, 2), (1, 3) (4, 9)]`:
     *
     * Since the first (working backwards) insertion is `(4, 9)`,
     * we need to to shift all elements after our first insertion
     * to the left 4 places:
     * `[1, 4, 5, 7, undef, undef, undef, undef, 11]`.
     * The element `11` will never need to be moved again,
     * since we've already made room for all future insertions.
     *
     * Next, we perform our first insertion (4, 9) at the last `undef` element:
     * `[1, 4, 5, 7, undef, undef, undef, 9, 11]`.
     * We only have 3 insertions left to perform,
     * so all future shifts will only need to move over two.
     * Then, we handle the group of insertions `[(1, 2), [(1, 3)]`,
     * and shift all elements past index 1 to the left 3 spaces:
     * [1, undef, undef, undef, 4, 5, 7, 9, 11].
     * Then we perform our desired insertions at index 1:
     * [1, undef, 2, 3, 4, 9, 11].
     * Finally, we perform the same process for the final insertion (0, 0),
     * resulting in the desired result: [0, 1, 2, 3, 4, 9, 11].
     */
    while !shifter.is_finished() {
        let Insertion { index, element } = insertions.next()
            .expect("Expected more insertions!");
        shifter.shift_original(index);
        shifter.push_shifted(element);
    }
    shifter.finish();
    assert!(insertions.is_empty(), "Unexpected insertions");
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OriginalLocation {
    /// The insertion with the specified index
    Insertion(usize),
    /// The original location at the specified original index
    Original(usize)
}

pub fn compute_updated_locations<T, I, F>(target: &[T], mut insertions: I, mut updated: F) where
    I: Iterator<Item=usize>,
    I: ExactSizeIterator,
    F: FnMut(OriginalLocation, usize) {
    // This mirrors `apply_bulk_insertions` without actually shifting memory
    let mut original_len = target.len();
    let shifted_end = original_len + insertions.len();
    let mut shifted_start = shifted_end;
    let mut insertion_id = 0;
    while original_len != shifted_start {
        let insertion_index = insertions.next()
            .expect("Expected more insertions!");
        let moved_memory = original_len - insertion_index;
        if moved_memory > 0 {
            assert!(shifted_start >= moved_memory && insertion_index <= shifted_start - moved_memory);
            update_range(
                insertion_index..original_len,
                shifted_start - moved_memory,
                &mut updated
            );
            shifted_start -= moved_memory;
            original_len = insertion_index;
        }
        assert!(shifted_start > original_len);
        shifted_start -= 1;
        updated(OriginalLocation::Insertion(insertion_id), shifted_start);
        insertion_id += 1;
    }
    for original_index in 0..original_len {
        updated(OriginalLocation::Original(original_index), original_index);
    }
    assert!(insertions.is_empty(), "Unexpected insertions");
}
#[inline]
fn update_range<F: FnMut(OriginalLocation, usize)>(original: Range<usize>, updated_start: usize, func: &mut F) {
    let mut updated = updated_start;
    for original_index in original {
        func(OriginalLocation::Original(original_index), updated);
        updated += 1;
    }
}


#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn basic() {
        /*
        * For example, given the vector `[1, 4, 5, 7, 11]`
        * and the InsertionSet `[(0, 0), (1, 2), (1, 3) (4, 9)]`:
        */
        let vector = vec![1, 4, 5, 7, 11];
        let insertions = [(0, 0), (1, 2), (1, 3), (4, 9)].iter()
            .cloned()
            .collect::<InsertionSet<u32>>();
        assert_eq!(
            insertions.applied(vector),
            vec![0, 1, 2, 3, 4, 5, 7, 9, 11]
        );
    }
    #[test]
    fn updated_locations() {
        /*
        * For example, given the vector `[1, 4, 5, 7, 11]`
        * and the InsertionSet `[(0, 0), (1, 2), (1, 3) (4, 9)]`:
        */
        let vector = vec![1, 4, 5, 7, 11];
        let mut insertions = [(0, 0), (1, 2), (1, 3), (4, 9)].iter()
            .cloned()
            .collect::<InsertionSet<u32>>();
        assert_eq!(
            insertions.list_updated_locations(&vector),
            vec![
                (OriginalLocation::Insertion(0), 0),
                (OriginalLocation::Original(0), 1),
                (OriginalLocation::Insertion(1), 2),
                (OriginalLocation::Insertion(2), 3),
                (OriginalLocation::Original(1), 4),
                (OriginalLocation::Original(2), 5),
                (OriginalLocation::Original(3), 6),
                (OriginalLocation::Insertion(3), 7),
                (OriginalLocation::Original(4), 8),
            ]
        );
    }
    #[test]
    fn empty_updated_locations() {
        let vector = vec![1, 4, 5, 7, 11];
        assert_eq!(
            InsertionSet::new().list_updated_locations(&vector),
            vec![
                (OriginalLocation::Original(0), 0),
                (OriginalLocation::Original(1), 1),
                (OriginalLocation::Original(2), 2),
                (OriginalLocation::Original(3), 3),
                (OriginalLocation::Original(4), 4),
            ]
        );
    }
}
