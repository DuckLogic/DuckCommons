//! Performs bulk insertions
use std::iter::FromIterator;
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
    /// Applies all the insertions to the specified target vector.
    ///
    /// The average runtime of this function is `O(n + m)`,
    /// where `n` is the number of existing elements and `m` is the number of insertions.
    pub fn apply(&mut self, target: &mut Vec<T>) {
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
        let mut shifter = BulkShifter::new(target, self.insertions.len());
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
            let Insertion { index, element } = self.insertions.pop()
                .expect("Expected more insertions!");
            shifter.shift_original(index);
            shifter.push_shifted(element);
        }
        shifter.finish();
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
}
