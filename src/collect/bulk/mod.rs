//! Performs bulk insertions
use std::cmp::{Ord, Ordering, Reverse};
use super::insertion_sort;
use std::ptr;

pub struct Insertion<T> {
    pub index: usize,
    pub element: T
}
impl<T> Ord for Insertion<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(other.index)
    }
}
pub struct InsertionSet<T>(Vec<Insertion<T>>);
impl<T> InsertionSet<T> {
    /// Applies all the insertions to the specified target vector.
    ///
    /// The average runtime of this function is `O(n + m)`,
    /// where `n` is the number of existing elements and `m` is the number of insertions.
    pub fn apply(self, target: &mut Vec<T>) {
        /*
         * Why would we possibly want to use insertion sort here?
         * First of all,
         * we need to maintain a stable sort to preserve the order of the `Insertion`s.
         * Insertion sort has many other advantages over mergesort and quicksort,
         * and can be significantly faster in some scenarios.
         *
         * Insertion sort has average running time `O(nk)`,
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
        insertion_sort(&mut self.0);
        /*
         * We perform insertions in reverse order to reduce moving memory,
         * and ensure that the function is panic safe.
         *
         * For example, given the vector `[1, 4, 5, 7, 11]`
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
        unsafe {
            let original_insertions = self.0.len();
            let original_len = target.len();
            let mut remaining_insertions = original_insertions;
            target.reserve(remaining_insertions);
            target.set_len(0);
            let ptr = target.as_mut_ptr();
            // TODO: Unit test so I can implement this correctly
            unimplemented!()
        }

    }
}

