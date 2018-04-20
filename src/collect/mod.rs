use std::cmp::Ordering;
use std::hash::BuildHasherDefault;

use itertools::Itertools;
use ordermap::{OrderSet, OrderMap};
use seahash::SeaHasher;

#[macro_use]
pub mod macros;
pub mod bitset;
pub mod vecmap;
pub mod vecset;
pub mod bulk;
pub mod compare;

pub use self::compare::SliceCompare;
pub use self::bitset::SmallBitSet;
#[deprecated(note = "Please import directly from `two_sided_vec`")]
pub use two_sided_vec::TwoSidedVec;
pub use self::vecmap::VecMap;
pub use self::vecset::VecSet;

pub type SeaHashBuildHasher = BuildHasherDefault<SeaHasher>;
pub type SeaHashOrderMap<K, V> = OrderMap<K, V, SeaHashBuildHasher>;
pub type SeaHashOrderSet<T> = OrderSet<T, SeaHashBuildHasher>;

#[inline]
pub fn is_sorted_by_key<T, B: Ord, F: FnMut(&T) -> B>(data: &[T], mut func: F) -> bool {
    is_sorted_with(data, |first, second| func(first).cmp(&func(second)))
}

#[inline]
pub fn is_sorted_with<T, F: FnMut(&T, &T) -> Ordering>(data: &[T], mut func: F) -> bool {
    for (prev, element) in data.iter().tuple_windows() {
        // if element < prev
        if func(element, prev) == Ordering::Less {
            return false;
        }
    }
    true
}

#[inline]
pub fn is_contiguous_keys<T, F>(values: &[T], mut func: F) -> bool where F: FnMut(&T) -> u64 {
    let mut iter = values.iter();
    if let Some(first) = iter.next() {
        let mut prev_id = func(first);
        for value in iter {
            let value_id = func(value);
            if prev_id + 1 != value_id {
                return false;
            }
            prev_id = value_id
        }
    }
    true
}

/// Performs an [insertion sort](https://en.wikipedia.org/wiki/Insertion_sort)
/// on the specified slice.
///
/// Unfortunately, this algorithm has quadratic worst-case complexity,
/// and is much slower then than quicksort and mergesort for large inputs.
/// Its should only be used if the input is small or already mostly sorted,
/// as described on the wikipedia page.
#[inline]
pub fn insertion_sort<T: Ord>(target: &mut [T]) {
    insertion_sort_by(target, T::cmp)
}

/// Performs an insertion sort on the specified slice,
/// comparing values using the specified function.
pub fn insertion_sort_by<T, F>(target: &mut [T], mut compare: F) where F: FnMut(&T, &T) -> Ordering {
    for i in 1..target.len() {
        let mut j = i;
        while j > 0 && compare(&target[j - 1], &target[j]) == Ordering::Greater {
            target.swap(j, j - 1);
            j -= 1;
        }
    }
}

/// Performs an insertion sort on the specified slice,
/// comparing values using the specified function.
#[inline]
pub fn insertion_sort_by_key<T, B, F>(target: &mut [T], mut func: F) where B: Ord, F: FnMut(&T) -> B {
    insertion_sort_by(target, |first, second| func(first).cmp(&func(second)))
}


#[inline]
pub fn find_duplicates<T: PartialEq, F>(target: &[T]) -> Option<((usize, &T), (usize, &T))> {
    find_duplicates_by(target, |first, second| first == second)
}

#[inline]
pub fn find_duplicates_by_key<T, B, F>(target: &[T], mut func: F) -> Option<((usize, &T), (usize, &T))>
    where B: PartialEq, F: FnMut(&T) -> B {
    find_duplicates_by(target, |first, second| func(first) == func(second))
}

#[inline]
pub fn find_duplicates_by<T, F>(target: &[T], mut same_bucket: F) -> Option<((usize, &T), (usize, &T))>
    where F: FnMut(&T, &T) -> bool {
    for ((prev_index, prev), (index, element)) in target.iter().enumerate().tuple_windows() {
        if same_bucket(prev, element) {
            return Some(((prev_index, prev), (index, element)))
        }
    }
    None
}
