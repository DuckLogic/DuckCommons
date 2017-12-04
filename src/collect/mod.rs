pub mod orderset;
pub mod bitset;
pub mod two_sided;
pub mod vecmap;
pub mod vecset;
pub mod bulk;

use std::cmp::Ordering;
use std::ptr;

pub use self::orderset::OrderSet;
pub use self::bitset::SmallBitSet;
pub use self::two_sided::TwoSidedVec;
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
pub fn is_sorted_with<T, F: FnMut(&T, &T) -> cmp::Ordering>(data: &[T], mut func: F) -> bool {
    for (prev, element) in data.iter().tuple_windows() {
        // if element < prev
        if func(element, prev) == cmp::Ordering::Less {
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
pub fn insertion_sort_by<T, F>(target: &mut [T], compare: F) where F: FnMut(&T, &T) -> Ordering {
    for i in 1..target.len() {
        let mut j = i;
        while j > 0 && compare(target[j - 1], target[j]) == Ordering::Greater {
            target.swap(j, j - 1);
            j -= 1;
        }
    }
}

/// Performs an insertion sort on the specified slice,
/// comparing values using the specified function.
#[inline]
pub fn insertion_sort_by_key<T, B, F>(target: &mut [T], func: F) where B: Ord, F: FnMut(&T) -> B {
    insertion_sort_by(target, |first, second| func(first).cmp(func(second)))
}

