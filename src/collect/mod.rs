use std::mem;
use std::cmp::Ordering;
use std::hash::BuildHasherDefault;
use std::alloc::{Layout, Global, Alloc, oom};
use std::ptr::NonNull;

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
pub fn is_unique<T: Eq + ::std::hash::Hash>(target: &[T]) -> bool {
    is_unique_by_key(target, |value| value)
}
pub fn is_unique_by_key<'a, T, U, F>(target: &'a [T], mut func: F) -> bool
    where U: Eq + ::std::hash::Hash, F: FnMut(&'a T) -> U {
    let mut set = SeaHashOrderSet::with_capacity_and_hasher(
        target.len(), Default::default());
    target.iter().all(|value| set.insert(func(value)))
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

/// Unwrap every `Option` in the specified vector,
/// reusing the existing memory if possible.
pub fn bulk_unwrap<T>(target: Vec<Option<T>>) -> Vec<T> {
    #[cold] #[inline(never)]
    fn unwrap_failed(index: usize) -> ! {
        panic!("Failed to bulk unwrap `None` at index {}", index)
    }
    #[inline]
    fn bulk_unwrap_element<T>(index: usize, element: Option<T>) -> T {
        match element {
            Some(value) => value,
            None => unwrap_failed(index)
        }
    }
    if mem::align_of::<Option<T>>() == mem::align_of::<T>() {
        if target.capacity() == 0 {
            debug_assert_eq!(target.len(), 0);
            return Vec::new();
        }
        /*
         * Since the alignments are the same,
         * we can just ask the allocator to reallocate.
         * Since this is *shrinking* memory it should
         * never need to reallocate unless
         * something really weird is going on.
         */
        assert!(mem::size_of::<T>() <= mem::size_of::<Option<T>>());
        let original_start_ptr = target.as_ptr();
        let updated_start_ptr = target.as_ptr() as *mut T;
        let len = target.len();
        let capacity = target.capacity();
        unsafe {
            let original_end_ptr = original_start_ptr.add(len);
            let mut original_ptr = original_start_ptr;
            let mut updated_ptr = updated_start_ptr;
            mem::forget(target);
            while original_ptr < original_end_ptr {
                let opt = original_ptr.read();
                match opt {
                    Some(value) => {
                        updated_ptr.write(value);
                    }
                    None => {
                        unwrap_failed(
                            original_ptr.offset_from(original_start_ptr) as usize
                        )
                    }
                }
                original_ptr = original_ptr.add(1);
                updated_ptr = updated_ptr.add(1);
            }
            // Realloc the memory with the new size of `T`
            debug_assert!(capacity > 0);
            let realloc_ptr = Global.realloc(
                NonNull::new_unchecked(original_start_ptr as *mut _),
                Layout::from_size_align(
                    capacity * mem::size_of::<Option<T>>(),
                    mem::align_of::<Option<T>>()
                ).unwrap(),
                capacity * mem::size_of::<T>()
            ).unwrap_or_else(|_| oom());
            Vec::from_raw_parts(realloc_ptr.as_ptr() as *mut T, len, capacity)
        }
    } else {
        // Do the niave version
        target.into_iter().enumerate()
            .map(|(index, opt)| bulk_unwrap_element(index, opt))
            .collect()
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn bulk_unwrap() {
        use super::bulk_unwrap;
        assert_eq!(bulk_unwrap::<u32>(vec![]), Vec::<u32>::new());
        assert_eq!(bulk_unwrap::<u32>(vec![Some(1), Some(2), Some(3)]), vec![1, 2, 3]);
        assert_eq!(bulk_unwrap::<u32>(vec![Some(1)]), vec![1]);
    }
    #[test]
    #[should_panic(expected = "Failed to bulk unwrap `None` at index 1")]
    fn bulk_unwrap_fail() {
        use super::bulk_unwrap;
        bulk_unwrap::<u32>(vec![Some(2), None]);
    }

    #[test]
    #[should_panic(expected = "Failed to bulk unwrap `None` at index 0")]
    fn bulk_unwrap_fail_single() {
        use super::bulk_unwrap;
        bulk_unwrap::<u32>(vec![None, None]);
    }
}