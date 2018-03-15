use std::cmp::{Ord};
use std::fmt::{self, Debug, Formatter};
use std::{slice, mem, iter};
use super::insertion_sort_by;

use itertools::{EitherOrBoth, PeekingNext};
use itertools::EitherOrBoth::*;

/// The threshold below which we prefer a linear search instead of a binary search.
///
/// Although binary searches are asymptotically more efficient for large sets,
/// you usually get better performance with a linear search for small sets.
/// After this threshold we fallback to an out-of-line binary search,
/// which will allow performance to degrade gracefully for large sets.
const LINEAR_SEARCH_THRESHOLD: usize = 8;

/// A sorted map of keys to values, using a vector for storage.
///
/// This will usually be faster than a `HashMap` for a small number of entries,
/// since hashing overhead is avoided during lookup.
/// The worst case performance of lookup is `O(log n)` since we use a binary search,
/// which is very fast for a small number of elements.
/// However for large maps the `O(1)` lookup performance of a hashmap beats a `VecMap`,
/// even with the hashing overhead included.
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize, Default)]
pub struct VecMap<K: Ord, V>(Vec<(K, V)>);
impl<K: Ord, V> VecMap<K, V> {
    #[inline]
    pub fn new() -> Self {
        VecMap(Vec::new())
    }
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        VecMap(Vec::with_capacity(capacity))
    }
    pub fn from_vector(mut target: Vec<(K, V)>, allow_duplicates: bool) -> Self {
        target.sort_by(|&(ref first_key, _), &(ref second_key, _)| first_key.cmp(second_key));
        if !allow_duplicates {
            match ::collect::find_duplicates_by(&target, |&(ref first_key, _), &(ref second_key, _)| first_key == second_key) {
                Some(((_, &(ref key, ref first)), (_, &(_, ref second)))) => {
                    panic!(
                        "Duplicate entries for {:?}: {:?} and {:?}",
                        maybe_debug!(key), maybe_debug!(first), maybe_debug!(second)
                    )
                },
                None => {}
            }
        } else {
            target.dedup_by(|&mut (ref first_key, _), &mut (ref second_key, _)| first_key == second_key);
        }
        VecMap(target)
    }
    /// Inserts a key-value pair into the map.
    ///
    /// Returns the previous value of the key,
    /// or `None` if the key wasn't present.
    ///
    /// This function computes in `O(log n)` time if the key already had a value,
    /// and `O(n)` time if the key was missing.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        match self.binary_search_by_key(&key) {
            Ok(existing_index) => {
                let entry = &mut self.0[existing_index];
                Some(mem::replace(&mut entry.1, value))
            },
            Err(target_index) => {
                self.0.insert(target_index, (key, value));
                None
            }
        }
    }
    /// Removes the value associated with the specified key from this map.
    ///
    /// This function computes in `O(log n)` time if the key is missing,
    /// and `O(n)` time if the key is present.
    pub fn remove(&mut self, key: &K) -> Option<V> {
        match self.find(&key) {
            Some(index) => Some(self.0.remove(index).1),
            None => None
        }
    }
    /// Lookup the value associated with the specified key in `O(log n)` time.
    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.find(key).map(|index| &self.0[index].1)
    }
    /// Lookup the value associated with the specified key in `O(log n)` time.
    #[inline]
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        match self.find(key) {
            Some(index) => Some(&mut self.0[index].1),
            None => None
        }
    }
    #[inline]
    pub fn entry(&mut self, key: K) -> Entry<K, V> {
        match self.binary_search_by_key(&key) {
            Ok(index) => Entry::Occupied(OccupiedEntry {
                index, key, map: self
            }),
            Err(index) => Entry::Vacant(VacantEntry {
                index, key, map: self
            }),
        }
    }
    /// Drain the entire VecMap
    #[inline]
    pub fn drain(&mut self) -> ::std::vec::Drain<(K, V)> {
        self.0.drain(..)
    }
    #[inline]
    pub fn iter(&self) -> Iter<K, V> {
        Iter(self.0.iter())
    }
    #[inline]
    pub fn keys(&self) -> Keys<K, V> {
        Keys(self.0.iter())
    }
    #[inline]
    pub fn values(&self) -> Values<K, V> {
        Values(self.0.iter())
    }
    #[inline]
    fn find(&self, key: &K) -> Option<usize> {
        if self.0.len() <= LINEAR_SEARCH_THRESHOLD {
            self.linear_search_by_key(key)
        } else {
            self.binary_search_by_key(key).ok()
        }
    }
    #[inline]
    fn linear_search_by_key(&self, expected_key: &K) -> Option<usize> {
        self.0.iter().position(|&(ref key, _)| key == expected_key)
    }
    #[inline]
    fn binary_search_by_key(&self, key: &K) -> Result<usize, usize> {
        self.0.binary_search_by_key(&key, |&(ref key, _)| key)
    }
    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear()
    }
    #[inline]
    pub fn intersection<'a, 'b, B>(&'a self, other: &'b VecMap<K, B>) -> Intersection<'a, 'b, K, V, B> {
        Intersection {
            first: self.iter(),
            second: other.iter()
        }
    }
    #[inline]
    pub fn union<'a, 'b, B>(&'a self, other: &'b VecMap<K, B>) -> Union<'a, 'b, K, V, B> {
        Union {
            first: self.iter(),
            second: other.iter()
        }
    }
}
/// Extend the map with the entries from the specified iterator.
impl<K: Ord, V> Extend<(K, V)> for VecMap<K, V> {
    fn extend<T: IntoIterator<Item=(K, V)>>(&mut self, iter: T) {
        self.0.extend(iter);
        // Sort and deduplicate the elements
        insertion_sort_by(&mut self.0, |first, second| first.0.cmp(&second.0));
        self.0.dedup_by(|first, second| first.0 == second.0);
    }
}
pub struct Iter<'a, K: Ord + 'a, V: 'a>(slice::Iter<'a, (K, V)>);
impl<'a, K: Ord + 'a, V: 'a> Clone for Iter<'a, K, V> {
    #[inline]
    fn clone(&self) -> Self {
        Iter(self.0.clone())
    }
}
impl<'a, K: Ord + 'a, V: 'a> Iter<'a, K, V> {
    /// Internal backwards counterpart to `peeking_next`
    #[inline]
    fn peeking_next_back<F>(&mut self, mut accept: F) -> Option<(&'a K, &'a V)>
        where F: FnMut(&K, &V) -> bool {
        let saved_state = self.clone();
        if let Some((key, value)) = self.next_back() {
            if !accept(key, value) {
                *self = saved_state;
                None
            } else {
                Some((key, value))
            }
        } else {
            None
        }
    }
}
impl<'a, K: Ord + 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&(ref key, ref value)| (key, value))
    }
}
impl<'a, K: Ord + 'a, V: 'a> iter::FusedIterator for Iter<'a, K, V> {}
impl<'a, K: Ord + 'a, V: 'a> iter::ExactSizeIterator for Iter<'a, K, V> {}
unsafe impl<'a, K: Ord + 'a, V: 'a> iter::TrustedLen for Iter<'a, K, V> {}
impl<'a, K: Ord + 'a, V: 'a> iter::DoubleEndedIterator for Iter<'a, K, V> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|&(ref key, ref value)| (key, value))
    }
}
impl<'a, K: Ord + 'a, V: 'a> PeekingNext for Iter<'a, K, V> {
    #[inline]
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item> where F: FnOnce(&Self::Item) -> bool {
        self.0.peeking_next(|&&(ref key, ref value)| accept(&(key, value)))
            .map(|&(ref key, ref value)| (key, value))
    }
}

pub struct Keys<'a, K: Ord + 'a, V: 'a>(slice::Iter<'a, (K, V)>);
impl<'a, K: Ord + 'a, V: 'a> Iterator for Keys<'a, K, V> {
    type Item = &'a K;
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    #[inline]
    fn next(&mut self) -> Option<&'a K> {
        self.0.next().map(|&(ref key, _)| key)
    }
}
impl<'a, K: Ord + 'a, V: 'a> iter::FusedIterator for Keys<'a, K, V> {}
impl<'a, K: Ord + 'a, V: 'a> iter::ExactSizeIterator for Keys<'a, K, V> {}
unsafe impl<'a, K: Ord + 'a, V: 'a> iter::TrustedLen for Keys<'a, K, V> {}

pub struct Values<'a, K: Ord + 'a, V: 'a>(slice::Iter<'a, (K, V)>);
impl<'a, K: Ord + 'a, V: 'a> Iterator for Values<'a, K, V> {
    type Item = &'a V;
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    #[inline]
    fn next(&mut self) -> Option<&'a V> {
        self.0.next().map(|&(_, ref value)| value)
    }
}
impl<'a, K: Ord + 'a, V: 'a> iter::FusedIterator for Values<'a, K, V> {}
impl<'a, K: Ord + 'a, V: 'a> iter::ExactSizeIterator for Values<'a, K, V> {}
unsafe impl<'a, K: Ord + 'a, V: 'a> iter::TrustedLen for Values<'a, K, V> {}

impl<K: Ord + Debug, V: Debug> Debug for VecMap<K, V> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K: Ord, V> IntoIterator for VecMap<K, V> {
    type Item = (K, V);
    type IntoIter = ::std::vec::IntoIter<(K, V)>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K: Ord + 'a, V: 'a> IntoIterator for &'a VecMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Iterates over the union of two `VecMap`s.
///
/// This only iterates over the keys that are present in *either* map.
/// The values are represented by the itertools `EitherOrBoth` enumeration.
pub struct Intersection<'a, 'b, K: Ord + 'a + 'b, A: 'a, B: 'b> {
    first: Iter<'a, K, A>,
    second: Iter<'b, K, B>
}
impl<'a, 'b, K: Ord, A, B> Iterator for Intersection<'a, 'b, K, A, B> {
    type Item = (&'a K, &'a A, &'b B);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((key, first)) = self.first.next() {
            while let Some((other_key, second)) = self.second
                .peeking_next(|&(other_key, _)| *other_key <= *key) {
                if *other_key == *key {
                    return Some((key, first, second))
                } else {
                    // Ignore this key, since it couldn't possibly be in both maps
                    debug_assert!(*other_key < *key);
                }
            }
        }
        None
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.first.len().min(self.second.len())))
    }
}
impl<'a, 'b, K: Ord, A, B> iter::FusedIterator for Intersection<'a, 'b, K, A, B> {}
impl<'a, 'b, K: Ord, A, B> iter::DoubleEndedIterator for Intersection<'a, 'b, K, A, B> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        while let Some((key, first)) = self.first.next_back() {
            while let Some((other_key, second)) = self.second
                .peeking_next_back(|other_key, _| *other_key >= *key) {
                if *other_key == *key {
                    return Some((key, first, second))
                } else {
                    // Ignore this key, since it couldn't possibly be in both maps
                    debug_assert!(*other_key > *key);
                }
            }
        }
        None
    }
}


/// Iterates over the intersection of two `VecMap`s.
///
/// This only iterates over the keys that are present in *both* maps.
/// Although the keys are guaranteed to be the same,
/// the values may end up being different so we return both.
pub struct Union<'a: 'b, 'b, K: Ord + 'a, A: 'a, B: 'b> {
    first: Iter<'a, K, A>,
    second: Iter<'b, K, B>
}
impl<'a: 'b, 'b, K: Ord + 'a, A: 'a, B: 'b> Iterator for Union<'a, 'b, K, A, B> {
    type Item = (&'b K, EitherOrBoth<&'a A, &'b B>);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let old_state = self.first.clone();
        if let Some((key, first)) = self.first.next() {
            while let Some((other_key, second)) = self.second
                .peeking_next(|&(other_key, _)| *other_key <= *key) {
                if *other_key == *key {
                    return Some((key, Both(first, second)))
                } else {
                    debug_assert!(*other_key < *key);
                    self.first = old_state;
                    return Some((other_key, Right(second)))
                }
            }
            Some((key, Left(first)))
        } else {
            while let Some((key, value)) = self.second.next() {
                return Some((key, Right(value)))
            }
            None
        }
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.first.len().max(self.second.len()), self.first.len().checked_add(self.second.len()))
    }
}
impl<'a: 'b, 'b, K: Ord + 'a, A, B> iter::FusedIterator for Union<'a, 'b, K, A, B> {}
impl<'a: 'b, 'b, K: Ord + 'a, A: 'a, B: 'b> iter::DoubleEndedIterator for Union<'a, 'b, K, A, B> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        let old_state = self.first.clone();
        if let Some((key, first)) = self.first.next_back() {
            while let Some((other_key, second)) = self.second
                .peeking_next_back(|other_key, _| *other_key >= *key) {
                if *other_key == *key {
                    return Some((key, Both(first, second)))
                } else {
                    debug_assert!(*other_key > *key);
                    self.first = old_state;
                    return Some((other_key, Right(second)))
                }
            }
            Some((key, Left(first)))
        } else {
            while let Some((key, value)) = self.second.next_back() {
                return Some((key, Right(value)))
            }
            None
        }
    }
}

pub enum Entry<'a, K: Ord + 'a, V: 'a> {
    Vacant(VacantEntry<'a, K, V>),
    Occupied(OccupiedEntry<'a, K, V>)
}
impl<'a, K: Ord + 'a, V: 'a> Entry<'a, K, V> {
    #[inline]
    pub fn or_insert(self, value: V) -> &'a mut V {
        self.or_insert_with(|| value)
    }
    #[inline]
    pub fn or_insert_with<F>(self, func: F) -> &'a mut V where F: FnOnce() -> V {
        match self {
            Entry::Occupied(entry) => entry.value(),
            Entry::Vacant(entry) => entry.or_insert_with(func)
        }
    }
}

pub struct VacantEntry<'a, K: Ord + 'a, V: 'a> {
    map: &'a mut VecMap<K, V>,
    key: K,
    index: usize,
}
impl<'a, K: Ord + 'a, V: 'a> VacantEntry<'a, K, V> {
    #[inline]
    pub fn key(&self) -> &K {
        &self.key
    }
    #[inline]
    pub fn insert(self, value: V) -> &'a mut V {
        self.map.0.insert(self.index, (self.key, value));
        &mut self.map.0[self.index].1
    }
    #[inline]
    pub fn or_insert_with<F>(self, func: F) -> &'a mut V where F: FnOnce() -> V {
        self.insert(func())
    }
}
pub struct OccupiedEntry<'a, K: Ord + 'a, V: 'a> {
    map: &'a mut VecMap<K, V>,
    key: K,
    index: usize,
}
impl<'a, K: Ord + 'a, V: 'a> OccupiedEntry<'a, K, V> {
    #[inline]
    pub fn key(&self) -> &K {
        &self.key
    }
    #[inline]
    pub fn value(self) -> &'a mut V {
        &mut self.map.0[self.index].1
    }
    #[inline]
    pub fn get(&self) -> &V {
        &self.map.0[self.index].1
    }
    #[inline]
    pub fn get_mut(&mut self) -> &mut V {
        &mut self.map.0[self.index].1
    }
    #[inline]
    pub fn insert(self, value: V) -> V {
        mem::replace(self.value(), value)
    }
    #[inline]
    pub fn remove(self) -> V {
        self.map.0.remove(self.index).1
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_intersection() {
        let mut first = VecMap::new();
        first.insert("food", 10);
        first.insert("tacos", 4);
        first.insert("shells", 3);
        first.insert("school", 10_000);
        let mut second = VecMap::new();
        second.insert("food", 0);
        second.insert("shells", 15);
        second.insert("poop", 13);
        let intersection = first.intersection(&second)
            .map(|(&key, &first, &second)| (key, first, second))
            .collect::<Vec<_>>();
        let reverse_intersection = first.intersection(&second)
            .rev()
            .map(|(&key, &first, &second)| (key, first, second))
            .collect::<Vec<_>>();
        assert_eq!(*intersection, *&[("food", 10, 0), ("shells", 3, 15)]);
        assert_eq!(*reverse_intersection, *&[("shells", 3, 15), ("food", 10, 0)])

    }
    fn copied_both<A: Copy, B: Copy>(target: EitherOrBoth<&A, &B>) -> EitherOrBoth<A, B> {
        match target {
            Left(&left) => Left(left),
            Right(&right) => Right(right),
            Both(&left, &right) => Both(left, right)
        }
    }
    #[test]
    fn test_union() {
        let mut first = VecMap::new();
        first.insert("food", 10);
        first.insert("tacos", 4);
        first.insert("shells", 3);
        first.insert("school", 10_000);
        let mut second = VecMap::new();
        second.insert("food", 0);
        second.insert("shells", 15);
        second.insert("poop", 13);
        let union = first.union(&second)
            .map(|(&key, entry)| (key, copied_both(entry)))
            .collect::<Vec<_>>();
        let reverse_union = first.union(&second)
            .rev()
            .map(|(&key, entry)| (key, copied_both(entry)))
            .collect::<Vec<_>>();
        let expected = vec![
            ("food", Both(10, 0)),
            ("poop", Right(13)),
            ("school", Left(10_000)),
            ("shells", Both(3, 15)),
            ("tacos", Left(4))
        ];
        let expected_reversed = expected.iter().cloned()
            .rev().collect::<Vec<_>>();
        assert_eq!(*union, *expected);
        assert_eq!(*reverse_union, *expected_reversed);
    }
}
