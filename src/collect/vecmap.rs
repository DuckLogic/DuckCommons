use std::cmp::{Ord};
use std::fmt::{self, Debug, Formatter};
use std::{slice, mem, iter};
use super::insertion_sort_by;

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
