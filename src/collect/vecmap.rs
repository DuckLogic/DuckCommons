use std::cmp::{Ord, Ordering};
use std::{slice, mem, iter};

/// The threshold below which we prefer a linear search instead of a binary search.
///
/// Although binary searches are asymptotically more efficient for large sets,
/// you usually get better .
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
        match self.binary_search_by_key(key) {
            Ok(existing_index) => {
                let entry = &mut self.0[existing_index];
                debug_assert_eq!(entry.0, key);
                Some(mem::replace(&mut entry.1, value))
            },
            Err(target_index) => {
                self.0.insert(target_index, value);
                None
            }
        }
    }
    /// Lookup the value associated with the specified key in `O(log n)` time.
    #[inline]
    pub fn get(&self, key: &K) -> Option<&V> {
        self.find(key).map(|index| &self.0[index].1)
    }

    /// Lookup the value associated with the specified key in `O(log n)` time.
    #[inline]
    pub fn get_mut(&self, key: &K) -> Option<&V> {
        self.find(key).map(|index| &mut self.0[index].1)
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
        self.0.binary_search_by_key(key, |&(ref key, _)| key)
    }
}
/// Extend the map with the entries from the specified iterator.
///
/// Given, `m` existing elements and `n` additional elements,
/// this operation is guaranteed to run in `O(m log m + n)` time.
impl<K: Ord, V> Extend<(K, V)> for VecMap<K, V> {
    fn extend<T: IntoIterator<Item=(K, V)>>(&mut self, iter: T) {
        let newly_added = self.0.len();
        self.0.extend(iter);
        // Sort the newly added elements in `O(m log m)` time
        self.0[newly_added..].sort();
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

