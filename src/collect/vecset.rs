use std::{mem, iter};
use std::fmt::{self, Formatter, Debug};

use super::VecMap;

/// A set backed by a vector of sorted elements.
///
/// This is a thin wrapper around a `VecMap`.
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct VecSet<T: Ord>(VecMap<T, ()>);
impl<T: Ord> VecSet<T> {
    #[inline]
    pub fn new() -> Self {
        VecSet(VecMap::new())
    }
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        VecSet(VecMap::with_capacity(capacity))
    }
    /// Insert the specified element into the set,
    /// returning `true` if it was successfully added,
    /// and `false` if it was already present
    #[inline]
    pub fn insert(&mut self, element: T) -> bool {
        self.0.insert(element, ()).is_none()
    }
    /// Removes the specified value from the set,
    /// returning `true` if the value was present.
    #[inline]
    pub fn remove(&mut self, element: &T) -> bool {
        self.take(element).is_some()
    }
    /// Removes the specified value from the set.
    #[inline]
    pub fn take(&mut self, element: &T) -> Option<T> {
        self.0.remove_entry(element).map(|(element, ())| element)
    }
    /// Check if the set contains the specified element.
    #[inline]
    pub fn contains(&self, element: &T) -> bool {
        self.0.get(element).is_some()
    }
    #[inline]
    pub fn iter(&self) -> Iter<T> {
        self.0.keys()
    }
    #[inline]
    pub fn drain(&mut self) -> Drain<T> {
        Drain(self.0.drain())
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
}
impl<T: Ord> From<Vec<T>> for VecSet<T> {
    #[inline]
    fn from(vec: Vec<T>) -> Self {
        // TODO: Is this really nessicarry?
        assert_eq!(mem::size_of::<T>(), mem::size_of::<(T, ())>());
        VecSet(VecMap::from_vector(unsafe { mem::transmute::<Vec<T>, Vec<(T, ())>>(vec) }, false))
    }
}
impl<T: Ord> Default for VecSet<T> {
    #[inline]
    fn default() -> Self {
        VecSet::new()
    }
}
impl<'a, T: Ord + 'a> IntoIterator for &'a VecSet<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<T: Ord> IntoIterator for VecSet<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.0.into_iter())
    }
}
pub type Iter<'a, T> = super::vecmap::Keys<'a, T, ()>;

impl<T: Ord + Debug> Debug for VecSet<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

pub struct IntoIter<T: Ord>(::std::vec::IntoIter<(T, ())>);
impl<T: Ord> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(element, ())| element)
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    #[inline]
    fn count(self) -> usize where Self: Sized {
        self.0.count()
    }
    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.0.nth(n).map(|(element, ())| element)
    }
}
impl<T: Ord> iter::FusedIterator for IntoIter<T> {}
impl<T: Ord> iter::ExactSizeIterator for IntoIter<T> {}
unsafe impl<T: Ord> iter::TrustedLen for IntoIter<T> {}
impl<T: Ord> iter::DoubleEndedIterator for IntoIter<T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(element, ())| element)
    }
}

pub struct Drain<'a, T: Ord + 'a>(::std::vec::Drain<'a, (T, ())>);
impl<'a, T: Ord + 'a> Iterator for Drain<'a, T> {
    type Item = T;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(value, ())| value)
    }
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
    #[inline]
    fn count(self) -> usize where Self: Sized {
        self.0.count()
    }
    #[inline]
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.0.nth(n).map(|(value, ())| value)
    }
}
impl<'a, T: Ord + 'a> iter::DoubleEndedIterator for Drain<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(value, ())| value)
    }
}
impl<'a, T: Ord + 'a> iter::ExactSizeIterator for Drain<'a, T> {}
impl<'a, T: Ord + 'a> iter::FusedIterator for Drain<'a, T> {}
unsafe impl<'a, T: Ord + 'a> iter::TrustedLen for Drain<'a, T> {}

#[cfg(test)]
mod test {
    use super::VecSet;
    #[test]
    fn test_from_vector() {
        let mut expected = VecSet::new();
        expected.insert(1);
        expected.insert(2);
        expected.insert(3);
        assert_eq!(
            VecSet::from(vec![1, 2, 3]),
            expected
        );
        let mut expected = VecSet::new();
        expected.insert(None);
        expected.insert(Some("food"));
        expected.insert(None);
        assert_eq!(
            VecSet::from(vec![None, Some("food"), None]),
            expected
        );

        let mut expected = VecSet::new();
        expected.insert(None);
        expected.insert(Some(428));
        expected.insert(None);
        assert_eq!(
            VecSet::from(vec![None, Some(428), None]),
            expected
        );
    }
}
