use std::iter;
use std::fmt::{self, Formatter, Debug};

use super::VecMap;

/// A set backed by a vector of sorted elements.
///
/// This is a thin wrapper around a `VecMap`.
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize, Default)]
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

}
pub type Iter<'a, T> = super::vecmap::Keys<'a, T, ()>;

impl<T: Ord + Debug> Debug for VecSet<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
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
