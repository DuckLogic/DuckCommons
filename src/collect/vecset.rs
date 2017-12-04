use super::VecMap;

/// A set backed by a vector of sorted elements.
///
/// This is a thin wrapper around a `VecMap`.
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
        self.0.iter()
    }

}
pub type Iter<'a, T> = super::vecmap::Keys<'a, T, ()>;
