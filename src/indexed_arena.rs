use std::cell::RefCell;
use std::marker::PhantomData;
use std::fmt::{self, Debug, Formatter};
use std::ptr::NonNull;
use typed_arena::Arena;
use std::iter::{FromIterator, TrustedLen};
use std::ops::{Index, IndexMut};

use math::index::NonZeroIndex;
use idmap::IntegerId;

pub struct ArenaIndex<T> {
    index: NonZeroIndex<u16>,
    // NOTE: We use a *const T instead of just a T to indicate we don't own it
    phantom: PhantomData<*const T>
}
impl<T> Copy for ArenaIndex<T> {}
impl<T> Clone for ArenaIndex<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> ArenaIndex<T> {
    #[inline]
    pub fn index(self) -> u16 {
        self.index.index()
    }
    #[inline]
    pub fn offset(self, amount: u64) -> Self {
        ArenaIndex {
            index: self.index.offset(amount),
            phantom: PhantomData
        }
    }
}
// TODO: Allow deriving for structs with PhantomData
impl<T> IntegerId for ArenaIndex<T> {
    type Storage = ();
    #[inline(always)]
    fn from_storage(_: (), id: u64) -> Self {
        ArenaIndex::from(id)
    }
    #[inline(always)]
    fn into_storage(self) {}
    #[inline(always)]
    fn id(&self) -> u64 {
        self.index() as u64
    }
    #[inline(always)]
    fn id32(&self) -> u32 {
        self.index() as u32
    }
}
impl<T> Debug for ArenaIndex<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("ArenaIndex")
            .field(&self.index())
            .finish()
    }
}
impl<T> From<u64> for ArenaIndex<T> {
    #[inline]
    fn from(index: u64) -> Self {
        ArenaIndex {
            index: NonZeroIndex::from(index),
            phantom: PhantomData
        }
    }
}
impl<T> From<usize> for ArenaIndex<T> {
    #[inline]
    fn from(index: usize) -> Self {
        ArenaIndex {
            index: NonZeroIndex::from(index),
            phantom: PhantomData
        }
    }
}
/// Implements 'identity equality' for `ArenaIndex`, based on whether the indexes are equal.
impl<T> PartialEq for ArenaIndex<T> {
    #[inline]
    fn eq(&self, other: &ArenaIndex<T>) -> bool {
        self.index() == other.index()
    }
}
/// An arena whose values can be later referred to by index
pub struct IndexedArena<T> {
    /*
     * NOTE: _must_ store the values in an arena so the values don't move,
     * as we need to make sure the references are always valid.
     * Although I used to use a simple vec, that was unsafe since it might need to reallocate while references are held.
     * The new design ensures that even if we need to reallocate, the old blocks are still valid since they're boxed.
     * A previous design reimplemented a custom arena that had O(1) lookup of values and didn't move the underlying memory,
     * but would be slower than a Vec because of extra bookkeeping.
     * Although the new design adds an additional indirection,
     * there's much less unsafe code since we don't need to reimplement the arena.
     */
    arena: Arena<T>,
    elements: RefCell<Vec<NonNull<T>>>,
}
impl<T> IndexedArena<T> {
    #[inline]
    pub fn new() -> Self {
        IndexedArena::with_capacity(0)
    }
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        IndexedArena {
            arena: Arena::with_capacity(capacity),
            elements: RefCell::new(Vec::with_capacity(capacity))
        }
    }
    pub fn bulk_alloc(&self, vec: Vec<T>) -> (&[T], ArenaIndex<T>) {
        let start_index = self.len();
        let start = ArenaIndex::from(start_index);
        start.offset(vec.len() as u64); // Ensure we can fit all the elements in a u16
        let allocated = self.arena.alloc_extend(vec);
        self.elements.borrow_mut().extend(allocated.iter().map(NonNull::from));
        (&*allocated, start)
    }
    #[inline]
    pub fn alloc(&self, value: T) -> (&T, ArenaIndex<T>) {
        let mut elements = self.elements.borrow_mut();
        // NOTE: Creating the ArenaIndex first ensures we panic on overflow
        let index = ArenaIndex::from(elements.len());
        let shared = NonNull::from(self.arena.alloc(value));
        elements.push(shared);
        unsafe {
            (&*shared.as_ptr(), index)
        }
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.elements.borrow().is_empty()
    }
    #[inline]
    pub fn len(&self) -> usize {
        // NOTE: Length does't necessarily fit in a u16, since it could safely be one past the maximum value
        self.elements.borrow().len()
    }

    pub fn clear(&mut self) {
        let elements = self.elements.get_mut();
        let len = elements.len();
        elements.clear();
        /*
         * NOTE: Although it's conventional to reuse allocated memory when clearing,
         * Arena doesn't support actually clearing, and it's better to reallocate than leak.
         */
        self.arena = Arena::with_capacity(len);
    }
    #[inline]
    pub fn into_vec(self) -> Vec<T> {
        // NOTE: This is always correct since we don't support removal
        self.arena.into_vec()
    }
    #[inline]
    pub fn iter(&self) -> Iter<T> {
        Iter { arena: self, index: 0, length: self.len() }
    }
}
impl<T> Index<usize> for IndexedArena<T> {
    type Output = T;
    #[inline]
    fn index(&self, index: usize) -> &T {
        unsafe {
            &*self.elements.borrow()[index].as_ptr()
        }
    }
}

impl<T> IndexMut<usize> for IndexedArena<T> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut T {
        unsafe {
            &mut *self.elements.get_mut()[index].as_ptr()
        }
    }
}
impl<T> Index<ArenaIndex<T>> for IndexedArena<T> {
    type Output = T;
    #[inline]
    fn index(&self, index: ArenaIndex<T>) -> &T {
        &self[index.index() as usize]
    }
}
impl<T> IndexMut<ArenaIndex<T>> for IndexedArena<T> {
    #[inline]
    fn index_mut(&mut self, index: ArenaIndex<T>) -> &mut T {
        &mut self[index.index() as usize]
    }
}

impl<T> From<Vec<T>> for IndexedArena<T> {
    #[inline]
    fn from(initial: Vec<T>) -> IndexedArena<T> {
        let result = IndexedArena::with_capacity(initial.len());
        result.bulk_alloc(initial);
        result
    }
}
impl<T> FromIterator<T> for IndexedArena<T> {
    #[inline]
    fn from_iter<I>(iterable: I) -> Self where I: IntoIterator<Item=T> {
        let iter = iterable.into_iter();
        let result = IndexedArena::with_capacity(iter.size_hint().1.unwrap_or(0));
        for value in iter {
            result.alloc(value);
        }
        result
    }
}
impl<T> Default for IndexedArena<T> {
    #[inline]
    fn default() -> Self {
        IndexedArena::new()
    }
}
impl<T: Clone> Clone for IndexedArena<T> {
    fn clone(&self) -> Self {
        self.iter().cloned().collect()
    }
}
pub struct Iter<'a, T: 'a> {
    arena: &'a IndexedArena<T>,
    index: usize,
    length: usize
}
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.length, Some(self.length))
    }
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        /*
         * NOTE: We can't simply delegate to the slice's iterator,
         * since the vector could be modified while we're iterating.
         * We have to re-index each time in case the vector reallocates.
         */
        let index = self.index;
        if index < self.length {
            self.index = index + 1;
            Some(&self.arena[index])
        } else {
            None
        }
    }
}
unsafe impl<'a, T> TrustedLen for Iter<'a, T> {}
