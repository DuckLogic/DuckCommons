use std::{slice, ptr, iter};
use std::fmt::{self, Formatter, Debug};
use std::ops::{Index, Range, RangeFull, RangeFrom, RangeTo, IndexMut};
use std::hash::{Hash, Hasher};

use serde::ser::{Serialize, Serializer, SerializeStruct};
use serde::de::{Deserialize, Deserializer};

use self::raw::{RawTwoSidedVec, Capacity, CapacityRequest};

pub mod raw;

/// A simple 'two sided' vector, that can grow both forwards and backwards.
///
/// The front and the back can be viewed as seperate and independent vectors,
/// with negative indexing accessing the back and positive indexing accessing the front.
/// This allows you to **append to the back without modifying positive indexes**.
/// Unless you actually need pushing to the back to appear to shift the front forward,
/// like `VecDeque` does, this negative index system will probably be better for your situation.
///
/// Internally this allows a much simpler and faster implementation,
/// since there's only a single pointer to the middle that grows up and down.
/// Internally, we have to reallocate the buffer if we run out of capacity in either the
/// negative or positive difes grow separately and
/// Although bounds checks are _slightly_ slower since they involve two comparisons,
/// the access itself should be just as fast.
pub struct TwoSidedVec<T> {
    memory: RawTwoSidedVec<T>,
    start_index: isize,
    end_index: isize,
}
impl<T> TwoSidedVec<T> {
    #[inline]
    pub fn new() -> Self {
        unsafe {
            TwoSidedVec::from_raw(RawTwoSidedVec::new())
        }
    }
    #[inline]
    pub fn with_capacity(back: usize, front: usize) -> Self {
        unsafe {
            TwoSidedVec::from_raw(RawTwoSidedVec::with_capacity(Capacity { back, front }))
        }
    }
    #[inline]
    unsafe fn from_raw(memory: RawTwoSidedVec<T>) -> Self {
        TwoSidedVec { memory, start_index: 0, end_index: 0 }
    }
    /// Take a slice of the front of this queue
    #[inline]
    pub fn front(&self) -> &[T] {
        unsafe {
            slice::from_raw_parts(self.middle_ptr(), self.len_front())
        }
    }
    /// Take a slice of the back of this queue
    #[inline]
    pub fn back(&self) -> &[T] {
        unsafe {
            slice::from_raw_parts(self.start_ptr(), self.len_back())
        }
    }
    /// Take a mutable slice of the front of this queue
    #[inline]
    pub fn front_mut(&mut self) -> &mut [T] {
        self.split_mut().1
    }
    /// Take a mutable slice of the back of this queue
    #[inline]
    pub fn back_mut(&mut self) -> &mut [T] {
        self.split_mut().0
    }
    /// Take seperate slices of the back and the front of the vector respectively.
    #[inline]
    pub fn split(&self) -> (&[T], &[T]) {
        (self.back(), self.front())
    }
    /// Take seperate mutable slices of the back and front of the vector respectively.
    #[inline]
    pub fn split_mut(&mut self) -> (&mut [T], &mut [T]) {
        unsafe {
            (
                slice::from_raw_parts_mut(self.start_ptr(), self.len_back()),
                slice::from_raw_parts_mut(self.middle_ptr(), self.len_front())
            )
        }
    }
    #[inline]
    pub fn push_front(&mut self, value: T) {
        self.reserve_front(1);
        unsafe {
            ptr::write(self.end_ptr(), value);
            self.end_index += 1;
        }
    }
    #[inline]
    pub fn pop_front(&mut self) -> Option<T> {
        if self.end_index > 0 {
            self.end_index -= 1;
            unsafe {
                Some(ptr::read(self.end_ptr()))
            }
        } else {
            None
        }
    }
    #[inline]
    pub fn pop_back(&mut self) -> Option<T> {
        if self.start_index < 0 {
            self.start_index += 1;
            unsafe {
                Some(ptr::read(self.middle_ptr().offset(self.start_index - 1)))
            }
        } else {
            None
        }
    }
    /// Push the specified value into the front of this queue,
    /// without modifying its `end` or touching the front of the queue.
    ///
    /// This effectively **preserves all positive indexes**,
    /// which may or may not be useful for your situation.
    #[inline]
    pub fn push_back(&mut self, value: T) {
        self.reserve_back(1);
        unsafe {
            ptr::write(self.start_ptr().offset(-1), value);
            self.start_index -= 1;
        }
    }
    pub fn extend_back<I>(&mut self, values: I) where I: IntoIterator<Item=T> {
        let iter = values.into_iter();
        if let Some(hint) = iter.size_hint().1 { self.reserve_back(hint) };
        for value in iter {
            self.push_back(value);
        }
    }
    pub fn extend_front<I>(&mut self, values: I) where I: IntoIterator<Item=T> {
        let iter = values.into_iter();
        if let Some(hint) = iter.size_hint().1 { self.reserve_front(hint) };
        for value in iter {
            self.push_front(value);
        }
    }
    #[inline]
    pub fn reserve_back(&mut self, amount: usize) {
        debug_assert!(self.check_sanity());
        if !self.can_fit_back(amount) {
            self.grow(0, amount);
        }
        debug_assert!(self.can_fit_back(amount))
    }
    #[inline]
    pub fn reserve_front(&mut self, amount: usize) {
        debug_assert!(self.check_sanity());
        if !self.can_fit_front(amount) {
            self.grow(amount, 0);
        }
        debug_assert!(
            self.can_fit_front(amount),
            "Insufficient capacity {:?} for {} additional elements. end_index = {}, start_index = {}, len = {}",
            self.memory.capacity(), amount, self.end_index, self.start_index, self.len()
        );
    }
    #[cold] #[inline(never)]
    fn grow(&mut self, front: usize, back: usize) {
        let request = CapacityRequest {
            used: Capacity { back: self.len_back(), front: self.len_front() },
            needed: Capacity { front, back }
        };
        self.memory.reserve(request);
    }
    #[inline]
    fn can_fit_front(&self, amount: usize) -> bool {
        let remaining_front = self.capacity_front() - self.len_front();
        remaining_front >= amount
    }
    #[inline]
    fn can_fit_back(&self, amount: usize) -> bool {
        let remaining_back = self.capacity_back() - self.len_back();
        remaining_back >= amount
    }
    #[inline]
    pub fn capacity_back(&self) -> usize {
        self.memory.capacity().back
    }
    #[inline]
    pub fn capacity_front(&self) -> usize {
        self.memory.capacity().front
    }
    /// Return the length of the entire vector, which is the sum of the
    /// lengths of the front and back parts.
    ///
    /// The **length isn't where the vector ends**,
    /// since it could have elements in the back with negative indexes.
    /// Use `vec.start()` and `vec.end()` if you want to know the start and end indexes.
    /// The total length is exactly equivalent to `vec.len_back() + vec.len_front()`
    #[inline]
    pub fn len(&self) -> usize {
        debug_assert!(self.start_index <= self.end_index);
        self.end_index.wrapping_sub(self.start_index) as usize
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start_index == self.end_index
    }
    /// Return the length of the back of the vector.
    #[inline]
    pub fn len_back(&self) -> usize {
        debug_assert!(self.start_index <= 0);
        // NOTE: We perform the cast immediately after the negation to handle overflow properly
        self.start_index.wrapping_neg() as usize
    }
    /// Return the length of the front of the vector
    #[inline]
    pub fn len_front(&self) -> usize {
        debug_assert!(self.end_index >= 0);
        self.end_index as usize
    }
    /// Give the (inclusive) start of the queue's elements.
    /// which may be negative if the queue's back isn't empty
    ///
    /// This is exactly equivelant to `-vec.back().len()`.
    #[inline]
    pub fn start(&self) -> isize {
        self.start_index
    }
    /// Give the (exclusive) end of the queue's elements,
    /// which may be less than the length if the queue's back contains some elements.
    ///
    /// This is exactly equivalent to `vec.front().len()`
    #[inline]
    pub fn end(&self) -> isize {
        self.end_index
    }
    /// Return the `[start, end)` range of the element indices,
    /// equivalent to a tuple of `(queue.start(), queue.end())`.
    #[inline]
    pub fn range(&self) -> Range<isize> {
        self.start_index..self.end_index
    }
    /// Iterate over the entire vector, including both the back and front.
    #[inline]
    pub fn iter_entire(&self) -> slice::Iter<T> {
        self[..].iter()
    }
    #[inline]
    pub fn get<I: TwoSidedIndex<T>>(&self, index: I) -> Option<&I::Output> {
        index.get(self)
    }
    #[inline]
    pub fn get_mut<I: TwoSidedIndex<T>>(&mut self, index: I) -> Option<&mut I::Output> {
        index.get_mut(self)
    }
    #[inline]
    pub unsafe fn get_unchecked<I: TwoSidedIndex<T>>(&self, index: I) -> &I::Output {
        index.get_unchecked(self)
    }
    #[inline]
    pub unsafe fn get_unchecked_mut<I: TwoSidedIndex<T>>(&mut self, index: I) -> &mut I::Output {
        index.get_unchecked_mut(self)
    }
    /// Give a raw pointer to the start of the elements
    #[inline]
    pub fn start_ptr(&self) -> *mut T {
        unsafe {
            self.middle_ptr().offset(self.start_index)
        }
    }
    /// Give a raw pointer to the middle of the elements
    #[inline]
    pub fn middle_ptr(&self) -> *mut T {
        self.memory.middle()
    }
    #[inline]
    pub fn end_ptr(&self) -> *mut T {
        unsafe {
            self.middle_ptr().offset(self.end_index)
        }
    }
    #[inline]
    pub fn split_at(&self, index: isize) -> (&[T], &[T]) {
        (&self[..index], &self[index..])
    }
    fn check_sanity(&self) -> bool {
        assert!(self.start_index <= 0 && self.end_index >= 0);
        // These should be implied by the other checks
        debug_assert!(self.start_ptr() <= self.middle_ptr());
        debug_assert!(self.end_ptr() >= self.middle_ptr());
        true
    }
    pub fn clear(&mut self) {
        while let Some(value) = self.pop_back()  {
            drop(value)
        }
        while let Some(value) = self.pop_front() {
            drop(value)
        }
        debug_assert_eq!(self.len(), 0);
    }
    /// Enumerate the indices and values of the elements in the back of the vector.
    ///
    /// The primary advantage over regular enumeration is that it
    /// gives proper negative indices since the elements are in the back.
    #[inline]
    pub fn enumerate_back(&self) -> SignedEnumerate<slice::Iter<T>> {
        SignedEnumerate::new(self.start_index, self.back().iter())
    }
    /// Enumerate the indices and values of the elements in the front of the vector.
    ///
    /// The only possible advantage over regular enumeration is that it
    /// gives positive `isize` indices for consistency with enumeration over the back.
    #[inline]
    pub fn enumerate_front(&self) -> SignedEnumerate<slice::Iter<T>> {
        SignedEnumerate::new(0, self.front().iter())
    }
    /// Enumerate the indices and values of each element in the front and back.
    ///
    /// The primary advantage over regular enumeration is that
    /// it gives proper negative indices for elements that are in the back.
    #[inline]
    pub fn enumerate(&self) -> SignedEnumerate<slice::Iter<T>> {
        SignedEnumerate::new(self.start(), self[..].iter())
    }
    /// Mutably enumerate the indices and values of each element in the front and back.
    ///
    /// The primary advantage over regular enumeration is that
    /// it gives proper negative indices for elements that are in the back.
    #[inline]
    pub fn enumerate_mut(&mut self) -> SignedEnumerate<slice::IterMut<T>> {
        SignedEnumerate::new(self.start(), self[..].iter_mut())
    }
}
impl<T: Clone> Clone for TwoSidedVec<T> {
    fn clone(&self) -> Self {
        let mut result = TwoSidedVec::with_capacity(
            self.len_back(),
            self.len_front()
        );
        result.extend_back(self.back().iter().rev().cloned());
        result.extend_front(self.front().iter().cloned());
        result
    }
}
impl<T> Default for TwoSidedVec<T> {
    #[inline]
    fn default() -> Self {
        TwoSidedVec::new()
    }
}
impl<T: Debug> Debug for TwoSidedVec<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("TwoSidedVec")
            .field("back", &self.back())
            .field("front", &self.front())
            .finish()
    }
}
unsafe impl<#[may_dangle] T> Drop for TwoSidedVec<T> {
    fn drop(&mut self) {
        unsafe {
            // use drop for owned slice `[T]` just like vec
            ptr::drop_in_place(&mut self[..])
        }
    }
}
pub trait TwoSidedIndex<T>: Sized + Debug {
    type Output: ?Sized;
    unsafe fn get_unchecked(self, target: &TwoSidedVec<T>) -> &Self::Output;
    unsafe fn get_unchecked_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output;
    fn check(&self, target: &TwoSidedVec<T>) -> bool;
    #[inline]
    fn get(self, target: &TwoSidedVec<T>) -> Option<&Self::Output> {
        if self.check(target) {
            Some(unsafe { self.get_unchecked(target) })
        } else {
            None
        }
    }
    #[inline]
    fn get_mut(self, target: &mut TwoSidedVec<T>) -> Option<&mut Self::Output> {
        if self.check(target) {
            Some(unsafe { self.get_unchecked_mut(target) })
        } else {
            None
        }
    }
    #[inline]
    fn index(self, target: &TwoSidedVec<T>) -> &Self::Output {
        if self.check(target) {
            unsafe { self.get_unchecked(target) }
        } else {
            self.invalid()
        }
    }
    #[inline]
    fn index_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output {
        if self.check(target) {
            unsafe { self.get_unchecked_mut(target) }
        } else {
            self.invalid()
        }
    }
    #[cold]
    #[inline(never)]
    fn invalid(self) -> ! {
        panic!("Invalid index: {:?}", self)
    }
}
impl<T, I: TwoSidedIndex<T>> Index<I> for TwoSidedVec<T> {
    type Output = I::Output;
    #[inline]
    fn index(&self, index: I) -> &I::Output {
        index.index(self)
    }
}
impl<T, I: TwoSidedIndex<T>> IndexMut<I> for TwoSidedVec<T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut I::Output {
        index.index_mut(self)
    }
}
impl<T> TwoSidedIndex<T> for isize {
    type Output = T;

    #[inline]
    unsafe fn get_unchecked(self, target: &TwoSidedVec<T>) -> &Self::Output {
        debug_assert!(self.check(target));
        &*target.middle_ptr().offset(self)
    }

    #[inline]
    unsafe fn get_unchecked_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output {
        &mut *target.middle_ptr().offset(self)
    }

    #[inline]
    fn check(&self, target: &TwoSidedVec<T>) -> bool {
        *self >= target.start_index && *self < target.end_index
    }
}
impl<T> TwoSidedIndex<T> for Range<isize> {
    type Output = [T];

    #[inline]
    unsafe fn get_unchecked(self, target: &TwoSidedVec<T>) -> &Self::Output {
        slice::from_raw_parts(
            target.middle_ptr().offset(self.start),
            (self.end - self.start) as usize
        )
    }

    #[inline]
    unsafe fn get_unchecked_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output {
        slice::from_raw_parts_mut(
            target.middle_ptr().offset(self.start),
            (self.end - self.start) as usize
        )
    }

    #[inline]
    fn check(&self, target: &TwoSidedVec<T>) -> bool {
        self.start >= target.start_index
            && self.start <= self.end
            && self.end <= target.end_index
    }
}

impl<T> TwoSidedIndex<T> for RangeFull {
    type Output = [T];

    #[inline]
    unsafe fn get_unchecked(self, target: &TwoSidedVec<T>) -> &Self::Output {
        slice::from_raw_parts(
            target.middle_ptr().offset(target.start_index),
            target.len()
        )
    }

    #[inline]
    unsafe fn get_unchecked_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output {
        slice::from_raw_parts_mut(
            target.middle_ptr().offset(target.start_index),
            target.len()
        )
    }

    #[inline]
    fn check(&self, _target: &TwoSidedVec<T>) -> bool {
        true
    }
}
impl<T> TwoSidedIndex<T> for RangeFrom<isize> {
    type Output = [T];

    #[inline]
    unsafe fn get_unchecked(self, target: &TwoSidedVec<T>) -> &Self::Output {
        slice::from_raw_parts(
            target.middle_ptr().offset(self.start),
            (target.end_index - self.start) as usize
        )
    }

    #[inline]
    unsafe fn get_unchecked_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output {
        slice::from_raw_parts_mut(
            target.middle_ptr().offset(self.start),
            (target.end_index - self.start) as usize
        )
    }

    #[inline]
    fn check(&self, target: &TwoSidedVec<T>) -> bool {
        self.start >= target.start_index && self.start <= target.end_index
    }
}
impl<T> TwoSidedIndex<T> for RangeTo<isize> {
    type Output = [T];

    #[inline]
    unsafe fn get_unchecked(self, target: &TwoSidedVec<T>) -> &Self::Output {
        slice::from_raw_parts(
            target.middle_ptr().offset(target.start_index),
            (self.end - target.start_index) as usize
        )
    }

    #[inline]
    unsafe fn get_unchecked_mut(self, target: &mut TwoSidedVec<T>) -> &mut Self::Output {
        slice::from_raw_parts_mut(
            target.middle_ptr().offset(target.start_index),
            (self.end - target.start_index) as usize
        )
    }

    #[inline]
    fn check(&self, target: &TwoSidedVec<T>) -> bool {
        self.end >= target.start_index && self.end <= target.end_index
    }
}


impl<T: Serialize> Serialize for TwoSidedVec<T> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {
        let mut two_sided = serializer.serialize_struct("TwoSidedVec", 2)?;
        two_sided.serialize_field("back", self.back())?;
        two_sided.serialize_field("front", self.front())?;
        two_sided.end()
    }
}
impl<'de, T: Deserialize<'de>> Deserialize<'de> for TwoSidedVec<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        /*
         * NOTE: Although this is slightly less efficient than deserializing directly,
         * the temporary vector uses ten times less serde boilerplate.
         */
        #[derive(Deserialize)]
        struct SerializedTwoSidedVec<T> {
            back: Vec<T>,
            front: Vec<T>
        }
        let serialized = SerializedTwoSidedVec::deserialize(deserializer)?;
        let mut result = TwoSidedVec::with_capacity(serialized.back.len(), serialized.front.len());
        result.extend_front(serialized.front);
        result.extend_back(serialized.back);
        Ok(result)
    }
}
pub struct SignedEnumerate<I> {
    index: isize,
    handle: I
}
impl<I: Iterator> SignedEnumerate<I> {
    #[inline]
    pub fn new(start: isize, handle: I) -> Self {
        debug_assert!((handle.size_hint().1.unwrap_or(0) as isize)
                          .checked_add(start).is_some(), "Overflow!");
        SignedEnumerate { index: start, handle }
    }
}
impl<T, I: Iterator<Item=T>> Iterator for SignedEnumerate<I> {
    type Item = (isize, T);

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.handle.size_hint()
    }

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.handle.next() {
            let index = self.index;
            self.index += 1;
            Some((index, value))
        } else {
            None
        }
    }
}
impl<I> iter::DoubleEndedIterator for SignedEnumerate<I>
    where I: iter::DoubleEndedIterator + iter::ExactSizeIterator {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.handle.next_back().map(|value| {
            let len = self.handle.len();
            // I'm going to pretend this is the caller's responsibility
            debug_assert!(len <= isize::max_value() as usize);
            (self.index + (len as isize), value)
        })
    }
}
impl<I: iter::FusedIterator> iter::FusedIterator for SignedEnumerate<I> {}
impl<I: iter::ExactSizeIterator> iter::ExactSizeIterator for SignedEnumerate<I> {}
unsafe impl<I: iter::TrustedLen> iter::TrustedLen for SignedEnumerate<I> {}

impl<T> From<Vec<T>> for TwoSidedVec<T> {
    #[inline]
    fn from(mut original: Vec<T>) -> Self {
        let ptr = original.as_mut_ptr();
        let capacity = original.capacity();
        let len = original.len();
        TwoSidedVec {
            memory: unsafe { RawTwoSidedVec::from_raw_parts(
                    ptr,
                    Capacity { back: 0, front: capacity }
            ) },
            end_index: len as isize,
            start_index: 0
        }
    }
}
impl<T: PartialEq<U>, U> PartialEq<TwoSidedVec<U>> for TwoSidedVec<T> {
    fn eq(&self, other: &TwoSidedVec<U>) -> bool {
        if self.start() == other.start() && self.end() == other.end() {
            for (first, second) in self.back().iter().zip(other.back()) {
                if first != second {
                    return false
                }
            }
            for (first, second) in self.front().iter().zip(other.front()) {
                if first != second {
                    return false
                }
            }
            true
        } else {
            false
        }
    }
}
impl<T: Eq> Eq for TwoSidedVec<T> {}
impl<T: Hash> Hash for TwoSidedVec<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        /*
         * NOTE: We also need to take their start into account,
         * since otherwise [; 1, 2, 3] and [1 ; 2, 3] would hash the same.
         */
        state.write_isize(self.start());
        T::hash_slice(self.back(), state);
        T::hash_slice(self.front(), state);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_push_front() {
        let mut result = TwoSidedVec::new();
        let expected = expected_front();
        for &i in &expected {
            result.push_front(i);
        }
        assert_expected(&result, vec![], expected);
    }
    #[test]
    fn test_push_back() {
        let mut result = TwoSidedVec::new();
        let expected = expected_back();
        for &i in &expected {
            result.push_back(i);
        }
        assert_expected(&result, expected, vec![]);
    }
    #[test]
    fn test_push() {
        let mut result = TwoSidedVec::new();
        let expected_back = expected_back();
        let expected_front = expected_front();
        for &i in &expected_back {
            result.push_back(i);
        }
        for &i in &expected_front {
            result.push_front(i);
        }
        assert_expected(&result, expected_back, expected_front);
    }
    #[test]
    fn test_pop() {
        let mut result = TwoSidedVec::new();
        let mut expected_back = expected_back();
        let mut expected_front = expected_front();
        for &i in &expected_back {
            result.push_back(i);
        }
        for &i in &expected_front {
            result.push_front(i);
        }
        assert_expected(
            &result,
            expected_back.clone(),
            expected_front.clone()
        );
        while let Some(expected) = expected_back.pop() {
            assert_eq!(expected, result.pop_back().unwrap());
        }
        assert_eq!(result.len_back(), expected_back.len());
        while let Some(expected) = expected_front.pop() {
            assert_eq!(expected, result.pop_front().unwrap());
        }
        assert_eq!(result.len_front(), expected_front.len());
        assert!(result.is_empty());
    }
    fn assert_expected<T: Debug + Eq + Clone>(
        target: &TwoSidedVec<T>,
        mut expected_back: Vec<T>,
        expected_front: Vec<T>
    ) {
        expected_back.reverse();
        let expected_start = -(expected_back.len() as isize);
        let expected_end = expected_front.len() as isize;
        assert_eq!(target.start(), expected_start);
        assert_eq!(target.end(), expected_end);
        assert_eq!(target.len(), expected_back.len() + expected_front.len());
        assert_eq!(&target[..0], &*expected_back);
        assert_eq!(&target[0..], &*expected_front);
        for (index, expected) in expected_back.iter().rev().enumerate() {
            assert_eq!(&target[-(index as isize) - 1], expected);
        }
        for (index, expected) in expected_front.iter().enumerate() {
            assert_eq!(&target[(index as isize)], expected);
        }
        let entire_expected = expected_back.iter()
            .chain(expected_front.iter())
            .cloned()
            .collect::<Vec<T>>();
        assert_eq!(&target[..], &*entire_expected);
    }
    fn expected_back() -> Vec<i32> {
        (0..30).map(|i| i * -2).collect()
    }
    fn expected_front() -> Vec<i32> {
        (0..30).map(|i| i * 2).collect()
    }
}
