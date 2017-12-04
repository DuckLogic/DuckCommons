
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
/// negative or positive direction so we initially reserve half for the front and the back,
/// although their capacities grow separately and
/// Although bounds checks are _slightly_ slower since they involve two comparisons,
/// the access itself should be just as fast.
// TODO: This needs to be refactored into a 'raw' two-sided vec
pub struct TwoSidedVec<T> {
    memory: RawVec<T>,
    start_index: isize,
    middle: *mut T,
    end_index: isize,
}
impl<T> TwoSidedVec<T> {
    #[inline]
    pub fn new() -> Self {
        unsafe {
            Self::from_raw(RawVec::new(), 0)
        }
    }
    #[inline]
    pub fn with_capacity(back: usize, front: usize) -> Self {
        unsafe {
            Self::from_raw(RawVec::with_capacity(back + front), back)
        }
    }
    #[inline]
    unsafe fn from_raw(raw: RawVec<T>, middle: usize) -> Self {
        assert!(mem::size_of::<T>() > 0, "Zero sized type!");
        debug_assert!(middle <= raw.cap());
        let middle = raw.ptr().offset(middle as isize);
        TwoSidedVec {
            memory: raw,
            middle,
            start_index: 0,
            end_index: 0
        }
    }
    /// Take a slice of the front of this queue
    #[inline]
    pub fn front(&self) -> &[T] {
        unsafe {
            slice::from_raw_parts(self.middle, self.len_front())
        }
    }
    /// Take a slice of the back of this queue
    #[inline]
    pub fn back(&self) -> &[T] {
        unsafe {
            slice::from_raw_parts(self.raw_start(), self.len_back())
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
                slice::from_raw_parts_mut(self.raw_start(), self.len_back()),
                slice::from_raw_parts_mut(self.middle, self.len_front())
            )
        }
    }
    #[inline]
    pub fn push_front(&mut self, value: T) {
        self.reserve_front(1);
        unsafe {
            ptr::write(self.raw_end(), value);
            self.end_index += 1;
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
            ptr::write(self.raw_start().offset(-1), value);
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
        let element_size = mem::size_of::<T>();
        let byte_capacity = self.memory.ptr().byte_offset(self.middle) as usize;
        /*
         * Algebraically equivelant to `self.back_capacity() - self.len_back() <= amount`,
         * while avoiding expensive division and allowing better constant folding.
         * TODO: Handle overflow properly
         */
        if byte_capacity >= (amount + self.len_back()) * element_size {
            self.grow(amount, 0);
        }
        debug_assert!(self.raw_start().wrapping_offset(-(amount as isize)) >= self.memory.ptr())
    }
    #[inline]
    pub fn reserve_front(&mut self, amount: usize) {
        debug_assert!((self.end_index as usize) < self.memory.cap());
        if amount <= self.memory.cap() - self.len_front() {
            self.grow(0, amount);
        }
        debug_assert!(
            self.raw_end().wrapping_offset(amount as isize) <=
                unsafe { self.memory.ptr().offset(self.memory.cap() as isize) }
        );
    }
    #[cold] #[inline(never)]
    fn grow(&mut self, needed_back: usize, needed_front: usize) {
        assert!(self.check_sanity()); // This is always the cold path, so check sanity
        /*
         * In order to avoid moving the memory around multiple times,
         * we first attempt to reallocate in-place if we only need
         * more elements for the front.
         * If that fails or we need elements for the back,
         * we're forced to allocate a completely new buffer and move the elements by hand.
         */
        let back_len = self.back().len();
        let front_len = self.front().len();
        let remaining_back = self.capacity_back() - back_len;
        if needed_back <= remaining_back {
            /*
             * The 'used' capacity doesn't necessarily need to be initialized,
             * so we include all the uninitialized back space as 'used',
             * since we want to include that in the capacity calculations.
             */
            let used_capacity = self.capacity_back() + front_len;
            if self.memory.reserve_in_place(used_capacity, needed_front) {
                debug_assert!(self.capacity_back() - self.len_back() >= needed_back);
                debug_assert!(self.capacity_front() - self.len_front() >= needed_front);
                debug_assert!(self.check_sanity());
                return;
            }
        }
        /*
         * Calculating the new capacity is significantly more complicated than a regular vector,
         * since we have to avoid growing the back too much unnecessarily while still
         * avoiding having to shift all the front elements just to expand the back of the vector.
         * The heuristics are the maximum of the following rules:
         *  - First, maintain the back capacity as a quarter of the front up to a max of 32.
         *  - Even if no additional capacity was explicitly requested for the back,
         *    we expand it to 1/8th of the front capacity as long as it's at least 1/4th in use.
         *  - If additional capacity was explicitly requested for the back,
         *    double the capacity unless the previous heuristics suggest more.
         *  - As always, add the capacity we need for the back to the current length
         */
        let front_capacity = cmp::max(front_len + needed_front, self.capacity_front() * 2);
        let back_capacity = {
            let mut back_rules = vec![
                back_len + needed_back,
                cmp::min(32, front_capacity / 4)
            ];
            if needed_back > remaining_back {
                back_rules.push(self.capacity_back() * 2);
            }
            if self.back().len() * 4 >= self.capacity_back() {
                back_rules.push(front_capacity / 8);
            }
            back_rules.into_iter().max().unwrap()
        };
        assert!(back_capacity >= back_len + needed_back);
        assert!(back_capacity <= isize::max_value() as usize);
        assert!(front_capacity <= isize::max_value() as usize);
        /*
         * Since either in-place allocation failed or we need to expand the back,
         * We need to completely reallocate a new vector than move the elements over.
         */
        let new_memory = RawVec::with_capacity(back_capacity + front_capacity);
        assert!(self.memory.cap() >= self.len());
        assert!(new_memory.cap() >= self.len());
        unsafe {
            let new_middle = new_memory.ptr().offset(back_capacity as isize);
            let new_start = new_middle.offset(self.start_index);
            assert!(new_middle.offset(self.end_index) <= new_middle.ptr().offset(new_memory.cap() as isize));
            assert!(self.raw_end() <= self.memory.ptr().offset(self.memory.cap() as isize));
            ptr::copy_nonoverlapping(self.raw_start(), new_start, self.len());
            // Now that we've successfully copied perform the actual replacement
            self.middle = new_middle;
            self.memory = new_memory;
        }
        debug_assert!(self.capacity_back() - self.len_back() >= needed_back);
        debug_assert!(self.capacity_front() - self.len_front() >= needed_front);
        debug_assert!(self.check_sanity());
    }
    #[inline]
    pub fn capacity_back(&self) -> usize {
        unsafe {
            self.memory.ptr().unchecked_offset_to(self.middle) as usize
        }
    }
    #[inline]
    pub fn capacity_front(&self) -> usize {
        self.memory.cap() - self.capacity_back()
    }
    /// Return the length of the entire vector, which is the sum of the
    /// lengths of the front and back parts.
    ///
    /// The **length isn't where the vector ends**,
    /// since it could have elements in the back with negative indexes.
    /// Use `vec.start()` and `vec.end()` if you want to know the start and end indexes.
    /// The total length is exactly equivelant to `(-queue.start() + queue.end())`.
    #[inline]
    pub fn len(&self) -> usize {
        self.len_back() + self.len_front()
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
    /// This is exactly equivelant to `vec.front().len()`
    #[inline]
    pub fn end(&self) -> isize {
        self.end_index
    }
    /// Return the `[start, end)` range of the element indices,
    /// equivalent to a tuple of `(queue.start(), queue.end())`.
    #[inline]
    pub fn range(&self) -> (isize, isize) {
        (self.start_index, self.end_index)
    }
    /// Iterate over the entire vector, including both the back and front.
    #[inline]
    pub fn iter_entire(&self) -> slice::Iter<T> {
        self.slice_entire().iter()
    }
    #[inline]
    pub fn get(&self, index: isize) -> Option<&T> {
        if index >= self.start_index && index < self.end_index {
            Some(unsafe { &*self.middle.offset(index)})
        } else {
            None
        }
    }
    #[inline]
    pub fn get_mut(&mut self, index: isize) -> Option<&mut T> {
        if index >= self.start_index && index < self.end_index {
            Some(unsafe { &mut *self.middle.offset(index) })
        } else {
            None
        }
    }
    /// Give a raw pointer to the start of the elements
    #[inline]
    pub fn raw_start(&self) -> *mut T {
        unsafe {
            self.middle.offset(self.start_index)
        }
    }
    /// Give a raw pointer to the middle of the elements
    #[inline]
    pub fn raw_middle(&self) -> *mut T {
        self.middle
    }
    #[inline]
    pub fn raw_end(&self) -> *mut T {
        unsafe {
            self.middle.offset(self.end_index)
        }
    }
    #[inline]
    pub fn split_at(&self, index: isize) -> (&[T], &[T]) {
        assert!(index >= self.start_index && index < self.end_index);
        unsafe {
            let first_length = (index - self.start_index) as usize;
            let second_length = (self.end_index - index) as usize;
            let split = self.middle.offset(index);
            (
                slice::from_raw_parts(
                    split.offset(-(first_length as isize)),
                    first_length
                ),
                slice::from_raw_parts(split, second_length)
            )
        }
    }
    fn check_sanity(&self) -> bool {
        assert!(self.start_index <= 0 && self.end_index >= 0);
        assert!(self.raw_end() < unsafe { self.memory.ptr().offset(self.memory.cap() as isize) });
        assert!(self.raw_start() >= self.memory.ptr());
        // These should be implied by the other checks
        debug_assert!(self.raw_start() <= self.middle);
        debug_assert!(self.raw_end() >= self.middle);
        true
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
        SignedEnumerate::new(self.start(), self.slice_entire().iter())
    }
    /// Mutably enumerate the indices and values of each element in the front and back.
    ///
    /// The primary advantage over regular enumeration is that
    /// it gives proper negative indices for elements that are in the back.
    #[inline]
    pub fn enumerate_mut(&mut self) -> SignedEnumerate<slice::IterMut<T>> {
        SignedEnumerate::new(self.start(), self.slice_entire_mut().iter_mut())
    }

    /// Take a slice over all the elements in both the front and back.
    ///
    /// This is a lossy operation that
    /// **looses information on which elements are in the front and back.**
    #[inline]
    pub fn slice_entire(&self) -> &[T] {
        unsafe {
            slice::from_raw_parts(self.raw_start(), self.len())
        }
    }
    /// Take a mutable slice over all the elements in both the front and back.
    ///
    /// This is a lossy operation that
    /// **looses information on which elements are in the front and back.**
    #[inline]
    pub fn slice_entire_mut(&mut self) -> &mut [T] {
        unsafe {
            slice::from_raw_parts_mut(self.raw_start(), self.len())
        }
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
            ptr::drop_in_place(self.slice_entire_mut())
        }
    }
}

impl<T> Index<isize> for TwoSidedVec<T> {
    type Output = T;
    #[inline]
    fn index(&self, index: isize) -> &T {
        self.get(index).expect("Index out of bounds")
    }
}
impl<T> IndexMut<isize> for TwoSidedVec<T> {
    #[inline]
    fn index_mut(&mut self, index: isize) -> &mut T {
        self.get_mut(index).expect("Index out of bounds")
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
impl<I: FusedIterator> FusedIterator for SignedEnumerate<I> {}
impl<I: ExactSizeIterator> ExactSizeIterator for SignedEnumerate<I> {}
unsafe impl<I: TrustedLen> TrustedLen for SignedEnumerate<I> {}
