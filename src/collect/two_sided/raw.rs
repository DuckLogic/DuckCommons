use std::ptr::{self, Unique};
use std::ops::{Add};
use std::mem;

use std::heap::{Alloc, Layout, Heap};

pub struct RawTwoSidedVec<T> {
    middle: Unique<T>,
    capacity: Capacity
}
impl<T> RawTwoSidedVec<T> {
    #[inline]
    pub fn new() -> Self {
        assert_ne!(mem::size_of::<T>(), 0, "Zero sized type!");
        RawTwoSidedVec {
            middle: Unique::empty(),
            capacity: Capacity { back: 0, front: 0 }
        }
    }
    pub fn with_capacity(capacity: Capacity) -> Self {
        assert_ne!(mem::size_of::<T>(), 0, "Zero sized type!");
        if capacity.is_empty() {
            return RawTwoSidedVec::new()
        }
        let mut heap = Heap::default();
        let raw = heap.alloc_array::<T>(capacity.checked_total())
            .unwrap_or_else(|e| heap.oom(e));
        unsafe {
            let middle = raw.as_ptr().add(capacity.back);
            RawTwoSidedVec::from_raw_parts(middle, capacity)
        }
    }
    #[inline]
    pub unsafe fn from_raw_parts(middle: *mut T, capacity: Capacity) -> Self {
        assert_ne!(mem::size_of::<T>(), 0, "Zero sized type!");
        debug_assert!(!middle.is_null());
        RawTwoSidedVec { middle: Unique::new_unchecked(middle), capacity }
    }
    #[inline]
    pub fn capacity(&self) -> &Capacity {
        &self.capacity
    }
    #[inline]
    pub fn middle(&self) -> *mut T {
        self.middle.as_ptr()
    }
    /// A pointer to the start of the allocation
    #[inline]
    fn alloc_start(&self) -> *mut T {
        unsafe { self.middle.as_ptr().sub(self.capacity.back) }
    }
    fn reserve_in_place(&mut self, request: CapacityRequest) -> bool {
        let requested_capacity = request.used + request.needed;
        /*
         * If we have enough room in the back,
         * we can attempt in-place reallocation first.
         * This avoids moving any memory unless we absolutely need to.
         */
        let mut heap = Heap::default();
        if !self.capacity.is_empty() && self.capacity.back >= requested_capacity.back {
            match unsafe { heap.grow_in_place(
                self.alloc_start() as *mut u8,
                self.capacity.layout::<T>(),
                requested_capacity.layout::<T>()
            ) } {
                Ok(()) => {
                    self.capacity = requested_capacity;
                    true
                },
                Err(_) => false
            }
        } else {
            false
        }
    }
    #[inline(never)] #[cold]
    pub fn reserve(&mut self, request: CapacityRequest) {
        assert!(self.capacity.can_fit(request.used));
        let requested_capacity = request.used + request.needed;
        if !self.capacity.can_fit(requested_capacity) && !self.reserve_in_place(request) {
            unsafe {
                let reallocated = Self::with_capacity(requested_capacity);
                // Fallback to reallocating the vector and moving its memory.
                ptr::copy_nonoverlapping(
                    self.middle().sub(request.used.back),
                    reallocated.middle().sub(request.used.back),
                    request.used.total()
                );
                drop(mem::replace(self, reallocated));
            }
        }
        debug_assert!(self.capacity.can_fit(requested_capacity));
    }
}
unsafe impl<#[may_dangle] T> Drop for RawTwoSidedVec<T> {
    #[inline]
    fn drop(&mut self) {
        if !self.capacity.is_empty() {
            let mut heap = Heap::default();
            unsafe {
                heap.dealloc_array(
                    Unique::new_unchecked(self.alloc_start()),
                    self.capacity.total()
                ).unwrap_or_else(|err| heap.oom(err))
            }
        }
    }
}
#[derive(Copy, Clone, Debug)]
pub struct Capacity {
    pub back: usize,
    pub front: usize
}
impl Capacity {
    #[inline]
    pub fn empty() -> Self {
        Capacity { back: 0, front: 0 }
    }
    #[inline]
    pub fn checked_total(&self) -> usize {
        self.back.checked_add(self.front).expect("Capacity overflow")
    }
    #[inline]
    pub fn total(&self) -> usize {
        self.back + self.front
    }
    #[inline]
    pub fn can_fit(&self, other: Capacity) -> bool {
        self.back >= other.back && self.front >= other.front
    }
    #[inline]
    fn layout<T>(&self) -> Layout {
        Layout::array::<T>(self.checked_total()).expect("Capacity overflow")
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.back == 0 && self.front == 0
    }
}
impl Add for Capacity {
    type Output = Capacity;

    #[inline]
    fn add(self, rhs: Capacity) -> Capacity {
        match (self.front.checked_add(rhs.front), self.back.checked_add(rhs.back)) {
            (Some(front), Some(back)) => Capacity { front, back },
            _ => panic!("Capacity overflow")
        }
    }
}
#[derive(Copy, Clone, Debug)]
pub struct CapacityRequest {
    pub used: Capacity,
    pub needed: Capacity
}
