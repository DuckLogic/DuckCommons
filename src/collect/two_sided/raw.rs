use std::ptr::{self, Unique};
use std::ops::{Add};
use std::mem::ManuallyDrop;

use alloc::raw_vec::RawVec;

pub struct RawTwoSidedVec<T> {
    middle: Unique<T>,
    capacity: Capacity
}
impl<T> RawTwoSidedVec<T> {
    #[inline]
    pub fn new() -> Self {
        RawTwoSidedVec {
            middle: Unique::empty(),
            capacity: Capacity { back: 0, front: 0 }
        }
    }
    #[inline]
    pub fn with_capacity(capacity: Capacity) -> Self {
        let raw = RawVec::with_capacity(capacity.checked_total());
        RawTwoSidedVec { capacity, middle: unsafe { Unique::new_unchecked(raw.ptr()) }, }
    }
    #[inline]
    pub fn capacity(&self) -> &Capacity {
        &self.capacity
    }
    #[inline]
    pub fn start(&self) -> *mut T {
        unsafe { self.middle().sub(self.capacity.back) }
    }
    #[inline]
    pub fn end(&self) -> *mut T {
        unsafe { self.middle().add(self.capacity.front) }
    }
    #[inline]
    pub fn middle(&self) -> *mut T {
        self.middle.as_ptr()
    }
    /// You're free to view this as a raw vector,
    /// as long as you promise not to free the underlying memory.
    #[inline]
    unsafe fn as_raw_vec(&self) -> ManuallyDrop<RawVec<T>> {
        ManuallyDrop::new(RawVec::from_raw_parts(self.middle(), self.capacity().total()))
    }
    #[inline(never)] #[cold]
    pub fn reserve_in_place(&mut self, request: CapacityRequest) -> bool {
        let requested_capacity = request.used + request.needed;
        let mut raw = unsafe { self.as_raw_vec() };
        /*
         * If we have enough room in the back,
         * we can attempt in-place reallocation first.
         * This avoids moving any memory unless we absolutely need to.
         */
        if self.capacity.back >= requested_capacity.back {
            if raw.reserve_in_place(
                request.used.total(),
                request.needed.total()
            ) {
                assert_eq!(self.middle(), raw.ptr());
                self.capacity = requested_capacity;
                return true;
            }
        }
        false
    }
    #[inline(never)] #[cold]
    pub fn reserve(&mut self, request: CapacityRequest) {
        assert!(self.capacity.can_fit(request.used));
        let requested_capacity = request.used + request.needed;
        if !self.capacity.can_fit(requested_capacity) && !self.reserve_in_place(request) {
            unsafe {
                let reallocated = RawTwoSidedVec::with_capacity(requested_capacity);
                // Fallback to reallocating the vector and moving its memory.
                ptr::copy_nonoverlapping(
                    self.start(),
                    reallocated.start(),
                    request.used.total()
                );
                self.middle = reallocated.middle;
                self.capacity = requested_capacity;
                ::std::mem::forget(reallocated);
            }
        }
    }
}
unsafe impl<#[may_dangle] T> Drop for RawTwoSidedVec<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe { ManuallyDrop::drop(&mut self.as_raw_vec()) }
    }
}
#[derive(Copy, Clone, Debug)]
pub struct Capacity {
    pub back: usize,
    pub front: usize
}
impl Capacity {
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
