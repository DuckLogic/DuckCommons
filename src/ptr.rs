use std::{mem, intrinsics};
use std::ptr::{NonNull};

/// Pointer extensions which apply directly to pointer types,
/// and to their wrappers `Shared<T>` and `Unique<T>`
pub trait PointerExt<T>: Sized + Copy {
    fn ptr(self) -> *mut T;

    #[inline]
    fn byte_distance(self, other: Self) -> usize {
        (other.ptr() as usize).wrapping_sub(self.ptr() as usize)
    }
    #[inline]
    fn byte_offset(self, other: Self) -> isize {
        (other.ptr() as isize).wrapping_sub(self.ptr() as isize)
    }
    /// Compute the distance from this pointer to the other pointer.
    ///
    /// ## Safety
    /// Undefined behavior if this pointer is greater than the other pointer,
    /// or if the type is a zero-sized type.
    #[inline]
    unsafe fn unchecked_distance_to(self, other: Self) -> usize {
        debug_assert_ne!(mem::size_of::<T>(), 0);
        debug_assert!(self.ptr() <= other.ptr());
        intrinsics::unchecked_div(
            self.byte_distance(other),
            mem::size_of::<T>()
        )
    }
    /// Compute the signed offset from this pointer to the other pointer
    ///
    /// ## Safety
    /// Undefined behavior if this is zero-sized type.
    #[inline]
    unsafe fn unchecked_offset_to(self, other: Self) -> isize {
        debug_assert_ne!(mem::size_of::<T>(), 0);
        intrinsics::unchecked_div(
            self.byte_offset(other),
            mem::size_of::<T>() as isize
        )
    }
}
impl<T> PointerExt<T> for NonNull<T> {
    #[inline]
    fn ptr(self) -> *mut T {
        self.as_ptr()
    }
}
impl<T> PointerExt<T> for *mut T {
    #[inline]
    fn ptr(self) -> *mut T {
        self
    }
}
