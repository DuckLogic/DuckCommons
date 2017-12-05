pub mod counter;
pub mod index;

use num_traits::PrimInt;

/// Trait for performing checked arithmetic with overflow checking via `OverflowError`.
///
/// This isn't just useful for checking the arithmetic ,
/// but is also useful for wrapper indexes like `InstructionRef`.
/// I was writing all these utilities by hand until I realized
/// that all of this could be abstracted into a trait using `OverflowError`.
pub trait CheckedMath<T: Copy = Self>: Copy {
    #[inline]
    fn cast(target: T) -> Result<T, OverflowError> {
        Ok(target)
    }
    #[inline]
    fn add(self, other: T) -> Result<Self, OverflowError>;
    #[inline]
    fn sub(self, other: T) -> Result<Self, OverflowError>;
    #[inline]
    fn mul(self, other: T) -> Result<Self, OverflowError>;
}
impl<T: PrimInt> CheckedMath for T {
    #[inline]
    fn cast(target: T) -> Result<Self, OverflowError> {
        Ok(target)
    }
    #[inline]
    fn add(self, other: Self) -> Result<Self, OverflowError> {
        self.checked_add(&other).ok_or(OverflowError)
    }
    #[inline]
    fn sub(self, other: Self) -> Result<Self, OverflowError> {
        self.checked_sub(&other).ok_or(OverflowError)
    }
    #[inline]
    fn mul(self, other: Self) -> Result<Self, OverflowError> {
        self.checked_sub(&other).ok_or(OverflowError)
    }
}

/// Dedicated error that indicates that some math has encountered unexpected arithmetic overflow.
///
/// A dedicated error type for arithmetic overflow not only better represents intended meaning,
/// but allows us to use the full power of rust's error handling system.
/// The beauty of using an error for arithmetic overflow is it can be easily propagated with `?`,
/// and we can give much better error messages.
/// Error conversion is also much cleaner,
/// since instead of using `From<NoneError>`, you can use `From<OverflowError>`.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct OverflowError;
