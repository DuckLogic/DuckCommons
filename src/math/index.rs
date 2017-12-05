use std::fmt::{self, Formatter, Debug, Display};
use core::nonzero::{NonZero, Zeroable};

use num_traits::{Unsigned, PrimInt};
use idmap::{IntegerId};

pub unsafe trait NonZeroIndexType: IntegerId + Debug + Display + Unsigned + PrimInt + Zeroable {
    fn type_name() -> &'static str;
    fn into_u64(self) -> u64;
    fn raw_from(value: u64) -> Self;
}
unsafe impl NonZeroIndexType for u16 {
    #[inline]
    fn type_name() -> &'static str {
        "u16"
    }
    #[inline]
    fn into_u64(self) -> u64 {
        self as u64
    }
    #[inline]
    fn raw_from(value: u64) -> Self {
        value as u16
    }
}
unsafe impl NonZeroIndexType for u32 {
    #[inline]
    fn type_name() -> &'static str {
        "u32"
    }
    #[inline]
    fn into_u64(self) -> u64 {
        self as u64
    }
    #[inline]
    fn raw_from(value: u64) -> Self {
        value as u32
    }
}

/// An index that's eligible for the 'null pointer optimization'.
/// It internally adds one to all indexes, making the zero index available.
#[derive(Copy, Clone, PartialEq, PartialOrd, Ord, Hash, Eq, Serialize, Deserialize)]
pub struct NonZeroIndex<T: NonZeroIndexType>(NonZero<T>);
impl<T: NonZeroIndexType> NonZeroIndex<T> {
    #[cold] #[inline(never)]
    fn overflow<N: Display>(index: N) -> ! {
        panic!("Index overflowed a {}: {}", T::type_name(), index)
    }
    #[inline]
    pub fn new(value: T) -> Self {
        if value < T::max_value() {
            NonZeroIndex(unsafe { NonZero::new_unchecked(value + T::one()) })
        } else {
            Self::overflow(value)
        }
    }
    #[inline]
    pub fn from_raw(index: T) -> Self {
        NonZeroIndex(NonZero::new(index).expect("Zero index"))
    }
    #[inline]
    pub unsafe fn from_raw_unchecked(index: T) -> Self {
        debug_assert!(!::core::nonzero::Zeroable::is_zero(&index), "Zero index!");
        NonZeroIndex(NonZero::new_unchecked(index))
    }
    #[inline]
    pub fn try_offset(self, offset: u64) -> Option<Self> {
        let old_value = self.0.get().into_u64();
        let max_value = T::max_value().into_u64();
        if offset <= max_value && old_value <= (max_value - offset) {
            Some(unsafe {
                NonZeroIndex::from_raw_unchecked(T::raw_from(
                    old_value + offset
                ))
            })
        } else {
            None
        }
    }
    #[inline]
    pub fn offset(self, offset: u64) -> Self {
        self.try_offset(offset).unwrap_or_else(|| self.offset_overflow(offset))
    }
    #[inline]
    pub fn sub(self, amount: u64) -> Self {
        let old_value = self.0.get().into_u64();
        if old_value > amount {
            return unsafe {
                NonZeroIndex::from_raw_unchecked(T::raw_from(
                    old_value - amount
                ))
            };
        }
        self.sub_overflow(amount)
    }
    #[cold]
    #[inline(never)]
    fn sub_overflow(self, amount: u64) -> ! {
        panic!("Subtracting {} from {:?} overflowed a {}!", amount, self, T::type_name())
    }
    #[cold]
    #[inline(never)]
    fn offset_overflow(self, offset: u64) -> ! {
        panic!("Offsetting {:?} by {} overflowed a {}!", self, offset, T::type_name())
    }
    #[inline]
    pub fn index(self) -> T {
        self.0.get() - T::one()
    }
}
impl<T: NonZeroIndexType> Debug for NonZeroIndex<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("NonZeroIndex")
            .field(&format_args!("{}", self.index()))
            .finish()
    }
}
impl<T: NonZeroIndexType> IntegerId for NonZeroIndex<T> {
    type Storage = ();
    #[inline]
    fn from_storage(_: Self::Storage, id: u64) -> Self {
        Self::from(id)
    }
    #[inline]
    fn into_storage(self) -> Self::Storage {}
    #[inline]
    fn id(&self) -> u64 {
        self.0.get().id()
    }
    #[inline]
    fn id32(&self) -> u32 {
        self.0.get().id32()
    }
}
impl<T: NonZeroIndexType> From<u64> for NonZeroIndex<T> {
    #[inline]
    fn from(index: u64) -> Self {
        /*
         * NOTE: This is only safe because of the check,
         * and the fact that we always add one to the result.
         */
        if index < T::max_value().into_u64() {
            return unsafe { NonZeroIndex::from_raw_unchecked(T::raw_from(index + 1)) }
        }
        Self::overflow(index)
    }
}

impl<T: NonZeroIndexType> From<usize> for NonZeroIndex<T> {
    #[inline]
    fn from(index: usize) -> Self {
        Self::from(index as u64)
    }
}

