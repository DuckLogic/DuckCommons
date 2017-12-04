pub mod counter;

macro_rules! exact_cast_primitives {
    { $($target:ty [ $($casted:ty)*, ] );* } => {
        $(
            impl ExactCast<$target> for $target {
                #[inline]
                fn from_exact(exact: $target) -> Self {
                    exact
                }
                #[inline]
                fn cast(self) -> T {
                    self
                }
            }
            $(
                impl ExactCast<$casted> for $target {
                    #[inline]
                    fn from_exact(exact: $casted) -> Self {
                        exact as $target
                    }
                    #[inline]
                    fn cast(self) -> $casted {
                        self as $casted
                    }
                }
            )*
        )*
    };
}
exact_cast_primitives! {
    u8 [ u64, u32, u16, usize, i16, i32, i64, isize ];
    u16 [ u64, u8, usize, i32, i64, isize ];
    u32 [ u64, u32, u16, usize, i64 ];
    u64 [ ];
    usize [ u64 ];
    i8 [ i16, i32, i64, isize ];
    i16 [ i32, i64, isize ];
    i32 [ i64, isize ];
    i64 [ ];
    isize [ i64 ];
}
/// Trait for performing checked arithmetic with overflow checking via `OverflowError`.
///
/// This isn't just useful for checking the arithmetic ,
/// but is also useful for wrapper indexes like `InstructionRef`.
/// I was writing all these utilities by hand until I realized
/// that all of this could be abstracted into a trait using `OverflowError`.
pub trait CheckedMath<T: ExactCast<Self> = Self>: Copy {
    #[inline]
    fn cast(target: T) -> Result<T, OverflowError> {
        Ok(target)
    }
    #[inline]
    fn add(self, other: T) -> Result<Self, OverflowError> where T: ExactCast<Self> {
        self.exact_cast().add(other.exact_cast()).map(Self::exact_from)
    }
    #[inline]
    fn sub(self, other: T) -> Result<Self, OverflowError> where T: ExactCast<Self> {
        self.exact_cast().sub(other.exact_cast()).map(Self::exact_from)
    }
    #[inline]
    fn mul(self, other: T) -> Result<Self, OverflowError> {
        self.exact_cast().mul(other.exact_cast()).map(Self::exact_from)
    }
}
impl<T: PrimInt, U: ExactCast<T> + PrimInt> CheckedMath<U> for T {
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
