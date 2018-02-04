use stdsimd::simd::*;

/// Utilities for comparing slices
pub trait SliceCompare<T> {
    fn all_equal(&self, expected_element: T) -> bool;
    #[inline]
    fn all_zero(&self) -> bool where T: ::num_traits::Zero {
        self.all_equal(T::zero())
    }
}
default impl<T: PartialEq> SliceCompare<T> for [T] {
    #[inline]
    fn all_equal(&self, expected_element: T) -> bool {
        self.iter().all(|element| *element == expected_element)
    }
}
macro_rules! vector_slice_compare {
    ($target:ty, $vector:ident, $len:expr) => {
        fn all_equal(&self, expected_element: $target) -> bool {
            // Yay, we get to use vector comparisons!
            let expected_vector = $vector::splat(expected_element);
            let mut offset = 0;
            while self.len() - offset >= $len {
                let actual_vector = $vector::load(self, offset);
                if actual_vector != expected_vector {
                    return false
                }
                offset += $len;
            }
            while offset < self.len() {
                if self[offset] != expected_element {
                    return false
                }
                offset += 1;
            }
            true
        }
    };
}
impl SliceCompare<u64> for [u64] {
    vector_slice_compare!(u64, u64x8, 8);
}
impl SliceCompare<u32> for [u32] {
    vector_slice_compare!(u32, u32x16, 16);
}
impl SliceCompare<u16> for [u16] {
    vector_slice_compare!(u16, u16x32, 32);
}
impl SliceCompare<u8> for [u8] {
    vector_slice_compare!(u8, u8x64, 64);
}

#[cfg(test)]
mod test {
    use super::*;
    macro_rules! test_compare {
        ($name:ident, $target:ty) => {
            #[test]
            fn $name() {
                use std::iter::{repeat};
                assert!(vec![0: $target; 4782].all_zero());
                assert!(vec![42: $target; 4284].all_equal(42));
                let mut failure = vec![42: $target; 2049];
                failure.push(43);
                assert!(!failure.all_equal(42));
                failure = vec![82: $target; 64];
                failure.push(42);
                failure.extend(repeat(82: $target).take(63));
                assert!(!failure.all_equal(82));
            }
        };
    }
    test_compare!(compare_u64, u64);
    test_compare!(compare_u32, u32);
    test_compare!(compare_u16, u16);
    test_compare!(compare_u8, u8);
}