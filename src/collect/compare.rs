use std::mem;

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
macro_rules! unrolled_compare {
    ($target:ty) => {
        fn all_equal(&self, expected_element: $target) -> bool {
            const IDEAL_SIZE: usize = 32 / mem::size_of::<$target>();
            let start = self.as_ptr();
            let overflow = self.len() % IDEAL_SIZE;
            let stop = unsafe { start.add(self.len() - overflow) };
            let mut ptr = start;
            while ptr < stop {
                unsafe {
                    let elements = *(ptr as *const [$target; IDEAL_SIZE]);
                    let mut all_equals = true;
                    for i in 0..IDEAL_SIZE {
                        all_equals &= *elements.get_unchecked(i) == expected_element;
                    }
                    if !all_equals {
                        return false;
                    }
                    ptr = ptr.add(IDEAL_SIZE);
                }
            }
            debug_assert_eq!(ptr as usize, stop as usize);
            let stop = unsafe { start.add(self.len()) };
            while ptr < stop {
                unsafe {
                    let element = *ptr;
                    if element != expected_element {
                        return false;
                    }
                    ptr = ptr.add(1);
                }
            }
            true
        }
    };
}
impl SliceCompare<u64> for [u64] {
    unrolled_compare!(u64);
}
impl SliceCompare<u32> for [u32] {
    unrolled_compare!(u32);
}
impl SliceCompare<u16> for [u16] {
    unrolled_compare!(u16);
}
impl SliceCompare<u8> for [u8] {
    unrolled_compare!(u8);
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
