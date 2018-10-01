/// Creates a `FixedBitSet` from the specified elements.
///
/// As always, the capacity must be specified in advance or the macro will panic.
#[macro_export]
macro_rules! fixed_bitset {
    ($capacity:expr, { $($element:expr),* }) => {{
        #[allow(unused_mut)]
        let mut result = ::fixedbitset::FixedBitSet::with_capacity($capacity);
        $(result.insert($element);)*
        result
    }};
}

#[cfg(test)]
mod test {
    #[test]
    fn test_fixed_bitset() {
        let empty = fixed_bitset!(12, {});
        assert_eq!(empty.len(), 12);
        let single = fixed_bitset!(12, { 4 });
        assert_eq!(single.len(), 12);
        assert!(single.contains(4));
        assert!(!single.contains(5));
        let duplicates = fixed_bitset!(12, { 4, 7, 4, 11 });
        assert_eq!(duplicates.len(), 12);
        assert!(duplicates.contains(4));
        assert!(duplicates.contains(7));
        assert!(duplicates.contains(11));
    }
}