#[cfg(test)]
mod test {
    use duckcommons_derive::strip_expr_nesting;
    #[test]
    fn strip_expr_nesting() {
        assert_eq!(strip_expr_nesting!([]): [u32; 0], []: [u32; 0]);
        assert_eq!(strip_expr_nesting!([1, []]), [1]);
        assert_eq!(strip_expr_nesting!([1, [2, []]]), [1, 2]);
        assert_eq!(strip_expr_nesting!([1, [2, [3, []]]]), [1, 2, 3]);
    }
}