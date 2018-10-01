#![feature(proc_macro_non_items, type_ascription)]
use duckcommons_derive::strip_expr_nesting;

#[test]
fn strip_ints() {
    assert_eq!(strip_expr_nesting!([]): [u32; 0], []: [u32; 0]);
    assert_eq!(strip_expr_nesting!([1, []]), [1]);
    assert_eq!(strip_expr_nesting!([1, [2, []]]), [1, 2]);
    assert_eq!(strip_expr_nesting!([1, [2, [3, []]]]), [1, 2, 3]);
}