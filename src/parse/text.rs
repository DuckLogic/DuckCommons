//! Other text utilities.
use std::marker::PhantomData;
use std::str::pattern::{Pattern, Searcher, SearchStep};

#[inline]
pub fn try_split_at(target: &str, index: usize) -> Option<(&str, &str)> {
    if target.is_char_boundary(index) {
        unsafe {
            Some(unchecked_split_at(target, index))
        }
    } else {
        None
    }
}


/// Perform an unchecked version of `str::split_at`, ignoring bounds and UTF8 checks.
/// However, checking is still performed in debug mode, although it shouldn't be relied on.
///
/// ## Safety
/// Undefined behavior if index is out of bounds
#[inline(always)]
pub unsafe fn unchecked_split_at(target: &str, index: usize) -> (&str, &str) {
    debug_assert!(target.is_char_boundary(index));
    (target.get_unchecked(0..index),
     target.get_unchecked(index..target.len()))
}


/// An inverted form of the specified pattern,
/// which matches when the underlying pattern doesn't and doesn't match when the pattern does.
pub struct InvertedPattern<'a, P: Pattern<'a>>(P, PhantomData<&'a ()>);
impl<'a, P: Pattern<'a>> From<P> for InvertedPattern<'a, P> {
    #[inline]
    fn from(pattern: P) -> Self {
        InvertedPattern(pattern, PhantomData)
    }
}
impl<'a, P: Pattern<'a>> Pattern<'a> for InvertedPattern<'a, P> {
    type Searcher = InvertedPatternSearcher<'a, P>;

    #[inline]
    fn into_searcher(self, haystack: &'a str) -> Self::Searcher {
        InvertedPatternSearcher(self.0.into_searcher(haystack))
    }
}
pub struct InvertedPatternSearcher<'a, P: Pattern<'a>>(P::Searcher);
unsafe impl<'a, P: Pattern<'a>> Searcher<'a> for InvertedPatternSearcher<'a, P> {
    #[inline]
    fn haystack(&self) -> &'a str {
        self.0.haystack()
    }

    #[inline]
    fn next(&mut self) -> SearchStep {
        match self.0.next() {
            SearchStep::Match(start, end) => SearchStep::Reject(start, end),
            SearchStep::Reject(start, end) => SearchStep::Match(start, end),
            SearchStep::Done => SearchStep::Done
        }
    }
    #[inline]
    fn next_match(&mut self) -> Option<(usize, usize)> {
        self.0.next_reject()
    }
    #[inline]
    fn next_reject(&mut self) -> Option<(usize, usize)> {
        self.0.next_match()
    }
}