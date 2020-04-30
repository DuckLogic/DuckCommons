//! Utilities for parsing ASCII text.
//!
//! The central feature of this module is `AsciiPattern`,
//! which is an ASCII-only version of the stdlib `Pattern`.
use std::str::pattern::{Pattern};

use crate::parse::text::unchecked_split_at;

/// Check if the specified string is an ASCII word,
/// matching the regex `\w*`
pub fn is_ascii_word(target: &str) -> bool {
    target.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
}

/// Take the first ASCII character from the string, returning a slice of the remaining data,
/// or returning `None` if the character's not ASCII, and panicking if the string's empty.
///
/// This should be prefered over `split_at` since it's easier to handle errors,
/// gives better panic messages, and might be faster.
#[inline]
pub fn try_take_ascii(text: &str) -> Option<(u8, &str)> {
    if let Some(&byte) = text.as_bytes().get(0) {
        if byte.is_ascii() {
            // Since the first char was ASCII, the next one must be a char boundary
            debug_assert!(text.is_char_boundary(1));
            let remaining = unsafe { text.get_unchecked(1..text.len()) };
            Some((byte, remaining))
        } else {
            None
        }
    } else {
        invalid_take_ascii(text)
    }
}

/// Take the first ASCII character from the string, returning a slice of the remaining data.
/// Panics with a descriptive error message if the first character isn't ASCII, or if the string's empty.
///
/// This should be prefered over `split_at` since it gives better error messages and might be faster.
/// See `try_take_ascii` for the variant that gracefully handles non-ASCII characters.
#[inline]
pub fn take_ascii(text: &str) -> (u8, &str) {
    try_take_ascii(text)
        .unwrap_or_else(|| invalid_take_ascii(text))
}
#[cold]
fn invalid_take_ascii(text: &str) -> ! {
    match text.chars().next() {
        None => panic!("Empty string!"),
        Some(value) => panic!("Unexpected char {:?} in {:?}", value, text)
    }
}

/// Split the string along the first ASCII character that matches the specified predicate,
/// Faster equivelant of `find_ascii(target, func).map(|index| target.split_at(index))`
#[inline]
pub fn split_ascii<P: AsciiPattern>(target: &str, pattern: P) -> Option<(&str, &str)> {
    find_ascii(target, pattern).map(|index| {
        unsafe {
            unchecked_split_at(target, index)
        }
    })
}
/// Iterate over all the indices and values of all the matches of the specified ascii values.
///
/// It's perfectly acceptable for unicode characters to occur in-between the matches,
/// but the pattern will never actually match unicode itself.
#[inline]
pub fn match_indices_ascii<P: AsciiPattern>(target: &str, pattern: P) -> AsciiMatchIndices<P> {
    AsciiMatchIndices {
        pattern,
        remaining: target,
        index: 0,
    }
}
pub struct AsciiMatchIndices<'a, P: AsciiPattern> {
    pattern: P,
    index: usize,
    remaining: &'a str
}

impl<'a, P: AsciiPattern> AsciiMatchIndices<'a, P> {
    #[inline]
    pub fn remaining(&self) -> &'a str {
        self.remaining
    }
}
impl<'a, P: AsciiPattern> Iterator for AsciiMatchIndices<'a, P> {
    type Item = (usize, u8);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(index) = self.pattern.find(self.remaining) {
            debug_assert!(self.remaining.is_char_boundary(index));
            debug_assert!(self.remaining.is_char_boundary(index + 1));
            unsafe {
                let value = *self.remaining.as_bytes().get_unchecked(index);
                self.remaining = self.remaining.get_unchecked((index + 1)..self.remaining.len());
                self.index += index;
                Some((index, value))
            }
        } else {
            None
        }
    }
}

/// Find the first ASCII character that matches the specified predicate,
/// skipping over any unicode that's encountered without needing to decode it.
/// This works because UTF-8 is 'self synchronizing',
/// and the sequence of bytes for a character (including ASCII) can't appear anywhere elsewhere,
/// so a naive ASCII byte search works transparently with UTF8 as long as you check `b.is_ascii()` first.
/// Long live the one true character encoding!
#[inline]
pub fn find_ascii<P: AsciiPattern>(target: &str, mut pattern: P) -> Option<usize> {
    // Desugared version, specialized to use `memchr` where possible
    pattern.find(target)
}

/// Find the first ascii character that matches the specified predicate,
/// stopping as soon unicode is encountered.
#[inline]
pub fn find_only_ascii<P: AsciiPattern>(target: &str, mut pattern: P) -> Result<usize, AsciiFindError> {
    for (index, value) in target.bytes().enumerate() {
        if !value.is_ascii() {
            return Err(AsciiFindError::Unicode(index))
        }
        if pattern.apply(value) {
            return Ok(index);
        }
    }
    Err(AsciiFindError::NotFound)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AsciiFindError {
    Unicode(usize),
    NotFound
}

/// Split the string along the first ascii character that matches the specified predicate,
/// or whenever unicode is encountered.
///
/// If no character matches, `None` whill be returned
#[inline]
pub fn split_only_ascii<P: AsciiPattern>(target: &str, pattern: P) -> Option<(&str, &str)> {
    match find_only_ascii(target, pattern) {
        Ok(index) | Err(AsciiFindError::Unicode(index)) => {
            unsafe { Some(unchecked_split_at(target, index)) }
        },
        Err(AsciiFindError::NotFound) => None
    }
}

/// A pattern that only operates on ASCII characters.
///
/// This trait is unsafe since it's assumed all returned indexes are correct,
/// and do not need to have further bounds checks.
/// In addition to being clearer then a predicate when searching for a `u8` or `&[u8]`,
/// it also allows for much faster searches for a `u8` using `memchr`.
// TODO: Support matching multiple characters
pub unsafe trait AsciiPattern: Sized where for<'a> Self::Unicode: Pattern<'a> {
    type Unicode: Sized;

    /// Apply the predicate against the specified ASCII character
    fn apply(&mut self, ascii: u8) -> bool;

    /// Desugared version of `find_ascii`, specialized to use `memchr` when possible
    #[inline]
    fn find(&mut self, target: &str) -> Option<usize> {
        target.bytes().enumerate()
            .find(|&(_, b)| b.is_ascii() && self.apply(b))
            .map(|(index, _)| index)
    }

    /// Convert this ASCII pattern into an equivelant stdlib pattern
    fn into_std(self) -> Self::Unicode;
}

unsafe impl<F> AsciiPattern for F where F: FnMut(u8) -> bool {
    type Unicode = StdAsciiPattern<Self>;
    #[inline]
    fn apply(&mut self, ascii: u8) -> bool {
        self(ascii)
    }

    fn into_std(self) -> Self::Unicode {
        StdAsciiPattern(self)
    }
}

unsafe impl AsciiPattern for u8 {
    type Unicode = char;

    #[inline]
    fn apply(&mut self, ascii: u8) -> bool {
        /*
         * It's a logic error to use a non-ASCII char as a pattern,
         * and may cause the search to match incorrectly.
         * However, since it's also a logic-error to give non-ASCII
         * data to the predicate, the predicate will just always be false
         * since ASCII never equals non-ASCII.
         */
        debug_assert!(self.is_ascii());
        ascii == *self
    }
    #[inline]
    fn find(&mut self, target: &str) -> Option<usize> {
        /*
         * memchr makes this search ridiculously fast,
         * and it's perfectly safe to use the resulting index
         * unchecked as long as we ensure that the char's ASCII.
         * If the char's a compile-time constant (usually),
         * then this check can be easily constant-folded away.
         */
        assert!(self.is_ascii(), "Byte isn't ascii: {}", *self);
        // NOTE: Can assume is_char_boundary
        ::memchr::memchr(*self, target.as_bytes())
    }

    #[inline]
    fn into_std(self) -> Self::Unicode {
        assert!(self.is_ascii(), "Byte isn't ascii: {}", self);
        self as char
    }
}
unsafe impl<'a> AsciiPattern for &'a [u8] {
    type Unicode = StdAsciiPattern<Self>;
    #[inline]
    fn apply(&mut self, ascii: u8) -> bool {
        debug_assert!(self.is_ascii(), "Invalid ASCII: {:?}", *self);
        self.contains(&ascii)
    }

    #[inline]
    fn into_std(self) -> Self::Unicode {
        StdAsciiPattern(self)
    }
}
/// An `AsciiPattern` wrapped as a stdlib `Pattern`
pub struct StdAsciiPattern<T: AsciiPattern>(T);
impl<T: AsciiPattern> FnMut<(char,)> for StdAsciiPattern<T> {
    #[inline]
    extern "rust-call" fn call_mut(&mut self, args: (char,)) -> bool {
        args.0.is_ascii() && self.0.apply(args.0 as u8)
    }
}
impl<T: AsciiPattern> FnOnce<(char,)> for StdAsciiPattern<T> {
    type Output = bool;
    #[inline]
    extern "rust-call" fn call_once(mut self, args: (char,)) -> Self::Output {
        self.call_mut(args)
    }
}

