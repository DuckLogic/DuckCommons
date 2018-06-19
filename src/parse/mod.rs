//! Utilities for parsing text centered around the `SimpleParse` and `SimpleParseError` traits,
//! and using a `SimpleParser` utility for handling the current position.

use std::{slice, mem};
use std::num::{ParseIntError};
use std::fmt::{self, Display, Formatter, Debug};
use std::str::pattern::Pattern;
use std::str::FromStr;
use std::sync::Arc;
use std::borrow::Borrow;
use std::ops::{Deref, Add, AddAssign};
use std::cmp::Ordering;
use std::cell::Cell;

use smallvec::SmallVec;
use regex::Regex;
use failure::Fail;
use itertools::Itertools;

pub mod ascii;
pub mod text;
use self::ascii::AsciiPattern;
use Lazy;

/// When compiling in debug mode, checks a special `PARSER_SHOULD_PANIC` flag,
/// which will make all unexpected tokens trigger a panic instead of an error.
///
/// Panics are much easier to debug than errors, since they include a proper backtrace.
#[inline]
pub fn parser_should_panic() -> bool {
    PanicLevel::current().level() != 0
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct PanicLevel {
    suppression_count: u32,
    raw_level: u32
}
impl PanicLevel {
    #[inline]
    pub fn disabled() -> PanicLevel {
        PanicLevel {
            suppression_count: 0,
            raw_level: 0
        }
    }
    #[inline]
    #[cfg(debug_assertions)]
    pub fn current() -> PanicLevel {
        PanicLevel {
            raw_level: *ENV_PARSER_PANIC_LEVEL,
            suppression_count: PARSER_PANIC_SUPPRESSIONS.with(|cell| cell.get())
        }
    }
    #[inline]
    #[cfg(not(debug_assertions))]
    pub fn current() -> PanicLevel {
        PanicLevel::disabled()
    }
    #[inline]
    pub fn is_suppressed(self) -> bool {
        self.suppression_count > 0
    }
    /// The level of verbrosity of the parser's panics,
    /// or `0` if we shouldn't panic at all.
    #[inline]
    pub fn level(self) -> u32 {
        if self.is_suppressed() { 0 } else { self.raw_level }
    }
    /// The raw level of vebrosity of the parser's panics,
    /// ignoring whether or not we're suppressed.
    #[inline]
    pub fn raw_level(self) -> u32 {
        self.raw_level
    }
}

thread_local! {
    #[cfg(debug_assertions)]
    static PARSER_PANIC_SUPPRESSIONS: Cell<u32> = Cell::new(0);
}
lazy_static! {
    static ref ENV_PARSER_PANIC_LEVEL: u32 = {
        match ::env::environment_config::<u32>("PARSER_PANIC_LEVEL") {
            Some(level) => level,
            None => {
                // Default panic verbosity level is 1
                match ::env::environment_flag("PARSER_SHOULD_PANIC") {
                    Some(true) => 1,
                    Some(false) | None => 0
                }
            }
        }
    };
}
/// Suppress the parser's panics
#[inline]
#[cfg(debug_assertions)]
pub fn with_panic_suppression<R, F>(func: F) -> R where F: FnOnce() -> R {
    PARSER_PANIC_SUPPRESSIONS.with(|cell| cell.set(cell.get() + 1));
    let result = func();
    PARSER_PANIC_SUPPRESSIONS.with(|cell| cell.set(cell.get() - 1));
    result
}
#[cfg(not(debug_assertions))]
#[inline]
pub fn with_panic_suppression<R, F>(func: F) -> R where F: FnOnce() -> R {
    func()
}

/// A span of bytes in the original text.
///
/// As always this is a half-open range `[start, end)`.
/// To save space this doesn't include the line numbers or character offsets,
/// and that needs to be computed using a `SpanCache`.
#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    end: usize
}
impl Span {
    #[inline]
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start <= end);
        Span { start, end }
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.end == self.start
    }
    #[inline]
    pub fn len(&self) -> usize {
        debug_assert!(self.start <= self.end);
        self.end - self.start
    }
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }
    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }
}
pub struct Spanned<T>(pub Span, pub T);

/// A token that has been produced by a lexer, and is usable with a `TokenStream`
pub trait Token: Debug + Clone + PartialEq + From<Symbol> + Send + Sync {
    type Err: SimpleParseError + Sized;
    /// If the token is an ASCII symbol, return its value
    fn symbol(&self) -> Option<Symbol>;
    /// If the token is an identifier, return its value
    fn identifier(&self) -> Option<&Ident>;
    /// Determine if this token is whitespace.
    ///
    /// This is useful for whitespace-significant languages where whitespace
    /// is permitted and meaningful in some contexts but not others.
    #[inline]
    fn is_whitespace(&self) -> bool {
        false
    }
}
/// Represents an ASCII symbol
#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
pub struct Symbol(pub u8);
impl From<char> for Symbol {
    #[inline]
    fn from(unicode: char) -> Self {
        assert!(unicode.is_ascii(), "Unicode symbol: {:?}", unicode);
        Symbol(unicode as u8)
    }
}

#[derive(Clone, Debug)]
pub struct TokenStream<'a, T: Token + 'a> {
    tokens: &'a [(Location, T)],
    token_index: usize
}
impl<'a, T: Token + 'a> TokenStream<'a, T> {
    #[inline]
    pub fn new(tokens: &'a [(Location, T)]) -> Self {
        if tokens.iter().all(|(location, _)| location.is_simple()) {
            assert!(::collect::is_sorted_by_key(tokens, |&(index, _)| index.unwrap_simple()));
        }
        TokenStream { tokens, token_index: 0 }
    }
    /// Advance the specified number of tokens,
    /// creating a subparser from the consumed tokens
    #[inline]
    pub fn take(&mut self, amount: usize) -> Self {
        let start = self.token_index;
        self.advance(amount);
        let end = self.token_index;
        self.slice(start, end)
    }
    /// Consume all the remaining tokens in the stream,
    /// creating a subparser from the consumed tokens
    #[inline]
    pub fn take_all(&mut self) -> Self {
        let amount = self.remaining_tokens();
        self.take(amount)
    }
    #[inline]
    pub fn advance(&mut self, amount: usize) {
        assert!(self.token_index + amount <= self.tokens.len(), "Unable to advance {} tokens", amount);
        self.token_index += amount;
    }
    #[inline]
    pub fn advance_until(&mut self, target: usize) {
        assert!(
            self.token_index <= target,
            "Unable to advance backwards from {} to {}", self.token_index, target
        );
        assert!(
            target <= self.tokens.len(),
            "Unable to advance past end token {} to reach {}", self.tokens.len(), target
        );
        self.token_index = target;
    }
    #[inline]
    pub fn ignore<U: Borrow<T>>(&mut self, token: U) -> bool {
        if self.peek() == Some(token.borrow()) {
            self.pop();
            true
        } else {
            false
        }
    }
    #[inline]
    pub fn ignore_all<U: Borrow<T>>(&mut self, token: U) -> bool {
        let token = token.borrow();
        let mut ignored = false;
        while self.peek() == Some(token) {
            self.pop();
            ignored = true;
        }
        ignored
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.token_index >= self.tokens.len()
    }
    #[inline]
    pub fn is_whitespace(&self) -> bool {
        self.peeking().all(Token::is_whitespace)
    }
    #[inline]
    pub fn pop(&mut self) -> &'a T {
        if let Some(&(_, ref token)) = self.tokens.get(self.token_index) {
            self.token_index += 1;
            token
        } else {
            panic!("Unexpected end of stream: {:?}", self)
        }
    }
    #[inline]
    pub fn peek(&self) -> Option<&'a T> {
        self.look(0)
    }
    #[inline]
    pub fn look(&self, ahead: usize) -> Option<&'a T> {
        if let Some(&(_, ref token)) = self.tokens.get(self.token_index + ahead) {
            Some(token)
        } else {
            None
        }
    }
    #[inline]
    pub fn token_index(&self) -> usize {
        self.token_index
    }
    /// The current position in the original text.
    #[inline]
    pub fn current_location(&self) -> Location {
        if let Some(&(location, _)) = self.tokens.get(self.token_index) {
            location
        } else if let Some(&(location, _)) = self.tokens.last() {
            location
        } else {
            Location::zero()
        }
    }
    #[inline]
    pub fn remaining_tokens(&self) -> usize {
        self.tokens.len() - self.token_index
    }
    /// Creates an error that indicates the next token is unexpected.
    ///
    /// Exactly equivalent to invoking `stream.unexpected().unwrap_err()`
    #[inline]
    pub fn unexpected_err(&self) -> T::Err where T::Err: UnexpectedParseError<T> {
        let Err(error) = self.unexpected();
        error
    }
    /// Indicates that the next token is unexpected,
    /// as if by invoking `unexpected(stream.current_location(), None, stream.peek())`
    /// However, if it's the end of stream, it returns an unexpected end error instead.
    #[cold]
    pub fn unexpected(&self) -> Result<!, T::Err> where T::Err: UnexpectedParseError<T> {
        self.unexpected_ahead(0)
    }
    /// Indicates that the current token is unexpected,
    /// as if by invoking `unexpected(stream.current_location() + ahead, None, stream.peek())`
    /// However, if it's the end of stream, it returns an unexpected end error instead.
    #[cold]
    pub fn unexpected_ahead(&self, ahead: usize) -> Result<!, T::Err> where T::Err: UnexpectedParseError<T> {
        self.unexpected_at(self.token_index + ahead)
    }
    /// Indicates that the current token is unexpected,
    /// and we expected one of the specified values to occur instead.
    #[cold]
    pub fn expected_any<U>(&self, expected: &[U]) -> Result<!, T::Err>
        where T::Err: UnexpectedParseError<T>, U: Borrow<T> {
        let expected = expected.iter()
            .map(Borrow::borrow)
            .cloned()
            .collect::<Vec<T>>();
        if let Some(&(location, ref actual)) = self.tokens.get(self.token_index) {
            debug_assert!(
                !expected.contains(actual),
                "Actual token {:?} was claimed unexpected, but listed in claimed expected values {:?}",
                actual, expected
            );
            self.maybe_panic(Err(T::Err::unexpected(
                location, expected, actual.clone()
            )))
        } else {
            self.unexpected()?
        }
    }
    #[inline]
    pub fn expect<U>(&mut self, expected: U) -> Result<(), T::Err>
        where T::Err: UnexpectedParseError<T>, U: Borrow<T> {
        self.expect_any(&[expected])?;
        Ok(())
    }
    #[inline]
    pub fn expect_any<U>(&mut self, expected: &[U]) -> Result<usize, T::Err>
        where T::Err: UnexpectedParseError<T>, U: Borrow<T> {
        if let Some(next) = self.peek() {
            for (index, possibility) in expected.iter()
                .map(U::borrow).enumerate() {
                if possibility == next {
                    self.advance(1);
                    return Ok(index)
                }
            }
        }
        self.expected_any(expected)?
    }
    #[inline]
    pub fn expect_identifier(&mut self) -> Result<&'a Ident, T::Err>
        where T::Err: UnexpectedParseError<T> {
        if let Some(ident) = self.peek().and_then(|token| token.identifier()) {
            self.advance(1);
            Ok(ident)
        } else {
            self.unexpected()?
        }
    }
    /// Skip all whitespace tokens, as determined by `Token::is_whitespace`
    #[inline]
    pub fn skip_whitespace(&mut self) -> usize {
        self.skip_while(Token::is_whitespace)
    }
    /// Skip all tokens as long as the specified predicate is true,
    /// returning the total number of elements actually skipped.
    ///
    /// This will empty the entire stream if all tokens match the predicate.
    #[inline]
    pub fn skip_while<F>(&mut self, mut func: F) -> usize where F: FnMut(&T) -> bool {
        let total_skipped = self.peeking()
            .take_while(|&token| func(token))
            .count();
        self.advance(total_skipped);
        total_skipped
    }
    /// Indicates that the current token is unexpected,
    /// as if by invoking `unexpected(self.index_at(token), None, stream.peek())`
    #[cold]
    pub fn unexpected_at<E: UnexpectedParseError<T>>(&self, token: usize) -> Result<!, E>
        where T::Err: UnexpectedParseError<T> {
        self.maybe_panic(if let Some(&(location, ref token)) = self.tokens.get(token) {
            Err(E::unexpected(location, vec![], token.clone()))
        } else {
            Err(E::unexpected_end(self.tokens.last().unwrap().0))
        })
    }
    #[inline]
    pub fn location_of(&self, token: usize) -> Location {
        self.tokens[token].0
    }
    /// Parse the specified, splitting it along the specified delimiter until the terminator is reached,
    /// parsing it into `U` by using the specified closure parser.
    ///
    /// Instead of directly determining where each item ends and splitting it there,
    /// it simply expects the parser to take as many tokens as it wants,
    /// then decides whether to continue parser based on whether the next token is the delimiter or end.
    /// This allows handling arbitrary nesting automatically by reusing the existing parser logic,
    /// without having to know anything about the grammar.
    /// A trailing delimiter is also supported to transparently allow `(1, 2, 3, 4,)`.
    /// The initial token stream shouldn't have had it's original starting token consumed to avoid confusion.
    pub fn parse_delimited<'b, S, U, F>(
        &'b mut self,
        delimiter: S, start: S, end: S,
        mut parser: F
    ) -> Result<Vec<U>, T::Err> where S: Into<T>, T::Err: UnmatchedTokenError<T>,
                                      T::Err: UnexpectedParseError<T>, 'a: 'b,
                                      F: FnMut(&mut TokenStream<T>) -> Result<U, T::Err> {
        trace!(
            "parse_delimiter({:?}, {:?}, {:?}) at {:?}",
            maybe_debug!(delimiter), maybe_debug!(start), maybe_debug!(end), self
        );
        let delimiter = delimiter.into();
        let start = start.into();
        let end = end.into();
        debug_assert_ne!(delimiter, end);
        self.expect(&start)?;
        let mut result = Vec::new();
        'parseLoop: loop {
            result.push(parser(self)?);
            match self.expect_any(&[&delimiter, &end])? {
                0 => {
                    continue 'parseLoop;
                },
                1 => {
                    trace!("Finished parse_delimiter at {:?}", self);
                    return Ok(result)
                },
                _ => unreachable!()
            }
        }
    }
    /// Attempt to parse a value using the specified closure,
    /// only advancing the stream if successful.
    #[inline]
    pub fn try_parse<F, R>(&mut self, func: F) -> Result<R, T::Err>
        where F: FnOnce(&mut TokenStream<T>) -> Result<R, T::Err> {
        with_panic_suppression(|| {
            let old_token_index = self.token_index;
            match func(self) {
                Ok(value) => Ok(value),
                Err(error) => {
                    self.token_index = old_token_index;
                    Err(error)
                }
            }
        })
    }
    #[inline]
    pub fn peeking(&self) -> PeekingIter<T> {
        PeekingIter(self.tokens[self.token_index..].iter())
    }
    /// Find the distance until the specified token occurs, or `None` if it isn't found
    #[inline]
    pub fn find<U: Borrow<T>>(&self, target: U) -> Option<usize> {
        let target = target.borrow();
        self.peeking().position(|token| *token == *target)
    }
    /// Find the ending of the specified start token, allowing arbitrary nesting.
    #[inline]
    pub fn find_ending(&self, start: T, end: T) -> Result<usize, T::Err>
        where T::Err: UnmatchedTokenError<T> {
        debug_assert_ne!(start, end);
        assert_eq!(self.peek(), Some(&start));
        let index = self.token_index();
        let mut level = 1;
        for (offset, result) in self.peeking().skip(1).enumerate() {
            if result == &start {
                level += 1;
            } else if result == &end {
                match level {
                    0 => return self.maybe_panic(Err(T::Err::unmatched(
                        self.location_of(index + offset), end
                    ))),
                    1 => return Ok(index + offset),
                    _ => level -= 1
                }
            }
        }
        self.maybe_panic(Err(T::Err::unmatched(self.location_of(index), start)))
    }
    #[inline]
    pub fn slice(&self, start: usize, end: usize) -> Self {
        assert!(
            end > start && end <= self.tokens.len(),
            "Invalid slice for {} tokens: ({}, {})",
            self.tokens.len(), start, end
        );
        TokenStream {
            tokens: &self.tokens[start..end],
            token_index: 0,
        }
    }
    /// Invoke the specified closure, calling `maybe_panic` on its result.
    #[inline]
    pub fn with_maybe_panic<O, E, F>(&self, func: F) -> Result<O, E>
        where E: Debug, F: FnOnce() -> Result<O, E> {
        self.maybe_panic(func())
    }
    /// When compiling in debug mode,
    /// panic if the `PARSER_SHOULD_PANIC` flag is set and the result is an error.
    ///
    /// Although this is automatically handled by `TokenStream`'s error utilities,
    /// user code may need to invoke this manually if they expect
    #[inline]
    pub fn maybe_panic<O, E: Debug>(&self, result: Result<O, E>) -> Result<O, E> {
        match result {
            Ok(value) => Ok(value),
            Err(error) => {
                let level = PanicLevel::current();
                match level.level() {
                    0 => Err(error),
                    1 => panic!("Parser error: `{:?}`", error),
                    _ => {
                        let tokens = self.print_tokens();
                        panic!("Parser error `{:?}` @ {} with tokens {}", error, self.current_location(), tokens)
                    }
                }
            }
        }
    }
    pub fn print_tokens(&self) -> String {
        format!("[{}]",self.tokens.iter()
            .map(|&(location, ref token)| format!("    ({}, {:?})", location, token))
            .join(",\n")
        )
    }
}
pub struct PeekingIter<'a, T: Token + 'a>(slice::Iter<'a, (Location, T)>);
impl<'a, T: Token + 'a> Iterator for PeekingIter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|&(_, ref token)| token)
    }
}
/// Indicates that a type has a default variant that can be created with just an index
pub trait DefaultParseError: SimpleParseError {
    fn default_error(index: usize) -> Self;
}
/// Indicates that a `SimpleParseError` can be created directly from a cause and index
pub trait FromParseError<T>: SimpleParseError {
    fn from_cause(location: Location, cause: T) -> Self;
}
impl<T: SimpleParseError> FromParseError<T> for T {
    #[inline]
    fn from_cause(location: Location, mut cause: T) -> Self {
        if let (&mut Location::Simple(ref mut cause_location),
            Location::Simple(location)) = (cause.location_mut(), location) {
            *cause_location += location;
        }
        cause
    }
}
/// Indicates that a `SimpleParseError` can be created from an unexpected item
pub trait UnexpectedParseError<T>: UnexpectedEndParseError + SimpleParseError {
    fn unexpected(location: Location, expected: Vec<T>, actual: T) -> Self;
}
/// Indicates that a `SimpleParseError` can be created from an unmatched start or end token
pub trait UnmatchedTokenError<T>: UnexpectedParseError<T> {
    fn unmatched(location: Location, start: T) -> Self;
}
/// Indicates that a `SimpleParseError` can be created when an unexpected EOF is encountered
pub trait UnexpectedEndParseError: SimpleParseError {
    fn unexpected_end(location: Location) -> Self;
}

/// A simple parser for text
pub struct SimpleParser<'a> {
    text: &'a str,
    remaining: &'a str,
    /// Remember, the cache is lazily loaded and boxed to save space.
    ///
    /// This shouldn't be a significant waste in the common case,
    /// since a `LocationCache` should usually only be required if we have an error.
    cache: Lazy<Box<LocationCache<'a>>>
}
impl<'a> SimpleParser<'a> {
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.remaining.is_empty()
    }
    /// Return the length of the remaining text
    #[inline]
    pub fn len(&self) -> usize {
        self.remaining.len()
    }
    #[inline]
    pub fn original(&self) -> &'a str { self.text }
    #[inline]
    pub fn remaining(&self) -> &'a str {
        self.remaining
    }
    /// Peek at a single character from the parser, panicking if empty.
    ///
    /// See `peek` for the rationale on why ASCII is usually what you want instead.
    #[inline]
    pub fn peek_char(&self) -> char {
        self.remaining.chars().next().expect("Unexpected end!")
    }
    /// Pop a single character from the parser, panicking if empty.
    ///
    /// This is the unicode counterpart to `pop`, which only supports ASCII.
    #[inline]
    pub fn pop_char(&mut self) -> char {
        let mut chars = self.remaining.chars();
        let char = chars.next().expect("Unexpected end!");
        self.remaining = chars.as_str();
        char
    }
    /// Pop a single ASCII character from the parser,
    /// panicking if the string's empty or the next character isn't ASCII.
    ///
    /// You're expected to have checked for invalid input before calling this method,
    /// and it should be a logic error to have an empty string or an unexpected character.
    /// See `peek` for the rationale on why ASCII is the default.
    #[inline]
    pub fn pop(&mut self) -> u8 {
        let (ascii, newly_remaining) = ascii::take_ascii(self.remaining);
        self.remaining = newly_remaining;
        ascii
    }
    /// Peek at a single ASCII character from the parser, returning `None` if it's not ASCII,
    /// and panicking if the string's empty.
    ///
    /// This optimizes for the common case of treating certain ASCII characters like `"` and `{`
    /// specially, as is common in programming languages, XML, JSON, and others.
    /// Supporting unicode when only ASCII needs to be handled would hurt performance
    /// for absolutely no gain, since unicode can still be supported elsewhere.
    /// For example, you can use `peek` to decide whether or not the next token is a string literal,
    /// then use string slicing to fully support unicode when it is.
    #[inline]
    pub fn peek(&self) -> Option<u8> {
        if let Some(&next_byte) = self.remaining.as_bytes().get(0) {
            if next_byte.is_ascii() {
                Some(next_byte)
            } else {
                None
            }
        } else {
            panic!("Unexpected end")
        }
    }
    /// Try and peek at a single ASCII character from the parser,
    /// returning `None` both if it's not ASCII and if the parser's empty.
    #[inline]
    pub fn try_peek(&self) -> Option<u8> {
        if !self.is_empty() {
            self.peek()
        } else {
            None
        }
    }
    /// Try and peek the specified number of bytes from the parser,
    /// returning `None` if it's not a char boundary or there's insufficeint input.
    #[inline]
    pub fn peek_str(&mut self, amount: usize) -> Option<&'a str> {
        self.remaining.get(0..amount)
    }
    /// Create a new parser for just the remaining text
    #[inline]
    pub fn remaining_parser(&self) -> Self {
        SimpleParser::from(self.remaining)
    }
    /// Take the specified number of bytes from the input, panicking if it's not a valid character boundary
    #[inline]
    pub fn take(&mut self, amount: usize) -> &'a str {
        let (result, remaining) = self.remaining.split_at(amount);
        self.remaining = remaining;
        result
    }
    /// Attempt to take the specified number of bytes from the input,
    /// returning `None` if it's not a valid character boundary.
    #[inline]
    pub fn try_take(&mut self, amount: usize) -> Option<&'a str> {
        if let Some((result, remaining)) = text::try_split_at(self.remaining, amount) {
            self.remaining = remaining;
            Some(result)
        } else {
            None
        }
    }
    /// Take an exact match of the specified string from the input,
    /// returning the matched characters if successful match or `None` if not.
    #[inline]
    pub fn take_equals(&mut self, target: &str) -> Option<&'a str> {
        if let Some(peeked) = self.peek_str(target.len()) {
            if peeked == target {
                Some(self.take(target.len()))
            } else {
                None
            }
        } else {
            None
        }
    }
    /// Take all text that matches the specified regex, ignoring matches that don't begin at the start of the string.
    #[inline]
    pub fn take_pattern(&mut self, pattern: &'static Regex) -> Option<&'a str> {
        let remaining = self.remaining;
        if let Some(find) = pattern.find(remaining) {
            if find.start() == 0 {
                let (result, newly_remaining) = remaining.split_at(find.end());
                self.remaining = newly_remaining;
                return Some(result)
            }
        }
        None
    }
    /// Take a single non-empty ascii word from the input, as if using the regex `[\w]+`
    #[inline]
    pub fn take_word(&mut self) -> Option<&'a str> {
        let result = self.take_only_ascii(|b: u8| b == b'_' || b.is_ascii_alphanumeric());
        if result.is_empty() { None } else { Some(result) }
    }
    /// Take everything until the ending of the specified start token,
    /// allowing arbitrary nesting and returning an `Err` if unmatched.
    ///
    /// The original start token must not've been consumed although it's not included
    /// as part of the resulting match.
    /// However, the function may decide panic if the next token isn't actually the start token.
    #[inline]
    pub fn take_delimited<E>(&mut self, start: u8, end: u8) -> Result<&str, E>
        where E: UnmatchedTokenError<char> {
        assert_ne!(start, end);
        assert_eq!(self.pop(), start);
        let start_index = self.current_index();
        let mut level = 1;
        let target = [start, end];
        let mut matches = ascii::match_indices_ascii(
            self.remaining, target.as_ref(): &[u8]
        );
        for (offset, result) in &mut matches {
            if result == start {
                level += 1;
            } else if result == end {
                match level {
                    0 => return Err(E::unmatched(
                        self.cache().determine_location(start_index + offset),
                        end as char
                    )),
                    1 => {
                        let (result, newly_remaining) = self.remaining.split_at(offset);
                        self.remaining = newly_remaining;
                        return Ok(result)
                    },
                    _ => level -= 1
                }
            } else {
                unreachable!(result)
            }
        }
        Err(E::unmatched(self.cache().determine_location(start_index), start as char))
    }
    /// Take all the input until the given predicate matches an ASCII character,
    /// returning None if the predicate never matches.
    /// The returned match **excludes** the final character that matches the predicate,
    /// and the character that actually triggered the match can be retreived with `peek`.
    ///
    /// Although unicode is supported in the returned slice of the string,
    /// the predicate is only able to match ASCII characters to speed up matching.
    /// Equivelant to an optimized version of `take_while(|c| c.is_ascii() && pattern.apply(c as u8))`,
    /// that never has to UTF8 decode while still supporting the unicode characters.
    /// See `utils::find_ascii` for details on how this search works.
    #[inline]
    pub fn take_until<P: AsciiPattern>(&mut self, pattern: P) -> Option<&'a str> {
        if let Some((result, remaining)) = ascii::split_ascii(self.remaining, pattern) {
            self.remaining = remaining;
            Some(result)
        } else {
            None
        }
    }
    /// Take a single non-empty ascii element the matches the specified predicate,
    /// stopping whenever unicode is encountered or the predicate fails.
    ///
    /// This is slightly slower than `take_until`, though it's still much faster than `take_while`.
    #[inline]
    pub fn take_only_ascii<P: AsciiPattern>(&mut self, mut pattern: P) -> &'a str {
        // NOTE: We invert the predicate since we want to split where the predicate fails
        if let Some((result, remaining)) = ascii::split_ascii(
            self.remaining,
            |b| !pattern.apply(b)) {
            self.remaining = remaining;
            result
        } else {
            mem::replace(&mut self.remaining, "")
        }
    }
    /// Take all the characters that match the specified predicate, or None if no characters match.
    ///
    /// This is somewhat slower than `take_until`, which should be used if possible.
    #[inline]
    pub fn take_while<P: Pattern<'a>>(&mut self, pattern: P) -> Option<&'a str> {
        let remaining = self.remaining;
        let end = remaining.find(text::InvertedPattern::from(pattern))
            .unwrap_or_else(|| remaining.len());
        let (result, remaining) = remaining.split_at(end);
        if !result.is_empty() {
            self.remaining = remaining;
            Some(result)
        } else {
            None
        }
    }
    /// Parse a value from the remaining text, advancing the parser if successful.
    #[inline]
    pub fn try_parse<T: SimpleParse<'a>>(&mut self) -> Result<T, T::Err> {
        let mut parser = self.remaining_parser();
        let result = T::parse(&mut parser)?;
        self.remaining = parser.remaining;
        Ok(result)
    }
    /// Parse a value from the remaining text, advancing the parser if successful,
    /// or creating a new error from the given/infered type.
    #[inline]
    pub fn parse<T, E>(&mut self) -> Result<T, E> where T: SimpleParse<'a>, E: FromParseError<T::Err> {
        let mut parser = self.remaining_parser();
        match T::parse(&mut parser) {
            Ok(value) => {
                self.remaining = parser.remaining;
                Ok(value)
            }
            Err(cause) => Err(E::from_cause(self.current_location(), cause))
        }
    }
    /// Create an error of the specified type from the current index, equivalent to `T::from(self.current_index())`.
    /// Very useful if you have some sort of default 'syntax error' type, which you want to create easily.
    #[inline]
    pub fn error<E: DefaultParseError>(&self) -> Result<!, E> {
        Err(E::default_error(self.current_index()))
    }
    #[inline]
    pub fn current_index(&self) -> usize {
        self.text.len() - self.remaining.len()
    }
    pub fn current_location(&self) -> Location {
        self.cache().determine_location(self.current_index())
    }
    /// The `LocationCache` for this parser
    #[inline]
    pub fn cache(&self) -> &LocationCache<'a> {
        self.cache.load(|| box LocationCache::new(self.text))
    }
    /// Skip all whitespace chars, returning the skipped characters
    #[inline]
    pub fn skip_whitespace(&mut self) -> &'a str {
        self.take_while(|c: char| c.is_whitespace()).unwrap_or("")
    }
}
impl<'a> From<&'a str> for SimpleParser<'a> {
    #[inline]
    fn from(text: &'a str) -> Self {
        SimpleParser {
            text,
            remaining: text,
            cache: Lazy::empty()
        }
    }
}

/// Caches information on line numbers in a text,
/// in order to make computing them fast
pub struct LocationCache<'a> {
    text: &'a str,
    line_starts: Lazy<Vec<usize>>
}
impl<'a> LocationCache<'a> {
    #[inline]
    pub fn new(text: &'a str) -> LocationCache<'a> {
        LocationCache { text, line_starts: Lazy::empty() }
    }
    pub fn determine_location(&self, index: usize) -> Location {
        assert!(index < self.text.len(), "Invalid index: {}", index);
        let line_starts = self.line_starts();
        match line_starts.len() {
            0 => unreachable!(),
            1 => Location::Simple(SimpleLocation {
                multiline: false,
                line: 0,
                index,
                char_offset: index
            }),
            _ => {
                let (line, line_start) = match line_starts.binary_search(&index) {
                    Ok(line) => (line, line_starts[line]),
                    Err(line) => {
                        assert!(line > 0);
                        (line - 1, line_starts[line - 1])
                    }
                };
                assert!(line_start <= index);
                Location::Simple(SimpleLocation {
                    multiline: true,
                    line,
                    index,
                    char_offset: index - line_start
                })
            }
        }
    }
    pub fn offset_location(&self, start: Location, offset: isize) -> Location {
        assert!(start.unwrap_simple().index <= isize::max_value() as usize, "Location overflow: {:?}", start);
        let result = offset + (start.unwrap_simple().index as isize);
        assert!(result >= 0, "Unable to offset {:?} by {}", start, offset);
        self.determine_location(result as usize)
    }
    fn line_starts(&self) -> &[usize] {
        self.line_starts.load(|| {
            let mut result = Vec::with_capacity((self.text.len() / 8)
                .min(64).max(1));
            result.push(0);
            result.extend(self.text.match_indices('\n').map(|(index, _)| index));
            result
        })
    }
}

/// The full location of a byte in a string, including it's line number,
/// character offset and whether or not the original text spanned multiple lines.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SimpleLocation {
    /// Whether or not the original text spanned multiple lines
    pub multiline: bool,
    /// The _line index_ of the original text, starting at zero not one
    pub line: usize,
    /// The character offset of the original text.
    pub char_offset: usize,
    /// The byte index in the original text
    pub index: usize,
}
impl SimpleLocation {
    /// Creates a location for the a single-lined piece line of text,
    /// with the specified byte and character index.
    #[inline]
    pub fn simple(index: usize) -> SimpleLocation {
        SimpleLocation {
            multiline: false,
            line: 0,
            index,
            char_offset: index
        }
    }
    /// The starting location of all text, with both `byte`, `line`, and `char_offset` set to zero.
    ///
    /// The text is assumed to be multiline,
    /// although that's not nessicarrily true.
    #[inline]
    pub fn zero() -> SimpleLocation {
        SimpleLocation {
            multiline: true,
            line: 0,
            char_offset: 0,
            index: 0
        }
    }
}
impl PartialOrd for SimpleLocation {
    #[inline]
    fn partial_cmp(&self, other: &SimpleLocation) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for SimpleLocation {
    #[inline]
    fn cmp(&self, other: &SimpleLocation) -> Ordering {
        self.index.cmp(&other.index)
    }
}
impl Display for SimpleLocation {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if !self.multiline {
            assert_eq!(self.line, 0);
            write!(f, "{}", self.char_offset)
        } else {
            // NOTE: Darn humans expect line numbers to start with one
            write!(f, "{}:{}", self.line + 1, self.char_offset)
        }
    }
}
impl Add for SimpleLocation {
    type Output = SimpleLocation;

    #[cfg_attr(feature = "cargo-clippy", allow(suspicious_arithmetic_impl))]
    fn add(self, rhs: SimpleLocation) -> Self::Output {
        SimpleLocation {
            multiline: self.multiline | rhs.multiline,
            line: self.line + rhs.line,
            char_offset: self.char_offset + rhs.char_offset,
            index: self.index + rhs.index
        }
    }
}
impl AddAssign for SimpleLocation {
    #[inline]
    fn add_assign(&mut self, rhs: SimpleLocation) {
        let result = *self + rhs;
        *self = result
    }
}
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Location {
    Simple(SimpleLocation),
    User(u32)
}
impl Location {
    #[inline]
    pub fn is_simple(&self) -> bool {
        self.simple().is_some()
    }
    /// The starting location of all text, with both `byte`, `line`, and `char_offset` set to zero.
    ///
    /// The text is assumed to be multiline,
    /// although that's not necessarily true.
    #[inline]
    pub fn zero() -> Location {
        Location::Simple(SimpleLocation::zero())
    }
    #[inline]
    pub fn simple(&self) -> Option<SimpleLocation> {
        if let Location::Simple(simple) = *self {
            Some(simple)
        } else {
            None
        }
    }
    #[inline]
    pub fn unwrap_simple(&self) -> SimpleLocation {
        self.simple().unwrap_or_else(|| panic!("Invalid location: {:?}", self))
    }
}
impl PartialOrd for Location {
    #[inline]
    fn partial_cmp(&self, other: &Location) -> Option<Ordering> {
        if let (Location::Simple(this), Location::Simple(other)) = (*self, *other) {
            Some(this.cmp(&other))
        } else {
            None
        }
    }
}
impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Location::Simple(simple) => write!(f, "{}", simple),
            Location::User(index) => write!(f, "user[{}]", index)
        }
    }
}

/// A lightweight trait that indicates the type can be parsed from a `SimpleParser`,
/// while ignoring any trailing data, and giving descriptive error messages on failure.
///
/// The only restriction on the specified text is that it must never be empty,
/// though this may or may not cause a panic.
pub trait SimpleParse<'a>: Sized {
    type Err: SimpleParseError;
    /// Parse this value from the specified parser, ignoring trailing data and returning errors.
    ///
    /// The parser _must_ be at the start of the text, even if this involves creating a sub-parser.
    fn parse(parser: &mut SimpleParser<'a>) -> Result<Self, Self::Err>;
    /// Parse this value from the specified string, returning an error if there's unexpected trailing data.
    #[inline]
    fn parse_str(text: &'a str) -> Result<Self, StringParseError<Self::Err>> {
        let mut parser = SimpleParser::from(text);
        let value = parser.try_parse::<Self>()
            .map_err(|cause| StringParseError::InvalidValue {
                location: cause.location(),
                cause
            })?;
        if !parser.is_empty() {
            Err(StringParseError::UnexpectedTrailing {
                location: parser.current_location()
            })
        } else {
            Ok(value)
        }
    }
}
#[derive(Fail, SimpleParseError, Debug)]
pub enum StringParseError<E: SimpleParseError> {
    #[fail(display = "Unexpected trailing data")]
    UnexpectedTrailing {
        location: Location
    },
    #[fail(display = "{}", cause)]
    InvalidValue {
        location: Location,
        cause: E
    }
}
/// Support method to panic on a missing index
#[doc(hidden)] #[cold] #[inline(never)]
pub fn _missing_index<T: SimpleParseError>(value: &T) -> ! {
    panic!("Missing index for {:?}", value)
}
/// Magic method to 'cast' an `Error` type into a `SimpleParseError` using specialization
///
/// Intended for use by the `ducklogic-derive` crate so they can check if a cause is actually a `SimpleParseError`,
/// without having to do an isinstance check which is impossible for a procedural macro.
#[doc(hidden)]
#[inline]
pub fn _cast_parse_error<T: Fail>(error: &T) -> Option<&SimpleParseError> {
    <T as CastParseError>::cast(error)
}
pub trait CastParseError {
    fn cast(&self) -> Option<&SimpleParseError>;
}
impl<T: Fail> CastParseError for T {
    #[inline]
    default fn cast(&self) -> Option<&SimpleParseError> {
        None
    }
}
impl<T: SimpleParseError> CastParseError for T {
    #[inline]
    fn cast(&self) -> Option<&SimpleParseError> {
        Some(self)
    }
}

/// A descriptive error message for a parse failure, which always includes the index it occurred at.
///
/// The convention for the error's display is that each error shouldn't include the index or cause directly,
/// and should instead rely on the caller to display the causes and position.
pub trait SimpleParseError: Fail {
    /// Return the byte-index of the error in the original text,
    /// or `None` if we are using a user location
    #[inline]
    fn index(&self) -> Option<usize> {
        self.location().simple().map(|simple| simple.index)
    }
    fn location(&self) -> Location;
    /// Give a mutable reference to this parser's location
    fn location_mut(&mut self) -> &mut Location;
    /// Return the underlying cause of this parse error, if it was caused by another `SimpleParseError`
    ///
    /// The cause is expected to have left its indexes untouched,
    /// although the caller may choose to Clone the cause and modify them.
    fn parse_cause(&self) -> Option<&SimpleParseError>;
    /// Fully describe the error over multiple lines,
    /// including the index where it occurred and all the underlying causes.
    fn fully_describe(&self, _original_text: Option<&str>) -> String {
        unimplemented!("Error descriptions")
    }
}
#[derive(Debug)]
pub enum NumericLiteral {
    Floating(f64),
    Integer(i64)
}
lazy_static! {
    static ref FLOAT_TAIL_PATTERN: Regex = Regex::new(r"^(.(\d*))?([eE][+-]?(\d+))?").unwrap();
}
impl<'a> SimpleParse<'a> for NumericLiteral {
    type Err = NumericLiteralParseError;

    fn parse(parser: &mut SimpleParser<'a>) -> Result<Self, Self::Err> {
        debug_assert_eq!(parser.current_index(), 0);
        // First we have to identify whether it's a integer or float
        let positive = match parser.peek() {
            // Handle leading signs first, which are allowed in either floats or integers
            Some(sign @ b'+') | Some(sign @ b'-') => {
                parser.pop();
                sign == b'+'
            },
            _ => true
        };
        // Now that I know calculus, this feels kind of strange.......
        let integral = parser.take_only_ascii(|b: u8| b.is_ascii_digit());
        /*
         * Peek at the next character to determine if it's a float or just an integer.
         * NOTE: We have to use try_peek here since we may be at the end.
         */
        match parser.try_peek() {
            Some(b'.') | Some(b'e') | Some(b'E') => {
                // Use a regular expression to determine where the float ends
                let mut floating = String::with_capacity(integral.len() * 2);
                floating.push(if positive { '+' } else { '-' });
                floating.push_str(integral);
                if let Some(tail) = parser.take_pattern(&*FLOAT_TAIL_PATTERN) {
                    floating.push_str(tail);
                    let value = floating.parse::<f64>().map_err(|_| NumericLiteralParseError::InvalidFloat {
                        location: Location::zero()
                    })?;
                    Ok(NumericLiteral::Floating(value))
                } else {
                    panic!("Expected match for {}", parser.remaining())
                }
            },
            _ => {
                if !integral.is_empty() {
                    // Fast path, when it's just an integer so we can parse without a regex or buffer
                    let raw = integral.parse::<i64>().map_err(|cause| NumericLiteralParseError::InvalidInteger {
                        location: parser.current_location(),
                        cause
                    })?;
                    Ok(NumericLiteral::Integer(if positive { raw } else { -raw }))
                } else {
                    Err(NumericLiteralParseError::InvalidNumber { location: Location::zero() })
                }
            }
        }
    }
}
impl Display for NumericLiteral {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            NumericLiteral::Floating(value) => {
                // NOTE: We have to include at least one decimal to preserve it's floating point status
                write!(f, "{:.1}", value)
            },
            NumericLiteral::Integer(value) => write!(f, "{}", value)
        }
    }
}
#[derive(Fail, SimpleParseError, Debug, Clone)]
pub enum NumericLiteralParseError {
    #[fail(display = "Invalid number")]
    InvalidNumber {
        location: Location,
    },
    #[fail(display = "Invalid integer, {}", cause)]
    InvalidInteger {
        location: Location,
        #[cause] cause: ParseIntError
    },
    #[fail(display = "Invalid float")]
    InvalidFloat {
        location: Location,
    }
}
/// A string literal that has been quoted and escaped
#[derive(Clone, Debug)]
pub struct StringLiteral(pub String);

impl Display for StringLiteral {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "\"{}\"", &self.0.escape_default())
    }
}

impl<'a> SimpleParse<'a> for StringLiteral {
    type Err = StringLiteralParseError;

    fn parse(parser: &mut SimpleParser<'a>) -> Result<Self, Self::Err> {
        if parser.peek() == Some(b'"') {
            parser.pop();
        } else {
            return Err(StringLiteralParseError::ExpectedStartQuote {
                location: Location::zero()
            })
        }
        // NOTE: zero-copy is certainly possible, but seems like premature optimization at this point
        let mut result = String::new();
        'scanLoop: while !parser.is_empty() {
            // Find the next character we're interested in
            if let Some(skipped) = parser.take_until([b'\\', b'"'].as_ref()) {
                result.push_str(skipped);
                let find = parser.pop();
                match find {
                    b'\\' => {
                        let escaped = match parser.try_peek() {
                            Some(b't') => '\t',
                            Some(b'r') => '\r',
                            Some(b'n') => '\n',
                            Some(b'\'') =>'\'',
                            Some(b'"') => '"',
                            Some(b'\\') => '\\',
                            Some(b'u') => return Err(StringLiteralParseError::UnsupportedEscape {
                                kind: 'u',
                                location: parser.current_location()
                            }),
                            _ => return Err(StringLiteralParseError::InvalidEscape {
                                location: parser.current_location()
                            })
                        };
                        result.push(escaped);
                        parser.pop();
                    },
                    b'"' => {
                        return Ok(StringLiteral(result))
                    },
                    _ => unreachable!(find)
                }
            } else {
                break 'scanLoop;
            }
        }
        Err(StringLiteralParseError::MissingEndQuote {
            location: parser.current_location()
        })
    }
}
#[derive(Debug, Clone, Copy, SimpleParseError, Fail)]
pub enum StringLiteralParseError {
    #[fail(display = "Invalid string, expected start quote")]
    ExpectedStartQuote {
        location: Location,
    },
    #[fail(display = "Invalid string, missing end quote")]
    MissingEndQuote {
        location: Location,
    },
    #[fail(display = "Invalid string, invalid escape sequence")]
    InvalidEscape {
        location: Location,
    },
    /// Indicates that an escape is valid but unsupported
    #[fail(display = "Invalid string, unsupported escape '\\{}'", kind)]
    UnsupportedEscape {
        location: Location,
        kind: char
    }
}


pub struct Hexadecimal(pub SmallVec<[u8; 16]>);
impl<'a> SimpleParse<'a> for Hexadecimal {
    type Err = HexadecimalParseError;

    #[inline]
    fn parse(parser: &mut SimpleParser<'a>) -> Result<Self, Self::Err> {
        parser.take_only_ascii(|b: u8| b.is_ascii_hexdigit()).parse()
    }
}
impl FromStr for Hexadecimal {
    type Err = HexadecimalParseError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        if text.is_empty() {
            return Err(HexadecimalParseError::EmptyHex {
                location: Location::zero()
            })
        }
        let mut buffer = SmallVec::new();
        let mut parser = SimpleParser::from(text);
        while !parser.is_empty() {
            let first = parser.peek().and_then(parse_hex_char).ok_or_else(|| HexadecimalParseError::InvalidChar {
                location: parser.current_location(),
                value: parser.peek_char()
            })?;
            parser.pop();
            let second = if !parser.is_empty() {
                parser.peek().and_then(parse_hex_char).ok_or_else(|| HexadecimalParseError::InvalidChar {
                    location: parser.current_location(),
                    value: parser.peek_char()
                })?
            } else {
                return Err(HexadecimalParseError::InvalidLength {
                    location: Location::zero(),
                    amount: buffer.len() + 1
                })
            };
            buffer.push(first | (second << 4));
        }
        Ok(Hexadecimal(buffer))
    }
}
#[inline]
fn parse_hex_char(b: u8) -> Option<u8> {
    match b {
        b'0'...b'9' => Some(b - b'0'),
        b'A'...b'F' => Some((b - b'A') + 10),
        b'a'...b'f' => Some((b - b'a') + 10),
        _ => None
    }
}
#[derive(Debug, Clone, SimpleParseError, Fail)]
pub enum HexadecimalParseError {
    #[fail(display = "Expected valid hexadecimal")]
    EmptyHex {
        location: Location,
    },
    #[fail(display = "Invalid hex char, {}", value)]
    InvalidChar {
        location: Location,
        value: char
    },
    #[fail(display = "Invalid hex length, {}", amount)]
    InvalidLength {
        location: Location,
        amount: usize
    }
}


#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Ident(Arc<str>);
impl Ident {
    #[inline]
    pub fn is_valid_starting_byte(b: u8) -> bool {
        b.is_ascii_alphabetic() || b == b'_'
    }
    #[inline]
    pub fn is_valid_starting_char(c: char) -> bool {
        c.is_ascii() && Ident::is_valid_starting_byte(c as u8)
    }
    #[inline]
    pub fn is_valid_byte(b: u8) -> bool {
        b.is_ascii_alphanumeric() || b == b'_'
    }
    #[inline]
    pub fn is_valid_char(c: char) -> bool {
        c.is_ascii() && Ident::is_valid_byte(c as u8)
    }
    #[inline]
    pub fn new<T: Into<Arc<str>>>(text: T) -> Ident {
        Ident::parse(text.into()).unwrap()
    }
    pub fn parse<T: Into<Arc<str>>>(text: T) -> Result<Ident, InvalidIdentError> {
        let text = text.into();
        match Ident::parse_str(&text) {
            Ok(result) => Ok(result),
            Err(StringParseError::InvalidValue { cause, .. }) => Err(cause),
            Err(StringParseError::UnexpectedTrailing { location }) => {
                Err(InvalidIdentError::InvalidChar {
                    location,
                    invalid: text[location.unwrap_simple().index..].chars().next().unwrap()
                })
            }
        }
    }
    #[inline]
    pub fn rc(&self) -> &Arc<str> {
        &self.0
    }
}
impl Debug for Ident {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Ident")
            .field(&format_args!("{}", self.0))
            .finish()
    }
}
impl Display for Ident {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(&**self)
    }
}
impl AsRef<str> for Ident {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.0
    }
}
impl Deref for Ident {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        &self.0
    }
}
impl PartialEq<str> for Ident {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        **self == *other
    }
}
impl PartialEq<String> for Ident {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        **self == **other
    }
}
impl<'a> SimpleParse<'a> for Ident {
    type Err = InvalidIdentError;

    fn parse(parser: &mut SimpleParser<'a>) -> Result<Self, Self::Err> {
        let mut result = String::new();
        if parser.peek().filter(|&first| Ident::is_valid_starting_byte(first)).is_some() {
            result.push(parser.pop() as char);
            result.push_str(parser.take_only_ascii(Ident::is_valid_byte));
            Ok(Ident(Arc::from(result)))
        } else {
            Err(InvalidIdentError::InvalidChar {
                location: parser.current_location(),
                invalid: parser.peek_char()
            })
        }
    }
}

#[derive(Debug, Fail, SimpleParseError, Clone, Eq, PartialEq)]
pub enum InvalidIdentError {
    #[fail(display = "Empty identifier")]
    Empty {
        location: Location
    },
    #[fail(display = "Invalid character: {:?}", invalid)]
    InvalidChar {
        location: Location,
        invalid: char,
    },
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn parse_ident() {
        assert_eq!(Ident::new("bob"), Ident(Arc::from("bob")));
        assert_eq!(Ident::parse("bob ").unwrap_err(), InvalidIdentError::InvalidChar {
            invalid: ' ',
            location: Location::simple(3)
        });
        let mut parser = SimpleParser::from("bob loves food");
        assert_eq!(parser.parse::<Ident, InvalidIdentError>().unwrap(), Ident::new("bob"));
        assert_eq!(parser.pop(), b' ');
        assert_eq!(parser.parse::<Ident, InvalidIdentError>().unwrap(), Ident::new("loves"));
        assert_eq!(parser.pop(), b' ');
        assert_eq!(parser.parse::<Ident, InvalidIdentError>().unwrap(), Ident::new("food"));
        assert_eq!(parser.remaining(), "");
    }
}
