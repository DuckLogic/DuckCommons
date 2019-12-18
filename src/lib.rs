#![feature(
    dropck_eyepatch, // Needed for custom collections
    const_fn, // I refuse to break encapsulation
    box_syntax, // This is much nicer than Box::new
    optin_builtin_traits, // This is much nicer than PhantomData
    specialization, // Needed for maybe_debug
    allocator_api, // Needed for RawTwoSidedVec
    trusted_len, // Faster iterators
    core_intrinsics, // I like microoptimization and undefined behavior
    type_ascription, // Type ascription is awesome
    unboxed_closures, // Apparently you can only manually implement closures on nightly -_-
    fn_traits, // I guess `FnMut` is also an unstable trait?
    pattern, // We use the pattern API for parsing
    drain_filter, // Better alternative to retain
    exhaustive_patterns, // Needed to match exhaustively on the never type
    proc_macro_hygiene, // Needed for strip_expr_nesting
    exact_size_is_empty, // Why isn't this already stable?
    ptr_offset_from, // Helps make pointer logic cleaner
    associated_type_defaults, // Avoids boilerplate in SimpleParseErrorKind
    never_type, // The never type is awesome
)]
#![cfg_attr(feature="cargo-clippy", allow(
    type_complexity, // Sometimes I just like complex types ^_^
    cast_lossless, // I disagree with this lint
))]
#![deny(
    bare_trait_objects, // These are unclear legacy baggage
)]

use std::hint;
use std::fmt::{Debug, Display};

/// Attempts to debug the specified value,
/// by casting it to a `&Debug` trait object if possible.
///
/// If the cast fails, this falls back to using `"<unknown>"`
/// though this behavior can be overridden if desired.
///
/// This is simply a thin wrapper around `cast_debug`,
/// which performs all of the underlying magic.
#[macro_export]
macro_rules! maybe_debug {
    ($target:ident) => (maybe_debug!(&$target));
    ($target:expr) => (maybe_debug!($target, "<unknown>"));
    ($target:expr, $fallback:expr) => ({
        const FALLBACK: &dyn ::std::fmt::Debug = &$fallback;
        ($crate::cast_debug($target)).unwrap_or(FALLBACK)
    });
}

pub mod collect;
pub mod math;
pub mod arena_set;
pub mod env;
pub mod indexed_arena;
pub mod lazy;
pub mod serialize;
pub mod ptr;
pub mod parse;
pub mod logging;

pub use self::lazy::{AtomicLazy, Lazy};
pub use self::collect::{
    SmallBitSet, SeaHashOrderMap, SliceCompare,
    SeaHashOrderSet, VecMap, VecSet, FindSingle
};
#[deprecated(note = "Please import directly from `two_sided_vec`")]
pub use two_sided_vec::TwoSidedVec;
pub use self::logging::{SerializeValue, IterValue};
pub use crate::math::counter::{IdCounter, IdCounted};

#[inline]
pub fn cast_display<T>(value: &T) -> Option<&dyn Display> {
    <T as CastDisplay>::maybe_display(value)
}

#[inline]
pub fn cast_debug<T>(value: &T) -> Option<&dyn Debug> {
    <T as CastDebug>::maybe_debug(value)
}
trait CastDisplay {
    fn maybe_display(&self) -> Option<&dyn Display>;
}
impl<T> CastDisplay for T {
    #[inline]
    default fn maybe_display(&self) -> Option<&dyn Display> {
        None
    }
}
impl<T: Display> CastDisplay for T {
    #[inline]
    fn maybe_display(&self) -> Option<&dyn Display> {
        Some(self)
    }
}
trait CastDebug {
    fn maybe_debug(&self) -> Option<&dyn Debug>;
}
impl<T> CastDebug for T {
    #[inline]
    default fn maybe_debug(&self) -> Option<&dyn Debug> {
        None
    }
}


impl<T: Debug> CastDebug for T {
    #[inline]
    fn maybe_debug(&self) -> Option<&dyn Debug> {
        Some(self)
    }
}
/// Generalization of the `ToOwned` trait,
/// that allows zero-cost conversion of owned values.
///
/// This is in contrast to `ToOwned` which takes `&self` and can't reuse already-owned input.
/// For example `String::to_owned` takes a `&String`
/// This is the real reason for why the entry API is forced to require owned values,
/// even if the key's already in the map.
///
/// This has both a blanket identity implementation that just returns `Self`,
/// and also has a blanket implementation for `Clone` references.
///
/// Please go call your congressman to try and get this in the stdlib.
pub trait IntoOwned<O> {
    fn into_owned(self) -> O;
}
impl<T> IntoOwned<T> for T {
    #[inline]
    fn into_owned(self) -> T {
        self
    }
}
impl<'a, T: Clone> IntoOwned<T> for &'a T {
    #[inline]
    fn into_owned(self) -> T {
        self.clone()
    }
}
impl<'a> IntoOwned<String> for &'a str {
    #[inline]
    fn into_owned(self) -> String {
        self.to_owned()
    }
}

#[cold] #[inline(never)]
pub fn display_panic_message(dynamic: Box<dyn ::std::any::Any + Send + 'static>) -> String {
    match dynamic.downcast::<String>() {
        Ok(message) => *message,
        Err(dynamic) => {
            match dynamic.downcast::<&'static str>() {
                Ok(message) => (*message).to_owned(),
                Err(_) => "<UNKNOWN PANIC MESSAGE>".to_owned()
            }
        }
    }
}

pub trait OptionExt<T> {
    /// Initialize this option with the specified value,
    /// panicking if it isn't already `None`
    fn initialize(&mut self, value: T) -> &mut T;
     unsafe fn unchecked_unwrap(self) -> T;
    unsafe fn unchecked_unwrap_none(self);
}
impl<T> OptionExt<T> for Option<T> {
    #[inline]
    fn initialize(&mut self, value: T) -> &mut T {
        if self.is_none() {
            *self = Some(value);
            self.as_mut().unwrap()
        } else {
            panic!(
                "Unable to initialize {:?} with {:?}",
                maybe_debug!(self), maybe_debug!(value)
            )
        }
    }

    #[inline(always)]
    unsafe fn unchecked_unwrap(self) -> T {
        match self {
            None => hint::unreachable_unchecked(),
            Some(value) => value,
        }
    }

    #[inline(always)]
    unsafe fn unchecked_unwrap_none(self) {
        match self {
            None => {},
            Some(_) => hint::unreachable_unchecked(),
        }
    }
}
