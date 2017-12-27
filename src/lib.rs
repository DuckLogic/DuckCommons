#![feature(
    nonzero, // Needed for NonZeroIndex
    core, // Needed for NonZero
    generic_param_attrs, // Needed for generic_param_attrs
    dropck_eyepatch, // Needed for custom collections
    const_fn, // I refuse to break encapsulation
    box_syntax, // This is much nicer than Box::new
    optin_builtin_traits, // This is much nicer than PhantomData
    attr_literals, // This seems to be needed for AutoError attributes
    specialization, // Needed for maybe_debug
    alloc, allocator_api, // Needed for RawTwoSidedVec
    fused, // Faster iterators
    trusted_len, // Faster iterators
    shared, unique, pointer_methods, // Awesome pointer helpers
    core_intrinsics, // I like microoptimization and undefined behavior
)]
extern crate petgraph;
extern crate smallvec;
extern crate seahash;
extern crate typed_arena;
extern crate ordermap;
extern crate owning_ref;
extern crate core;
extern crate alloc;
extern crate serde;
extern crate num_traits;
#[macro_use]
extern crate serde_derive;
extern crate idmap;
extern crate parking_lot;
#[macro_use]
extern crate duckcommons_derive;
#[cfg(feature="bincode")]
extern crate bincode;
#[cfg(feature="lz4")]
extern crate lz4;
extern crate itertools;

use std::fmt::{Debug};
use std::error::Error;

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
        const FALLBACK: &::std::fmt::Debug = &$fallback;
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

pub use self::lazy::{AtomicLazy, Lazy};
pub use self::collect::{
    TwoSidedVec, SmallBitSet, SeaHashOrderMap,
    SeaHashOrderSet, OrderSet, VecMap, VecSet
};

#[inline]
pub fn cast_debug<T>(value: &T) -> Option<&Debug> {
    <T as CastDebug>::maybe_debug(value)
}
trait CastDebug {
    fn maybe_debug(&self) -> Option<&Debug>;
}
default impl<T> CastDebug for T {
    #[inline]
    fn maybe_debug(&self) -> Option<&Debug> {
        None
    }
}


impl<T: Debug> CastDebug for T {
    #[inline]
    fn maybe_debug(&self) -> Option<&Debug> {
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


/// Marker trait to indicate that the type wants its `Error` implementation automatically derived.
///
/// Configuration is done by adding `error` attributes to each variant,
/// which may be either `description` which gives the error's description,
/// and optionally `display` which is a format string that indicates how the type should be displayed.
///
/// A field named `cause` is special and will automatically implement the `Error::cause` method,
/// and if it's the only variant in the struct it will cause a `From` implementation to be automatically derived.
/// ## Examples
/// ```
/// # #![feature(attr_literals)]
/// # #[macro_use]
/// # extern crate duckcommons_derive;
/// # extern crate duckcommons;
/// # use duckcommons::AutoError;
/// # use std::io::{Error as IoError, ErrorKind as IoErrorKind};
/// #[derive(AutoError, Debug)]
/// pub enum ExampleError {
///     #[error(description("Invalid input"), display("Invalid input: {input}"))]
///     InvalidInput { input: String },
///     #[error(description("IoError"), display("IoError: {cause}"))]
///     IOError { cause: IoError },
///     #[error(description("Monty python rocks"))]
///     MontyPythonOverload
/// }
/// # fn main() {
///     assert_eq!(
///         format!("{}", ExampleError::from(IoError::from(IoErrorKind::NotFound))),
///         "IoError: entity not found"
///     );
///     assert_eq!(
///         format!("{}", ExampleError::InvalidInput { input: "your mom".to_owned() }),
///         "Invalid input: your mom"
///     );
///     /*
///      * Since we didn't provide a seperate `display`, the error defaults to using the description,
///      * as monty python needs no more display beyond the existing description.
///      */
///     assert_eq!(
///         format!("{}", ExampleError::MontyPythonOverload),
///         "Monty python rocks"
///     );
/// # }
/// ```
pub trait AutoError: Error {}

#[cold] #[inline(never)]
pub fn display_panic_message(dynamic: Box<::std::any::Any + Send + 'static>) -> String {
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
