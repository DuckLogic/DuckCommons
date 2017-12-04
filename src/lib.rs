extern crate petgraph;

pub mod collect;
pub mod math;
pub mod arena_set;
pub mod env;
pub mod indexed_arena;
pub mod lazy;

pub use self::lazy::{AtomicLazy, Lazy, EnvironmentFlag};
pub use self::collect::{
    TwoSidedVec, SmallBitSet, SeaHashOrderMap, SeaHashOrderSet, OrderSet, VecMap, VecSet
};

/// Serialize the specified value into lz4 compressed bincode
#[inline]
pub fn serialize_compressed_bincode<T: Serialize, W: Write>(value: &T, write: W) -> Result<(), SerializationError> {
    let mut encoder = BufWriter::new(Lz4EncoderBuilder::new().build(write)?);
    ::bincode::serialize_into(&mut encoder, value, ::bincode::Infinite)?;
    Ok(())
}
/// Deserialize the specified value from lz4 compresed bincode
#[inline]
pub fn deserialize_compressed_bincode<T: DeserializeOwned, R: Read>(source: R) -> Result<T, SerializationError> {
    let mut decoder = BufReader::new(Lz4Decoder::new(source)?);
    Ok(::bincode::deserialize_from(&mut decoder, ::bincode::Infinite)?)
}

#[derive(AutoError, Debug)]
pub enum SerializationError {
    #[error(description("IOError"), display("{cause}"))]
    IoError {
        cause: IoError
    },
    #[error(description("Invalid bincode"), display("Invalid bincode: {cause}"))]
    Bincode {
        cause: ::bincode::Error
    }
}

pub struct NodeRange<T: IndexType = u32> {
    index: NodeIndex<T>,
    end: NodeIndex<T>
}
impl<T: IndexType> NodeRange<T> {
    #[inline]
    pub fn until(end: NodeIndex<T>) -> Self {
        NodeRange::new(NodeIndex::new(0), end)
    }
    #[inline]
    pub fn new(start: NodeIndex<T>, end: NodeIndex<T>) -> Self {
        debug_assert!(start <= end);
        NodeRange { index: start, end }
    }
}
impl<T: IndexType> Iterator for NodeRange<T> {
    type Item = NodeIndex<T>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let index = self.index;
        if index < self.end {
            self.index = NodeIndex::new(index.index() + 1);
            Some(index)
        } else {
            None
        }
    }
}
pub fn maybe_debug<T>(value: &T) -> Option<String> {
    value.maybe_debug()
}
trait MaybeDebug {
    fn maybe_debug(&self) -> Option<String>;
}
default impl<T> MaybeDebug for T {
    #[inline]
    fn maybe_debug(&self) -> Option<String> {
        None
    }
}
impl<T: Debug> MaybeDebug for T {
    #[inline]
    fn maybe_debug(&self) -> Option<String> {
        let mut buffer = String::new();
        write!(buffer, "{:?}", self).expect("Unable to debug!");
        Some(buffer)
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

/// Pointer extensions which apply directly to pointer types,
/// and to their wrappers `Shared<T>` and `Unique<T>`
pub trait PointerExt<T>: Sized + Copy {
    fn ptr(self) -> *mut T;

    #[inline]
    fn byte_distance(self, other: Self) -> usize {
        (other.ptr() as usize).wrapping_sub(self.ptr() as usize)
    }
    #[inline]
    fn byte_offset(self, other: Self) -> isize {
        (other.ptr() as isize).wrapping_sub(self.ptr() as isize)
    }
    /// Compute the distance from this pointer to the other pointer,
    /// resulting in undefined behavior if this pointer is greater than the other pointer,
    /// or if the type is a zero-sized type.
    #[inline]
    unsafe fn unchecked_distance_to(self, other: Self) -> usize {
        debug_assert_ne!(mem::size_of::<T>(), 0);
        debug_assert!(self.ptr() <= other.ptr());
        intrinsics::unchecked_div(
            self.byte_distance(other),
            mem::size_of::<T>()
        )
    }
    #[inline]
    unsafe fn unchecked_offset_to(self, other: Self) -> isize {
        debug_assert_ne!(mem::size_of::<T>(), 0);
        intrinsics::unchecked_div(
            self.byte_offset(other),
            mem::size_of::<T>() as isize
        )
    }
}
impl<T> PointerExt<T> for Shared<T> {
    #[inline]
    fn ptr(self) -> *mut T {
        self.as_ptr()
    }
}
impl<T> PointerExt<T> for *mut T {
    #[inline]
    fn ptr(self) -> *mut T {
        self
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
/// # #[macro_use]
/// # extern crate ducklogic_derivee;
/// # use std::io::{Error as IOError, ErrorKind as IOErrorKind};
/// #[derive(AutoError, Debug)]
/// pub enum ExampleError {
///     #[error(description("Invalid input"), display("Invalid input: {input}"))]
///     InvalidInput { input: String },
///     #[error(description("IOError"), display("IOError: {cause}"))]
///     IOError { cause: IOError },
///     #[error(description("Monty python rocks"))]
///     MontyPythonOverload
/// }
/// # fn main() {
///     assert_eq!(
///         format!("{}", ExampleError::from(IOError::new(ErrorKind::NotFound, None))),
///         "IOError: entity not found
///     );
///     assert_eq!(
///         format!("{}", ExampleError::InvalidInput { input: "your mom".to_owned }),
///         "Invalid input: your mom"
///     );
///     /*
///      * Since we didn't provide a seperate `display`, the error defaults to using the description,
///      * as monty python needs no more display beyond the existing description.
///      */
///     assert_eq!(
///         format!("{}", ExampleError::MontyPythonOverload),
///         "Monty python rocks
///     );
/// # }
/// ```
pub trait AutoError: Error {}
