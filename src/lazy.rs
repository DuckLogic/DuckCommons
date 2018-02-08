use std::{ptr, mem, intrinsics};
use std::cell::Cell;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::fmt::{self, Formatter, Debug};
use std::hash::{Hash, Hasher};

use parking_lot::Once;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

pub struct AtomicLazy<T: Sync> {
    value: AtomicPtr<T>,
    once: Once
}
impl<T: Sync> Default for AtomicLazy<T> {
    fn default() -> Self {
        AtomicLazy::empty()
    }
}
impl<T: Sync> AtomicLazy<T> {
    #[inline]
    pub const fn empty() -> Self {
        AtomicLazy {
            value: AtomicPtr::new(ptr::null_mut()),
            once: Once::new()
        }
    }
    #[inline]
    pub fn get(&self) -> Option<&T> {
        unsafe {
            let ptr = self.value.load(Ordering::SeqCst);
            if !ptr.is_null() {
                Some(&*ptr)
            } else {
                None
            }
        }
    }
    #[inline]
    pub fn load<F: FnOnce() -> T>(&self, loader: F) -> &T {
        unsafe {
            // NOTE: It's safe to be relaxed since we can always fallback to load_fallback
            let ptr = self.value.load(Ordering::Relaxed);
            if !ptr.is_null() {
                &*ptr
            } else {
                self.load_fallback(loader)
            }
        }
    }
    #[cold]
    unsafe fn load_fallback<F: FnOnce() -> T>(&self, loader: F) -> &T {
        self.once.call_once_force(|_| {
            if !self.get().is_some() {
                if let Err((value, existing)) = self.try_initialize(loader()) {
                    panic!(
                        "Can't initialize with {:?} since already {:?}",
                        maybe_debug!(value),
                        maybe_debug!(existing)
                    )
                }
            }
        });
        self.get().unwrap()
    }
    #[inline]
    pub fn into_inner(self) -> Option<T> {
        unsafe {
            let ptr = self.value.load(Ordering::SeqCst);
            mem::forget(self);
            if !ptr.is_null() {
                Some(*Box::from_raw(ptr))
            } else {
                None
            }
        }
    }
    // NOTE: invalidate would be unsafe since `get_or` uses relaxed ordering
    #[inline]
    pub fn initialize(&self, value: T) -> &T {
        match self.try_initialize(value) {
            Ok(result) => result,
            Err((value, existing)) => {
                panic!(
                    "Can't initialize with {:?} since already {:?}",
                    maybe_debug!(&value),
                    maybe_debug!(existing)
                )
            }
        }
    }
    #[inline]
    pub fn try_initialize(&self, value: T) -> Result<&T, (T, &T)> {
        unsafe {
            let raw = Box::into_raw(box value);
            let previous = self.value.compare_and_swap(ptr::null_mut(), raw, Ordering::SeqCst);
            if !previous.is_null() {
                let value = *Box::from_raw(raw);
                Err((value, &*previous))
            } else {
                Ok(&*raw)
            }
        }
    }
}
unsafe impl<T: Sync> Sync for AtomicLazy<T> {}
unsafe impl<T: Sync> Send for AtomicLazy<T> {}

impl<T: Sync + Clone> Clone for AtomicLazy<T> {
    #[inline]
    fn clone(&self) -> Self {
        if let Some(value) = self.get() {
            AtomicLazy::from(box value.clone())
        } else {
            AtomicLazy::empty()
        }
    }
}

impl<T: Sync> Drop for AtomicLazy<T> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            let ptr = self.value.load(Ordering::SeqCst);
            if !ptr.is_null() {
                drop(Box::from_raw(ptr))
            }
        }
    }
}
impl<T: Sync> From<Option<Box<T>>> for AtomicLazy<T> {
    #[inline]
    fn from(opt: Option<Box<T>>) -> Self {
        let raw = opt.map(Box::into_raw).unwrap_or(ptr::null_mut());
        AtomicLazy {
            value: AtomicPtr::new(raw),
            once: Once::new()
        }
    }
}
impl<T: Sync> From<Box<T>> for AtomicLazy<T> {
    #[inline]
    fn from(boxed: Box<T>) -> Self {
        AtomicLazy::from(Some(boxed))
    }
}
impl<T: Sync> From<T> for AtomicLazy<T> {
    #[inline]
    fn from(value: T) -> Self {
        AtomicLazy::from(box value)
    }
}


/// An unboxed, single-threaded, lazily computed value.
///
/// This is perfectly safe to use since the value can only be initialized once.
/// It has no more overhead than a `Cell<Option<T>>`, even for `Copy` types.
pub struct Lazy<T> {
    value: Cell<Option<T>>
}
impl<T> Default for Lazy<T> {
    fn default() -> Self {
        Lazy::empty()
    }
}
impl<T> Lazy<T> {
    pub const fn new(value: T) -> Self {
        Lazy { value: Cell::new(Some(value)) }
    }
    #[inline]
    pub const fn empty() -> Self {
        Lazy { value: Cell::new(None) }
    }
    #[inline]
    pub fn is_initialized(&self) -> bool {
        self.get().is_some()
    }
    #[inline]
    pub fn get(&self) -> Option<&T> {
        unsafe {
            (*self.value.as_ptr()).as_ref()
        }
    }
    #[inline]
    pub fn unwrap(&self) -> &T {
        self.get().expect("Called Lazy::unwrap() on an uninitialized value!")
    }
    #[inline]
    pub fn load<F: FnOnce() -> T>(&self, loader: F) -> &T {
        self.get().unwrap_or_else(|| self.load_fallback(loader))
    }
    #[cold]
    fn load_fallback<F: FnOnce() -> T>(&self, loader: F) -> &T {
        /*
         * NOTE: We can't just unchecked_set in case the load callback manually initialized.
         * In that case we want to panic instead of triggering undefined behavior
         */
        self.initialize(loader())
    }
    #[inline]
    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.value.get_mut().as_mut()
    }
    #[inline]
    pub fn into_inner(self) -> Option<T> {
        self.value.into_inner()
    }
    #[inline]
    pub fn invalidate(&mut self) -> Option<T> {
        self.value.replace(None)
    }
    pub fn initialize(&self, value: T) -> &T {
        match self.try_initialize(value) {
            Ok(result) => result,
            Err(existing) => {
                panic!(
                    "Can't initialize since already {:?}",
                    maybe_debug!(existing)
                )
            }
        }
    }
    #[inline]
    pub fn try_initialize(&self, value: T) -> Result<&T, &T> {
        if let Some(existing) = self.get() {
            Err(existing)
        } else {
            Ok(unsafe { self.unchecked_set(value) })
        }
    }
    /// Unsafely set the underlying value of this Lazy,
    /// ignoring an existing value and returning a reference to the new one.
    ///
    /// Undefined behavior will occur if the existing value is in use,
    /// and the existing value won't be dropped or read.
    #[inline]
    pub unsafe fn unchecked_set(&self, value: T) -> &T {
        let ptr = self.value.as_ptr();
        ptr::write(ptr, Some(value));
        match *ptr {
            Some(ref value) => value,
            None => intrinsics::unreachable()
        }
    }
}
impl<'de, T> Deserialize<'de> for Lazy<T> where T: Deserialize<'de> {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        Ok(Lazy::from(Option::deserialize(deserializer)?))
    }
}
impl<T: Serialize> Serialize for Lazy<T> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        self.get().serialize(serializer)
    }
}
impl<T: Clone> Clone for Lazy<T> {
    #[inline]
    fn clone(&self) -> Self {
        Lazy::from(self.get().cloned())
    }
}
impl<T> !Sync for Lazy<T> {}
unsafe impl<T> Send for Lazy<T> {}
impl<T: Debug> Debug for Lazy<T> {
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Lazy")
            .field(&self.get())
            .finish()
    }
}
impl<T> From<Option<T>> for Lazy<T> {
    #[inline]
    fn from(opt: Option<T>) -> Lazy<T> {
        Lazy {
            value: Cell::new(opt)
        }
    }
}
impl<T> From<T> for Lazy<T> {
    #[inline]
    fn from(value: T) -> Self {
        Lazy::new(value)
    }
}
impl<T: PartialEq> PartialEq for Lazy<T> {
    #[inline]
    fn eq(&self, other: &Lazy<T>) -> bool {
        self.get() == other.get()
    }
}
impl<T: Eq> Eq for Lazy<T> {}
impl<T: Hash> Hash for Lazy<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get().hash(state)
    }
}
