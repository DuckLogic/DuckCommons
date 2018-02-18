use std::ptr::NonNull;
use std::hash::{Hash, Hasher, BuildHasherDefault};
use std::cell::{RefCell, RefMut, Ref};
use std::marker::PhantomData;

use seahash::SeaHasher;
use typed_arena::Arena;
use ordermap::{OrderMap, Equivalent};

use IntoOwned;

type InternalMap<T> = OrderMap<HashedPtr<T>, NonNull<T>, BuildHasherDefault<IdiotHasher>>;

/// An hash set of unique arena-allocated values,
/// internally backed by a `OrderMap`.
/// This is useful to avoid wasting memory with duplicate values,
/// although it does incur hashing overhead.
///
/// This is only necessarily because rust doesn't allow safe code to implement
/// self-referential data structures.
pub struct ArenaSet<T: Eq + Hash> {
    arena: Arena<T>,
    /*
     * NOTE: We can't use a empty value since the entry doesn't support getting a reference
     * to a key we've just inserted using the entry API.
     * This would be fixed if an `OrderSet` was added and it had an entry API.
     * Since these are just pointers into the arena's memory,
     * no special action needs to be taken when they're deallocated.
     *
     * Since the entry API doesn't support lookup with `ToOwned` keys,
     * and always demands a owned key,
     * we have to first check for an existing key with a get call.
     * However, since we don't want to have to hash the key twice,
     * we use an `IdiotHasher` which forces everything to have a precomputed hash.
     * Since we already have a `EquivelentLookupHack` wrapper,
     * it's relatively simple to make the wrapper use precomputed hashes.
     */
    values: RefCell<InternalMap<T>>
}
unsafe impl<T: Eq + Hash> Send for ArenaSet<T> {}
impl<T: Eq + Hash> ArenaSet<T> {
    #[inline]
    pub fn new() -> Self {
        ArenaSet::with_capacity(0)
    }
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        ArenaSet {
            arena: Arena::with_capacity(capacity),
            values: RefCell::new(OrderMap::with_capacity_and_hasher(
                capacity, Default::default()))
        }
    }
    pub fn find<Q>(&self, key: &Q) -> Option<&T> where Q: Equivalent<T> + Hash + Eq {
        self.values.borrow()
            .get(&EquivalentLookupHack::hashed(key))
            .map(|value| {
                // Items allocated in the arena are valid for its entire lifetime
                unsafe { &*value.as_ptr() }
            })
    }
    #[inline]
    pub fn alloc<Q>(&self, value: Q) -> &T where Q: Equivalent<T>, Q: Eq + Hash, Q: IntoOwned<T> {
        /*
         * Always demand an exclusive mutable reference up front,
         * to prevent any possible allocation while iterating from ever working.
         */
        let exclusive_borrow = self.values.try_borrow_mut()
            .expect("Unable to exclusively borrow ArenaSet internals");
        let hash = {
            let lookup_hack = EquivalentLookupHack::<T, Q>::hashed(&value);
            if let Some(ptr) = exclusive_borrow.get(&lookup_hack) {
                /*
                 * Since references to items allocated in the arena are guaranteed to never move,
                 * their pointers are valid for the entire lifetime of the `Arena`.'
                 * This could be safely expressed with a hypothetical `&self` lifetime,
                 * which is currently missing from Rust :(
                 */
                return unsafe { &*ptr.as_ptr() }
            }
            lookup_hack.hash
        };
        self.alloc_fallback(exclusive_borrow, value, hash)
    }

    #[cold] #[inline(never)]
    fn alloc_fallback<Q: IntoOwned<T>>(&self, mut exclusive_borrow: RefMut<InternalMap<T>>,
                                       value: Q, hash: u64) -> &T {
        let value = value.into_owned();
        unsafe {
            let result = NonNull::from(self.arena.alloc(value));
            exclusive_borrow.insert(HashedPtr::with_hash(hash, result), result);
            &*result.as_ptr()
        }
    }
    #[inline]
    pub fn iter(&self) -> ArenaSetIter<T> {
        let handle = self.values.borrow();
        ArenaSetIter {
            handle,
            index: 0,
            _marker: PhantomData
        }
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.values.borrow().len()
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.values.borrow().is_empty()
    }
}
pub struct ArenaSetIter<'a, T: Eq + Hash + 'a> {
    _marker: PhantomData<&'a ArenaSet<T>>,
    handle: Ref<'a, InternalMap<T>>,
    index: usize,
}
impl<'a, T: Eq + Hash + 'a> Iterator for ArenaSetIter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        if let Some((&key, _)) = self.handle.get_index(self.index) {
            self.index += 1;
            Some(unsafe { &*key.as_ptr() })
        } else {
            None
        }
    }
}

impl<T: Eq + Hash> Default for ArenaSet<T> {
    #[inline]
    fn default() -> Self {
        ArenaSet::new()
    }
}
impl<T: Eq + Hash + Clone> Clone for ArenaSet<T> {
    fn clone(&self) -> Self {
        let result = ArenaSet::with_capacity(self.len());
        for value in self.iter() {
            result.alloc(value.clone());
        }
        result
    }
}


struct HashedPtr<T> {
    hash: u64,
    unsafe_value: NonNull<T>
}
impl<T> Clone for HashedPtr<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for HashedPtr<T> {}
impl<T> HashedPtr<T> {
    #[inline]
    unsafe fn with_hash(hash: u64, value: NonNull<T>) -> Self {
        HashedPtr {
            hash,
            unsafe_value: value
        }
    }
    #[inline]
    fn as_ptr(self) -> *mut T {
        self.unsafe_value.as_ptr()
    }
}
impl<T: Hash> Hash for HashedPtr<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}
impl<T: PartialEq<Q>, Q> PartialEq<HashedPtr<Q>> for HashedPtr<T> {
    #[inline]
    fn eq(&self, other: &HashedPtr<Q>) -> bool {
        unsafe {
            (*self.as_ptr()).eq(&*other.as_ptr())
        }
    }
}
impl<T: Eq> Eq for HashedPtr<T> {}
// I am so done with rust's type system right now -_-
#[derive(Copy, Clone)]
struct EquivalentLookupHack<'a, T, Q: Eq + Equivalent<T> + 'a> {
    target: &'a Q,
    hash: u64,
    marker: PhantomData<*const T>
}
impl<'a, T, Q: Hash + Eq + Equivalent<T> + 'a> EquivalentLookupHack<'a, T, Q> {
    #[inline]
    fn hashed(target: &'a Q) -> Self {
        let mut hasher = SeaHasher::default();
        target.hash(&mut hasher);
        let hash = hasher.finish();
        EquivalentLookupHack { hash, target, marker: PhantomData }
    }
}
impl<'a, T, Q: Eq + Equivalent<T> + 'a> Hash for EquivalentLookupHack<'a, T, Q> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash)
    }
}
impl<'a, T, Q: Equivalent<T> + Eq + 'a> Equivalent<HashedPtr<T>> for EquivalentLookupHack<'a, T, Q> {
    #[inline]
    fn equivalent(&self, key: &HashedPtr<T>) -> bool {
        self.target.equivalent(unsafe { &*key.as_ptr() })
    }
}
#[derive(Default)]
pub struct IdiotHasher {
    result: Option<u64>
}
impl Hasher for IdiotHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.result.unwrap()
    }

    #[inline]
    fn write(&mut self, _bytes: &[u8]) {
        unreachable!()
    }
    #[inline]
    fn write_u64(&mut self, result: u64) {
        assert!(self.result.is_none(), "Already finished!");
        self.result = Some(result);
    }
}
