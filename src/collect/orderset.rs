
#[derive(Clone)]
pub struct OrderSet<T: Eq + Hash, S: BuildHasher = RandomState>(OrderMap<T, (), S>);
impl<T: Eq + Hash> OrderSet<T> {
    #[inline]
    pub fn new() -> Self {
        OrderSet(OrderMap::new())
    }
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        OrderSet(OrderMap::with_capacity(capacity))
    }
}
impl<T: Eq + Hash + Serialize, H: BuildHasher> Serialize for OrderSet<T, H> {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for item in self {
            seq.serialize_element(item)?;
        }
        seq.end()
    }
}
pub struct OrderSetVisitor<T: Eq + Hash, S: BuildHasher>(PhantomData<OrderSet<T, S>>);
impl<'de, T, S> Visitor<'de> for OrderSetVisitor<T, S> where T: Eq + Hash + Deserialize<'de>,
                                                             S: BuildHasher + Default {
    type Value = OrderSet<T, S>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("an OrderSet")
    }
    #[inline]
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where
        A: SeqAccess<'de>, {
        let mut result = OrderSet::with_capacity_and_hasher(
            seq.size_hint().unwrap_or(0), S::default());
        while let Some(element) = seq.next_element()? {
            result.insert(element);
        }
        Ok(result)
    }
}
impl<'de, T, S> Deserialize<'de> for OrderSet<T, S> where T: Eq + Hash + Deserialize<'de>,
                                                          S: BuildHasher + Default {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where
        D: Deserializer<'de> {
        deserializer.deserialize_seq(OrderSetVisitor(PhantomData))
    }
}
impl<T: Eq + Hash, S: BuildHasher> PartialEq for OrderSet<T, S> {
    #[inline]
    fn eq(&self, other: &OrderSet<T, S>) -> bool {
        self.0 == other.0
    }
}
impl<T: Eq + Hash, S: BuildHasher> Eq for OrderSet<T, S> {}
impl<T: Eq + Hash, S: BuildHasher> OrderSet<T, S> {
    #[inline]
    pub fn with_hasher(hash_builder: S) -> Self {
        OrderSet(OrderMap::with_hasher(hash_builder))
    }
    #[inline]
    pub fn with_capacity_and_hasher(capacity: usize, hash_builder: S) -> Self {
        OrderSet(OrderMap::with_capacity_and_hasher(capacity, hash_builder))
    }
    #[inline]
    pub fn insert(&mut self, value: T) -> bool {
        self.0.insert(value, ()).is_none()
    }
    #[inline]
    pub fn contains<Q: ?Sized>(&self, value: &T) -> bool where Q: Hash, T: Borrow<Q> {
        self.0.contains_key(value)
    }
    #[inline]
    pub fn iter(&self) -> ::ordermap::Keys<T, ()> {
        self.0.keys()
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
impl<T: Eq + Hash, S: BuildHasher + Default> Default for OrderSet<T, S> {
    #[inline]
    fn default() -> Self {
        OrderSet::with_hasher(S::default())
    }
}
impl<T: Eq + Hash + Debug, S: BuildHasher> Debug for OrderSet<T, S> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_set()
            .entries(self.iter())
            .finish()
    }
}
impl<'a, T: Eq + Hash + 'a, S: BuildHasher + 'a> IntoIterator for &'a OrderSet<T, S> {
    type Item = &'a T;
    type IntoIter = ::ordermap::Keys<'a, T, ()>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}