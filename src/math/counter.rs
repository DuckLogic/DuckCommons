pub trait IdCounted: ::num_traits::PrimInt + ::num_traits::Unsigned  + Display {
    #[inline]
    fn into_u64(self) -> u64 {
        self.to_u64().unwrap()
    }
}
impl IdCounted for u64 {}
impl IdCounted for usize {}
impl IdCounted for u32 {}
impl IdCounted for u16 {}
#[derive(Default, Clone, Serialize, Deserialize)]
pub struct IdCounter<T: IdCounted = usize>(Cell<T>);
impl<T: IdCounted> IdCounter<T> {
    #[inline]
    pub fn new() -> Self {
        IdCounter(Cell::new(T::zero()))
    }
    #[inline]
    pub fn next(&self) -> T {
        self.take(T::one())
    }
    #[inline]
    pub fn take<U>(&self, amount: U) -> T where U: PrimInt + Display {
        if let Some(amount) = T::from(amount) {
            if amount >= T::one() {
                let old_id = self.0.get();
                if let Some(new_id) = old_id.checked_add(&amount) {
                    self.0.set(new_id);
                    return old_id
                }
            }
        }
        panic!("Unable to request {} ids", amount)
    }
    #[inline]
    pub fn current(&self) -> T {
        self.0.get()
    }
    #[inline]
    pub fn reset(&self) {
        self.0.set(T::zero());
    }
}