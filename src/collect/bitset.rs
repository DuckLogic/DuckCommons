use std::slice;
use std::ops::Index;
use std::iter::Enumerate;

use smallvec::{SmallVec, Array};
use std::fmt::{self, Debug};

/// A custom bitset that uses a `SmallVec` internally,
/// resulting in significant space and speed gains.
/// The set appears to be initialized to an infinite array of `default_value`,
/// and the underlying storage is only expands if an item is explicitly set to a different value.
/// The default is [u64; 4] that wastes little space, but provides capacity for 256 elements.
/// Using less could save some space, but using more could avoid spilling to a heap vector.
#[derive(Clone)]
pub struct SmallBitSet<A: Array<Item=u64> = [u64; 4]> {
    data: SmallVec<A>,
    default_value: bool
}
const LOG2_U64_BITCOUNT: u32 = 6; // 2^6 == 64

#[inline(always)]
fn determine_index(index: usize) -> (usize, u64) {
    let word_index = index >> LOG2_U64_BITCOUNT;
    let bit_index = index & 63; // Mask the low bits
    let bitmask = 1 << bit_index;
    (word_index, bitmask)
}
impl Default for SmallBitSet {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
impl SmallBitSet {
    #[inline]
    pub fn new() -> Self {
        Self::with_default_value(false)
    }
    #[inline]
    pub fn with_default_value(default_value: bool) -> Self {
        let data = SmallVec::new();
        SmallBitSet { data, default_value }
    }
}
// We don't want the A: Default bound
#[cfg_attr(feature="cargo-clippy", allow(new_without_default_derive))]
impl<A: Array<Item=u64>> SmallBitSet<A> {
    /// Return if all bits are equal to the `default_value`
    pub fn is_empty(&self) -> bool {
        let default_word = self.default_word();
        for &word in &self.data {
            if word != default_word {
                return false;
            }
        }
        true
    }
    #[inline]
    pub fn count_ones(&self) -> usize {
        let mut count = 0;
        for &word in &self.data {
            count += word.count_ones() as usize;
        }
        count
    }
    #[inline]
    pub fn count_zeros(&self) -> usize {
        let mut count = 0;
        for &word in &self.data {
            count += word.count_ones() as usize;
        }
        count
    }
    /// Reset all bits back to the `default_value`, without clearing the underlying storage
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear()
    }
    #[inline]
    pub fn get(&self, index: usize) -> bool {
        let (word_index, bitmask) = determine_index(index);
        if let Some(word) = self.data.get(word_index) {
            (word & bitmask) != 0
        } else {
            self.default_value
        }
    }
    #[inline]
    pub fn set(&mut self, index: usize, value: bool) -> bool {
        let (word_index, bitmask) = determine_index(index);
        if let Some(word) = self.data.get_mut(word_index) {
            let old_word = *word;
            *word = if value { old_word | bitmask } else { old_word & !bitmask };
            return (old_word & bitmask) != 0
        }
        if value == self.default_value {
            // No need to expand, since we're just setting to the default value
            self.default_value
        } else {
            self.expand_set(index, value)
        }
    }
    #[cold]
    #[inline(never)]
    fn expand_set(&mut self, index: usize, value: bool) -> bool {
        let (word_index, bitmask) = determine_index(index);
        let default_word = self.default_word();
        while word_index >= self.data.len() {
            self.data.push(default_word);
        }
        let word = &mut self.data[word_index];
        let old_word = *word;
        *word = if value { old_word | bitmask } else { old_word & !bitmask };
        (old_word & bitmask) != 0
    }

    #[inline(always)]
    fn default_word(&self) -> u64 {
        if self.default_value { !0 } else { 0 }
    }
    /// Enumerate over the indexes of all the bits that are set to the specified flag
    #[inline]
    pub fn enumerate(&self, target: bool) -> BitSetEnumerate {
        BitSetEnumerate {
            words: self.data.iter().enumerate(),
            target,
            current_word: None,
        }
    }
    pub fn shrink_to_fit(&mut self) {
        let default_word = self.default_word();
        while let Some(&word) = self.data.last() {
            if word == default_word {
                self.data.pop();
            } else {
                break
            }
        }
        self.data.shrink_to_fit();
    }
}
pub struct BitSetEnumerate<'a> {
    words: Enumerate<slice::Iter<'a, u64>>,
    target: bool,
    current_word: Option<(usize, u64)>
}
impl<'a> BitSetEnumerate<'a> {
    #[inline]
    fn current_word(&mut self) -> Option<(usize, u64)> {
        self.current_word.or_else(|| {
            self.words.next().map(|(word_index, &word)| {
                let word = if self.target { word } else {
                    /*
                     * When we're looking for zero bits, invert the word so we can
                     * just search for the one bits that used to be zeros.
                     * This avoids complicated special-cases for looking for zero bits,
                     * which would otherwise involve keeping track of the number of bits already seen
                     * to avoid yielding the zeros created by shifting the word left.
                     */
                    !word
                };
                self.current_word = Some((word_index * 64, word));
                (word_index, word)
            })
        })
    }
}
impl<'a> Iterator for BitSetEnumerate<'a> {
    type Item = usize;
    #[inline]
    fn next(&mut self) -> Option<usize> {
        while let Some((index, remaining_word)) = self.current_word() {
            if remaining_word != 0 {
                let leading_zeros = remaining_word.leading_zeros();
                self.current_word = Some((
                    index + (leading_zeros as usize) + 1,
                    remaining_word << (leading_zeros + 1)
                ));
                return Some(index + (leading_zeros as usize));
            } else {
                // Start looking for the next word
                self.current_word = None;
            }
        }
        None
    }
}
const STATIC_TRUE: &bool = &true;
const STATIC_FALSE: &bool = &false;
impl<A: Array<Item=u64>> Index<usize> for SmallBitSet<A> {
    type Output = bool;
    #[inline]
    fn index(&self, index: usize) -> &bool {
        if self.get(index) { STATIC_TRUE } else { STATIC_FALSE }
    }
}

impl<A: Array<Item=u64>> Debug for SmallBitSet<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set().entries(self.enumerate(true)).finish()
    }
}