use std::fmt::{self, Debug, Formatter};

use serde::{Serialize, Serializer};
use serde_derive::*;


/// A `slog::Value` value which serializes an iterator as a serde sequence.
#[derive(Copy, Clone)]
pub struct IterValue<I: IntoIterator + Clone>(pub I)
    where I::Item: Serialize + Debug;
impl<I: IntoIterator + Clone> ::slog::Value for IterValue<I>
    where I::Item: Serialize + Debug {
    #[inline]
    fn serialize(
        &self,
        _record: &::slog::Record,
        key: ::slog::Key,
        serializer: &mut ::slog::Serializer
    ) -> ::slog::Result {
        serializer.emit_serde(key, self)
    }
}
impl<I: IntoIterator + Clone> ::slog::SerdeValue for IterValue<I>
    where I::Item: Serialize + Debug {
    #[inline]
    fn serialize_fallback(&self, key: ::slog::Key, serializer: &mut ::slog::Serializer) -> ::slog::Result {
        serializer.emit_arguments(key, &format_args!("{:?}", self))
    }

    #[inline]
    fn as_serde(&self) -> &::erased_serde::Serialize {
        self
    }

    #[inline]
    fn to_sendable(&self) -> Box<::slog::SerdeValue + Send> {
        box SerializeValue(::serde_json::to_value(self).unwrap())
    }
}
impl<I: IntoIterator + Clone> Serialize for IterValue<I>
    where I::Item: Serialize + Debug {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
        S: Serializer {
        self.0.clone().into_iter()
            .collect::<Vec<I::Item>>()
            .serialize(serializer)
    }
}
impl<I: IntoIterator + Clone> Debug for IterValue<I>
    where I::Item: Serialize + Debug {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_list().entries(self.0.clone()).finish()
    }
}

/// A `slog::Value` value which serializes its contents using serde.
///
/// This allows for easier parsing by debug scripts
#[derive(Copy, Clone, Serialize)]
pub struct SerializeValue<T: Debug + Serialize>(pub T);

impl<T: Debug + Serialize> ::slog::Value for SerializeValue<T> {
    #[inline]
    fn serialize(&self, _record: &::slog::Record, key: ::slog::Key, serializer: &mut ::slog::Serializer) -> ::slog::Result {
        serializer.emit_serde(key, self)
    }
}
impl<T: Debug + Serialize> ::slog::SerdeValue for SerializeValue<T> {
    fn serialize_fallback(&self, key: ::slog::Key, serializer: &mut ::slog::Serializer) -> ::slog::Result {
        serializer.emit_str(key, &format!("{:?}", self.0))
    }

    #[inline]
    fn as_serde(&self) -> &::erased_serde::Serialize {
        self
    }

    fn to_sendable(&self) -> Box<::slog::SerdeValue + Send + 'static> {
        box SerializeValue(::serde_json::to_value(&self.0).unwrap())
    }
}
