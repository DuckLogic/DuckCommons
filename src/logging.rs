use std::fmt::Debug;

use serde::Serialize;

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
