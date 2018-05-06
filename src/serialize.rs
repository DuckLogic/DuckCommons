use std::io::{Error as IoError};

/// Serialize the specified value into lz4 compressed bincode
#[inline]
#[cfg(all(feature="bincode", feature="lz4"))]
pub fn serialize_compressed_bincode<T, W>(value: &T, write: W) -> Result<(), SerializationError>
    where T: ::serde::Serialize, W: ::std::io::Write {
    let mut encoder = ::std::io::BufWriter::new(
        ::lz4::EncoderBuilder::new().build(write)?);
    ::bincode::serialize_into(&mut encoder, value, ::bincode::Infinite)?;
    Ok(())
}
/// Deserialize the specified value from lz4 compresed bincode
#[inline]
#[cfg(all(feature="bincode", feature="lz4"))]
pub fn deserialize_compressed_bincode<T, R>(source: R) -> Result<T, SerializationError>
    where T: ::serde::de::DeserializeOwned, R: ::std::io::Read {
    let mut decoder = ::std::io::BufReader::new(
        ::lz4::Decoder::new(source)?);
    Ok(::bincode::deserialize_from(&mut decoder, ::bincode::Infinite)?)
}

#[derive(Fail, Debug)]
pub enum SerializationError {
    #[fail(display = "{}", cause)]
    IoError {
        #[cause]
        cause: IoError
    },
    #[cfg(feature="bincode")]
    #[fail(display = "Invalid bincode: {}", cause)]
    Bincode {
        #[cause]
        cause: ::bincode::Error
    }
}
impl From<IoError> for SerializationError {
    #[inline]
    fn from(cause: IoError) -> SerializationError {
        SerializationError::IoError { cause }
    }
}
#[cfg(feature="bincode")]
impl From<::bincode::Error> for SerializationError {
    #[inline]
    fn from(cause: ::bincode::Error) -> SerializationError {
        SerializationError::Bincode { cause }
    }
}
