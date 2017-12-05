use std::io::{Error as IoError};
use ::{AutoError};

/// Serialize the specified value into lz4 compressed bincode
#[inline]
#[cfg(all(feature="bincode", feature="lz4)"))]
pub fn serialize_compressed_bincode<T: Serialize, W: Write>(value: &T, write: W) -> Result<(), SerializationError> {
    use std::io::{BufReader, BufWriter};
    let mut encoder = BufWriter::new(::lz4::encoder::EncoderBuilder::new().build(write)?);
    ::bincode::serialize_into(&mut encoder, value, ::bincode::Infinite)?;
    Ok(())
}
/// Deserialize the specified value from lz4 compresed bincode
#[inline]
#[cfg(all(feature="bincode", feature="lz4)"))]
pub fn deserialize_compressed_bincode<T: DeserializeOwned, R: Read>(source: R) -> Result<T, SerializationError> {
    use std::io::{BufReader, BufWriter};
    let mut decoder = BufReader::new(::lz4::decoder::Decoder::new(source)?);
    Ok(::bincode::deserialize_from(&mut decoder, ::bincode::Infinite)?)
}

#[derive(AutoError, Debug)]
pub enum SerializationError {
    #[error(description("IOError"), display("{cause}"))]
    IoError {
        cause: IoError
    },
    #[cfg(feature="bincode")]
    #[error(description("Invalid bincode"), display("Invalid bincode: {cause}"))]
    Bincode {
        cause: ::bincode::Error
    }
}
