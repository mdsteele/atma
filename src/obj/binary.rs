use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use std::io;

//===========================================================================//

/// A trait for types that can be stored in a binary object file.
pub trait BinaryIo: Sized {
    /// Reads a value of this type from the binary object file.
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self>;

    /// Writes this value into the binary object file.
    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()>;

    /// Like `read_from`, but reads an `Option<Self>` from the binary object
    /// file instead of a `Self`.  This is used to implement the blanket
    /// `BinaryIo` implementation for `Option<T>`.
    ///
    /// The default implementation reads one byte to indicate whether the value
    /// is present; if so, it then defers to `read_from` to read the value.
    /// However, a type can override this implementation if there is a more
    /// efficient way to represent a possibly-missing value.
    fn read_option_from<R: io::BufRead>(
        reader: &mut R,
    ) -> io::Result<Option<Self>> {
        Ok(if bool::read_from(reader)? {
            Some(Self::read_from(reader)?)
        } else {
            None
        })
    }

    /// Like `write_to`, but writes an `Option<Self>` to the binary object file
    /// instead of `self`.  This is used to implement the blanket `BinaryIo`
    /// implementation for `Option<T>`.
    ///
    /// The default implementation writes one byte to indicate whether the
    /// value is present; if it is, it then defers to `write_to` to encode the
    /// value afterwards.  However, a type can override this implementation if
    /// there is a more efficient way to represent a possibly-missing value.
    fn write_option_to<W: io::Write>(
        option: &Option<Self>,
        writer: &mut W,
    ) -> io::Result<()> {
        match option {
            None => false.write_to(writer),
            Some(value) => {
                true.write_to(writer)?;
                value.write_to(writer)
            }
        }
    }

    /// Like `read_from`, but reads a `Vec<Self>` from the binary object file
    /// instead of a `Self`.  This is used to implement the blanket `BinaryIo`
    /// implementation for `Vec<T>`.
    ///
    /// The default implementation first reads the length of the vector by
    /// decoding a `usize` from the stream, then calls `read_from` once for for
    /// each item in the list.  However, a type can override this
    /// implementation if there is a more efficient way to read the data.  For
    /// example, the implementation for `u8` reads the contents of the vector
    /// with a single [`io::Read::read_exact`] call, rather than one call per
    /// byte.
    fn read_vec_from<R: io::BufRead>(reader: &mut R) -> io::Result<Vec<Self>> {
        let len = usize::read_from(reader)?;
        let mut result: Vec<Self> = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(Self::read_from(reader)?);
        }
        Ok(result)
    }

    /// Like `write_to`, but writes a `&[Self]` to the binary object file
    /// instead of `self`.  This is used to implement the blanket `BinaryIo`
    /// implementation for `Vec<T>`.
    ///
    /// The default implementation first writes the length of the vector to the
    /// stream, then calls `write_to` once for for each item in the list.
    /// However, a type can override this implementation if there is a more
    /// efficient way to write the data.  For example, the implementation for
    /// `u8` writes the contents of the vector with a single
    /// [`io::Write::write_all`] call, rather than one call per byte.
    fn write_slice_to<W: io::Write>(
        slice: &[Self],
        writer: &mut W,
    ) -> io::Result<()> {
        slice.len().write_to(writer)?;
        for item in slice {
            item.write_to(writer)?;
        }
        Ok(())
    }
}

impl BinaryIo for bool {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        match u8::read_from(reader)? {
            0 => Ok(false),
            1 => Ok(true),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected bool, found {}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        (*self as u8).write_to(writer)
    }
}

impl BinaryIo for u8 {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let mut byte: [u8; 1] = [0];
        reader.read_exact(&mut byte)?;
        Ok(byte[0])
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        writer.write_all(&[*self])
    }

    fn read_vec_from<R: io::BufRead>(reader: &mut R) -> io::Result<Vec<Self>> {
        let len = usize::read_from(reader)?;
        let mut result: Vec<u8> = vec![0u8; len];
        reader.read_exact(&mut result)?;
        Ok(result)
    }

    fn write_slice_to<W: io::Write>(
        slice: &[Self],
        writer: &mut W,
    ) -> io::Result<()> {
        slice.len().write_to(writer)?;
        writer.write_all(slice)
    }
}

impl BinaryIo for usize {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let value = BigUint::read_from(reader)?;
        value.to_usize().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected usize, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        BigUint::from(*self).write_to(writer)
    }
}

impl BinaryIo for BigUint {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let mut data: Vec<u8> = Vec::new();
        loop {
            let buffer = reader.fill_buf()?;
            if buffer.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unterminated integer value",
                ));
            }
            for (index, &byte) in buffer.iter().enumerate() {
                data.push(byte & 0x7f);
                if byte <= 0x7f {
                    reader.consume(index + 1);
                    return Ok(BigUint::from_radix_le(&data, 0x80).unwrap());
                }
            }
            let length = buffer.len();
            reader.consume(length);
        }
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        let mut data: Vec<u8> = self.to_radix_le(0x80);
        for digit in data.iter_mut().rev().skip(1) {
            *digit |= 0x80;
        }
        writer.write_all(&data)
    }
}

impl BinaryIo for BigInt {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let mut magnitude = BigUint::read_from(reader)?;
        let sign = if magnitude.bit(0) {
            magnitude += 1u8;
            Sign::Minus
        } else {
            Sign::Plus
        };
        magnitude >>= 1u8;
        Ok(BigInt::from_biguint(sign, magnitude))
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        let mut unsigned: BigUint = self.magnitude() << 1u8;
        if self.sign() == Sign::Minus {
            unsigned -= 1u8;
        }
        unsigned.write_to(writer)
    }
}

impl<T: BinaryIo> BinaryIo for Option<T> {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        T::read_option_from(reader)
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        T::write_option_to(self, writer)
    }
}

impl BinaryIo for String {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let len = usize::read_from(reader)?;
        let mut bytes: Vec<u8> = vec![0; len];
        reader.read_exact(&mut bytes)?;
        String::from_utf8(bytes)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.len().write_to(writer)?;
        writer.write_all(self.as_bytes())
    }
}

impl<T: BinaryIo> BinaryIo for Vec<T> {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        T::read_vec_from(reader)
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        T::write_slice_to(self.as_slice(), writer)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::BinaryIo;
    use num_bigint::{BigInt, BigUint};
    use std::fmt::Debug;

    fn round_trip<T: BinaryIo + Debug + Eq>(original: T) {
        let mut data: Vec<u8> = Vec::new();
        original.write_to(&mut data).expect("write_to");
        let parsed = T::read_from(&mut data.as_slice()).expect("read_from");
        assert_eq!(parsed, original);
    }

    #[test]
    fn round_trip_biguint() {
        round_trip(BigUint::from(0u32));
        round_trip(BigUint::from(1u32));
        round_trip(BigUint::from(2u32));
        round_trip(BigUint::from(63u32));
        round_trip(BigUint::from(64u32));
        round_trip(BigUint::from(65u32));
        round_trip(BigUint::from(127u32));
        round_trip(BigUint::from(128u32));
        round_trip(BigUint::from(129u32));
        round_trip(BigUint::from(1_000u32));
        round_trip(BigUint::from(1_000_000u32));
        round_trip(BigUint::from(1_000_000_000u32));
    }

    #[test]
    fn round_trip_bigint() {
        round_trip(BigInt::from(0i32));
        round_trip(BigInt::from(1i32));
        round_trip(BigInt::from(-1i32));
        round_trip(BigInt::from(2i32));
        round_trip(BigInt::from(-2i32));
        round_trip(BigInt::from(63i32));
        round_trip(BigInt::from(-63i32));
        round_trip(BigInt::from(64i32));
        round_trip(BigInt::from(-64i32));
        round_trip(BigInt::from(65i32));
        round_trip(BigInt::from(-65i32));
        round_trip(BigInt::from(1_000i32));
        round_trip(BigInt::from(-1_000i32));
        round_trip(BigInt::from(1_000_000i32));
        round_trip(BigInt::from(-1_000_000i32));
        round_trip(BigInt::from(1_000_000_000i32));
        round_trip(BigInt::from(-1_000_000_000i32));
    }

    #[test]
    fn round_trip_bool() {
        round_trip(false);
        round_trip(true);
    }

    #[test]
    fn round_trip_usize() {
        round_trip(0usize);
        round_trip(1usize);
        round_trip(2usize);
        round_trip(63usize);
        round_trip(64usize);
        round_trip(65usize);
        round_trip(127usize);
        round_trip(128usize);
        round_trip(129usize);
        round_trip(1_000usize);
        round_trip(1_000_000usize);
        round_trip(1_000_000_000usize);
    }

    #[test]
    fn round_trip_option() {
        round_trip(Option::<u8>::None);
        round_trip(Some(42u8));
    }

    #[test]
    fn round_trip_string() {
        round_trip("".to_string());
        round_trip("foobar".to_string());
    }

    #[test]
    fn round_trip_vec() {
        round_trip(Vec::<u8>::new());
        round_trip(b"hello".to_vec());
    }

    #[test]
    fn consistent_integer_representation() {
        let mut biguint_binary: Vec<u8> = Vec::new();
        (BigUint::from(123456789u32)).write_to(&mut biguint_binary).unwrap();
        let mut usize_binary: Vec<u8> = Vec::new();
        (BigUint::from(123456789usize)).write_to(&mut usize_binary).unwrap();
        assert_eq!(biguint_binary, usize_binary);
    }
}

//===========================================================================//
