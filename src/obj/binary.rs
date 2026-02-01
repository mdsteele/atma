use crate::bus::{Addr, Align, Offset, Size};
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use std::io;
use std::rc::Rc;

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

impl BinaryIo for u32 {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let value = BigUint::read_from(reader)?;
        value.to_u32().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected u32, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        BigUint::from(*self).write_to(writer)
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

impl<T: BinaryIo> BinaryIo for Rc<[T]> {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        Ok(Rc::from(T::read_vec_from(reader)?))
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        T::write_slice_to(self, writer)
    }
}

impl BinaryIo for Rc<str> {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        Ok(Rc::from(String::read_from(reader)?))
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.len().write_to(writer)?;
        writer.write_all(self.as_bytes())
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

impl BinaryIo for Addr {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let value = BigUint::read_from(reader)?;
        Addr::try_from(&value).map_err(|()| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected Addr, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        BigUint::from(*self).write_to(writer)
    }
}

impl BinaryIo for Align {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        Align::decode_from_u8(u8::read_from(reader)?)
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        self.encode_as_u8().write_to(writer)
    }

    fn read_option_from<R: io::BufRead>(
        reader: &mut R,
    ) -> io::Result<Option<Self>> {
        let byte = u8::read_from(reader)?;
        if byte == 0 {
            Ok(None)
        } else {
            Align::decode_from_u8(byte).map(Some)
        }
    }

    fn write_option_to<W: io::Write>(
        option: &Option<Align>,
        writer: &mut W,
    ) -> io::Result<()> {
        option.map(Align::encode_as_u8).unwrap_or(0u8).write_to(writer)
    }
}

impl BinaryIo for Offset {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let value = BigInt::read_from(reader)?;
        Offset::try_from(&value).map_err(|()| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected Offset, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        BigInt::from(*self).write_to(writer)
    }
}

impl BinaryIo for Size {
    fn read_from<R: io::BufRead>(reader: &mut R) -> io::Result<Self> {
        let value = BigUint::read_from(reader)?;
        Size::try_from(&value).map_err(|()| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected Size, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(&self, writer: &mut W) -> io::Result<()> {
        BigUint::from(*self).write_to(writer)
    }
}

//===========================================================================//

#[cfg(test)]
pub(crate) fn assert_round_trips<T: BinaryIo + std::fmt::Debug + Eq>(
    original: T,
) {
    let mut data: Vec<u8> = Vec::new();
    original.write_to(&mut data).expect("write_to");
    let parsed = T::read_from(&mut data.as_slice()).expect("read_from");
    assert_eq!(parsed, original);
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{BinaryIo, assert_round_trips};
    use crate::bus::{Addr, Align, Offset, Size};
    use num_bigint::{BigInt, BigUint};
    use std::rc::Rc;

    #[test]
    fn round_trip_align() {
        for i in 0..Addr::BITS {
            assert_round_trips(Align::try_from(1u64 << i).unwrap());
            assert_round_trips(Some(Align::try_from(1u64 << i).unwrap()));
        }
        assert_round_trips(Option::<Align>::None);
    }

    #[test]
    fn round_trip_addr() {
        assert_round_trips(Addr::MIN);
        assert_round_trips(Addr::from(0xbeu8));
        assert_round_trips(Addr::from(0xfaceu16));
        assert_round_trips(Addr::from(0x12345678u32));
        assert_round_trips(Addr::MAX);
    }

    #[test]
    fn round_trip_biguint() {
        assert_round_trips(BigUint::from(0u32));
        assert_round_trips(BigUint::from(1u32));
        assert_round_trips(BigUint::from(2u32));
        assert_round_trips(BigUint::from(63u32));
        assert_round_trips(BigUint::from(64u32));
        assert_round_trips(BigUint::from(65u32));
        assert_round_trips(BigUint::from(127u32));
        assert_round_trips(BigUint::from(128u32));
        assert_round_trips(BigUint::from(129u32));
        assert_round_trips(BigUint::from(1_000u32));
        assert_round_trips(BigUint::from(1_000_000u32));
        assert_round_trips(BigUint::from(1_000_000_000u32));
    }

    #[test]
    fn round_trip_bigint() {
        assert_round_trips(BigInt::from(0i32));
        assert_round_trips(BigInt::from(1i32));
        assert_round_trips(BigInt::from(-1i32));
        assert_round_trips(BigInt::from(2i32));
        assert_round_trips(BigInt::from(-2i32));
        assert_round_trips(BigInt::from(63i32));
        assert_round_trips(BigInt::from(-63i32));
        assert_round_trips(BigInt::from(64i32));
        assert_round_trips(BigInt::from(-64i32));
        assert_round_trips(BigInt::from(65i32));
        assert_round_trips(BigInt::from(-65i32));
        assert_round_trips(BigInt::from(1_000i32));
        assert_round_trips(BigInt::from(-1_000i32));
        assert_round_trips(BigInt::from(1_000_000i32));
        assert_round_trips(BigInt::from(-1_000_000i32));
        assert_round_trips(BigInt::from(1_000_000_000i32));
        assert_round_trips(BigInt::from(-1_000_000_000i32));
    }

    #[test]
    fn round_trip_bool() {
        assert_round_trips(false);
        assert_round_trips(true);
    }

    #[test]
    fn round_trip_u32() {
        assert_round_trips(0u32);
        assert_round_trips(1u32);
        assert_round_trips(2u32);
        assert_round_trips(63u32);
        assert_round_trips(64u32);
        assert_round_trips(65u32);
        assert_round_trips(127u32);
        assert_round_trips(128u32);
        assert_round_trips(129u32);
        assert_round_trips(1_000u32);
        assert_round_trips(1_000_000u32);
        assert_round_trips(1_000_000_000u32);
    }

    #[test]
    fn round_trip_usize() {
        assert_round_trips(0usize);
        assert_round_trips(1usize);
        assert_round_trips(2usize);
        assert_round_trips(63usize);
        assert_round_trips(64usize);
        assert_round_trips(65usize);
        assert_round_trips(127usize);
        assert_round_trips(128usize);
        assert_round_trips(129usize);
        assert_round_trips(1_000usize);
        assert_round_trips(1_000_000usize);
        assert_round_trips(1_000_000_000usize);
    }

    #[test]
    fn round_trip_offset() {
        assert_round_trips(Offset::MIN);
        assert_round_trips(Offset::from(0xbeu8));
        assert_round_trips(Offset::from(0xfaceu16));
        assert_round_trips(Offset::from(0x12345678u32));
        assert_round_trips(Offset::MAX);
    }

    #[test]
    fn round_trip_option() {
        assert_round_trips(Option::<u8>::None);
        assert_round_trips(Some(42u8));
    }

    #[test]
    fn round_trip_rc_slice() {
        assert_round_trips(Rc::<[usize]>::from(vec![]));
        assert_round_trips(Rc::<[usize]>::from(vec![1usize, 2usize, 3usize]));
    }

    #[test]
    fn round_trip_rc_str() {
        assert_round_trips(Rc::<str>::from("".to_string()));
        assert_round_trips(Rc::<str>::from("foobar".to_string()));
    }

    #[test]
    fn round_trip_size() {
        assert_round_trips(Size::MIN);
        assert_round_trips(Size::from(0xbeu8));
        assert_round_trips(Size::from(0xfaceu16));
        assert_round_trips(Size::from(0x12345678u32));
        assert_round_trips(Size::MAX);
    }

    #[test]
    fn round_trip_string() {
        assert_round_trips("".to_string());
        assert_round_trips("foobar".to_string());
    }

    #[test]
    fn round_trip_vec() {
        assert_round_trips(Vec::<u8>::new());
        assert_round_trips(b"hello".to_vec());
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
