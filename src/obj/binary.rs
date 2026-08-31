use crate::addr::{Addr, Align, Offset, Size};
use crate::error::SrcSpan;
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use std::any::{Any, TypeId, type_name};
use std::collections::HashMap;
use std::hash::Hash;
use std::io;
use std::rc::Rc;

//===========================================================================//

/// Holds state for decoding object data via the [`BinaryIo`] trait.
pub struct Decoder<R> {
    reader: R,
    cache: TypeMap,
}

impl<R: io::BufRead> Decoder<R> {
    /// Constructs a new `Decoder` that will decode data from the given
    /// underlying reader.
    pub fn new(reader: R) -> Self {
        Self { reader, cache: TypeMap::new() }
    }

    /// Consumes `self` and returns the underlying reader.
    pub fn into_reader(self) -> R {
        self.reader
    }
}

//===========================================================================//

/// Holds state for encoding object data via the [`BinaryIo`] trait.
pub struct Encoder<W> {
    writer: W,
    cache: TypeMap,
}

impl<W: io::Write> Encoder<W> {
    /// Constructs a new `Encoder` that will write encoded data to the given
    /// underlying writer.
    pub fn new(writer: W) -> Self {
        Self { writer, cache: TypeMap::new() }
    }

    /// Consumes `self` and returns the underlying writer.
    pub fn into_writer(self) -> W {
        self.writer
    }
}

//===========================================================================//

/// A trait for types that can be stored in a binary object file.
pub trait BinaryIo: Sized {
    /// Reads a value of this type from the binary object file.
    fn read_from<R: io::BufRead>(decoder: &mut Decoder<R>)
    -> io::Result<Self>;

    /// Writes this value into the binary object file.
    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()>;

    /// Like `read_from`, but reads an `Option<Self>` from the binary object
    /// file instead of a `Self`.  This is used to implement the blanket
    /// `BinaryIo` implementation for `Option<T>`.
    ///
    /// The default implementation reads one byte to indicate whether the value
    /// is present; if so, it then defers to `read_from` to read the value.
    /// However, a type can override this implementation if there is a more
    /// efficient way to represent a possibly-missing value.
    fn read_option_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Option<Self>> {
        Ok(if bool::read_from(decoder)? {
            Some(Self::read_from(decoder)?)
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
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        match option {
            None => false.write_to(encoder),
            Some(value) => {
                true.write_to(encoder)?;
                value.write_to(encoder)
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
    fn read_vec_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Vec<Self>> {
        let len = usize::read_from(decoder)?;
        let mut result: Vec<Self> = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(Self::read_from(decoder)?);
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
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        slice.len().write_to(encoder)?;
        for item in slice {
            item.write_to(encoder)?;
        }
        Ok(())
    }
}

impl<T0: BinaryIo, T1: BinaryIo> BinaryIo for (T0, T1) {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let t0 = T0::read_from(decoder)?;
        let t1 = T1::read_from(decoder)?;
        Ok((t0, t1))
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.0.write_to(encoder)?;
        self.1.write_to(encoder)
    }
}

impl BinaryIo for bool {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            0 => Ok(false),
            1 => Ok(true),
            byte => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected bool, found {}", byte),
            )),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        (*self as u8).write_to(encoder)
    }
}

impl BinaryIo for u8 {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let mut byte: [u8; 1] = [0];
        decoder.reader.read_exact(&mut byte)?;
        Ok(byte[0])
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        encoder.writer.write_all(&[*self])
    }

    fn read_vec_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Vec<Self>> {
        let len = usize::read_from(decoder)?;
        let mut result: Vec<u8> = vec![0u8; len];
        decoder.reader.read_exact(&mut result)?;
        Ok(result)
    }

    fn write_slice_to<W: io::Write>(
        slice: &[Self],
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        slice.len().write_to(encoder)?;
        encoder.writer.write_all(slice)
    }
}

impl BinaryIo for u32 {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let value = BigUint::read_from(decoder)?;
        value.to_u32().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected u32, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        BigUint::from(*self).write_to(encoder)
    }
}

impl BinaryIo for usize {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let value = BigUint::read_from(decoder)?;
        value.to_usize().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected usize, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        BigUint::from(*self).write_to(encoder)
    }
}

impl BinaryIo for BigUint {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let mut data: Vec<u8> = Vec::new();
        loop {
            let buffer = decoder.reader.fill_buf()?;
            if buffer.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unterminated integer value",
                ));
            }
            for (index, &byte) in buffer.iter().enumerate() {
                data.push(byte & 0x7f);
                if byte <= 0x7f {
                    decoder.reader.consume(index + 1);
                    return Ok(BigUint::from_radix_le(&data, 0x80).unwrap());
                }
            }
            let length = buffer.len();
            decoder.reader.consume(length);
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let mut data: Vec<u8> = self.to_radix_le(0x80);
        for digit in data.iter_mut().rev().skip(1) {
            *digit |= 0x80;
        }
        encoder.writer.write_all(&data)
    }
}

impl BinaryIo for BigInt {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let mut magnitude = BigUint::read_from(decoder)?;
        let sign = if magnitude.bit(0) {
            magnitude += 1u8;
            Sign::Minus
        } else {
            Sign::Plus
        };
        magnitude >>= 1u8;
        Ok(BigInt::from_biguint(sign, magnitude))
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let mut unsigned: BigUint = self.magnitude() << 1u8;
        if self.sign() == Sign::Minus {
            unsigned -= 1u8;
        }
        unsigned.write_to(encoder)
    }
}

impl<T: BinaryIo> BinaryIo for Box<[T]> {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        Ok(Box::from(T::read_vec_from(decoder)?))
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        T::write_slice_to(self, encoder)
    }
}

impl<T: BinaryIo> BinaryIo for Option<T> {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        T::read_option_from(decoder)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        T::write_option_to(self, encoder)
    }
}

impl<T: Any + BinaryIo + Eq + Hash> BinaryIo for Rc<T> {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        read_rc_from(decoder)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        write_rc_to(self, encoder)
    }
}

impl<T: Any + BinaryIo + Eq + Hash> BinaryIo for Rc<[T]> {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        read_rc_from(decoder)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        write_rc_to(self, encoder)
    }
}

impl BinaryIo for Rc<str> {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        read_rc_from(decoder)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        write_rc_to(self, encoder)
    }
}

impl BinaryIo for String {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let len = usize::read_from(decoder)?;
        let mut bytes: Vec<u8> = vec![0; len];
        decoder.reader.read_exact(&mut bytes)?;
        String::from_utf8(bytes)
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.len().write_to(encoder)?;
        encoder.writer.write_all(self.as_bytes())
    }
}

impl<T: BinaryIo> BinaryIo for Vec<T> {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        T::read_vec_from(decoder)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        T::write_slice_to(self.as_slice(), encoder)
    }
}

impl BinaryIo for Addr {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let value = BigUint::read_from(decoder)?;
        Addr::try_from(&value).map_err(|()| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected Addr, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        BigUint::from(*self).write_to(encoder)
    }
}

impl BinaryIo for Align {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        Align::decode_from_u8(u8::read_from(decoder)?)
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.encode_as_u8().write_to(encoder)
    }

    fn read_option_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Option<Self>> {
        let byte = u8::read_from(decoder)?;
        if byte == 0 {
            Ok(None)
        } else {
            Align::decode_from_u8(byte).map(Some)
        }
    }

    fn write_option_to<W: io::Write>(
        option: &Option<Align>,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        option.map(Align::encode_as_u8).unwrap_or(0u8).write_to(encoder)
    }
}

impl BinaryIo for Offset {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let value = BigUint::read_from(decoder)?;
        Offset::try_from(&value).map_err(|()| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected Offset, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        BigUint::from(*self).write_to(encoder)
    }
}

impl BinaryIo for Size {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let value = BigUint::read_from(decoder)?;
        Size::try_from(&value).map_err(|()| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                format!("expected Size, found {}", value),
            )
        })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        BigUint::from(*self).write_to(encoder)
    }
}

impl BinaryIo for SrcSpan {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let start = usize::read_from(decoder)?;
        let size = usize::read_from(decoder)?;
        Ok(SrcSpan::from_byte_range(start..(start + size)))
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        // Most `SrcSpan`s are short, and small `usize` values can be encoded
        // in fewer bytes than large `usize` values, so we can save some
        // encoding size by encoding the start start and size of the byte range
        // rather than the start and the end.
        let byte_range = self.byte_range();
        byte_range.start.write_to(encoder)?;
        (byte_range.end - byte_range.start).write_to(encoder)?;
        Ok(())
    }
}

//===========================================================================//

/// Helper function for `BinaryIo::read_from` implementations for `Rc` types.
fn read_rc_from<T: Any + BinaryRc + ?Sized, R: io::BufRead>(
    decoder: &mut Decoder<R>,
) -> io::Result<Rc<T>> {
    let key = usize::read_from(decoder)?;
    if key == 0 {
        let value = T::read_rc_value_from(decoder)?;
        let list = decoder.cache.get_mut_or_default::<Vec<Rc<T>>>();
        list.push(value.clone());
        return Ok(value);
    }
    let index = key - 1;
    let list = decoder.cache.get_mut_or_default::<Vec<Rc<T>>>();
    if index < list.len() {
        return Ok(list[index].clone());
    }
    Err(io::Error::new(
        io::ErrorKind::InvalidData,
        format!("unknown Rc<{}> key: {key}", type_name::<T>()),
    ))
}

/// Helper function for `BinaryIo::write_to` implementations for `Rc` types.
fn write_rc_to<T: Any + BinaryRc + Eq + Hash + ?Sized, W: io::Write>(
    rc: &Rc<T>,
    encoder: &mut Encoder<W>,
) -> io::Result<()> {
    let key: usize = {
        let map = encoder.cache.get_mut_or_default::<HashMap<Rc<T>, usize>>();
        match map.get(rc) {
            None => {
                let new_key = map.len() + 1;
                map.insert(rc.clone(), new_key);
                0
            }
            Some(key) => *key,
        }
    };
    key.write_to(encoder)?;
    if key == 0 {
        T::write_rc_value_to(rc, encoder)?;
    }
    Ok(())
}

//===========================================================================//

/// Private helper trait for implementing [`BinaryIo`] for various `Rc` types.
/// Unlike `BinaryIo`, `BinaryRc` can be implemented for `?Sized` types like
/// `str` and `[T]`.
trait BinaryRc {
    /// Reads a `Self` from the stream, and puts it into a new `Rc`.
    fn read_rc_value_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Rc<Self>>;

    /// Writes the contents of an `Rc<Self>` into the stream.
    fn write_rc_value_to<W: io::Write>(
        rc: &Rc<Self>,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()>;
}

impl BinaryRc for str {
    fn read_rc_value_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Rc<Self>> {
        Ok(Rc::from(String::read_from(decoder)?))
    }

    fn write_rc_value_to<W: io::Write>(
        rc: &Rc<Self>,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        rc.len().write_to(encoder)?;
        encoder.writer.write_all(rc.as_bytes())
    }
}

impl<T: BinaryIo> BinaryRc for [T] {
    fn read_rc_value_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Rc<Self>> {
        Ok(Rc::from(T::read_vec_from(decoder)?))
    }

    fn write_rc_value_to<W: io::Write>(
        rc: &Rc<Self>,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        T::write_slice_to(rc, encoder)
    }
}

impl<T: BinaryIo> BinaryRc for T {
    fn read_rc_value_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Rc<Self>> {
        Ok(Rc::new(T::read_from(decoder)?))
    }

    fn write_rc_value_to<W: io::Write>(
        rc: &Rc<Self>,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        Rc::as_ref(rc).write_to(encoder)
    }
}

//===========================================================================//

/// A collection that stores up to one value of any given type.
struct TypeMap {
    map: HashMap<TypeId, Box<dyn Any>>,
}

impl TypeMap {
    /// Returns a new, empty `TypeMap`.
    pub fn new() -> Self {
        TypeMap { map: HashMap::new() }
    }

    /// Returns a mutable reference to the value of the specified type; if none
    /// exists yet, adds a default value to the map first.
    pub fn get_mut_or_default<T: Any + Default>(&mut self) -> &mut T {
        self.map
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(T::default()) as Box<dyn Any>)
            .downcast_mut()
            .unwrap()
    }
}

//===========================================================================//

#[cfg(test)]
pub(crate) fn assert_round_trips<T: BinaryIo + std::fmt::Debug + Eq>(
    original: T,
) {
    let mut encoder = Encoder::new(Vec::<u8>::new());
    original.write_to(&mut encoder).expect("write_to");
    let data = encoder.into_writer();
    let mut data_slice = data.as_slice();
    let mut decoder = Decoder::new(&mut data_slice);
    let parsed = T::read_from(&mut decoder).expect("read_from");
    assert_eq!(parsed, original);
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{BinaryIo, Decoder, Encoder, assert_round_trips};
    use crate::addr::{Addr, Align, Offset, Size};
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
    fn round_trip_rc() {
        let rc1 = Rc::from(123456789u32);
        let rc2 = Rc::from(987654321u32);
        assert_round_trips(rc1.clone());
        assert_round_trips(rc2.clone());
        assert_round_trips(vec![
            rc1.clone(),
            rc2.clone(),
            rc1.clone(),
            rc2.clone(),
        ]);
    }

    #[test]
    fn round_trip_rc_slice() {
        assert_round_trips(Rc::<[usize]>::from([]));
        assert_round_trips(Rc::<[usize]>::from([1usize, 2usize, 3usize]));
    }

    #[test]
    fn round_trip_rc_str() {
        assert_round_trips(Rc::<str>::from(""));
        assert_round_trips(Rc::<str>::from("foobar"));
        assert_round_trips(vec![
            Rc::<str>::from("foo"),
            Rc::<str>::from("bar"),
            Rc::<str>::from("foo"),
            Rc::<str>::from("bar"),
        ]);
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
        let mut encoder = Encoder::new(Vec::<u8>::new());
        (BigUint::from(123456789u32)).write_to(&mut encoder).unwrap();
        let biguint_binary = encoder.into_writer();

        let mut encoder = Encoder::new(Vec::<u8>::new());
        (BigUint::from(123456789usize)).write_to(&mut encoder).unwrap();
        let usize_binary = encoder.into_writer();

        assert_eq!(biguint_binary, usize_binary);
    }

    #[test]
    fn rc_backreferences() {
        let original: Vec<Rc<u8>> = vec![
            Rc::from(42u8),
            Rc::from(37u8),
            Rc::from(42u8),
            Rc::from(37u8),
            Rc::from(42u8),
        ];
        let binary = {
            let mut encoder = Encoder::new(Vec::<u8>::new());
            original.write_to(&mut encoder).unwrap();
            encoder.into_writer()
        };
        assert_eq!(binary.as_slice(), &[5, 0, 42, 0, 37, 1, 2, 1]);

        let parsed = {
            let mut binary_slice = binary.as_slice();
            let mut decoder = Decoder::new(&mut binary_slice);
            Vec::<Rc<u8>>::read_from(&mut decoder).unwrap()
        };
        assert_eq!(parsed, original);
        assert!(Rc::ptr_eq(&parsed[0], &parsed[2]));
        assert!(Rc::ptr_eq(&parsed[0], &parsed[4]));
        assert!(Rc::ptr_eq(&parsed[1], &parsed[3]));
    }
}

//===========================================================================//
