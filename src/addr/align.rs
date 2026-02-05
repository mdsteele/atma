use super::{Addr, Size};
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use static_assertions::const_assert;
use std::fmt;
use std::io;
use std::num::NonZero;

//===========================================================================//

/// Represents a memory alignment for a bus address.
///
/// Essentially, an `Align` is a [`Size`] that is guaranteed to be a power of
/// two.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Align(NonZero<u8>);

impl Align {
    /// The smallest possible alignment, 1.
    ///
    /// All addresses are always aligned at least this much.
    pub const MIN: Align = Align(NonZero::new(1).unwrap());

    /// The largest possible alignment, `1 << Addr::BITS`.
    ///
    /// The only address that is aligned to this alignment is [`Addr::MIN`].
    pub const MAX: Align = Align(NonZero::new(Addr::BITS as u8 + 1).unwrap());

    /// Returns the base-2 logarithm of the alignment.
    ///
    /// This is always exact, as `self` represents a power of two. It is also
    /// very cheap to call.
    pub fn log2(self) -> u32 {
        u32::from(self.0.get() - 1)
    }

    /// Returns a bit mask that can be used to match this alignment.
    ///
    /// That is, an address `addr` is aligned to `self` if and only if `addr &
    /// self.mask() == addr`.
    pub fn mask(self) -> Addr {
        Addr(!(((1u64 << self.log2()) - 1) as u32))
    }

    pub(crate) fn decode_from_u8(byte: u8) -> io::Result<Align> {
        if (1..=(Addr::BITS as u8 + 1)).contains(&byte) {
            Ok(Align(NonZero::new(byte).unwrap()))
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid Align byte: {:#02x}", byte),
            ))
        }
    }

    pub(crate) fn encode_as_u8(self) -> u8 {
        self.0.get()
    }
}

impl Default for Align {
    fn default() -> Align {
        Align::MIN
    }
}

impl fmt::Debug for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Align(1 << {:?})", self.0.get() - 1)
    }
}

impl fmt::Display for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Size::from(*self).fmt(f)
    }
}

impl fmt::Binary for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Size::from(*self).fmt(f)
    }
}

impl fmt::Octal for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Size::from(*self).fmt(f)
    }
}

impl fmt::LowerHex for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Size::from(*self).fmt(f)
    }
}

impl fmt::UpperHex for Align {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        Size::from(*self).fmt(f)
    }
}

impl TryFrom<u64> for Align {
    type Error = AlignTryFromError;

    fn try_from(value: u64) -> Result<Align, AlignTryFromError> {
        if value.is_power_of_two() {
            let log = value.ilog2();
            if log > Addr::BITS {
                Err(AlignTryFromError::TooLargePowerOfTwo)
            } else {
                Ok(Align(NonZero::<u8>::new(log as u8 + 1).unwrap()))
            }
        } else {
            Err(AlignTryFromError::NotAPowerOfTwo)
        }
    }
}

impl TryFrom<&BigUint> for Align {
    type Error = AlignTryFromError;

    fn try_from(value: &BigUint) -> Result<Align, AlignTryFromError> {
        match value.to_u64() {
            Some(int) => Align::try_from(int),
            None => Err(if value.count_ones() == 1 {
                AlignTryFromError::TooLargePowerOfTwo
            } else {
                AlignTryFromError::NotAPowerOfTwo
            }),
        }
    }
}

impl TryFrom<&BigInt> for Align {
    type Error = AlignTryFromError;

    fn try_from(value: &BigInt) -> Result<Align, AlignTryFromError> {
        if let Sign::Minus = value.sign() {
            Err(AlignTryFromError::NotAPowerOfTwo)
        } else {
            Align::try_from(value.magnitude())
        }
    }
}

impl TryFrom<Align> for u64 {
    type Error = ();

    // Currently this always succeeds, but that could change in the future if
    // we ever want to increase `Addr::BITS` to 64.
    fn try_from(value: Align) -> Result<u64, ()> {
        const_assert!(Addr::BITS < 64);
        Ok(1u64 << value.log2())
    }
}

impl TryFrom<Align> for usize {
    type Error = ();

    fn try_from(value: Align) -> Result<usize, ()> {
        1usize.checked_shl(value.log2()).ok_or(())
    }
}

impl From<Align> for BigUint {
    fn from(value: Align) -> BigUint {
        const_assert!(Addr::BITS < 64);
        BigUint::from(1u64 << value.log2())
    }
}

impl From<Align> for BigInt {
    fn from(value: Align) -> BigInt {
        const_assert!(Addr::BITS < 64);
        BigInt::from(1u64 << value.log2())
    }
}

//===========================================================================//

/// The error for when a conversion to [Align] fails.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AlignTryFromError {
    /// The value to be converted wasn't a power of two, and so can't be used
    /// as an alignment.
    NotAPowerOfTwo,
    /// The value to be converted was too large a power of two to fit into an
    /// [Align].
    TooLargePowerOfTwo,
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{Addr, Align};
    use std::mem::size_of;

    #[test]
    fn align_efficient_option() {
        assert_eq!(size_of::<Option<Align>>(), size_of::<Align>());
    }

    #[test]
    fn align_format() {
        let align = Align::try_from(64u64).unwrap();
        assert_eq!(format!("{:?}", align), "Align(1 << 6)");
        assert_eq!(format!("{}", align), "64");
        assert_eq!(format!("${:04x}", align), "$0040");
        assert_eq!(format!("{:#X}", align), "0x40");
        assert_eq!(format!("0{:o}", align), "0100");
        assert_eq!(format!("%{:08b}", align), "%01000000");
    }

    #[test]
    fn align_mask() {
        assert_eq!(Align::MIN.mask(), Addr::MAX);
        assert_eq!(Align::MAX.mask(), Addr::MIN);
        let align = Align::try_from(64u64).unwrap();
        assert_eq!(align.mask(), !Addr::from(63u16));
    }
}

//===========================================================================/
