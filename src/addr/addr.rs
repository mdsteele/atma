use super::{Align, Offset, Range, Size};
use num_bigint::{BigInt, BigUint};
use num_traits::ToPrimitive;
use rangemap;
use static_assertions::const_assert;
use std::fmt;
use std::ops;

//===========================================================================//

/// Represents a memory bus address.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Addr(pub(super) u32);

impl Addr {
    /// The size of this integer type in bits.
    pub const BITS: u32 = 32;

    /// The smallest address value (0).
    pub const MIN: Addr = Addr(0);

    /// The largest address value (`(1 << BITS) - 1`).
    pub const MAX: Addr = Addr(!0);

    /// Converts a `BigInt` into an `Addr`. If the value of the `BigInt` is
    /// outside the range of `Addr`, that value is wrapped.
    pub fn wrap_bigint(value: &BigInt) -> Addr {
        Addr(u32::try_from(value & BigInt::from(0xffffffffu32)).unwrap())
    }

    /// Converts a `usize` into an `Addr`. If the value of the `usize` is
    /// outside the range of `Addr`, that value is wrapped.
    pub fn wrap_usize(value: usize) -> Addr {
        Addr(value as u32)
    }

    /// Returns the lowest 8 bits of the address.
    pub fn as_u8(self) -> u8 {
        self.0 as u8
    }

    /// Returns the lowest 16 bits of the address.
    pub fn as_u16(self) -> u16 {
        self.0 as u16
    }

    /// Returns the lowest 32 bits of the address.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Converts the address into a `usize`.
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    /// Returns true if `self` is aligned to `align`.
    pub fn is_aligned_to(self, align: Align) -> bool {
        self & !align.mask() == Addr(0)
    }

    /// Calculates the lowest address greater than or equal to `self` that is
    /// aligned to `align`, or `None` if the next such address would be greater
    /// than `Addr::MAX`.
    pub fn next_aligned_to(self, align: Align) -> Option<Addr> {
        let next = u64::from(self.0).next_multiple_of(1u64 << align.log2());
        u32::try_from(next).ok().map(Addr)
    }

    /// Returns an address range that starts with `self` and contains `size`
    /// distinct addresses, or `None` if `size` is zero or if such a range
    /// would have an end address greater than `Addr::MAX`.
    pub fn range_with_size(self, size: Size) -> Option<Range> {
        if size == Size::ZERO {
            None
        } else if let Ok(end) = u32::try_from(u64::from(self.0) + size.0 - 1) {
            Some(Range { first: self, last: Addr(end) })
        } else {
            None
        }
    }

    /// Returns the largest possible address range that starts with `self` and
    /// does not cross any alignment boundary of `align`. That is, `addr &
    /// align.mask()` will be the same for every `addr` in the returned range.
    pub fn range_within(self, align: Align) -> Range {
        const_assert!(Addr::BITS < 64);
        let alignment = 1u64 << align.log2();
        let limit = (u64::from(self.0) + 1).next_multiple_of(alignment);
        debug_assert!(limit > 0);
        debug_assert!(limit - 1 <= u64::from(u32::MAX));
        Range { first: self, last: Addr((limit - 1) as u32) }
    }
}

impl From<u8> for Addr {
    fn from(value: u8) -> Addr {
        Addr(value.into())
    }
}

impl From<u16> for Addr {
    fn from(value: u16) -> Addr {
        Addr(value.into())
    }
}

impl From<u32> for Addr {
    fn from(value: u32) -> Addr {
        Addr(value)
    }
}

impl TryFrom<u64> for Addr {
    type Error = ();

    fn try_from(value: u64) -> Result<Addr, ()> {
        match u32::try_from(value) {
            Ok(int) => Ok(Addr(int)),
            Err(_) => Err(()),
        }
    }
}

impl TryFrom<usize> for Addr {
    type Error = ();

    fn try_from(value: usize) -> Result<Addr, ()> {
        match u32::try_from(value) {
            Ok(int) => Ok(Addr(int)),
            Err(_) => Err(()),
        }
    }
}

impl TryFrom<&BigUint> for Addr {
    type Error = ();

    fn try_from(value: &BigUint) -> Result<Addr, ()> {
        match value.to_u32() {
            Some(int) => Ok(Addr(int)),
            None => Err(()),
        }
    }
}

impl TryFrom<&BigInt> for Addr {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Addr, ()> {
        match value.to_u32() {
            Some(int) => Ok(Addr(int)),
            None => Err(()),
        }
    }
}

impl From<Addr> for u64 {
    fn from(value: Addr) -> u64 {
        u64::from(value.0)
    }
}

impl From<Addr> for BigUint {
    fn from(value: Addr) -> BigUint {
        BigUint::from(value.0)
    }
}

impl From<Addr> for BigInt {
    fn from(value: Addr) -> BigInt {
        BigInt::from(value.0)
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl ops::Add<Offset> for Addr {
    type Output = Addr;

    fn add(self, rhs: Offset) -> Addr {
        Addr(self.0.strict_add(rhs.0))
    }
}

impl ops::AddAssign<Offset> for Addr {
    fn add_assign(&mut self, rhs: Offset) {
        *self = *self + rhs;
    }
}

impl ops::BitAnd for Addr {
    type Output = Addr;

    fn bitand(self, rhs: Addr) -> Addr {
        Addr(self.0 & rhs.0)
    }
}

impl ops::BitAndAssign for Addr {
    fn bitand_assign(&mut self, rhs: Addr) {
        *self = *self & rhs;
    }
}

impl ops::BitOr for Addr {
    type Output = Addr;

    fn bitor(self, rhs: Addr) -> Addr {
        Addr(self.0 | rhs.0)
    }
}

impl ops::BitOrAssign for Addr {
    fn bitor_assign(&mut self, rhs: Addr) {
        *self = *self | rhs;
    }
}

impl ops::BitXor for Addr {
    type Output = Addr;

    fn bitxor(self, rhs: Addr) -> Addr {
        Addr(self.0 ^ rhs.0)
    }
}

impl ops::BitXorAssign for Addr {
    fn bitxor_assign(&mut self, rhs: Addr) {
        *self = *self ^ rhs;
    }
}

impl ops::Not for Addr {
    type Output = Addr;

    fn not(self) -> Addr {
        Addr(!self.0)
    }
}

impl ops::Shl<u32> for Addr {
    type Output = Addr;

    fn shl(self, rhs: u32) -> Addr {
        Addr(self.0.strict_shl(rhs))
    }
}

impl ops::ShlAssign<u32> for Addr {
    fn shl_assign(&mut self, rhs: u32) {
        *self = *self << rhs;
    }
}

impl ops::Shr<u32> for Addr {
    type Output = Addr;

    fn shr(self, rhs: u32) -> Addr {
        Addr(self.0.strict_shr(rhs))
    }
}

impl ops::ShrAssign<u32> for Addr {
    fn shr_assign(&mut self, rhs: u32) {
        *self = *self >> rhs;
    }
}

impl ops::Sub<Addr> for Addr {
    type Output = Offset;

    fn sub(self, rhs: Addr) -> Offset {
        Offset(self.0.strict_sub(rhs.0))
    }
}

impl rangemap::StepLite for Addr {
    fn add_one(&self) -> Addr {
        Addr(self.0.strict_add(1))
    }

    fn sub_one(&self) -> Addr {
        Addr(self.0.strict_sub(1))
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{Addr, Align, Offset};

    #[test]
    fn addr_add() {
        assert_eq!(Addr::MIN + Offset::ZERO, Addr::MIN);
        assert_eq!(Addr::MAX + Offset::ZERO, Addr::MAX);
        assert_eq!(Addr::MIN + Offset::MAX, Addr::MAX);
        assert_eq!(
            Addr::from(100u32) + Offset::from(23u32),
            Addr::from(123u32)
        );
    }

    #[test]
    fn addr_is_aligned_to() {
        assert!(Addr::MIN.is_aligned_to(Align::MAX));

        assert!(Addr::MAX.is_aligned_to(Align::MIN));
        assert!(!Addr::MAX.is_aligned_to(Align::try_from(0x2).unwrap()));

        let addr = Addr::from(0x1230u32);
        assert!(addr.is_aligned_to(Align::MIN));
        assert!(addr.is_aligned_to(Align::try_from(0x8).unwrap()));
        assert!(addr.is_aligned_to(Align::try_from(0x10).unwrap()));
        assert!(!addr.is_aligned_to(Align::try_from(0x20).unwrap()));
    }

    #[test]
    fn addr_sub() {
        assert_eq!(Addr::MIN - Addr::MIN, Offset::ZERO);
        assert_eq!(Addr::MAX - Addr::MAX, Offset::ZERO);
        assert_eq!(Addr::MAX - Addr::MIN, Offset::MAX);
        assert_eq!(
            Addr::from(123u32) - Addr::from(23u32),
            Offset::from(100u32)
        );
    }
}

//===========================================================================/
