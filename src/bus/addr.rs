use num_bigint::{BigInt, BigUint, Sign};
use num_traits::ToPrimitive;
use rangemap;
use std::fmt;
use std::io;
use std::num::NonZero;
use std::ops;

//===========================================================================//

/// Represents a memory bus address.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Addr(u32);

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
    fn range_within(self, align: Align) -> Range {
        let alignment = 1u64 << align.log2();
        let limit = (u64::from(self.0) + 1).next_multiple_of(alignment);
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
        Addr((i64::from(self.0) + rhs.0) as u32)
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
        Addr(self.0 << rhs)
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
        Addr(self.0 >> rhs)
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
        Offset(i64::from(self.0) - i64::from(rhs.0))
    }
}

impl rangemap::StepLite for Addr {
    fn add_one(&self) -> Addr {
        Addr(self.0 + 1)
    }

    fn sub_one(&self) -> Addr {
        Addr(self.0 - 1)
    }
}

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
    pub const MAX: Align = Align(NonZero::new(Addr::BITS as u8 + 1).unwrap());

    /// Returns the base-2 logarithm of the alignment.
    ///
    /// This is always exact, as `self` represents a power of two.
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
        write!(f, "Align(1 << {:?})", u8::from(self.0) - 1)
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

/// Represents a positive or negative offset between bus memory addresses.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Offset(i64);

impl Offset {
    /// An offset of zero.
    pub const ZERO: Offset = Offset(0);

    const MAX_INTERNAL: i64 = Addr::MAX.0 as i64;
    const MIN_INTERNAL: i64 = -Offset::MAX_INTERNAL;

    /// The lowest negative offset, equivalent to `Addr::MIN - Addr::MAX`.
    pub const MIN: Offset = Offset(Offset::MIN_INTERNAL);

    /// The largest positive offset, equivalent to `Addr::MAX - Addr::MIN`.
    pub const MAX: Offset = Offset(Offset::MAX_INTERNAL);
}

impl From<u8> for Offset {
    fn from(value: u8) -> Offset {
        Offset(value.into())
    }
}

impl From<u16> for Offset {
    fn from(value: u16) -> Offset {
        Offset(value.into())
    }
}

impl From<u32> for Offset {
    fn from(value: u32) -> Offset {
        Offset(value.into())
    }
}

impl From<i8> for Offset {
    fn from(value: i8) -> Offset {
        Offset(value.into())
    }
}

impl From<i16> for Offset {
    fn from(value: i16) -> Offset {
        Offset(value.into())
    }
}

impl From<i32> for Offset {
    fn from(value: i32) -> Offset {
        Offset(value.into())
    }
}

impl From<Offset> for BigInt {
    fn from(value: Offset) -> BigInt {
        BigInt::from(value.0)
    }
}

impl TryFrom<i64> for Offset {
    type Error = ();

    fn try_from(value: i64) -> Result<Offset, ()> {
        if (Offset::MIN_INTERNAL..=Offset::MAX_INTERNAL).contains(&value) {
            Ok(Offset(value))
        } else {
            Err(())
        }
    }
}

impl TryFrom<usize> for Offset {
    type Error = ();

    fn try_from(value: usize) -> Result<Offset, ()> {
        Offset::try_from(value as i64)
    }
}

impl TryFrom<&BigInt> for Offset {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Offset, ()> {
        match value.to_i64() {
            Some(int) => Offset::try_from(int),
            None => Err(()),
        }
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

//===========================================================================//

/// Represents a nonempty range of bus addresses.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct Range {
    first: Addr,
    last: Addr,
}

impl Range {
    /// A range that covers all possible addresses.
    pub const FULL: Range = Range { first: Addr::MIN, last: Addr::MAX };

    /// Returns the first address in the range.
    pub fn start(self) -> Addr {
        self.first
    }

    /// Returns the last address in the range.
    pub fn end(self) -> Addr {
        self.last
    }

    /// Returns the number of distinct addresses in this range.
    pub fn size(self) -> Size {
        debug_assert!(self.first <= self.last);
        Size((self.last.0 - self.first.0) as u64 + 1)
    }

    /// Returns true if this range contains `addr`.
    pub fn contains(self, addr: Addr) -> bool {
        (self.first..=self.last).contains(&addr)
    }

    /// Returns the first address in `self`, if any, that is aligned to
    /// `align`.
    pub fn first_aligned_to(self, align: Align) -> Option<Addr> {
        if let Some(addr) = self.first.next_aligned_to(align)
            && self.contains(addr)
        {
            Some(addr)
        } else {
            None
        }
    }

    /// Returns an iterator over subranges of this range, where the subranges
    /// are split at the specified alignment boundaries.
    pub fn split_at(self, align: Align) -> Subranges {
        Subranges {
            range: self,
            split_at: align,
            next_start: Some(self.first),
        }
    }
}

impl From<Range> for ops::RangeInclusive<Addr> {
    fn from(value: Range) -> ops::RangeInclusive<Addr> {
        value.first..=value.last
    }
}

impl TryFrom<&ops::RangeInclusive<Addr>> for Range {
    type Error = ();

    fn try_from(value: &ops::RangeInclusive<Addr>) -> Result<Range, ()> {
        let first = *value.start();
        let last = *value.end();
        if first <= last { Ok(Range { first, last }) } else { Err(()) }
    }
}

//===========================================================================//

/// An iterator over subranges of an address range.
pub struct Subranges {
    range: Range,
    split_at: Align,
    next_start: Option<Addr>,
}

impl Iterator for Subranges {
    type Item = Range;

    fn next(&mut self) -> Option<Range> {
        if let Some(start) = self.next_start {
            let end =
                start.range_within(self.split_at).last.min(self.range.last);
            self.next_start = if end < self.range.last {
                Some(end + Offset::from(1))
            } else {
                None
            };
            Some(Range { first: start, last: end })
        } else {
            None
        }
    }
}

impl std::iter::FusedIterator for Subranges {}

//===========================================================================//

/// Represents the size of a region of bus memory, in bytes.
#[derive(
    Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd,
)]
pub struct Size(u64);

impl Size {
    /// A size of zero.
    pub const ZERO: Size = Size(0);

    /// The smallest size value (zero).
    pub const MIN: Size = Size(0);

    const MAX_INTERNAL: u64 = 1u64 << Addr::BITS;

    /// The largest possible size value, `1 << Addr::BITS`.
    pub const MAX: Size = Size(Size::MAX_INTERNAL);

    /// Returns the size of a range of addresses.
    pub fn of_range(range: &ops::RangeInclusive<Addr>) -> Size {
        if range.is_empty() {
            Size::ZERO
        } else {
            Size((range.end().0 - range.start().0) as u64 + 1)
        }
    }
}

impl From<Align> for Size {
    fn from(align: Align) -> Size {
        Size(1 << align.log2())
    }
}

impl From<u8> for Size {
    fn from(value: u8) -> Size {
        Size(value.into())
    }
}

impl From<u16> for Size {
    fn from(value: u16) -> Size {
        Size(value.into())
    }
}

impl From<u32> for Size {
    fn from(value: u32) -> Size {
        Size(value.into())
    }
}

impl TryFrom<u64> for Size {
    type Error = ();

    fn try_from(value: u64) -> Result<Size, ()> {
        if value <= Size::MAX_INTERNAL { Ok(Size(value)) } else { Err(()) }
    }
}

impl TryFrom<usize> for Size {
    type Error = ();

    fn try_from(value: usize) -> Result<Size, ()> {
        Size::try_from(value as u64)
    }
}

impl TryFrom<&BigUint> for Size {
    type Error = ();

    fn try_from(value: &BigUint) -> Result<Size, ()> {
        match value.to_u64() {
            Some(int) => Size::try_from(int),
            None => Err(()),
        }
    }
}

impl TryFrom<&BigInt> for Size {
    type Error = ();

    fn try_from(value: &BigInt) -> Result<Size, ()> {
        match value.to_u64() {
            Some(int) => Size::try_from(int),
            None => Err(()),
        }
    }
}

impl From<Size> for u64 {
    fn from(value: Size) -> u64 {
        value.0
    }
}

impl From<Size> for BigUint {
    fn from(value: Size) -> BigUint {
        BigUint::from(value.0)
    }
}

impl From<Size> for BigInt {
    fn from(value: Size) -> BigInt {
        BigInt::from(value.0)
    }
}

impl TryFrom<Size> for usize {
    type Error = ();

    fn try_from(value: Size) -> Result<usize, ()> {
        usize::try_from(value.0).map_err(|_| ())
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Binary for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::Octal for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::LowerHex for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl fmt::UpperHex for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{Addr, Align, Range, Size};
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

    #[test]
    fn range_contains() {
        assert!(Range::FULL.contains(Addr::MIN));
        assert!(Range::FULL.contains(Addr::MAX));

        let range =
            Range::try_from(&(Addr::from(0x1234u16)..=Addr::from(0x2345u16)))
                .unwrap();
        assert!(!range.contains(Addr::from(0x1233u16)));
        assert!(range.contains(Addr::from(0x1234u16)));
        assert!(range.contains(Addr::from(0x2000u16)));
        assert!(range.contains(Addr::from(0x2345u16)));
        assert!(!range.contains(Addr::from(0x2346u16)));
    }

    #[test]
    fn range_size() {
        assert_eq!(Range::FULL.size(), Size::MAX);

        let range =
            Range::try_from(&(Addr::from(0x1000u16)..=Addr::from(0x1fffu16)))
                .unwrap();
        assert_eq!(range.size(), Size::from(0x1000u16));

        let size = Size::from(0x2345u16);
        let range = Addr::from(0x1234u16).range_with_size(size).unwrap();
        assert_eq!(range.size(), size);
    }
}

//===========================================================================/
