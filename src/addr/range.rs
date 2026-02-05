use super::{Addr, Align, Offset, Size};
use std::ops;

//===========================================================================//

/// Represents a nonempty range of bus addresses.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct Range {
    pub(super) first: Addr,
    pub(super) last: Addr,
}

impl Range {
    /// A range that covers all possible addresses.
    pub const FULL: Range = Range { first: Addr::MIN, last: Addr::MAX };

    /// Returns an address range that contains `first`, `last`, and all
    /// addresses in between.
    ///
    /// Panics if `last < first`.
    pub fn with_bounds(first: Addr, last: Addr) -> Range {
        assert!(first <= last);
        Range { first, last }
    }

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

    /// Returns true if `self` contains all addresses in `other`.
    pub fn is_superset(self, other: Range) -> bool {
        self.first <= other.first && self.last >= other.last
    }

    /// Returns true if this range crosses any alignment boundary of the
    /// specified alignment.
    pub fn crosses_alignment(self, align: Align) -> bool {
        if self.first == self.last {
            return false;
        }
        let second = self.first + Offset::from(1u32);
        if let Some(addr) = second.next_aligned_to(align)
            && addr <= self.last
        {
            return true;
        }
        false
    }

    /// Returns the first address in `self`, if any, that is aligned to
    /// `align`.
    pub fn first_aligned_to(self, align: Align) -> Option<Addr> {
        if let Some(addr) = self.first.next_aligned_to(align)
            && addr <= self.last
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
                Some(end + Offset::from(1u32))
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

#[cfg(test)]
mod tests {
    use super::{Addr, Align, Range, Size};

    #[test]
    fn range_contains() {
        assert!(Range::FULL.contains(Addr::MIN));
        assert!(Range::FULL.contains(Addr::MAX));

        let range =
            Range::with_bounds(Addr::from(0x1234u16), Addr::from(0x2345u16));
        assert!(!range.contains(Addr::from(0x1233u16)));
        assert!(range.contains(Addr::from(0x1234u16)));
        assert!(range.contains(Addr::from(0x2000u16)));
        assert!(range.contains(Addr::from(0x2345u16)));
        assert!(!range.contains(Addr::from(0x2346u16)));
    }

    #[test]
    fn range_crosses_alignment() {
        assert!(!Range::FULL.crosses_alignment(Align::MAX));
        assert!(
            !Range::with_bounds(Addr::from(0x1234u16), Addr::from(0x1234u16))
                .crosses_alignment(Align::MIN)
        );
        assert!(
            !Range::with_bounds(Addr::from(0x1230u16), Addr::from(0x123fu16))
                .crosses_alignment(Align::try_from(0x10).unwrap())
        );
        assert!(
            Range::with_bounds(Addr::from(0x1230u16), Addr::from(0x1240u16))
                .crosses_alignment(Align::try_from(0x10).unwrap())
        );
    }

    #[test]
    fn range_size() {
        assert_eq!(Range::FULL.size(), Size::MAX);

        let range =
            Range::with_bounds(Addr::from(0x1000u16), Addr::from(0x1fffu16));
        assert_eq!(range.size(), Size::from(0x1000u16));

        let size = Size::from(0x2345u16);
        let range = Addr::from(0x1234u16).range_with_size(size).unwrap();
        assert_eq!(range.size(), size);
    }
}

//===========================================================================/
