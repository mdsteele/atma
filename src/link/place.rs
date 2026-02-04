use crate::bus::{Addr, Align, Range, Size};
use rangemap::RangeInclusiveSet;

//===========================================================================//

// TODO: Make `try_place` return `Result`, and introduce an error type that
// indicates why the chunk/section couldn't be placed. Then bubble that error
// up into the `LinkError`.
pub fn try_place(
    range_set: &RangeInclusiveSet<Addr>,
    outer_range: Range,
    size: Size,
    opt_start: Option<Addr>,
    align: Align,
    opt_within: Option<Align>,
) -> Option<Range> {
    debug_assert_ne!(size, Size::ZERO);
    if let Some(start) = opt_start {
        if let Some(range) = start.range_with_size(size)
            && outer_range.is_superset(range)
        {
            if !start.is_aligned_to(align) {
                // Error: `start` does not satisfy `align`.
                return None;
            }
            if let Some(_within) = opt_within {
                // TODO: check `range` against `within` restriction
            }
            if range_set.overlaps(&range.into()) {
                // Error: `range` overlaps with another chunk/section.
                return None;
            }
            return Some(range);
        }
        // Error: `start` + `size` extends past `outer_range`.
        return None;
    }
    for gap in range_set.gaps(&outer_range.into()) {
        let gap = Range::try_from(&gap).unwrap();
        if let Some(within) = opt_within
            && within > align
        {
            for subgap in gap.split_at(within) {
                if let Some(start) = subgap.first_aligned_to(align)
                    && let Some(range) = start.range_with_size(size)
                    && subgap.is_superset(range)
                {
                    return Some(range);
                }
            }
        } else if let Some(start) = gap.first_aligned_to(align)
            && let Some(range) = start.range_with_size(size)
            && gap.is_superset(range)
        {
            return Some(range);
        }
    }
    None
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::try_place;
    use crate::bus::{Addr, Align, Range, Size};
    use rangemap::RangeInclusiveSet;

    #[test]
    fn place_in_empty_range_set() {
        let range_set = RangeInclusiveSet::new();
        let size = Size::from(0x100u32);
        let align = Align::default();
        let range =
            try_place(&range_set, Range::FULL, size, None, align, None);
        let expected = Addr::MIN.range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn place_full_addr_range_in_empty_range_set() {
        let range_set = RangeInclusiveSet::new();
        let size = Size::MAX;
        let align = Align::default();
        let range =
            try_place(&range_set, Range::FULL, size, None, align, None);
        let expected = Addr::MIN.range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn try_place_full_addr_range_in_nonempty_range_set() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::MIN..=Addr::MIN);
        let size = Size::MAX;
        let align = Align::default();
        let range =
            try_place(&range_set, Range::FULL, size, None, align, None);
        assert_eq!(range, None);
    }

    #[test]
    fn place_at_start() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::from(0x00u16)..=Addr::from(0x07u16));
        range_set.insert(Addr::from(0x10u16)..=Addr::from(0x18u16));
        let size = Size::from(0x8u32);
        let start = Addr::from(0x08u16);
        let align = Align::MIN;
        let range =
            try_place(&range_set, Range::FULL, size, Some(start), align, None);
        let expected = start.range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn try_place_at_start_overflowing_outer_range() {
        let range_set = RangeInclusiveSet::new();
        let outer_range = Range::with_bounds(Addr::MIN, Addr::from(0x100u16));
        let size = Size::from(0x20u32);
        let start = Addr::from(0xf0u16);
        let align = Align::MIN;
        let range =
            try_place(&range_set, outer_range, size, Some(start), align, None);
        assert_eq!(range, None);
    }

    #[test]
    fn try_place_at_start_with_incompatible_align() {
        let range_set = RangeInclusiveSet::new();
        let size = Size::from(0x8u32);
        let start = Addr::from(0x08u16);
        let align = Align::try_from(0x10).unwrap();
        let range =
            try_place(&range_set, Range::FULL, size, Some(start), align, None);
        assert_eq!(range, None);
    }

    #[test]
    fn place_aligned() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::from(0x00u16)..=Addr::from(0x03u16));
        range_set.insert(Addr::from(0x16u16)..=Addr::from(0x18u16));
        range_set.insert(Addr::from(0x28u16)..=Addr::from(0x2fu16));
        let size = Size::from(0x7u32);
        let align = Align::try_from(0x10).unwrap();
        let range =
            try_place(&range_set, Range::FULL, size, None, align, None);
        let expected = Addr::from(0x20u16).range_with_size(size).unwrap();
        assert_eq!(range, Some(expected));
    }

    #[test]
    fn place_within() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(Addr::from(0x070u16)..=Addr::from(0x18fu16));
        range_set.insert(Addr::from(0x270u16)..=Addr::from(0x274u16));
        range_set.insert(Addr::from(0x4a0u16)..=Addr::from(0x4ffu16));
        let size = Size::from(0x7fu32);
        let align = Align::default();
        let within = Some(Align::try_from(0x100).unwrap());
        let start =
            try_place(&range_set, Range::FULL, size, None, align, within);
        let expected = Addr::from(0x275u16).range_with_size(size).unwrap();
        assert_eq!(start, Some(expected));
    }
}

//===========================================================================//
