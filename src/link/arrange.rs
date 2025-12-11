use super::config::SectionConfig;
use crate::obj::{Align32, ObjChunk};
use rangemap::RangeInclusiveSet;
use std::ops::RangeInclusive;
use std::rc::Rc;

//===========================================================================//

/// An error encountered during linking.
pub struct LinkError; // TODO: add fields

//===========================================================================//

/// Represents a data section after all chunks have been arranged within the
/// section.
pub struct ArrangedSection {
    /// The required alignment for this section, within its address space,
    /// after taking chunk placement restrictions into account.
    pub align: Align32,
    /// The total size of this section's data, in bytes.
    pub size: u32,
    /// The chunks in this section, along with their byte offsets relative to
    /// the starting address of the section.
    pub chunks: Rc<[(ObjChunk, u32)]>,
}

impl ArrangedSection {
    /// Given the list of chunks that belong to this section, arranges the
    /// chunks relative to the eventual starting address of the section.
    pub fn arrange_chunks(
        section: &SectionConfig,
        chunks: &[&ObjChunk],
    ) -> Result<ArrangedSection, LinkError> {
        let mut sorted = Vec::from(chunks);
        sorted.sort_by(|a, b| b.align.cmp(&a.align));
        let mut range_set = RangeInclusiveSet::<u32>::new();
        let mut arranged = Vec::<(ObjChunk, u32)>::with_capacity(chunks.len());
        let mut align = section.align;
        for chunk in sorted {
            align = align.max(chunk.align);
            if let Some(within) = chunk.within {
                align = align.max(within);
            }
            if (chunk.size as usize) < chunk.data.len() {
                return Err(LinkError);
            }
            if let Some(within) = chunk.within
                && chunk.size > u32::from(within)
            {
                return Err(LinkError);
            }
            if chunk.size == 0 {
                arranged.push((chunk.clone(), 0));
                continue;
            }
            let size_offset: u32 = chunk.size - 1;
            let start = try_place(
                &range_set,
                0..=u32::MAX,
                size_offset,
                chunk.align,
                chunk.within,
            )
            .ok_or(LinkError)?;
            arranged.push((chunk.clone(), start));
            range_set.insert(start..=(start + size_offset));
        }
        debug_assert_eq!(arranged.len(), chunks.len());

        let size = range_set.last().map(|r| r.end() + 1).unwrap_or(0);
        Ok(ArrangedSection { align, size, chunks: Rc::from(arranged) })
    }
}

//===========================================================================//

fn try_place(
    range_set: &RangeInclusiveSet<u32>,
    outer_range: RangeInclusive<u32>,
    size_offset: u32,
    align: Align32,
    opt_within: Option<Align32>,
) -> Option<u32> {
    for gap in range_set.gaps(&outer_range) {
        if let Some(within) = opt_within
            && within > align
        {
            let within = u32::from(within);
            let mut subgap_start: u32 = *gap.start();
            loop {
                let subgap_end = ((u64::from(subgap_start) + 1)
                    .next_multiple_of(u64::from(within))
                    - 1)
                .min(u64::from(*gap.end()))
                    as u32;
                let subgap = subgap_start..=subgap_end;
                let start = subgap_start.next_multiple_of(u32::from(align));
                if subgap.contains(&start) && subgap_end - start >= size_offset
                {
                    return Some(start);
                }
                if subgap_end == *gap.end() {
                    break;
                }
                subgap_start = subgap_end + 1;
            }
        } else {
            let start = gap.start().next_multiple_of(u32::from(align));
            if gap.contains(&start) && gap.end() - start >= size_offset {
                return Some(start);
            }
        }
    }
    None
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::try_place;
    use crate::obj::Align32;
    use rangemap::RangeInclusiveSet;

    #[test]
    fn place_in_empty_range_set() {
        let range_set = RangeInclusiveSet::new();
        let align = Align32::default();
        let start = try_place(&range_set, 0..=u32::MAX, 0xff, align, None);
        assert_eq!(start, Some(0));
    }

    #[test]
    fn place_full_u32_range_in_empty_range_set() {
        let range_set = RangeInclusiveSet::new();
        let align = Align32::default();
        let start = try_place(&range_set, 0..=u32::MAX, u32::MAX, align, None);
        assert_eq!(start, Some(0));
    }

    #[test]
    fn try_place_full_u32_range_in_nonempty_range_set() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(0..=0);
        let align = Align32::default();
        let start = try_place(&range_set, 0..=u32::MAX, u32::MAX, align, None);
        assert_eq!(start, None);
    }

    #[test]
    fn place_aligned() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(0x00..=0x03);
        range_set.insert(0x16..=0x18);
        range_set.insert(0x28..=0x2f);
        let align = Align32::try_from(0x10).unwrap();
        let start = try_place(&range_set, 0..=u32::MAX, 0x7, align, None);
        assert_eq!(start, Some(0x20));
    }

    #[test]
    fn place_within() {
        let mut range_set = RangeInclusiveSet::new();
        range_set.insert(0x070..=0x18f);
        range_set.insert(0x270..=0x274);
        range_set.insert(0x4a0..=0x4ff);
        let align = Align32::default();
        let within = Some(Align32::try_from(0x100).unwrap());
        let start = try_place(&range_set, 0..=u32::MAX, 0x7f, align, within);
        assert_eq!(start, Some(0x275));
    }
}

//===========================================================================//
