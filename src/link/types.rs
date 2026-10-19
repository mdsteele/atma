use crate::addr::{Addr, Align};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

/// Uniquely identifies one chunk in one of a collection of object files.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) struct ChunkId {
    /// The index of the object file that contains the chunk.
    pub object_index: usize,
    /// The index of the chunk within its object file.
    pub chunk_index: usize,
}

//===========================================================================//

/// An address space that an `AbsoluteLabel` exists in.
#[derive(Clone)]
pub(super) struct AddressSpace {
    /// The name of this address space.
    pub name: Rc<str>,
    /// The bit width of this address space.
    pub width: Align,
}

//===========================================================================//

/// A fully-resolved label address.
#[derive(Clone)]
pub(super) struct AbsoluteLabel {
    /// The address space that the label exists in.
    pub space: AddressSpace,
    /// The address of the label.
    pub address: Addr,
}

impl AbsoluteLabel {
    pub(super) fn plus_offset(&self, offset: &BigInt) -> AbsoluteLabel {
        let address = &BigInt::from(self.address) + offset;
        AbsoluteLabel {
            space: self.space.clone(),
            address: Addr::wrap_bigint(&address),
        }
    }
}

//===========================================================================//

#[derive(Clone)]
pub(super) struct ChunkMetadata {
    /// The the address space and starting address for the chunk.
    pub start: AbsoluteLabel,
    /// The fill byte to use for any `Fill` patches in the chunk.
    pub fill: u8,
    /// The offset into the final binary where this chunk's data should be
    /// written, or `None` if this chunk will not be included in the final
    /// binary.
    pub binary_offset: Option<u64>,
}

//===========================================================================//
