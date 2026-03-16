use crate::addr::Addr;
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

#[derive(Clone)]
pub(super) struct AbsoluteLabel {
    /// The name of the address space that the label exists in.
    pub space: Rc<str>,
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
