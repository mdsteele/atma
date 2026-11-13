use crate::addr::{Addr, BitWidth};
use crate::expr::ExprSymbolProvenance;
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

/// An address space that an `AbsoluteAddr` exists in.
#[derive(Clone)]
pub(super) struct AddressSpace {
    /// The name of this address space.
    pub name: Rc<str>,
    /// The bit width of this address space.
    pub bit_width: BitWidth,
}

//===========================================================================//

/// An absolute address in a specific address space.
#[derive(Clone)]
pub(super) struct AbsoluteAddr {
    /// The address space that the label exists in.
    pub space: AddressSpace,
    /// The address of the label.
    pub address: Addr,
}

impl AbsoluteAddr {
    pub(super) fn new(space: AddressSpace, address: &BigInt) -> Self {
        Self {
            space,
            // TODO: Should this wrap to the address space's bit width?
            address: Addr::wrap_bigint(address),
        }
    }

    pub(super) fn plus_offset(&self, offset: &BigInt) -> Self {
        let address = &BigInt::from(self.address) + offset;
        Self {
            space: self.space.clone(),
            // TODO: Should this wrap to the address space's bit width?
            address: Addr::wrap_bigint(&address),
        }
    }
}

//===========================================================================//

/// A fully-resolved label.
#[derive(Clone)]
pub(super) struct ResolvedLabel {
    /// The absolute address of the label.
    pub absolute: AbsoluteAddr,
    /// The symbol, plus offset, associated with this label, if any.  This will
    /// be `None` if this label has no association with any symbol (e.g. if it
    /// was created from a raw address via a unary `*` operator).
    pub provenance: Option<ExprSymbolProvenance>,
}

//===========================================================================//

#[derive(Clone)]
pub(super) struct ChunkMetadata {
    /// The the address space and starting address for the chunk.
    pub start: AbsoluteAddr,
    /// The fill byte to use for any `Fill` patches in the chunk.
    pub fill: u8,
    /// The offset into the final binary where this chunk's data should be
    /// written, or `None` if this chunk will not be included in the final
    /// binary.
    pub binary_offset: Option<u64>,
}

//===========================================================================//
