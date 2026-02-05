use crate::addr::Size;
use crate::obj::PatchKind;
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

/// An error encountered during linking.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LinkError {
    /// A miscellaneous error.
    Misc, // TODO: remove this
    /// The final binary size would be too large to create.
    BinaryTooLarge {
        /// The name of the memory region that would not fit in the final
        /// binary.
        region_name: Rc<str>,
    },
    /// A chunk was unable to be arranged within its section, given the
    /// constraints.
    ChunkCannotBePlaced,
    /// A chunk in an object file has a declared size smaller than its data
    /// payload.
    ChunkDataLargerThanSize {
        /// The length of the chunk's data payload.
        chunk_data_len: usize,
        /// The declared size of the chunk.
        chunk_size: Size,
    },
    /// A chunk in an object file belongs to a section that is not declared in
    /// the linker config.
    ChunkSectionDoesNotExist {
        /// The name of the nonexistent linker section to which the chunk
        /// should belong.
        section_name: Rc<str>,
    },
    /// A patch's offset/size was out of range for the size of the chunk data.
    PatchOffsetOutOfRange,
    /// A patch expression evaluated to a value that is out of range for that
    /// patch's kind.
    PatchValueOutOfRange {
        /// The kind of patch that failed.
        kind: PatchKind,
        /// The out-of-range expression value.
        value: BigInt,
    },
    /// A patch expression evaluated to a value of the wrong type.
    PatchValueWrongType,
    /// A section was unable to be positioned within its memory region, given
    /// the constraints.
    SectionCannotBePlaced,
    /// A section contained no data.
    SectionIsEmpty {
        /// The name of the empty section.
        section_name: Rc<str>,
    },
    /// Two symbols were exported with the same name.
    SymbolExportCollision {
        /// The fully qualified name shared by the symbols.
        symbol_name: Rc<str>,
    },
}

//===========================================================================//
