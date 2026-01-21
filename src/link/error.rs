use crate::obj::PatchKind;
use num_bigint::BigInt;

//===========================================================================//

/// An error encountered during linking.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LinkError {
    /// A miscellaneous error.
    Misc, // TODO: remove this
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
}

//===========================================================================//
