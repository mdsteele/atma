use super::align::Align32;
use std::rc::Rc;

//===========================================================================//

/// Represents one data chunk of an object file.
#[derive(Clone)]
pub struct ObjectChunk {
    /// The name of the linker section to which this chunk belongs.
    pub section_name: Rc<str>,
    /// Static data (before rewrite rules are applied) at the start of this
    /// chunk.
    pub data: Rc<[u8]>,
    /// The size of the chunk, in bytes.  This may be greater than `data.len()`
    /// if the chunk requires additional padding after the data.  This should
    /// not be less than `data.len()`, or else the linker will return an error
    /// when trying to place this chunk.
    pub size: u32,
    /// The required alignment for this chunk's data, within its address space.
    pub align: Align32,
    /// If set, then this entire chunk (data + padding) must not cross any
    /// alignment boundary of this size within its address space.
    pub within: Option<Align32>,
}

//===========================================================================//
