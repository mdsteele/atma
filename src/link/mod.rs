//! Facilities for linking objects files together into a binary.

mod arrange;
mod config;
mod expr;

pub use arrange::{
    ArrangedChunk, ArrangedSection, LinkError, PositionedMemory,
    PositionedSection,
};
pub use config::{AddrspaceConfig, LinkConfig, MemoryConfig, SectionConfig};

//===========================================================================//
