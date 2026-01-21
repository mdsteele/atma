//! Facilities for linking objects files together into a binary.

mod arrange;
mod config;
mod error;
mod expr;
mod patch;

pub use arrange::{
    ArrangedChunk, ArrangedSection, PositionedMemory, PositionedSection,
};
pub use config::{AddrspaceConfig, LinkConfig, MemoryConfig, SectionConfig};
pub use error::LinkError;
pub use patch::link_positioned_memory;

//===========================================================================//
