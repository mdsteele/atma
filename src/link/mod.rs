//! Facilities for linking objects files together into a binary.

mod arrange;
mod config;
mod expr;

pub use arrange::{ArrangedSection, LinkError};
pub use config::{AddrspaceConfig, LinkConfig, MemoryConfig, SectionConfig};

//===========================================================================//
