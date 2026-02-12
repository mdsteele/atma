//! Facilities for linking objects files together into a binary.

mod arranged;
mod config;
mod error;
mod expr;
mod fragment;
mod loose;
mod patch;
mod place;
mod positioned;

pub use config::{AddrspaceConfig, LinkConfig, MemoryConfig, SectionConfig};
pub use error::LinkError;
pub use fragment::LinkFragment;

//===========================================================================//
