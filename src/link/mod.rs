//! Facilities for linking objects files together into a binary.

mod arranged;
mod binary;
mod config;
mod error;
mod eval;
mod loose;
mod patch;
mod place;
mod positioned;
mod types;

pub use binary::LinkedBinary;
pub use config::{
    AddrspaceConfig, ChecksumConfig, ChecksumFormat, ChecksumRange,
    ConfigAttr, ConfigEntryKind, ConfigError, ConfigResult, ConfigVariableOr,
    ExportConfig, LinkConfig, RegionConfig, SectionConfig,
};
pub use error::{LinkError, LinkResult};

//===========================================================================//
