//! Facilities for assembling source files into object files.

mod arch;
mod build;
mod env;
mod error;
mod macros;
mod predef;
mod repeat;

pub use build::assemble_source;
pub use error::{AsmError, AsmResult};

//===========================================================================//
