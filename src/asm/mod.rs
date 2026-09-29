//! Facilities for assembling source files into object files.

mod arch;
mod build;
mod check;
mod chunk;
mod env;
mod error;
mod int_data;
mod macros;
mod predef;
mod repeat;
mod str_data;

pub use build::assemble_source;
pub use error::{AsmError, AsmResult};

//===========================================================================//
