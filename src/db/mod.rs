//! Facilities for simulating and debugging compiled binaries.

mod env;
mod load;
mod proc;

pub use env::SimEnv;
pub use load::load_binary;
pub use proc::{Breakpoint, Mos6502, SharpSm83, SimBreak, SimProc};

//===========================================================================//
