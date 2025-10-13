//! Facilities for simulating a processor.

mod mos6502;
mod nop;
mod sm83;
mod spc700;
mod util;
mod w65c816;

pub use mos6502::Mos6502;
pub use nop::NopProc;
pub use sm83::SharpSm83;
pub use spc700::Spc700;
pub use w65c816::Wdc65c816;

use crate::bus::{SimBus, WatchId, WatchKind};

//===========================================================================//

/// A condition that pauses or halts the simulation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SimBreak {
    /// A watchpoint was triggered.
    Watchpoint(WatchKind, WatchId),
    /// The processor executed an instruction (with the given mnemonic and
    /// opcode) that halts the processor, and now the processor cannot continue
    /// until a reset and/or interrupt occurs.
    HaltOpcode(&'static str, u8),
}

//===========================================================================//

/// A simulated processor.
pub trait SimProc {
    /// Returns a human-readable description of this simulated processor.
    fn description(&self) -> String;

    /// Disassembles the instruction starting at the given address, returning
    /// the length of the instruction in bytes, and a human-readable string
    /// with the assembly code for that instruction.
    fn disassemble(&self, bus: &dyn SimBus, addr: u32) -> (u32, String);

    /// Returns the current address of the program counter.
    fn pc(&self) -> u32;

    /// Sets the current address of the program counter.  If the processor is
    /// currently paused in the middle of an instruction due to the last
    /// `step()` being interrupted by a breakpoint condition, that instruction
    /// will be aborted.
    fn set_pc(&mut self, addr: u32);

    /// Returns a list of the this processor's register names.
    fn register_names(&self) -> &'static [&'static str];

    /// Returns the value of the specified register, or `None` if no such
    /// register exists.
    fn get_register(&self, name: &str) -> Option<u32>;

    /// Sets the value of the specified register.  Does nothing if no such
    /// register exists.
    fn set_register(&mut self, name: &str, value: u32);

    /// Runs this processor forward until it finishes an instruction or
    /// encounters a breakpoint condition.
    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak>;

    /// Returns true if this processor is currently paused in the middle of an
    /// instruction due to the last `step()` being interrupted by a breakpoint
    /// condition.  Returns false if the processor is between instructions.
    fn is_mid_instruction(&self) -> bool;
}

//===========================================================================//
