//! Facilities for simulating a processor.

mod mos6502;
mod nop;
mod sm83;

pub use mos6502::Mos6502;
pub use nop::NopProc;
pub use sm83::SharpSm83;

//===========================================================================//

/// Specifies a condition under which the simulation should be paused.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Breakpoint {
    /// Break if the processor tries execute an instruction with the given
    /// opcode.
    Opcode(u8),
    /// Break when the program counter becomes equal to the given value.
    Pc(u32),
    /// Break when the processor tries to read from the given address on its
    /// bus.
    ReadAddr(u32),
    /// Break when the processor tries to write to the given address on its
    /// bus.
    WriteAddr(u32),
}

//===========================================================================//

/// A condition that pauses or halts the simulation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SimBreak {
    /// A breakpoint was reached.
    Breakpoint(Breakpoint),
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
    fn disassemble(&self, addr: u32) -> (usize, String);

    /// Returns the current address of the program counter.
    fn pc(&self) -> u32;

    /// Sets the current address of the program counter.
    fn set_pc(&mut self, addr: u32);

    /// Returns a list of the this processor's register names.
    fn register_names(&self) -> &'static [&'static str];

    /// Returns the value of the specified register, or `None` if no such
    /// register exists.
    fn get_register(&self, name: &str) -> Option<u32>;

    /// Sets the value of the specified register.  Does nothing if no such
    /// register exists.
    fn set_register(&mut self, name: &str, value: u32);

    /// Advances this processor by one instruction.
    fn step(&mut self) -> Result<(), SimBreak>;
}

//===========================================================================//
