mod mos6502;

pub use mos6502::Mos6502;

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

    /// Returns the current address of the program counter.
    fn pc(&self) -> u32;

    /// Advances this processor by one instruction.
    fn step(&mut self) -> Result<(), SimBreak>;
}

//===========================================================================//
