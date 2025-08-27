use crate::proc::{SimBreak, SimProc};

//===========================================================================//

/// A simulated processor that has no registers and treats every opcode as NOP.
/// This can be used as a stub for testing.
#[derive(Default)]
pub struct NopProc {
    pc: u32,
}

impl NopProc {
    /// Returns a new `NopProc` that starts execution at address zero.
    pub fn new() -> NopProc {
        NopProc::default()
    }
}

impl SimProc for NopProc {
    fn description(&self) -> String {
        "null processor".to_string()
    }

    fn disassemble(&self, _addr: u32) -> (usize, String) {
        (1, "NOP".to_string())
    }

    fn pc(&self) -> u32 {
        self.pc
    }

    fn set_pc(&mut self, addr: u32) {
        self.pc = addr;
    }

    fn register_names(&self) -> &'static [&'static str] {
        &[]
    }

    fn get_register(&self, _name: &str) -> Option<u32> {
        None
    }

    fn set_register(&mut self, _name: &str, _value: u32) {}

    fn step(&mut self) -> Result<(), SimBreak> {
        self.pc += 1;
        Ok(())
    }
}

//===========================================================================//
