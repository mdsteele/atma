use super::util::watch;
use crate::bus::{SimBus, WatchKind};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

#[derive(Clone, Copy, Default)]
enum Cycle {
    #[default]
    BetweenInstructions,
    ExecOpcode,
}

//===========================================================================//

/// A simulated processor that treats every opcode as NOP.  This can be used as
/// a stub for testing.
#[derive(Default)]
pub struct NopProc {
    pc: u32,
    data: u8,
    cycle: Cycle,
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

    fn disassemble(&self, _bus: &dyn SimBus, _addr: u32) -> (usize, String) {
        (1, "NOP".to_string())
    }

    fn pc(&self) -> u32 {
        self.pc
    }

    fn set_pc(&mut self, addr: u32) {
        self.pc = addr;
        self.cycle = Cycle::BetweenInstructions;
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["DATA"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        if name == "DATA" { Some(u32::from(self.data)) } else { None }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        if name == "DATA" {
            self.data = value as u8;
        }
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        if matches!(self.cycle, Cycle::BetweenInstructions) {
            self.cycle = Cycle::ExecOpcode;
            self.data = bus.read_byte(self.pc);
            watch(bus, self.pc, WatchKind::Read)?;
        }
        debug_assert!(matches!(self.cycle, Cycle::ExecOpcode));
        self.cycle = Cycle::BetweenInstructions;
        self.pc += 1;
        watch(bus, self.pc, WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        !matches!(self.cycle, Cycle::BetweenInstructions)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{NopProc, SimBreak, SimProc};
    use crate::bus::{WatchKind, new_open_bus};

    #[test]
    fn set_pc_mid_instruction() {
        let mut bus = new_open_bus(16);
        let mut proc = NopProc::new();
        let id = bus.watch_address(0x0000, WatchKind::Read);
        assert_eq!(
            proc.step(&mut *bus),
            Err(SimBreak::Watchpoint(WatchKind::Read, id)),
        );
        assert!(proc.is_mid_instruction());
        proc.set_pc(0x0100);
        assert!(!proc.is_mid_instruction());
    }
}

//===========================================================================//
