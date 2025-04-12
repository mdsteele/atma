use crate::db::bus::SimBus;
use crate::db::proc::SimProc;

//===========================================================================//

/// A simulated MOS 6502 processor.
#[allow(dead_code)] // TODO
pub struct Mos6502 {
    pc: u16,
    reg_s: u8,
    reg_p: u8,
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    bus: Box<dyn SimBus>,
}

impl Mos6502 {
    /// Returns a new simulated MOS 6502 processor connected to the given
    /// memory bus.
    pub fn new(mut bus: Box<dyn SimBus>) -> Mos6502 {
        let reset_lo: u8 = bus.read_byte(0xfffc);
        let reset_hi: u8 = bus.read_byte(0xfffd);
        let pc = ((reset_hi as u16) << 8) | (reset_lo as u16);
        Mos6502 { pc, reg_s: 0, reg_p: 0, reg_a: 0, reg_x: 0, reg_y: 0, bus }
    }
}

impl SimProc for Mos6502 {
    fn description(&self) -> String {
        format!("MOS 6502 with {}", self.bus.description())
    }

    fn pc(&self) -> u32 {
        self.pc as u32
    }

    fn step(&mut self) {
        // TODO: implement this
    }
}

//===========================================================================//
