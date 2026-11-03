use super::util::watch;
use crate::addr::Addr;
use crate::bus::{SimBus, SuperFxState, WatchKind};
use crate::dis::superfx::{Instruction, Operation, Reg};
use crate::proc::{SimBreak, SimProc};
use std::cell::RefCell;
use std::rc::Rc;

//===========================================================================//

#[derive(Clone, Copy)]
enum Microcode {
    DecodeOpcode,     // decode DATA as opcode, push new microcode
    FetchOpcode,      // DATA = next opcode (using prefetched if available)
    ReadByte,         // DATA = [ADDR], watch(ADDR, Read)
    ReadInst,         // ADDR = PBR:R15, push microcode to inc R15, ReadByte
    SetAddr(u32),     // ADDR = value
    SetPrefetched,    // prefetched_opcode = DATA
    SetReg(Reg, u16), // reg = value
    SetRegLo(Reg),    // reg = (reg & 0xff00) | DATA
    UpdatePc,         // PC16 = R15, push microcode to prefetch next opcode
    ZeroRegHi(Reg),   // reg = reg & 0x00ff
}

//===========================================================================//

/// A simulated SuperFX processor.
pub struct SuperFx {
    state: Rc<RefCell<SuperFxState>>,
    pc16: u16, // virtual lower bits of the 24-bit PC, copied from R15
    src: Reg,
    dst: Reg,
    data: u8,
    addr: u32,
    prefetched_opcode: Option<u8>,
    microcode: Vec<Microcode>,
}

impl SuperFx {
    /// Returns a new simulated SuperFX processor.
    pub fn new(state: Rc<RefCell<SuperFxState>>) -> SuperFx {
        let pc16 = state.borrow().regs[15];
        SuperFx {
            state,
            pc16,
            src: Reg::R0,
            dst: Reg::R0,
            data: 0,
            addr: 0,
            prefetched_opcode: None,
            microcode: Vec::new(),
        }
    }

    fn alt(&self) -> u8 {
        let sfr = self.state.borrow().sfr;
        let mut alt = 0u8;
        if sfr & SuperFxState::FLAG_ALT1 != 0 {
            alt |= 1;
        }
        if sfr & SuperFxState::FLAG_ALT2 != 0 {
            alt |= 2;
        }
        alt
    }

    fn clear_alt(&mut self) {
        self.state.borrow_mut().sfr &= !(SuperFxState::FLAG_ALT1
            | SuperFxState::FLAG_ALT2
            | SuperFxState::FLAG_B);
        self.src = Reg::R0;
        self.dst = Reg::R0;
    }

    fn execute_microcode(
        &mut self,
        bus: &mut dyn SimBus,
        microcode: Microcode,
    ) -> Result<(), SimBreak> {
        match microcode {
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::FetchOpcode => self.exec_fetch_opcode(bus),
            Microcode::ReadByte => self.exec_read_byte(bus),
            Microcode::ReadInst => self.exec_read_inst(bus),
            Microcode::SetAddr(addr) => self.exec_set_addr(addr),
            Microcode::SetPrefetched => self.exec_set_prefetched(),
            Microcode::SetReg(reg, value) => self.exec_set_reg(reg, value),
            Microcode::SetRegLo(reg) => self.exec_set_reg_lo(reg),
            Microcode::UpdatePc => self.exec_update_pc(),
            Microcode::ZeroRegHi(reg) => self.exec_zero_reg_hi(reg),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let opcode = self.data;
        match Operation::from_opcode(opcode, self.alt()) {
            Operation::Alt1 => {
                self.state.borrow_mut().sfr |= SuperFxState::FLAG_ALT1;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::Alt2 => {
                self.state.borrow_mut().sfr |= SuperFxState::FLAG_ALT2;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::Alt3 => {
                self.state.borrow_mut().sfr |=
                    SuperFxState::FLAG_ALT1 | SuperFxState::FLAG_ALT2;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::And(reg) => {
                let value = self.get_reg(self.src) & self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Andi(imm) => {
                let value = self.get_reg(self.src) & u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Bic(reg) => {
                let value = self.get_reg(self.src) & !self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Bici(imm) => {
                let value = self.get_reg(self.src) & !u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Div2 => {
                let source = self.get_reg(self.src);
                self.set_flag(SuperFxState::FLAG_CY, source & 0x0001 != 0);
                let value = ((source as i16) / 2) as u16;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::From(reg) => {
                // If the B flag is set (by a WITH prefix), then FROM acts as a
                // MOVES instruction (see
                // https://sneslab.net/wiki/MOVES_(Super_FX)).  Otherwise, it's
                // a prefix that sets the source register.
                if !self.get_flag(SuperFxState::FLAG_B) {
                    self.src = reg;
                    self.microcode.push(Microcode::UpdatePc);
                    return Ok(()); // don't clear_alt()
                }
                let value = self.get_reg(reg);
                self.set_flag(SuperFxState::FLAG_OV, value & 0x0080 != 0);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Hib => {
                let value = self.get_reg(self.src) >> 8;
                self.set_flag(SuperFxState::FLAG_Z, value == 0);
                self.set_flag(SuperFxState::FLAG_S, value >= 0x80);
                self.microcode.push(Microcode::SetReg(self.dst, value));
            }
            Operation::Ldb(reg) => {
                self.microcode.push(Microcode::ZeroRegHi(self.dst));
                self.microcode.push(Microcode::SetRegLo(self.dst));
                self.microcode.push(Microcode::ReadByte);
                let addr = (u32::from(0x70 | self.get_rambr()) << 16)
                    | u32::from(self.get_reg(reg));
                self.microcode.push(Microcode::SetAddr(addr));
            }
            Operation::Link(offset) => {
                let mut state = self.state.borrow_mut();
                state.regs[11] =
                    state.regs[15].wrapping_add(u16::from(offset));
            }
            Operation::Lob => {
                let value = self.get_reg(self.src) & 0x00ff;
                self.set_flag(SuperFxState::FLAG_Z, value == 0);
                self.set_flag(SuperFxState::FLAG_S, value >= 0x80);
                self.microcode.push(Microcode::SetReg(self.dst, value));
            }
            Operation::Lsr => {
                let source = self.get_reg(self.src);
                self.set_flag(SuperFxState::FLAG_CY, source & 0x0001 != 0);
                let value = source >> 1;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Merge => {
                let state = self.state.borrow();
                let value = (state.regs[7] & 0xff00) | (state.regs[8] >> 8);
                std::mem::drop(state);
                self.set_flag(SuperFxState::FLAG_S, value & 0x8080 != 0);
                self.set_flag(SuperFxState::FLAG_OV, value & 0xc0c0 != 0);
                self.set_flag(SuperFxState::FLAG_CY, value & 0xe0e0 != 0);
                self.set_flag(SuperFxState::FLAG_Z, value & 0xf0f0 != 0);
                self.microcode.push(Microcode::SetReg(self.dst, value));
            }
            Operation::Mult(reg) => {
                let lhs = (self.get_reg(self.src) as i8) as i16;
                let rhs = (self.get_reg(reg) as i8) as i16;
                let value = (lhs * rhs) as u16;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Multi(imm) => {
                let lhs = (self.get_reg(self.src) as i8) as i16;
                let rhs = i16::from(imm);
                let value = (lhs * rhs) as u16;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Nop => {}
            Operation::Not => {
                let value = !self.get_reg(self.src);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Or(reg) => {
                let value = self.get_reg(self.src) | self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Ori(imm) => {
                let value = self.get_reg(self.src) | u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Ramb => {
                self.set_rambr(self.get_reg(self.src) as u8);
            }
            Operation::Romb => {
                self.set_rombr(self.get_reg(self.src) as u8);
            }
            Operation::Sex => {
                let mut value = self.get_reg(self.src) & 0x00ff;
                if value >= 0x80 {
                    value |= 0xff00;
                }
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Stop => {
                // According to fullsnes section "SNES Cart GSU-n Code-Cache",
                // STOP doesn't empty the cache, even though it sets the GO
                // flag to zero.
                let mut state = self.state.borrow_mut();
                state.sfr &= !SuperFxState::FLAG_GO;
                state.sfr |= SuperFxState::FLAG_IRQ;
                std::mem::drop(state);
                self.clear_alt();
                return Err(SimBreak::HaltOpcode("STOP", opcode));
            }
            Operation::Swap => {
                let value = self.get_reg(self.src).rotate_right(8);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::To(reg) => {
                // If B flag is set (by a WITH prefix), then TO acts as a MOVE
                // instruction (see https://sneslab.net/wiki/MOVE_(Super_FX)).
                // Otherwise, it's a prefix that sets the destination register.
                if !self.get_flag(SuperFxState::FLAG_B) {
                    self.dst = reg;
                    self.microcode.push(Microcode::UpdatePc);
                    return Ok(()); // don't clear_alt()
                }
                let value = self.get_reg(self.src);
                // MOVE does not update the S or Z flags.
                self.microcode.push(Microcode::SetReg(reg, value));
            }
            Operation::Umult(reg) => {
                let lhs = self.get_reg(self.src) & 0x00ff;
                let rhs = self.get_reg(reg) & 0x00ff;
                let value = lhs * rhs;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Umulti(imm) => {
                let lhs = self.get_reg(self.src) & 0x00ff;
                let rhs = u16::from(imm);
                let value = lhs * rhs;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::With(reg) => {
                self.src = reg;
                self.dst = reg;
                self.state.borrow_mut().sfr |= SuperFxState::FLAG_B;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::Xor(reg) => {
                let value = self.get_reg(self.src) ^ self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Xori(imm) => {
                let value = self.get_reg(self.src) ^ u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            other => todo!("{other:?}"),
        }
        self.clear_alt();
        self.microcode.push(Microcode::UpdatePc);
        Ok(())
    }

    fn exec_fetch_opcode(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        if let Some(opcode) = self.prefetched_opcode.take() {
            self.data = opcode;
            Ok(())
        } else {
            self.exec_read_inst(bus)
        }
    }

    fn exec_read_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let addr = Addr::from(self.addr);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_read_inst(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let state = self.state.borrow();
        let next = state.regs[15].wrapping_add(1);
        self.microcode.push(Microcode::SetReg(Reg::R15, next));
        self.addr = (u32::from(state.pbr) << 16) | u32::from(state.regs[15]);
        std::mem::drop(state);
        self.exec_read_byte(bus)
    }

    fn exec_set_addr(&mut self, addr: u32) -> Result<(), SimBreak> {
        self.addr = addr;
        Ok(())
    }

    fn exec_set_prefetched(&mut self) -> Result<(), SimBreak> {
        self.prefetched_opcode = Some(self.data);
        Ok(())
    }

    fn exec_set_reg(&mut self, reg: Reg, value: u16) -> Result<(), SimBreak> {
        self.set_reg(reg, value);
        Ok(())
    }

    fn exec_set_reg_lo(&mut self, reg: Reg) -> Result<(), SimBreak> {
        let mut state = self.state.borrow_mut();
        state.regs[reg.index()] &= 0xff00;
        state.regs[reg.index()] |= self.data as u16;
        Ok(())
    }

    fn exec_update_pc(&mut self) -> Result<(), SimBreak> {
        self.pc16 = self.state.borrow().regs[15];
        // TODO: optionally skip this part if pipelining is disabled
        if true {
            self.microcode.push(Microcode::SetPrefetched);
            self.microcode.push(Microcode::ReadInst);
        }
        Ok(())
    }

    fn exec_zero_reg_hi(&mut self, reg: Reg) -> Result<(), SimBreak> {
        self.state.borrow_mut().regs[reg.index()] &= 0x00ff;
        Ok(())
    }

    fn get_flag(&self, flag: u16) -> bool {
        self.state.borrow().sfr & flag != 0
    }

    fn set_flag(&mut self, flag: u16, value: bool) {
        let mut state = self.state.borrow_mut();
        if value {
            state.sfr |= flag;
        } else {
            state.sfr &= !flag;
        }
    }

    fn update_sz_flags_16(&mut self, value: u16) {
        self.set_flag(SuperFxState::FLAG_Z, value == 0);
        self.set_flag(SuperFxState::FLAG_S, value >= 0x8000);
    }

    fn get_reg(&self, reg: Reg) -> u16 {
        self.state.borrow().regs[reg.index()]
    }

    fn set_reg(&mut self, reg: Reg, value: u16) {
        self.state.borrow_mut().regs[reg.index()] = value;
    }

    fn get_rambr(&self) -> u8 {
        self.state.borrow().rambr
    }

    fn set_rambr(&mut self, value: u8) {
        // RAMBR is only 1 bit wide.  Accordingly, fullsnes section "SNES Cart
        // GSU-n CPU MOV Opcodes", the RAMB instruction sets RAMBR to the
        // lowest bit of the source register (note that `RAMBR_MASK` is 0x01),
        // ignoring all other bits.
        //
        // By constrast, SNES Book II page 2-9-101 is unclear on this point,
        // describing the RAMB instruction as setting the RAMBR to the low
        // *byte* of the source register, and giving an example of setting it
        // to 0x70 (which is the actual bank used when RAMBR is zero).  That
        // page also says that RAMBR's initial value is invalid, suggesting
        // that perhaps it is important to use a valid bank byte here.  But the
        // Star Fox ROM apparently executes RAMB with the source register equal
        // to zero while starting up, so it looks like there's no need to use a
        // value of 0x70.
        self.state.borrow_mut().rambr = value & SuperFxState::RAMBR_MASK;
    }

    fn get_rombr(&self) -> u8 {
        self.state.borrow().rombr
    }

    fn set_rombr(&mut self, value: u8) {
        self.state.borrow_mut().rombr = value;
    }

    fn update_sz_flags_and_push_set_dst_reg(&mut self, value: u16) {
        self.update_sz_flags_16(value);
        self.microcode.push(Microcode::SetReg(self.dst, value));
    }
}

impl SimProc for SuperFx {
    fn description(&self) -> String {
        "SuperFX".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, pc: Addr) -> (u32, String) {
        let pc = pc.as_u32();
        let instruction = Instruction::decode(bus, pc, self.alt());
        (instruction.size(), instruction.format(bus, pc, self.get_rambr()))
    }

    fn pc(&self) -> Addr {
        let state = self.state.borrow();
        (Addr::from(state.pbr) << 16) | Addr::from(state.regs[15])
    }

    fn set_pc(&mut self, addr: Addr) {
        let mut state = self.state.borrow_mut();
        state.regs[15] = addr.as_u16();
        state.pbr = (addr >> 16).as_u8();
        self.prefetched_opcode = None;
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &[
            "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
            "R11", "R12", "R13", "R14", "R15", "CBR", "RAMBR", "ROMBR", "SFR",
            "DATA",
        ]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "R0" => Some(u32::from(self.get_reg(Reg::R0))),
            "R1" => Some(u32::from(self.get_reg(Reg::R1))),
            "R2" => Some(u32::from(self.get_reg(Reg::R2))),
            "R3" => Some(u32::from(self.get_reg(Reg::R3))),
            "R4" => Some(u32::from(self.get_reg(Reg::R4))),
            "R5" => Some(u32::from(self.get_reg(Reg::R5))),
            "R6" => Some(u32::from(self.get_reg(Reg::R6))),
            "R7" => Some(u32::from(self.get_reg(Reg::R7))),
            "R8" => Some(u32::from(self.get_reg(Reg::R8))),
            "R9" => Some(u32::from(self.get_reg(Reg::R9))),
            "R10" => Some(u32::from(self.get_reg(Reg::R10))),
            "R11" => Some(u32::from(self.get_reg(Reg::R11))),
            "R12" => Some(u32::from(self.get_reg(Reg::R12))),
            "R13" => Some(u32::from(self.get_reg(Reg::R13))),
            "R14" => Some(u32::from(self.get_reg(Reg::R14))),
            "R15" => Some(u32::from(self.get_reg(Reg::R15))),
            "CBR" => Some(u32::from(self.state.borrow().cbr)),
            "RAMBR" => Some(u32::from(self.get_rambr())),
            "ROMBR" => Some(u32::from(self.get_rombr())),
            "SFR" => Some(u32::from(self.state.borrow().sfr)),
            "DATA" => Some(u32::from(self.data)),
            _ => None,
        }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        match name {
            "R0" => self.set_reg(Reg::R0, value as u16),
            "R1" => self.set_reg(Reg::R1, value as u16),
            "R2" => self.set_reg(Reg::R2, value as u16),
            "R3" => self.set_reg(Reg::R3, value as u16),
            "R4" => self.set_reg(Reg::R4, value as u16),
            "R5" => self.set_reg(Reg::R5, value as u16),
            "R6" => self.set_reg(Reg::R6, value as u16),
            "R7" => self.set_reg(Reg::R7, value as u16),
            "R8" => self.set_reg(Reg::R8, value as u16),
            "R9" => self.set_reg(Reg::R9, value as u16),
            "R10" => self.set_reg(Reg::R10, value as u16),
            "R11" => self.set_reg(Reg::R11, value as u16),
            "R12" => self.set_reg(Reg::R12, value as u16),
            "R13" => self.set_reg(Reg::R13, value as u16),
            "R14" => self.set_reg(Reg::R14, value as u16),
            "R15" => self.set_reg(Reg::R15, value as u16),
            "CBR" => {
                self.state.borrow_mut().cbr =
                    (value as u16) & SuperFxState::CBR_MASK
            }
            "RAMBR" => self.set_rambr(value as u8),
            "ROMBR" => self.set_rombr(value as u8),
            "SFR" => {
                let mut state = self.state.borrow_mut();
                state.sfr = (value as u16) & SuperFxState::SFR_MASK;
                // According to fullsnes section "SNES Cart GSU-n Code-Cache",
                // setting the GO flag to zero forces the CBR to zero and marks
                // all cache blocks as empty.
                if state.sfr & SuperFxState::FLAG_GO == 0 {
                    state.cbr = 0;
                    state.cache_flags = 0;
                }
            }
            "DATA" => self.data = value as u8,
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        if self.microcode.is_empty() {
            self.microcode.push(Microcode::DecodeOpcode);
            self.microcode.push(Microcode::FetchOpcode);
        }
        while let Some(microcode) = self.microcode.pop() {
            self.execute_microcode(bus, microcode)?;
        }
        watch(bus, self.pc(), WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        !self.microcode.is_empty()
    }
}

//===========================================================================//
