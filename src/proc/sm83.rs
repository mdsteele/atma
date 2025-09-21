use crate::bus::{SimBus, WatchKind};
use crate::dis::sm83::{
    Condition, Operation, Reg8, Reg16, decode_opcode, disassemble_instruction,
    format_instruction,
};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

const PROC_FLAG_Z: u8 = 0b1000_0000;
const PROC_FLAG_N: u8 = 0b0100_0000;
const PROC_FLAG_H: u8 = 0b0010_0000;
const PROC_FLAG_C: u8 = 0b0001_0000;

const REG_F_MASK: u8 = PROC_FLAG_Z | PROC_FLAG_N | PROC_FLAG_H | PROC_FLAG_C;

//===========================================================================//

/// Describes the state of the IME flag.
enum Ime {
    /// Interrupts are disabled.
    Disabled,
    /// Interrupts are disabled, but will become enabled after the next
    /// two instructions.
    Pending2,
    /// Interrupts are disabled, but will become enabled after the next
    /// instruction.
    Pending1,
    /// Interrupts are enabled.
    Enabled,
}

fn pack(hi: u8, lo: u8) -> u16 {
    ((hi as u16) << 8) | (lo as u16)
}

fn unpack(word: u16) -> (u8, u8) {
    ((word >> 8) as u8, (word & 0xff) as u8)
}

//===========================================================================//

/// A simulated Sharp SM83 processor.
pub struct SharpSm83 {
    pc: u16,
    sp: u16,
    reg_a: u8,
    reg_f: u8,
    reg_b: u8,
    reg_c: u8,
    reg_d: u8,
    reg_e: u8,
    reg_h: u8,
    reg_l: u8,
    ime: Ime,
}

impl Default for SharpSm83 {
    fn default() -> SharpSm83 {
        SharpSm83::new()
    }
}

impl SharpSm83 {
    /// Returns a new simulated Sharp SM83 processor.
    pub fn new() -> SharpSm83 {
        SharpSm83 {
            pc: 0,
            sp: 0xffff,
            reg_a: 0xff,
            reg_f: REG_F_MASK,
            reg_b: 0,
            reg_c: 0,
            reg_d: 0,
            reg_e: 0,
            reg_h: 0,
            reg_l: 0,
            ime: Ime::Disabled,
        }
    }

    fn op_call(
        &mut self,
        bus: &mut dyn SimBus,
        cond: Condition,
    ) -> Result<(), SimBreak> {
        let dest: u16 = self.read_immediate_word(bus);
        if self.condition_met(cond) {
            self.push_word(bus, self.pc)?;
            self.pc = dest;
        }
        Ok(())
    }

    fn op_ccf(&mut self) -> Result<(), SimBreak> {
        self.reg_f &= !(PROC_FLAG_N | PROC_FLAG_H);
        self.reg_f ^= PROC_FLAG_C;
        Ok(())
    }

    fn op_di(&mut self) -> Result<(), SimBreak> {
        self.ime = Ime::Disabled;
        Ok(())
    }

    fn op_ei(&mut self) -> Result<(), SimBreak> {
        self.ime = Ime::Pending2;
        Ok(())
    }

    fn op_invalid(&mut self, opcode: u8) -> Result<(), SimBreak> {
        // Invalid instructions hard-lock the processor until power off.
        // See https://gbdev.io/pandocs/CPU_Instruction_Set.html and
        // https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595
        self.pc = self.pc.wrapping_sub(1); // keep PC at invalid instruction
        Err(SimBreak::HaltOpcode("invalid", opcode))
    }

    fn op_jp_u16(
        &mut self,
        bus: &mut dyn SimBus,
        cond: Condition,
    ) -> Result<(), SimBreak> {
        let dest: u16 = self.read_immediate_word(bus);
        if self.condition_met(cond) {
            self.pc = dest;
        }
        Ok(())
    }

    fn op_jp_hl(&mut self) -> Result<(), SimBreak> {
        self.pc = pack(self.reg_h, self.reg_l);
        Ok(())
    }

    fn op_jr(
        &mut self,
        bus: &mut dyn SimBus,
        cond: Condition,
    ) -> Result<(), SimBreak> {
        let offset: i8 = self.read_immediate_byte(bus) as i8;
        if self.condition_met(cond) {
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        Ok(())
    }

    fn op_ld_r8_r8(
        &mut self,
        bus: &mut dyn SimBus,
        dst: Reg8,
        src: Reg8,
    ) -> Result<(), SimBreak> {
        let byte = self.get_r8(bus, src);
        self.set_r8(bus, dst, byte);
        Ok(())
    }

    fn op_ld_r8_u8(
        &mut self,
        bus: &mut dyn SimBus,
        dst: Reg8,
    ) -> Result<(), SimBreak> {
        let byte = self.read_immediate_byte(bus);
        self.set_r8(bus, dst, byte);
        Ok(())
    }

    fn op_ld_r16_u16(
        &mut self,
        bus: &mut dyn SimBus,
        dst: Reg16,
    ) -> Result<(), SimBreak> {
        let word = self.read_immediate_word(bus);
        self.set_r16(dst, word);
        Ok(())
    }

    fn op_ld_sp_hl(&mut self) -> Result<(), SimBreak> {
        self.sp = pack(self.reg_h, self.reg_l);
        Ok(())
    }

    fn op_ldh_a_mc(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let addr: u16 = pack(0xff, self.reg_c);
        self.reg_a = bus.read_byte(addr as u32);
        Ok(())
    }

    fn op_ldh_a_u8(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let lo: u8 = self.read_immediate_byte(bus);
        let addr: u16 = pack(0xff, lo);
        self.reg_a = bus.read_byte(addr as u32);
        Ok(())
    }

    fn op_ldh_mc_a(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let addr: u16 = pack(0xff, self.reg_c);
        bus.write_byte(addr as u32, self.reg_a);
        Ok(())
    }

    fn op_ldh_u8_a(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let lo: u8 = self.read_immediate_byte(bus);
        let addr: u16 = pack(0xff, lo);
        bus.write_byte(addr as u32, self.reg_a);
        Ok(())
    }

    fn op_nop(&mut self) -> Result<(), SimBreak> {
        Ok(())
    }

    fn op_pop_r16(
        &mut self,
        bus: &mut dyn SimBus,
        reg: Reg16,
    ) -> Result<(), SimBreak> {
        let word = self.pop_word(bus)?;
        self.set_r16(reg, word);
        Ok(())
    }

    fn op_push_r16(
        &mut self,
        bus: &mut dyn SimBus,
        reg: Reg16,
    ) -> Result<(), SimBreak> {
        self.push_word(bus, self.get_r16(reg))
    }

    fn op_ret(
        &mut self,
        bus: &mut dyn SimBus,
        cond: Condition,
    ) -> Result<(), SimBreak> {
        if self.condition_met(cond) {
            self.pc = self.pop_word(bus)?;
        }
        Ok(())
    }

    fn op_rst(
        &mut self,
        bus: &mut dyn SimBus,
        zp: u8,
    ) -> Result<(), SimBreak> {
        self.push_word(bus, self.pc)?;
        self.pc = u16::from(zp);
        Ok(())
    }

    fn op_scf(&mut self) -> Result<(), SimBreak> {
        self.reg_f &= !(PROC_FLAG_N | PROC_FLAG_H);
        self.reg_f |= PROC_FLAG_C;
        Ok(())
    }

    fn condition_met(&self, cond: Condition) -> bool {
        match cond {
            Condition::Always => true,
            Condition::C => (self.reg_f & PROC_FLAG_C) != 0,
            Condition::Nc => (self.reg_f & PROC_FLAG_C) == 0,
            Condition::Z => (self.reg_f & PROC_FLAG_Z) != 0,
            Condition::Nz => (self.reg_f & PROC_FLAG_Z) == 0,
        }
    }

    fn pop_word(&mut self, bus: &mut dyn SimBus) -> Result<u16, SimBreak> {
        let lo = bus.read_byte(self.sp as u32);
        self.sp = self.sp.wrapping_add(1);
        let hi = bus.read_byte(self.sp as u32);
        self.sp = self.sp.wrapping_add(1);
        Ok(pack(hi, lo))
    }

    fn push_word(
        &mut self,
        bus: &mut dyn SimBus,
        word: u16,
    ) -> Result<(), SimBreak> {
        let (hi, lo) = unpack(word);
        self.sp = self.sp.wrapping_sub(1);
        bus.write_byte(self.sp as u32, hi);
        self.sp = self.sp.wrapping_sub(1);
        bus.write_byte(self.sp as u32, lo);
        Ok(())
    }

    fn read_immediate_byte(&mut self, bus: &mut dyn SimBus) -> u8 {
        let byte = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        byte
    }

    fn read_immediate_word(&mut self, bus: &mut dyn SimBus) -> u16 {
        let lo = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        let hi = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        pack(hi, lo)
    }

    fn get_r8(&mut self, bus: &mut dyn SimBus, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.reg_a,
            Reg8::B => self.reg_b,
            Reg8::C => self.reg_c,
            Reg8::D => self.reg_d,
            Reg8::E => self.reg_e,
            Reg8::H => self.reg_h,
            Reg8::L => self.reg_l,
            Reg8::Mbc => {
                let addr = self.get_r16(Reg16::Bc);
                bus.read_byte(u32::from(addr))
            }
            Reg8::Mde => {
                let addr = self.get_r16(Reg16::De);
                bus.read_byte(u32::from(addr))
            }
            Reg8::Mhl => {
                let addr = self.get_r16(Reg16::Hl);
                bus.read_byte(u32::from(addr))
            }
            Reg8::Mhli => {
                let addr = self.get_r16(Reg16::Hl);
                self.set_r16(Reg16::Hl, addr.wrapping_add(1));
                bus.read_byte(u32::from(addr))
            }
            Reg8::Mhld => {
                let addr = self.get_r16(Reg16::Hl);
                self.set_r16(Reg16::Hl, addr.wrapping_sub(1));
                bus.read_byte(u32::from(addr))
            }
        }
    }

    fn set_r8(&mut self, bus: &mut dyn SimBus, reg: Reg8, byte: u8) {
        match reg {
            Reg8::A => self.reg_a = byte,
            Reg8::B => self.reg_b = byte,
            Reg8::C => self.reg_c = byte,
            Reg8::D => self.reg_d = byte,
            Reg8::E => self.reg_e = byte,
            Reg8::H => self.reg_h = byte,
            Reg8::L => self.reg_l = byte,
            Reg8::Mbc => {
                let addr = self.get_r16(Reg16::Bc);
                bus.write_byte(u32::from(addr), byte);
            }
            Reg8::Mde => {
                let addr = self.get_r16(Reg16::De);
                bus.write_byte(u32::from(addr), byte);
            }
            Reg8::Mhl => {
                let addr = self.get_r16(Reg16::Hl);
                bus.write_byte(u32::from(addr), byte);
            }
            Reg8::Mhli => {
                let addr = self.get_r16(Reg16::Hl);
                self.set_r16(Reg16::Hl, addr.wrapping_add(1));
                bus.write_byte(u32::from(addr), byte);
            }
            Reg8::Mhld => {
                let addr = self.get_r16(Reg16::Hl);
                self.set_r16(Reg16::Hl, addr.wrapping_sub(1));
                bus.write_byte(u32::from(addr), byte);
            }
        }
    }

    fn get_r16(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::Af => pack(self.reg_a, self.reg_f),
            Reg16::Bc => pack(self.reg_b, self.reg_c),
            Reg16::De => pack(self.reg_d, self.reg_e),
            Reg16::Hl => pack(self.reg_h, self.reg_l),
            Reg16::Sp => self.sp,
        }
    }

    fn set_r16(&mut self, reg: Reg16, word: u16) {
        match reg {
            Reg16::Af => {
                (self.reg_a, self.reg_f) = unpack(word);
                self.reg_f &= REG_F_MASK;
            }
            Reg16::Bc => (self.reg_b, self.reg_c) = unpack(word),
            Reg16::De => (self.reg_d, self.reg_e) = unpack(word),
            Reg16::Hl => (self.reg_h, self.reg_l) = unpack(word),
            Reg16::Sp => self.sp = word,
        }
    }
}

impl SimProc for SharpSm83 {
    fn description(&self) -> String {
        "Sharp SM83".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, addr: u32) -> (usize, String) {
        let (_, operation, operand) = disassemble_instruction(bus, addr);
        let instruction_size = operand.size() + 1;
        let string = format_instruction(operation, operand, addr as u16, bus);
        (instruction_size, string)
    }

    fn pc(&self) -> u32 {
        self.pc as u32
    }

    fn set_pc(&mut self, addr: u32) {
        self.pc = (addr & 0xffff) as u16;
        match self.ime {
            Ime::Pending1 | Ime::Pending2 => self.ime = Ime::Enabled,
            Ime::Disabled | Ime::Enabled => {}
        }
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["A", "F", "B", "C", "D", "E", "H", "L", "AF", "BC", "DE", "HL", "SP"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "A" => Some(u32::from(self.reg_a)),
            "F" => Some(u32::from(self.reg_f)),
            "B" => Some(u32::from(self.reg_b)),
            "C" => Some(u32::from(self.reg_c)),
            "D" => Some(u32::from(self.reg_d)),
            "E" => Some(u32::from(self.reg_e)),
            "H" => Some(u32::from(self.reg_h)),
            "L" => Some(u32::from(self.reg_l)),
            "AF" => Some(u32::from(pack(self.reg_a, self.reg_f))),
            "BC" => Some(u32::from(pack(self.reg_b, self.reg_c))),
            "DE" => Some(u32::from(pack(self.reg_d, self.reg_e))),
            "HL" => Some(u32::from(pack(self.reg_h, self.reg_l))),
            "SP" => Some(u32::from(self.sp)),
            _ => None,
        }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        match name {
            "A" => self.reg_a = (value & 0xff) as u8,
            "F" => self.reg_f = (value & u32::from(REG_F_MASK)) as u8,
            "B" => self.reg_b = (value & 0xff) as u8,
            "C" => self.reg_c = (value & 0xff) as u8,
            "D" => self.reg_d = (value & 0xff) as u8,
            "E" => self.reg_e = (value & 0xff) as u8,
            "H" => self.reg_h = (value & 0xff) as u8,
            "L" => self.reg_l = (value & 0xff) as u8,
            "AF" => {
                (self.reg_a, self.reg_f) = unpack((value & 0xffff) as u16);
                self.reg_f &= REG_F_MASK;
            }
            "BC" => (self.reg_b, self.reg_c) = unpack((value & 0xffff) as u16),
            "DE" => (self.reg_d, self.reg_e) = unpack((value & 0xffff) as u16),
            "HL" => (self.reg_h, self.reg_l) = unpack((value & 0xffff) as u16),
            "SP" => self.sp = value as u16,
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let opcode = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        match decode_opcode(opcode) {
            Operation::AdcAI8 => todo!(),
            Operation::AdcAR8(_reg) => todo!(),
            Operation::AddHlR16(_reg) => todo!(),
            Operation::AddAI8 => todo!(),
            Operation::AddAR8(_reg) => todo!(),
            Operation::AddSpI8 => todo!(),
            Operation::AndAI8 => todo!(),
            Operation::AndAR8(_reg) => todo!(),
            Operation::CallM16(cond) => self.op_call(bus, cond),
            Operation::Ccf => self.op_ccf(),
            Operation::CpAI8 => todo!(),
            Operation::CpAR8(_reg) => todo!(),
            Operation::Cpl => todo!(),
            Operation::Daa => todo!(),
            Operation::DecR16(_reg) => todo!(),
            Operation::DecR8(_reg) => todo!(),
            Operation::Di => self.op_di(),
            Operation::Ei => self.op_ei(),
            Operation::Halt => todo!(),
            Operation::IncR16(_reg) => todo!(),
            Operation::IncR8(_reg) => todo!(),
            Operation::Invalid => self.op_invalid(opcode),
            Operation::JpI16(cond) => self.op_jp_u16(bus, cond),
            Operation::JpHl => self.op_jp_hl(),
            Operation::JrI8(cond) => self.op_jr(bus, cond),
            Operation::LdAM16 => todo!(),
            Operation::LdM16A => todo!(),
            Operation::LdM16Sp => todo!(),
            Operation::LdHlSpI8 => todo!(),
            Operation::LdR16I16(reg) => self.op_ld_r16_u16(bus, reg),
            Operation::LdR8I8(reg) => self.op_ld_r8_u8(bus, reg),
            Operation::LdR8R8(r1, r2) => self.op_ld_r8_r8(bus, r1, r2),
            Operation::LdSpHl => self.op_ld_sp_hl(),
            Operation::LdhAM8 => self.op_ldh_a_u8(bus),
            Operation::LdhAMc => self.op_ldh_a_mc(bus),
            Operation::LdhM8A => self.op_ldh_u8_a(bus),
            Operation::LdhMcA => self.op_ldh_mc_a(bus),
            Operation::Nop => self.op_nop(),
            Operation::OrAI8 => todo!(),
            Operation::OrAR8(_reg) => todo!(),
            Operation::Pop(reg) => self.op_pop_r16(bus, reg),
            Operation::Prefix => todo!(),
            Operation::Push(reg) => self.op_push_r16(bus, reg),
            Operation::Ret(cond) => self.op_ret(bus, cond),
            Operation::Reti => todo!(),
            Operation::Rla => todo!(),
            Operation::Rlca => todo!(),
            Operation::Rra => todo!(),
            Operation::Rrca => todo!(),
            Operation::Rst(zp) => self.op_rst(bus, zp),
            Operation::SbcAI8 => todo!(),
            Operation::SbcAR8(_reg) => todo!(),
            Operation::Scf => self.op_scf(),
            Operation::Stop => todo!(),
            Operation::SubAI8 => todo!(),
            Operation::SubAR8(_reg) => todo!(),
            Operation::XorAI8 => todo!(),
            Operation::XorAR8(_reg) => todo!(),
        }?;
        if let Ime::Pending1 = self.ime {
            self.ime = Ime::Enabled;
        } else if let Ime::Pending2 = self.ime {
            self.ime = Ime::Pending1;
        }
        watch(bus, u32::from(self.pc), WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        false
    }
}

fn watch(
    bus: &dyn SimBus,
    addr: u32,
    kind: WatchKind,
) -> Result<(), SimBreak> {
    if let Some(id) = bus.watchpoint_at(addr, kind) {
        Err(SimBreak::Watchpoint(kind, id))
    } else {
        Ok(())
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{REG_F_MASK, SharpSm83, SimProc};

    #[test]
    fn get_registers() {
        let proc = SharpSm83::new();
        for &register in proc.register_names() {
            assert!(proc.get_register(register).is_some());
        }
    }

    #[test]
    fn set_registers() {
        let mut proc = SharpSm83::new();
        for &register in proc.register_names() {
            proc.set_register(register, 0);
            assert_eq!(proc.get_register(register), Some(0));
            let value = u32::from(REG_F_MASK);
            proc.set_register(register, value);
            assert_eq!(proc.get_register(register), Some(value));
        }
    }

    #[test]
    fn set_f_register() {
        let mut proc = SharpSm83::new();
        proc.set_register("F", 0);
        assert_eq!(proc.get_register("F"), Some(0));
        // Invalid bits should get masked off of the F register.
        proc.set_register("F", 0xff);
        assert_eq!(proc.get_register("F"), Some(u32::from(REG_F_MASK)));
    }
}

//===========================================================================//
