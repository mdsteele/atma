use crate::bus::{SimBus, WatchKind};
use crate::dis::sm83::{disassemble_instruction, format_instruction};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

const PROC_FLAG_Z: u8 = 0b1000_0000;
const PROC_FLAG_N: u8 = 0b0100_0000;
const PROC_FLAG_H: u8 = 0b0010_0000;
const PROC_FLAG_C: u8 = 0b0001_0000;

const REG_F_MASK: u8 = PROC_FLAG_Z | PROC_FLAG_N | PROC_FLAG_H | PROC_FLAG_C;

//===========================================================================//

/// A condition under which a conditional instruction will be executed.
enum Cond {
    Always,
    C,
    Nc,
    Z,
    Nz,
}

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

/// A general-purpose 8-bit register (for an SM83 processor).
enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Mhl,
}

/// A general-purpose 16-bit register (for an SM83 processor).
enum Reg16 {
    Bc,
    De,
    Hl,
    Sp,
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
        cond: Cond,
    ) -> Result<(), SimBreak> {
        let dest: u16 = self.read_immediate_word(bus);
        if self.condition_met(cond) {
            self.push_word(bus, self.pc)?;
            self.pc = dest;
        }
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
        cond: Cond,
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
        cond: Cond,
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

    fn op_pop_af(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        (self.reg_a, self.reg_f) = unpack(self.pop_word(bus)?);
        self.reg_f &= REG_F_MASK;
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

    fn op_push_af(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.push_word(bus, pack(self.reg_a, self.reg_f))
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
        cond: Cond,
    ) -> Result<(), SimBreak> {
        if self.condition_met(cond) {
            self.pc = self.pop_word(bus)?;
        }
        Ok(())
    }

    fn op_rst(
        &mut self,
        bus: &mut dyn SimBus,
        dest: u16,
    ) -> Result<(), SimBreak> {
        self.push_word(bus, self.pc)?;
        self.pc = dest;
        Ok(())
    }

    fn condition_met(&self, cond: Cond) -> bool {
        match cond {
            Cond::Always => true,
            Cond::C => (self.reg_f & PROC_FLAG_C) != 0,
            Cond::Nc => (self.reg_f & PROC_FLAG_C) == 0,
            Cond::Z => (self.reg_f & PROC_FLAG_Z) != 0,
            Cond::Nz => (self.reg_f & PROC_FLAG_Z) == 0,
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
            Reg8::Mhl => bus.read_byte(pack(self.reg_h, self.reg_l) as u32),
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
            Reg8::Mhl => {
                bus.write_byte(pack(self.reg_h, self.reg_l) as u32, byte)
            }
        }
    }

    fn get_r16(&self, reg: Reg16) -> u16 {
        match reg {
            Reg16::Bc => pack(self.reg_b, self.reg_c),
            Reg16::De => pack(self.reg_d, self.reg_e),
            Reg16::Hl => pack(self.reg_h, self.reg_l),
            Reg16::Sp => self.sp,
        }
    }

    fn set_r16(&mut self, reg: Reg16, word: u16) {
        match reg {
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
        &["A", "F", "B", "C", "D", "E", "H", "L", "AF", "BC", "DE", "HL"]
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
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let opcode = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        match opcode {
            0x00 => self.op_nop(),
            0x01 => self.op_ld_r16_u16(bus, Reg16::Bc),
            0x06 => self.op_ld_r8_u8(bus, Reg8::B),
            0x0e => self.op_ld_r8_u8(bus, Reg8::C),
            0x11 => self.op_ld_r16_u16(bus, Reg16::De),
            0x16 => self.op_ld_r8_u8(bus, Reg8::D),
            0x18 => self.op_jr(bus, Cond::Always),
            0x1e => self.op_ld_r8_u8(bus, Reg8::E),
            0x20 => self.op_jr(bus, Cond::Nz),
            0x21 => self.op_ld_r16_u16(bus, Reg16::Hl),
            0x26 => self.op_ld_r8_u8(bus, Reg8::H),
            0x28 => self.op_jr(bus, Cond::Z),
            0x2e => self.op_ld_r8_u8(bus, Reg8::L),
            0x30 => self.op_jr(bus, Cond::Nc),
            0x31 => self.op_ld_r16_u16(bus, Reg16::Sp),
            0x36 => self.op_ld_r8_u8(bus, Reg8::Mhl),
            0x38 => self.op_jr(bus, Cond::C),
            0x3e => self.op_ld_r8_u8(bus, Reg8::A),
            0x40 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::B),
            0x41 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::C),
            0x42 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::D),
            0x43 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::E),
            0x44 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::H),
            0x45 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::L),
            0x46 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::Mhl),
            0x47 => self.op_ld_r8_r8(bus, Reg8::B, Reg8::A),
            0x48 => self.op_ld_r8_r8(bus, Reg8::C, Reg8::B),
            0x49 => self.op_ld_r8_r8(bus, Reg8::C, Reg8::C),
            0x4a => self.op_ld_r8_r8(bus, Reg8::C, Reg8::D),
            0x4b => self.op_ld_r8_r8(bus, Reg8::C, Reg8::E),
            0x4c => self.op_ld_r8_r8(bus, Reg8::C, Reg8::H),
            0x4d => self.op_ld_r8_r8(bus, Reg8::C, Reg8::L),
            0x4e => self.op_ld_r8_r8(bus, Reg8::C, Reg8::Mhl),
            0x4f => self.op_ld_r8_r8(bus, Reg8::C, Reg8::A),
            0x50 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::B),
            0x51 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::C),
            0x52 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::D),
            0x53 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::E),
            0x54 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::H),
            0x55 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::L),
            0x56 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::Mhl),
            0x57 => self.op_ld_r8_r8(bus, Reg8::D, Reg8::A),
            0x58 => self.op_ld_r8_r8(bus, Reg8::E, Reg8::B),
            0x59 => self.op_ld_r8_r8(bus, Reg8::E, Reg8::C),
            0x5a => self.op_ld_r8_r8(bus, Reg8::E, Reg8::D),
            0x5b => self.op_ld_r8_r8(bus, Reg8::E, Reg8::E),
            0x5c => self.op_ld_r8_r8(bus, Reg8::E, Reg8::H),
            0x5d => self.op_ld_r8_r8(bus, Reg8::E, Reg8::L),
            0x5e => self.op_ld_r8_r8(bus, Reg8::E, Reg8::Mhl),
            0x5f => self.op_ld_r8_r8(bus, Reg8::E, Reg8::A),
            0x60 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::B),
            0x61 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::C),
            0x62 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::D),
            0x63 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::E),
            0x64 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::H),
            0x65 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::L),
            0x66 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::Mhl),
            0x67 => self.op_ld_r8_r8(bus, Reg8::H, Reg8::A),
            0x68 => self.op_ld_r8_r8(bus, Reg8::L, Reg8::B),
            0x69 => self.op_ld_r8_r8(bus, Reg8::L, Reg8::C),
            0x6a => self.op_ld_r8_r8(bus, Reg8::L, Reg8::D),
            0x6b => self.op_ld_r8_r8(bus, Reg8::L, Reg8::E),
            0x6c => self.op_ld_r8_r8(bus, Reg8::L, Reg8::H),
            0x6d => self.op_ld_r8_r8(bus, Reg8::L, Reg8::L),
            0x6e => self.op_ld_r8_r8(bus, Reg8::L, Reg8::Mhl),
            0x6f => self.op_ld_r8_r8(bus, Reg8::L, Reg8::A),
            0x70 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::B),
            0x71 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::C),
            0x72 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::D),
            0x73 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::E),
            0x74 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::H),
            0x75 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::L),
            0x77 => self.op_ld_r8_r8(bus, Reg8::Mhl, Reg8::A),
            0x78 => self.op_ld_r8_r8(bus, Reg8::A, Reg8::B),
            0x79 => self.op_ld_r8_r8(bus, Reg8::A, Reg8::C),
            0x7a => self.op_ld_r8_r8(bus, Reg8::A, Reg8::D),
            0x7b => self.op_ld_r8_r8(bus, Reg8::A, Reg8::E),
            0x7c => self.op_ld_r8_r8(bus, Reg8::A, Reg8::H),
            0x7d => self.op_ld_r8_r8(bus, Reg8::A, Reg8::L),
            0x7e => self.op_ld_r8_r8(bus, Reg8::A, Reg8::Mhl),
            0x7f => self.op_ld_r8_r8(bus, Reg8::A, Reg8::A),
            0xc0 => self.op_ret(bus, Cond::Nz),
            0xc1 => self.op_pop_r16(bus, Reg16::Bc),
            0xc2 => self.op_jp_u16(bus, Cond::Nz),
            0xc3 => self.op_jp_u16(bus, Cond::Always),
            0xc4 => self.op_call(bus, Cond::Nz),
            0xc5 => self.op_push_r16(bus, Reg16::Bc),
            0xc7 => self.op_rst(bus, 0x00),
            0xc8 => self.op_ret(bus, Cond::Z),
            0xc9 => self.op_ret(bus, Cond::Always),
            0xca => self.op_jp_u16(bus, Cond::Z),
            0xcc => self.op_call(bus, Cond::Z),
            0xcd => self.op_call(bus, Cond::Always),
            0xcf => self.op_rst(bus, 0x08),
            0xd0 => self.op_ret(bus, Cond::Nc),
            0xd1 => self.op_pop_r16(bus, Reg16::De),
            0xd2 => self.op_jp_u16(bus, Cond::Nc),
            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xeb | 0xec | 0xed | 0xf4
            | 0xfc | 0xfd => self.op_invalid(opcode),
            0xd4 => self.op_call(bus, Cond::Nc),
            0xd5 => self.op_push_r16(bus, Reg16::De),
            0xd7 => self.op_rst(bus, 0x10),
            0xd8 => self.op_ret(bus, Cond::C),
            0xda => self.op_jp_u16(bus, Cond::C),
            0xdc => self.op_call(bus, Cond::C),
            0xdf => self.op_rst(bus, 0x18),
            0xe0 => self.op_ldh_u8_a(bus),
            0xe1 => self.op_pop_r16(bus, Reg16::Hl),
            0xe2 => self.op_ldh_mc_a(bus),
            0xe5 => self.op_push_r16(bus, Reg16::Hl),
            0xe7 => self.op_rst(bus, 0x20),
            0xe9 => self.op_jp_hl(),
            0xef => self.op_rst(bus, 0x28),
            0xf0 => self.op_ldh_a_u8(bus),
            0xf1 => self.op_pop_af(bus),
            0xf2 => self.op_ldh_a_mc(bus),
            0xf3 => self.op_di(),
            0xf5 => self.op_push_af(bus),
            0xf7 => self.op_rst(bus, 0x30),
            0xf9 => self.op_ld_sp_hl(),
            0xfb => self.op_ei(),
            0xff => self.op_rst(bus, 0x38),
            // TODO: implement remaining opcodes
            _ => Err(SimBreak::HaltOpcode("unimplemented", opcode)),
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
