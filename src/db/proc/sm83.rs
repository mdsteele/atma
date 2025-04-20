use crate::bus::SimBus;
use crate::db::proc::{SimBreak, SimProc};

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
    bus: Box<dyn SimBus>,
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

impl SharpSm83 {
    /// Returns a new simulated Sharp SM83 processor connected to the given
    /// memory bus.
    pub fn new(bus: Box<dyn SimBus>) -> SharpSm83 {
        SharpSm83 {
            bus,
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

    fn op_call(&mut self, cond: Cond) -> Result<(), SimBreak> {
        let dest: u16 = self.read_immediate_word();
        if self.condition_met(cond) {
            self.push_word(self.pc)?;
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

    fn op_jp_u16(&mut self, cond: Cond) -> Result<(), SimBreak> {
        let dest: u16 = self.read_immediate_word();
        if self.condition_met(cond) {
            self.pc = dest;
        }
        Ok(())
    }

    fn op_jp_hl(&mut self) -> Result<(), SimBreak> {
        self.pc = pack(self.reg_h, self.reg_l);
        Ok(())
    }

    fn op_jr(&mut self, cond: Cond) -> Result<(), SimBreak> {
        let offset: i8 = self.read_immediate_byte() as i8;
        if self.condition_met(cond) {
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        Ok(())
    }

    fn op_ld_r8_r8(&mut self, dst: Reg8, src: Reg8) -> Result<(), SimBreak> {
        let byte = self.get_r8(src);
        self.set_r8(dst, byte);
        Ok(())
    }

    fn op_ld_r8_u8(&mut self, dst: Reg8) -> Result<(), SimBreak> {
        let byte = self.read_immediate_byte();
        self.set_r8(dst, byte);
        Ok(())
    }

    fn op_ld_r16_u16(&mut self, dst: Reg16) -> Result<(), SimBreak> {
        let word = self.read_immediate_word();
        self.set_r16(dst, word);
        Ok(())
    }

    fn op_ld_sp_hl(&mut self) -> Result<(), SimBreak> {
        self.sp = pack(self.reg_h, self.reg_l);
        Ok(())
    }

    fn op_ldh_a_mc(&mut self) -> Result<(), SimBreak> {
        let addr: u16 = pack(0xff, self.reg_c);
        self.reg_a = self.bus.read_byte(addr as u32);
        Ok(())
    }

    fn op_ldh_a_u8(&mut self) -> Result<(), SimBreak> {
        let lo: u8 = self.read_immediate_byte();
        let addr: u16 = pack(0xff, lo);
        self.reg_a = self.bus.read_byte(addr as u32);
        Ok(())
    }

    fn op_ldh_mc_a(&mut self) -> Result<(), SimBreak> {
        let addr: u16 = pack(0xff, self.reg_c);
        self.bus.write_byte(addr as u32, self.reg_a);
        Ok(())
    }

    fn op_ldh_u8_a(&mut self) -> Result<(), SimBreak> {
        let lo: u8 = self.read_immediate_byte();
        let addr: u16 = pack(0xff, lo);
        self.bus.write_byte(addr as u32, self.reg_a);
        Ok(())
    }

    fn op_nop(&mut self) -> Result<(), SimBreak> {
        Ok(())
    }

    fn op_pop_af(&mut self) -> Result<(), SimBreak> {
        (self.reg_a, self.reg_f) = unpack(self.pop_word()?);
        self.reg_f &= REG_F_MASK;
        Ok(())
    }

    fn op_pop_r16(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        let word = self.pop_word()?;
        self.set_r16(reg, word);
        Ok(())
    }

    fn op_push_af(&mut self) -> Result<(), SimBreak> {
        self.push_word(pack(self.reg_a, self.reg_f))
    }

    fn op_push_r16(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        self.push_word(self.get_r16(reg))
    }

    fn op_ret(&mut self, cond: Cond) -> Result<(), SimBreak> {
        if self.condition_met(cond) {
            self.pc = self.pop_word()?;
        }
        Ok(())
    }

    fn op_rst(&mut self, dest: u16) -> Result<(), SimBreak> {
        self.push_word(self.pc)?;
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

    fn pop_word(&mut self) -> Result<u16, SimBreak> {
        let lo = self.bus.read_byte(self.sp as u32);
        self.sp = self.sp.wrapping_add(1);
        let hi = self.bus.read_byte(self.sp as u32);
        self.sp = self.sp.wrapping_add(1);
        Ok(pack(hi, lo))
    }

    fn push_word(&mut self, word: u16) -> Result<(), SimBreak> {
        let (hi, lo) = unpack(word);
        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp as u32, hi);
        self.sp = self.sp.wrapping_sub(1);
        self.bus.write_byte(self.sp as u32, lo);
        Ok(())
    }

    fn read_immediate_byte(&mut self) -> u8 {
        let byte = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        byte
    }

    fn read_immediate_word(&mut self) -> u16 {
        let lo = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        let hi = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        pack(hi, lo)
    }

    fn get_r8(&mut self, reg: Reg8) -> u8 {
        match reg {
            Reg8::A => self.reg_a,
            Reg8::B => self.reg_b,
            Reg8::C => self.reg_c,
            Reg8::D => self.reg_d,
            Reg8::E => self.reg_e,
            Reg8::H => self.reg_h,
            Reg8::L => self.reg_l,
            Reg8::Mhl => {
                self.bus.read_byte(pack(self.reg_h, self.reg_l) as u32)
            }
        }
    }

    fn set_r8(&mut self, reg: Reg8, byte: u8) {
        match reg {
            Reg8::A => self.reg_a = byte,
            Reg8::B => self.reg_b = byte,
            Reg8::C => self.reg_c = byte,
            Reg8::D => self.reg_d = byte,
            Reg8::E => self.reg_e = byte,
            Reg8::H => self.reg_h = byte,
            Reg8::L => self.reg_l = byte,
            Reg8::Mhl => {
                self.bus.write_byte(pack(self.reg_h, self.reg_l) as u32, byte)
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
        format!("Sharp SM83 with {}", self.bus.description())
    }

    fn disassemble(&self, _addr: u32) -> (usize, String) {
        (0, String::new()) // TODO
    }

    fn pc(&self) -> u32 {
        self.pc as u32
    }

    fn step(&mut self) -> Result<(), SimBreak> {
        let opcode = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        match opcode {
            0x00 => self.op_nop(),
            0x01 => self.op_ld_r16_u16(Reg16::Bc),
            0x06 => self.op_ld_r8_u8(Reg8::B),
            0x0e => self.op_ld_r8_u8(Reg8::C),
            0x11 => self.op_ld_r16_u16(Reg16::De),
            0x16 => self.op_ld_r8_u8(Reg8::D),
            0x18 => self.op_jr(Cond::Always),
            0x1e => self.op_ld_r8_u8(Reg8::E),
            0x20 => self.op_jr(Cond::Nz),
            0x21 => self.op_ld_r16_u16(Reg16::Hl),
            0x26 => self.op_ld_r8_u8(Reg8::H),
            0x28 => self.op_jr(Cond::Z),
            0x2e => self.op_ld_r8_u8(Reg8::L),
            0x30 => self.op_jr(Cond::Nc),
            0x31 => self.op_ld_r16_u16(Reg16::Sp),
            0x36 => self.op_ld_r8_u8(Reg8::Mhl),
            0x38 => self.op_jr(Cond::C),
            0x3e => self.op_ld_r8_u8(Reg8::A),
            0x40 => self.op_ld_r8_r8(Reg8::B, Reg8::B),
            0x41 => self.op_ld_r8_r8(Reg8::B, Reg8::C),
            0x42 => self.op_ld_r8_r8(Reg8::B, Reg8::D),
            0x43 => self.op_ld_r8_r8(Reg8::B, Reg8::E),
            0x44 => self.op_ld_r8_r8(Reg8::B, Reg8::H),
            0x45 => self.op_ld_r8_r8(Reg8::B, Reg8::L),
            0x46 => self.op_ld_r8_r8(Reg8::B, Reg8::Mhl),
            0x47 => self.op_ld_r8_r8(Reg8::B, Reg8::A),
            0x48 => self.op_ld_r8_r8(Reg8::C, Reg8::B),
            0x49 => self.op_ld_r8_r8(Reg8::C, Reg8::C),
            0x4a => self.op_ld_r8_r8(Reg8::C, Reg8::D),
            0x4b => self.op_ld_r8_r8(Reg8::C, Reg8::E),
            0x4c => self.op_ld_r8_r8(Reg8::C, Reg8::H),
            0x4d => self.op_ld_r8_r8(Reg8::C, Reg8::L),
            0x4e => self.op_ld_r8_r8(Reg8::C, Reg8::Mhl),
            0x4f => self.op_ld_r8_r8(Reg8::C, Reg8::A),
            0x50 => self.op_ld_r8_r8(Reg8::D, Reg8::B),
            0x51 => self.op_ld_r8_r8(Reg8::D, Reg8::C),
            0x52 => self.op_ld_r8_r8(Reg8::D, Reg8::D),
            0x53 => self.op_ld_r8_r8(Reg8::D, Reg8::E),
            0x54 => self.op_ld_r8_r8(Reg8::D, Reg8::H),
            0x55 => self.op_ld_r8_r8(Reg8::D, Reg8::L),
            0x56 => self.op_ld_r8_r8(Reg8::D, Reg8::Mhl),
            0x57 => self.op_ld_r8_r8(Reg8::D, Reg8::A),
            0x58 => self.op_ld_r8_r8(Reg8::E, Reg8::B),
            0x59 => self.op_ld_r8_r8(Reg8::E, Reg8::C),
            0x5a => self.op_ld_r8_r8(Reg8::E, Reg8::D),
            0x5b => self.op_ld_r8_r8(Reg8::E, Reg8::E),
            0x5c => self.op_ld_r8_r8(Reg8::E, Reg8::H),
            0x5d => self.op_ld_r8_r8(Reg8::E, Reg8::L),
            0x5e => self.op_ld_r8_r8(Reg8::E, Reg8::Mhl),
            0x5f => self.op_ld_r8_r8(Reg8::E, Reg8::A),
            0x60 => self.op_ld_r8_r8(Reg8::H, Reg8::B),
            0x61 => self.op_ld_r8_r8(Reg8::H, Reg8::C),
            0x62 => self.op_ld_r8_r8(Reg8::H, Reg8::D),
            0x63 => self.op_ld_r8_r8(Reg8::H, Reg8::E),
            0x64 => self.op_ld_r8_r8(Reg8::H, Reg8::H),
            0x65 => self.op_ld_r8_r8(Reg8::H, Reg8::L),
            0x66 => self.op_ld_r8_r8(Reg8::H, Reg8::Mhl),
            0x67 => self.op_ld_r8_r8(Reg8::H, Reg8::A),
            0x68 => self.op_ld_r8_r8(Reg8::L, Reg8::B),
            0x69 => self.op_ld_r8_r8(Reg8::L, Reg8::C),
            0x6a => self.op_ld_r8_r8(Reg8::L, Reg8::D),
            0x6b => self.op_ld_r8_r8(Reg8::L, Reg8::E),
            0x6c => self.op_ld_r8_r8(Reg8::L, Reg8::H),
            0x6d => self.op_ld_r8_r8(Reg8::L, Reg8::L),
            0x6e => self.op_ld_r8_r8(Reg8::L, Reg8::Mhl),
            0x6f => self.op_ld_r8_r8(Reg8::L, Reg8::A),
            0x70 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::B),
            0x71 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::C),
            0x72 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::D),
            0x73 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::E),
            0x74 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::H),
            0x75 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::L),
            0x77 => self.op_ld_r8_r8(Reg8::Mhl, Reg8::A),
            0x78 => self.op_ld_r8_r8(Reg8::A, Reg8::B),
            0x79 => self.op_ld_r8_r8(Reg8::A, Reg8::C),
            0x7a => self.op_ld_r8_r8(Reg8::A, Reg8::D),
            0x7b => self.op_ld_r8_r8(Reg8::A, Reg8::E),
            0x7c => self.op_ld_r8_r8(Reg8::A, Reg8::H),
            0x7d => self.op_ld_r8_r8(Reg8::A, Reg8::L),
            0x7e => self.op_ld_r8_r8(Reg8::A, Reg8::Mhl),
            0x7f => self.op_ld_r8_r8(Reg8::A, Reg8::A),
            0xc0 => self.op_ret(Cond::Nz),
            0xc1 => self.op_pop_r16(Reg16::Bc),
            0xc2 => self.op_jp_u16(Cond::Nz),
            0xc3 => self.op_jp_u16(Cond::Always),
            0xc4 => self.op_call(Cond::Nz),
            0xc5 => self.op_push_r16(Reg16::Bc),
            0xc7 => self.op_rst(0x00),
            0xc8 => self.op_ret(Cond::Z),
            0xc9 => self.op_ret(Cond::Always),
            0xca => self.op_jp_u16(Cond::Z),
            0xcc => self.op_call(Cond::Z),
            0xcd => self.op_call(Cond::Always),
            0xcf => self.op_rst(0x08),
            0xd0 => self.op_ret(Cond::Nc),
            0xd1 => self.op_pop_r16(Reg16::De),
            0xd2 => self.op_jp_u16(Cond::Nc),
            0xd3 | 0xdb | 0xdd | 0xe3 | 0xe4 | 0xeb | 0xec | 0xed | 0xf4
            | 0xfc | 0xfd => self.op_invalid(opcode),
            0xd4 => self.op_call(Cond::Nc),
            0xd5 => self.op_push_r16(Reg16::De),
            0xd7 => self.op_rst(0x10),
            0xd8 => self.op_ret(Cond::C),
            0xda => self.op_jp_u16(Cond::C),
            0xdc => self.op_call(Cond::C),
            0xdf => self.op_rst(0x18),
            0xe0 => self.op_ldh_u8_a(),
            0xe1 => self.op_pop_r16(Reg16::Hl),
            0xe2 => self.op_ldh_mc_a(),
            0xe5 => self.op_push_r16(Reg16::Hl),
            0xe7 => self.op_rst(0x20),
            0xe9 => self.op_jp_hl(),
            0xef => self.op_rst(0x28),
            0xf0 => self.op_ldh_a_u8(),
            0xf1 => self.op_pop_af(),
            0xf2 => self.op_ldh_a_mc(),
            0xf3 => self.op_di(),
            0xf5 => self.op_push_af(),
            0xf7 => self.op_rst(0x30),
            0xf9 => self.op_ld_sp_hl(),
            0xfb => self.op_ei(),
            0xff => self.op_rst(0x38),
            // TODO: implement remaining opcodes
            _ => Err(SimBreak::HaltOpcode("unimplemented", opcode)),
        }?;
        if let Ime::Pending1 = self.ime {
            self.ime = Ime::Enabled;
        } else if let Ime::Pending2 = self.ime {
            self.ime = Ime::Pending1;
        }
        Ok(())
    }
}

//===========================================================================//
