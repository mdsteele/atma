use crate::bus::{BusPeeker, SimBus};
use crate::db::proc::{SimBreak, SimProc};
use crate::dis::mos6502::{disassemble_instruction, format_instruction};
use std::borrow::Borrow;

//===========================================================================//

const PROC_FLAG_N: u8 = 0b1000_0000;
const PROC_FLAG_V: u8 = 0b0100_0000;
const PROC_FLAG_D: u8 = 0b0000_1000;
const PROC_FLAG_I: u8 = 0b0000_0100;
const PROC_FLAG_Z: u8 = 0b0000_0010;
const PROC_FLAG_C: u8 = 0b0000_0001;

// The P register in the 6502 only has six physical bits that can be set; any
// others will be discarded by the PLP instruction.
const REG_P_MASK: u8 = PROC_FLAG_N
    | PROC_FLAG_V
    | PROC_FLAG_D
    | PROC_FLAG_I
    | PROC_FLAG_Z
    | PROC_FLAG_C;

//===========================================================================//

/// An instruction addressing mode (for a 6502 processor).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum AddrMode {
    Immediate,
    Absolute,
    AbsoluteIndirect,
    XIndexedAbsolute,
    YIndexedAbsolute,
    ZeroPage,
    XIndexedZeroPage,
    YIndexedZeroPage,
    XIndexedZeroPageIndirect,
    ZeroPageIndirectYIndexed,
}

//===========================================================================//

/// A simulated MOS 6502 processor.
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

    fn op_sbc(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs = !self.read_mode_data(mode)?;
        self.add_to(rhs)
    }

    fn op_adc(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs = self.read_mode_data(mode)?;
        self.add_to(rhs)
    }

    fn op_and(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.reg_a &= rhs;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_asl(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let lhs: u8 = self.bus.read_byte(addr as u32);
        let result: u8 = self.shift_left(lhs, false)?;
        self.bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_asl_a(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.shift_left(self.reg_a, false)?;
        Ok(())
    }

    fn op_bcc(&mut self) -> Result<(), SimBreak> {
        self.branch_if(!self.get_flag(PROC_FLAG_C))
    }

    fn op_bcs(&mut self) -> Result<(), SimBreak> {
        self.branch_if(self.get_flag(PROC_FLAG_C))
    }

    fn op_beq(&mut self) -> Result<(), SimBreak> {
        self.branch_if(self.get_flag(PROC_FLAG_Z))
    }

    fn op_bmi(&mut self) -> Result<(), SimBreak> {
        self.branch_if(self.get_flag(PROC_FLAG_N))
    }

    fn op_bne(&mut self) -> Result<(), SimBreak> {
        self.branch_if(!self.get_flag(PROC_FLAG_Z))
    }

    fn op_bpl(&mut self) -> Result<(), SimBreak> {
        self.branch_if(!self.get_flag(PROC_FLAG_N))
    }

    fn op_brk(&mut self) -> Result<(), SimBreak> {
        self.read_immediate_byte(); // the 6502 reads and ignores this byte
        self.push_word(self.pc)?;
        // Bits that don't actually exist in the P register are set to 1 on the
        // stack by the BRK instruction.
        self.push_byte(self.reg_p | !REG_P_MASK)?;
        self.reg_p |= PROC_FLAG_I;
        let lo = self.bus.read_byte(0xfffe);
        let hi = self.bus.read_byte(0xffff);
        self.pc = ((hi as u16) << 8) | (lo as u16);
        Ok(())
    }

    fn op_bvc(&mut self) -> Result<(), SimBreak> {
        self.branch_if(!self.get_flag(PROC_FLAG_V))
    }

    fn op_bvs(&mut self) -> Result<(), SimBreak> {
        self.branch_if(self.get_flag(PROC_FLAG_V))
    }

    fn op_bit(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let data: u8 = self.read_mode_data(mode)?;
        self.reg_p &= !(PROC_FLAG_N | PROC_FLAG_V | PROC_FLAG_Z);
        self.reg_p |= data & (PROC_FLAG_N | PROC_FLAG_V);
        if data & self.reg_a == 0 {
            self.reg_p |= PROC_FLAG_Z;
        }
        Ok(())
    }

    fn op_clc(&mut self) -> Result<(), SimBreak> {
        self.reg_p &= !PROC_FLAG_C;
        Ok(())
    }

    fn op_cld(&mut self) -> Result<(), SimBreak> {
        self.reg_p &= !PROC_FLAG_D;
        Ok(())
    }

    fn op_cli(&mut self) -> Result<(), SimBreak> {
        self.reg_p &= !PROC_FLAG_I;
        Ok(())
    }

    fn op_clv(&mut self) -> Result<(), SimBreak> {
        self.reg_p &= !PROC_FLAG_V;
        Ok(())
    }

    fn op_cmp(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.compare_to(self.reg_a, mode)
    }

    fn op_cpx(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.compare_to(self.reg_x, mode)
    }

    fn op_cpy(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.compare_to(self.reg_y, mode)
    }

    fn op_dec(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let mut data: u8 = self.bus.read_byte(addr as u32);
        data = data.wrapping_sub(1);
        self.set_nz_flags(data);
        self.bus.write_byte(addr as u32, data);
        Ok(())
    }

    fn op_dex(&mut self) -> Result<(), SimBreak> {
        self.reg_x = self.reg_x.wrapping_sub(1);
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn op_dey(&mut self) -> Result<(), SimBreak> {
        self.reg_y = self.reg_y.wrapping_sub(1);
        self.set_nz_flags(self.reg_y);
        Ok(())
    }

    fn op_eor(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.reg_a ^= rhs;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_inc(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let mut data: u8 = self.bus.read_byte(addr as u32);
        data = data.wrapping_add(1);
        self.set_nz_flags(data);
        self.bus.write_byte(addr as u32, data);
        Ok(())
    }

    fn op_inx(&mut self) -> Result<(), SimBreak> {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn op_iny(&mut self) -> Result<(), SimBreak> {
        self.reg_y = self.reg_y.wrapping_add(1);
        self.set_nz_flags(self.reg_y);
        Ok(())
    }

    fn op_jmp(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.pc = self.read_mode_addr(mode)?;
        Ok(())
    }

    fn op_jsr(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let dest: u16 = self.read_mode_addr(mode)?;
        let ret: u16 = self.pc.wrapping_sub(1);
        self.push_word(ret)?;
        self.pc = dest;
        Ok(())
    }

    fn op_lda(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.reg_a = self.read_mode_data(mode)?;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_ldx(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.reg_x = self.read_mode_data(mode)?;
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn op_ldy(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.reg_y = self.read_mode_data(mode)?;
        self.set_nz_flags(self.reg_y);
        Ok(())
    }

    fn op_lsr(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let lhs: u8 = self.bus.read_byte(addr as u32);
        let result: u8 = self.shift_right(lhs, false)?;
        self.bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_lsr_a(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.shift_right(self.reg_a, false)?;
        Ok(())
    }

    fn op_nop(&mut self) -> Result<(), SimBreak> {
        Ok(())
    }

    fn op_ora(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.reg_a |= rhs;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_pla(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.pull_byte()?;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_plp(&mut self) -> Result<(), SimBreak> {
        self.reg_p = self.pull_byte()? & REG_P_MASK;
        Ok(())
    }

    fn op_pha(&mut self) -> Result<(), SimBreak> {
        self.push_byte(self.reg_a)
    }

    fn op_php(&mut self) -> Result<(), SimBreak> {
        // Bits that don't actually exist in the P register are set to 1 on the
        // stack by the PHP instruction.
        self.push_byte(self.reg_p | !REG_P_MASK)
    }

    fn op_rol(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let lhs: u8 = self.bus.read_byte(addr as u32);
        let carry_in = self.get_flag(PROC_FLAG_C);
        let result: u8 = self.shift_left(lhs, carry_in)?;
        self.bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_rol_a(&mut self) -> Result<(), SimBreak> {
        let carry_in = self.get_flag(PROC_FLAG_C);
        self.reg_a = self.shift_left(self.reg_a, carry_in)?;
        Ok(())
    }

    fn op_ror(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let lhs: u8 = self.bus.read_byte(addr as u32);
        let carry_in = self.get_flag(PROC_FLAG_C);
        let result: u8 = self.shift_right(lhs, carry_in)?;
        self.bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_ror_a(&mut self) -> Result<(), SimBreak> {
        let carry_in = self.get_flag(PROC_FLAG_C);
        self.reg_a = self.shift_right(self.reg_a, carry_in)?;
        Ok(())
    }

    fn op_rti(&mut self) -> Result<(), SimBreak> {
        self.reg_p = self.pull_byte()?;
        self.pc = self.pull_word()?;
        Ok(())
    }

    fn op_rts(&mut self) -> Result<(), SimBreak> {
        let addr: u16 = self.pull_word()?;
        self.pc = addr.wrapping_add(1);
        Ok(())
    }

    fn op_sec(&mut self) -> Result<(), SimBreak> {
        self.reg_p |= PROC_FLAG_C;
        Ok(())
    }

    fn op_sed(&mut self) -> Result<(), SimBreak> {
        self.reg_p |= PROC_FLAG_D;
        Ok(())
    }

    fn op_sei(&mut self) -> Result<(), SimBreak> {
        self.reg_p |= PROC_FLAG_I;
        Ok(())
    }

    fn op_sta(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        self.bus.write_byte(addr as u32, self.reg_a);
        Ok(())
    }

    fn op_stx(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        self.bus.write_byte(addr as u32, self.reg_x);
        Ok(())
    }

    fn op_sty(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        self.bus.write_byte(addr as u32, self.reg_y);
        Ok(())
    }

    fn op_tax(&mut self) -> Result<(), SimBreak> {
        self.reg_x = self.reg_a;
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn op_tay(&mut self) -> Result<(), SimBreak> {
        self.reg_y = self.reg_a;
        self.set_nz_flags(self.reg_y);
        Ok(())
    }

    fn op_tsx(&mut self) -> Result<(), SimBreak> {
        self.reg_x = self.reg_s;
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn op_txa(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.reg_x;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_txs(&mut self) -> Result<(), SimBreak> {
        self.reg_s = self.reg_x;
        Ok(()) // TXS does not update processor flags
    }

    fn op_tya(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.reg_y;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_undocumented_alr(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.reg_a &= rhs;
        self.reg_a = self.shift_right(self.reg_a, false)?;
        Ok(())
    }

    fn op_undocumented_anc(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.reg_a &= rhs;
        self.set_nz_flags(self.reg_a);
        self.set_flag(PROC_FLAG_C, (self.reg_a & 0x80) != 0);
        Ok(())
    }

    fn op_undocumented_arr(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.reg_a &= rhs;
        let carry_in = self.get_flag(PROC_FLAG_C);
        self.reg_a = self.shift_right(self.reg_a, carry_in)?;
        Ok(())
    }

    fn op_undocumented_dcp(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(mode)?;
        let mut data: u8 = self.bus.read_byte(addr as u32);
        data = data.wrapping_sub(1);
        self.set_nz_flags(data);
        self.bus.write_byte(addr as u32, data);
        self.set_flag(PROC_FLAG_C, self.reg_a >= data);
        self.set_nz_flags(self.reg_a.wrapping_sub(data));
        Ok(())
    }

    fn op_undocumented_jam(&mut self, opcode: u8) -> Result<(), SimBreak> {
        self.pc = self.pc.wrapping_sub(1); // keep PC at JAM instruction
        Err(SimBreak::HaltOpcode("JAM", opcode))
    }

    fn op_undocumented_nop(
        &mut self,
        opt_mode: Option<AddrMode>,
    ) -> Result<(), SimBreak> {
        if let Some(mode) = opt_mode {
            self.read_mode_addr(mode)?; // ignore the result
        }
        Ok(())
    }

    fn op_undocumented_sbc(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        self.op_sbc(mode)
    }

    fn op_undocumented_sbx(&mut self, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        let lhs: u8 = self.reg_a & self.reg_x;
        self.reg_x = lhs.wrapping_sub(rhs);
        self.set_flag(PROC_FLAG_C, lhs >= rhs);
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn add_to(&mut self, rhs: u8) -> Result<(), SimBreak> {
        let lhs: u8 = self.reg_a;
        let sum: u8 = if self.get_flag(PROC_FLAG_D) {
            0 // TODO: implement decimal mode
        } else {
            let mut sum: u16 = (lhs as u16) + (rhs as u16);
            if self.get_flag(PROC_FLAG_C) {
                sum += 1;
            }
            self.set_flag(PROC_FLAG_C, sum >= 0x100);
            (sum & 0xff) as u8
        };
        self.set_flag(PROC_FLAG_V, ((lhs ^ sum) & (rhs ^ sum) & 0x80) != 0);
        self.reg_a = sum;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn branch_if(&mut self, condition: bool) -> Result<(), SimBreak> {
        let offset: i8 = self.read_immediate_byte() as i8;
        if condition {
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        Ok(())
    }

    fn compare_to(&mut self, lhs: u8, mode: AddrMode) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(mode)?;
        self.set_flag(PROC_FLAG_C, lhs >= rhs);
        self.set_nz_flags(lhs.wrapping_sub(rhs));
        Ok(())
    }

    fn pull_byte(&mut self) -> Result<u8, SimBreak> {
        self.reg_s = self.reg_s.wrapping_add(1);
        let addr: u16 = 0x0100 | (self.reg_s as u16);
        Ok(self.bus.read_byte(addr as u32))
    }

    fn pull_word(&mut self) -> Result<u16, SimBreak> {
        let lo: u8 = self.pull_byte()?;
        let hi: u8 = self.pull_byte()?;
        Ok(((hi as u16) << 8) | (lo as u16))
    }

    fn push_byte(&mut self, data: u8) -> Result<(), SimBreak> {
        let addr: u16 = 0x0100 | (self.reg_s as u16);
        self.bus.write_byte(addr as u32, data);
        self.reg_s = self.reg_s.wrapping_sub(1);
        Ok(())
    }

    fn push_word(&mut self, data: u16) -> Result<(), SimBreak> {
        let lo: u8 = (data & 0xff) as u8;
        let hi: u8 = (data >> 8) as u8;
        self.push_byte(hi)?;
        self.push_byte(lo)
    }

    fn shift_left(&mut self, lhs: u8, carry_in: bool) -> Result<u8, SimBreak> {
        self.set_flag(PROC_FLAG_C, (lhs & 0x80) != 0);
        let mut result: u8 = lhs << 1;
        if carry_in {
            result |= 0x01;
        }
        self.set_nz_flags(result);
        Ok(result)
    }

    fn shift_right(
        &mut self,
        lhs: u8,
        carry_in: bool,
    ) -> Result<u8, SimBreak> {
        self.set_flag(PROC_FLAG_C, (lhs & 0x01) != 0);
        let mut result: u8 = lhs >> 1;
        if carry_in {
            result |= 0x80;
        }
        self.set_nz_flags(result);
        Ok(result)
    }

    fn read_immediate_byte(&mut self) -> u8 {
        let value = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        value
    }

    fn read_immediate_word(&mut self) -> u16 {
        let lo = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        let hi = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        ((hi as u16) << 8) | (lo as u16)
    }

    fn read_zeropage_word(&mut self, addr: u8) -> u16 {
        let lo = self.bus.read_byte(addr as u32);
        let hi = self.bus.read_byte(addr.wrapping_add(1) as u32);
        ((hi as u16) << 8) | (lo as u16)
    }

    fn read_mode_addr(&mut self, mode: AddrMode) -> Result<u16, SimBreak> {
        match mode {
            AddrMode::Immediate => {
                let addr: u16 = self.pc;
                self.pc = self.pc.wrapping_add(1);
                Ok(addr)
            }
            AddrMode::Absolute => Ok(self.read_immediate_word()),
            AddrMode::AbsoluteIndirect => {
                let lo_ptr: u16 = self.read_immediate_word();
                // The 6502 doesn't perform the carry when adding 1 to the base
                // address to read the hi byte of the pointer.  Therefore, for
                // example, JMP ($xxFF) will read the destination address from
                // $xxFF and $xx00.
                let hi_ptr: u16 = (lo_ptr & 0xff00) | ((lo_ptr + 1) & 0x00ff);
                let lo = self.bus.read_byte(lo_ptr as u32);
                let hi = self.bus.read_byte(hi_ptr as u32);
                Ok(((hi as u16) << 8) | (lo as u16))
            }
            AddrMode::XIndexedAbsolute => {
                let base: u16 = self.read_immediate_word();
                // The value at the base address, ignoring the index offset, is
                // read and discarded before the final address is read. This
                // may cause side effects in hardware registers.
                self.bus.read_byte(base as u32);
                Ok(base.wrapping_add(self.reg_x as u16))
            }
            AddrMode::YIndexedAbsolute => {
                let base: u16 = self.read_immediate_word();
                // The value at the base address, ignoring the index offset, is
                // read and discarded before the final address is read. This
                // may cause side effects in hardware registers.
                self.bus.read_byte(base as u32);
                Ok(base.wrapping_add(self.reg_y as u16))
            }
            AddrMode::ZeroPage => Ok(self.read_immediate_byte() as u16),
            AddrMode::XIndexedZeroPage => {
                let base: u8 = self.read_immediate_byte();
                let addr: u8 = base.wrapping_add(self.reg_x);
                Ok(addr as u16)
            }
            AddrMode::YIndexedZeroPage => {
                let base: u8 = self.read_immediate_byte();
                let addr: u8 = base.wrapping_add(self.reg_y);
                Ok(addr as u16)
            }
            AddrMode::XIndexedZeroPageIndirect => {
                let base: u8 = self.read_immediate_byte();
                let ptr: u8 = base.wrapping_add(self.reg_x);
                Ok(self.read_zeropage_word(ptr))
            }
            AddrMode::ZeroPageIndirectYIndexed => {
                let ptr: u8 = self.read_immediate_byte();
                let base: u16 = self.read_zeropage_word(ptr);
                let addr: u16 = base.wrapping_add(self.reg_y as u16);
                Ok(addr)
            }
        }
    }

    fn read_mode_data(&mut self, mode: AddrMode) -> Result<u8, SimBreak> {
        let addr = self.read_mode_addr(mode)?;
        Ok(self.bus.read_byte(addr as u32))
    }

    fn get_flag(&self, flag: u8) -> bool {
        (self.reg_p & flag) != 0
    }

    fn set_flag(&mut self, flag: u8, value: bool) {
        if value {
            self.reg_p |= flag;
        } else {
            self.reg_p &= !flag;
        }
    }

    fn set_nz_flags(&mut self, value: u8) {
        self.reg_p &= !(PROC_FLAG_N | PROC_FLAG_Z);
        if value == 0 {
            self.reg_p |= PROC_FLAG_Z;
        }
        if value >= 0x80 {
            self.reg_p |= PROC_FLAG_N;
        }
    }
}

impl SimProc for Mos6502 {
    fn description(&self) -> String {
        format!("MOS 6502 with {}", self.bus.description())
    }

    fn disassemble(&self, addr: u32) -> (usize, String) {
        let bus = self.bus.borrow();
        let mut reader = BusPeeker::new(bus, addr);
        let (_, operation, operand) =
            disassemble_instruction(&mut reader).unwrap();
        let instruction_size = operand.size() + 1;
        let instruction_string = format_instruction(
            operation,
            operand,
            (addr & 0xffff) as u16,
            bus,
        );
        (instruction_size, instruction_string)
    }

    fn pc(&self) -> u32 {
        self.pc as u32
    }

    fn registers(&self) -> Vec<(&'static str, u32)> {
        vec![
            ("A", self.reg_a as u32),
            ("X", self.reg_x as u32),
            ("Y", self.reg_y as u32),
            ("P", self.reg_p as u32),
            ("S", self.reg_s as u32),
        ]
    }

    fn step(&mut self) -> Result<(), SimBreak> {
        let opcode = self.bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        match opcode {
            0x00 => self.op_brk(),
            0x01 => self.op_ora(AddrMode::XIndexedZeroPageIndirect),
            0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92
            | 0xb2 | 0xd2 | 0xf2 => self.op_undocumented_jam(opcode),
            0x04 | 0x44 | 0x64 => {
                self.op_undocumented_nop(Some(AddrMode::ZeroPage))
            }
            0x05 => self.op_ora(AddrMode::ZeroPage),
            0x06 => self.op_asl(AddrMode::ZeroPage),
            0x08 => self.op_php(),
            0x09 => self.op_ora(AddrMode::Immediate),
            0x0a => self.op_asl_a(),
            0x0b | 0x2b => self.op_undocumented_anc(AddrMode::Immediate),
            0x0c => self.op_undocumented_nop(Some(AddrMode::Absolute)),
            0x0d => self.op_ora(AddrMode::Absolute),
            0x0e => self.op_asl(AddrMode::Absolute),
            0x10 => self.op_bpl(),
            0x11 => self.op_ora(AddrMode::ZeroPageIndirectYIndexed),
            0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 => {
                self.op_undocumented_nop(Some(AddrMode::XIndexedZeroPage))
            }
            0x15 => self.op_ora(AddrMode::XIndexedZeroPage),
            0x16 => self.op_asl(AddrMode::XIndexedZeroPage),
            0x18 => self.op_clc(),
            0x19 => self.op_ora(AddrMode::YIndexedAbsolute),
            0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa => {
                self.op_undocumented_nop(None)
            }
            0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc => {
                self.op_undocumented_nop(Some(AddrMode::XIndexedAbsolute))
            }
            0x1d => self.op_ora(AddrMode::XIndexedAbsolute),
            0x1e => self.op_asl(AddrMode::XIndexedAbsolute),
            0x20 => self.op_jsr(AddrMode::Absolute),
            0x21 => self.op_and(AddrMode::XIndexedZeroPageIndirect),
            0x24 => self.op_bit(AddrMode::ZeroPage),
            0x25 => self.op_and(AddrMode::ZeroPage),
            0x26 => self.op_rol(AddrMode::ZeroPage),
            0x28 => self.op_plp(),
            0x29 => self.op_and(AddrMode::Immediate),
            0x2a => self.op_rol_a(),
            0x2c => self.op_bit(AddrMode::Absolute),
            0x2d => self.op_and(AddrMode::Absolute),
            0x2e => self.op_rol(AddrMode::Absolute),
            0x30 => self.op_bmi(),
            0x31 => self.op_and(AddrMode::ZeroPageIndirectYIndexed),
            0x35 => self.op_and(AddrMode::XIndexedZeroPage),
            0x36 => self.op_rol(AddrMode::XIndexedZeroPage),
            0x38 => self.op_sec(),
            0x39 => self.op_and(AddrMode::YIndexedAbsolute),
            0x3d => self.op_and(AddrMode::XIndexedAbsolute),
            0x3e => self.op_rol(AddrMode::XIndexedAbsolute),
            0x40 => self.op_rti(),
            0x41 => self.op_eor(AddrMode::XIndexedZeroPageIndirect),
            0x45 => self.op_eor(AddrMode::ZeroPage),
            0x46 => self.op_lsr(AddrMode::ZeroPage),
            0x48 => self.op_pha(),
            0x49 => self.op_eor(AddrMode::Immediate),
            0x4a => self.op_lsr_a(),
            0x4b => self.op_undocumented_alr(AddrMode::Immediate),
            0x4c => self.op_jmp(AddrMode::Absolute),
            0x4d => self.op_eor(AddrMode::Absolute),
            0x4e => self.op_lsr(AddrMode::Absolute),
            0x50 => self.op_bvc(),
            0x51 => self.op_eor(AddrMode::ZeroPageIndirectYIndexed),
            0x55 => self.op_eor(AddrMode::XIndexedZeroPage),
            0x56 => self.op_lsr(AddrMode::XIndexedZeroPage),
            0x58 => self.op_cli(),
            0x59 => self.op_eor(AddrMode::YIndexedAbsolute),
            0x5d => self.op_eor(AddrMode::XIndexedAbsolute),
            0x5e => self.op_lsr(AddrMode::XIndexedAbsolute),
            0x60 => self.op_rts(),
            0x61 => self.op_adc(AddrMode::XIndexedZeroPageIndirect),
            0x65 => self.op_adc(AddrMode::ZeroPage),
            0x66 => self.op_ror(AddrMode::ZeroPage),
            0x68 => self.op_pla(),
            0x69 => self.op_adc(AddrMode::Immediate),
            0x6a => self.op_ror_a(),
            0x6b => self.op_undocumented_arr(AddrMode::Immediate),
            0x6c => self.op_jmp(AddrMode::AbsoluteIndirect),
            0x6d => self.op_adc(AddrMode::Absolute),
            0x6e => self.op_ror(AddrMode::Absolute),
            0x70 => self.op_bvs(),
            0x71 => self.op_adc(AddrMode::ZeroPageIndirectYIndexed),
            0x75 => self.op_adc(AddrMode::XIndexedZeroPage),
            0x76 => self.op_ror(AddrMode::XIndexedZeroPage),
            0x78 => self.op_sei(),
            0x79 => self.op_adc(AddrMode::YIndexedAbsolute),
            0x7d => self.op_adc(AddrMode::XIndexedAbsolute),
            0x7e => self.op_ror(AddrMode::XIndexedAbsolute),
            0x80 | 0x82 | 0x89 | 0xc2 | 0xe2 => {
                self.op_undocumented_nop(Some(AddrMode::Immediate))
            }
            0x81 => self.op_sta(AddrMode::XIndexedZeroPageIndirect),
            0x84 => self.op_sty(AddrMode::ZeroPage),
            0x85 => self.op_sta(AddrMode::ZeroPage),
            0x86 => self.op_stx(AddrMode::ZeroPage),
            0x88 => self.op_dey(),
            0x8a => self.op_txa(),
            0x8c => self.op_sty(AddrMode::Absolute),
            0x8d => self.op_sta(AddrMode::Absolute),
            0x8e => self.op_stx(AddrMode::Absolute),
            0x90 => self.op_bcc(),
            0x91 => self.op_sta(AddrMode::ZeroPageIndirectYIndexed),
            0x94 => self.op_sty(AddrMode::XIndexedZeroPage),
            0x95 => self.op_sta(AddrMode::XIndexedZeroPage),
            0x96 => self.op_stx(AddrMode::YIndexedZeroPage),
            0x98 => self.op_tya(),
            0x99 => self.op_sta(AddrMode::YIndexedAbsolute),
            0x9a => self.op_txs(),
            0x9d => self.op_sta(AddrMode::XIndexedAbsolute),
            0xa0 => self.op_ldy(AddrMode::Immediate),
            0xa1 => self.op_lda(AddrMode::XIndexedZeroPageIndirect),
            0xa2 => self.op_ldx(AddrMode::Immediate),
            0xa4 => self.op_ldy(AddrMode::ZeroPage),
            0xa5 => self.op_lda(AddrMode::ZeroPage),
            0xa6 => self.op_ldx(AddrMode::ZeroPage),
            0xa8 => self.op_tay(),
            0xa9 => self.op_lda(AddrMode::Immediate),
            0xaa => self.op_tax(),
            0xac => self.op_ldy(AddrMode::Absolute),
            0xad => self.op_lda(AddrMode::Absolute),
            0xae => self.op_ldx(AddrMode::Absolute),
            0xb0 => self.op_bcs(),
            0xb1 => self.op_lda(AddrMode::ZeroPageIndirectYIndexed),
            0xb4 => self.op_ldy(AddrMode::XIndexedZeroPage),
            0xb5 => self.op_lda(AddrMode::XIndexedZeroPage),
            0xb6 => self.op_ldx(AddrMode::YIndexedZeroPage),
            0xb8 => self.op_clv(),
            0xb9 => self.op_lda(AddrMode::YIndexedAbsolute),
            0xba => self.op_tsx(),
            0xbc => self.op_ldy(AddrMode::XIndexedAbsolute),
            0xbd => self.op_lda(AddrMode::XIndexedAbsolute),
            0xbe => self.op_ldx(AddrMode::YIndexedAbsolute),
            0xc0 => self.op_cpy(AddrMode::Immediate),
            0xc1 => self.op_cmp(AddrMode::XIndexedZeroPageIndirect),
            0xc3 => {
                self.op_undocumented_dcp(AddrMode::XIndexedZeroPageIndirect)
            }
            0xc4 => self.op_cpy(AddrMode::ZeroPage),
            0xc5 => self.op_cmp(AddrMode::ZeroPage),
            0xc6 => self.op_dec(AddrMode::ZeroPage),
            0xc7 => self.op_undocumented_dcp(AddrMode::ZeroPage),
            0xc8 => self.op_iny(),
            0xc9 => self.op_cmp(AddrMode::Immediate),
            0xca => self.op_dex(),
            0xcb => self.op_undocumented_sbx(AddrMode::Immediate),
            0xcc => self.op_cpy(AddrMode::Absolute),
            0xcd => self.op_cmp(AddrMode::Absolute),
            0xce => self.op_dec(AddrMode::Absolute),
            0xcf => self.op_undocumented_dcp(AddrMode::Absolute),
            0xd0 => self.op_bne(),
            0xd1 => self.op_cmp(AddrMode::ZeroPageIndirectYIndexed),
            0xd3 => {
                self.op_undocumented_dcp(AddrMode::ZeroPageIndirectYIndexed)
            }
            0xd5 => self.op_cmp(AddrMode::XIndexedZeroPage),
            0xd6 => self.op_dec(AddrMode::XIndexedZeroPage),
            0xd7 => self.op_undocumented_dcp(AddrMode::XIndexedZeroPage),
            0xd8 => self.op_cld(),
            0xd9 => self.op_cmp(AddrMode::YIndexedAbsolute),
            0xdb => self.op_undocumented_dcp(AddrMode::YIndexedAbsolute),
            0xdd => self.op_cmp(AddrMode::XIndexedAbsolute),
            0xde => self.op_dec(AddrMode::XIndexedAbsolute),
            0xdf => self.op_undocumented_dcp(AddrMode::XIndexedAbsolute),
            0xe0 => self.op_cpx(AddrMode::Immediate),
            0xe1 => self.op_sbc(AddrMode::XIndexedZeroPageIndirect),
            0xe4 => self.op_cpx(AddrMode::ZeroPage),
            0xe5 => self.op_sbc(AddrMode::ZeroPage),
            0xe6 => self.op_inc(AddrMode::ZeroPage),
            0xe8 => self.op_inx(),
            0xe9 => self.op_sbc(AddrMode::Immediate),
            0xea => self.op_nop(),
            0xeb => self.op_undocumented_sbc(AddrMode::Immediate),
            0xec => self.op_cpx(AddrMode::Absolute),
            0xed => self.op_sbc(AddrMode::Absolute),
            0xee => self.op_inc(AddrMode::Absolute),
            0xf0 => self.op_beq(),
            0xf1 => self.op_sbc(AddrMode::ZeroPageIndirectYIndexed),
            0xf5 => self.op_sbc(AddrMode::XIndexedZeroPage),
            0xf6 => self.op_inc(AddrMode::XIndexedZeroPage),
            0xf8 => self.op_sed(),
            0xf9 => self.op_sbc(AddrMode::YIndexedAbsolute),
            0xfd => self.op_sbc(AddrMode::XIndexedAbsolute),
            0xfe => self.op_inc(AddrMode::XIndexedAbsolute),
            // TODO: implement remaining undocumented opcodes
            _ => Err(SimBreak::HaltOpcode("unimplemented", opcode)),
        }
    }
}

//===========================================================================//
