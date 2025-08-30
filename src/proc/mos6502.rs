use crate::bus::{BusPeeker, SimBus};
use crate::dis::mos6502::{disassemble_instruction, format_instruction};
use crate::proc::{SimBreak, SimProc};

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
}

impl Mos6502 {
    /// Returns a new simulated MOS 6502 processor.
    pub fn new(bus: &mut dyn SimBus) -> Mos6502 {
        let reset_lo: u8 = bus.read_byte(0xfffc);
        let reset_hi: u8 = bus.read_byte(0xfffd);
        let pc = ((reset_hi as u16) << 8) | (reset_lo as u16);
        Mos6502 { pc, reg_s: 0, reg_p: 0, reg_a: 0, reg_x: 0, reg_y: 0 }
    }

    fn op_sbc(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs = !self.read_mode_data(bus, mode)?;
        self.add_to(rhs)
    }

    fn op_adc(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs = self.read_mode_data(bus, mode)?;
        self.add_to(rhs)
    }

    fn op_and(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.reg_a &= rhs;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_asl(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let lhs: u8 = bus.read_byte(addr as u32);
        let result: u8 = self.shift_left(lhs, false)?;
        bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_asl_a(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.shift_left(self.reg_a, false)?;
        Ok(())
    }

    fn op_bcc(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, !self.get_flag(PROC_FLAG_C))
    }

    fn op_bcs(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, self.get_flag(PROC_FLAG_C))
    }

    fn op_beq(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, self.get_flag(PROC_FLAG_Z))
    }

    fn op_bmi(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, self.get_flag(PROC_FLAG_N))
    }

    fn op_bne(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, !self.get_flag(PROC_FLAG_Z))
    }

    fn op_bpl(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, !self.get_flag(PROC_FLAG_N))
    }

    fn op_brk(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.read_immediate_byte(bus); // the 6502 reads and ignores this byte
        self.push_word(bus, self.pc)?;
        // Bits that don't actually exist in the P register are set to 1 on the
        // stack by the BRK instruction.
        self.push_byte(bus, self.reg_p | !REG_P_MASK)?;
        self.reg_p |= PROC_FLAG_I;
        let lo = bus.read_byte(0xfffe);
        let hi = bus.read_byte(0xffff);
        self.pc = ((hi as u16) << 8) | (lo as u16);
        Ok(())
    }

    fn op_bvc(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, !self.get_flag(PROC_FLAG_V))
    }

    fn op_bvs(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.branch_if(bus, self.get_flag(PROC_FLAG_V))
    }

    fn op_bit(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let data: u8 = self.read_mode_data(bus, mode)?;
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

    fn op_cmp(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.compare_to(bus, self.reg_a, mode)
    }

    fn op_cpx(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.compare_to(bus, self.reg_x, mode)
    }

    fn op_cpy(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.compare_to(bus, self.reg_y, mode)
    }

    fn op_dec(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let mut data: u8 = bus.read_byte(addr as u32);
        data = data.wrapping_sub(1);
        self.set_nz_flags(data);
        bus.write_byte(addr as u32, data);
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

    fn op_eor(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.reg_a ^= rhs;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_inc(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let mut data: u8 = bus.read_byte(addr as u32);
        data = data.wrapping_add(1);
        self.set_nz_flags(data);
        bus.write_byte(addr as u32, data);
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

    fn op_jmp(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.pc = self.read_mode_addr(bus, mode)?;
        Ok(())
    }

    fn op_jsr(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let dest: u16 = self.read_mode_addr(bus, mode)?;
        let ret: u16 = self.pc.wrapping_sub(1);
        self.push_word(bus, ret)?;
        self.pc = dest;
        Ok(())
    }

    fn op_lda(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.reg_a = self.read_mode_data(bus, mode)?;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_ldx(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.reg_x = self.read_mode_data(bus, mode)?;
        self.set_nz_flags(self.reg_x);
        Ok(())
    }

    fn op_ldy(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.reg_y = self.read_mode_data(bus, mode)?;
        self.set_nz_flags(self.reg_y);
        Ok(())
    }

    fn op_lsr(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let lhs: u8 = bus.read_byte(addr as u32);
        let result: u8 = self.shift_right(lhs, false)?;
        bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_lsr_a(&mut self) -> Result<(), SimBreak> {
        self.reg_a = self.shift_right(self.reg_a, false)?;
        Ok(())
    }

    fn op_nop(&mut self) -> Result<(), SimBreak> {
        Ok(())
    }

    fn op_ora(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.reg_a |= rhs;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_pla(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.reg_a = self.pull_byte(bus)?;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn op_plp(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.reg_p = self.pull_byte(bus)? & REG_P_MASK;
        Ok(())
    }

    fn op_pha(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.push_byte(bus, self.reg_a)
    }

    fn op_php(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        // Bits that don't actually exist in the P register are set to 1 on the
        // stack by the PHP instruction.
        self.push_byte(bus, self.reg_p | !REG_P_MASK)
    }

    fn op_rol(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let lhs: u8 = bus.read_byte(addr as u32);
        let carry_in = self.get_flag(PROC_FLAG_C);
        let result: u8 = self.shift_left(lhs, carry_in)?;
        bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_rol_a(&mut self) -> Result<(), SimBreak> {
        let carry_in = self.get_flag(PROC_FLAG_C);
        self.reg_a = self.shift_left(self.reg_a, carry_in)?;
        Ok(())
    }

    fn op_ror(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let lhs: u8 = bus.read_byte(addr as u32);
        let carry_in = self.get_flag(PROC_FLAG_C);
        let result: u8 = self.shift_right(lhs, carry_in)?;
        bus.write_byte(addr as u32, result);
        Ok(())
    }

    fn op_ror_a(&mut self) -> Result<(), SimBreak> {
        let carry_in = self.get_flag(PROC_FLAG_C);
        self.reg_a = self.shift_right(self.reg_a, carry_in)?;
        Ok(())
    }

    fn op_rti(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.reg_p = self.pull_byte(bus)?;
        self.pc = self.pull_word(bus)?;
        Ok(())
    }

    fn op_rts(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let addr: u16 = self.pull_word(bus)?;
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

    fn op_sta(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        bus.write_byte(addr as u32, self.reg_a);
        Ok(())
    }

    fn op_stx(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        bus.write_byte(addr as u32, self.reg_x);
        Ok(())
    }

    fn op_sty(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        bus.write_byte(addr as u32, self.reg_y);
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

    fn op_undocumented_alr(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.reg_a &= rhs;
        self.reg_a = self.shift_right(self.reg_a, false)?;
        Ok(())
    }

    fn op_undocumented_anc(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.reg_a &= rhs;
        self.set_nz_flags(self.reg_a);
        self.set_flag(PROC_FLAG_C, (self.reg_a & 0x80) != 0);
        Ok(())
    }

    fn op_undocumented_arr(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.reg_a &= rhs;
        let carry_in = self.get_flag(PROC_FLAG_C);
        self.reg_a = self.shift_right(self.reg_a, carry_in)?;
        Ok(())
    }

    fn op_undocumented_dcp(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let addr: u16 = self.read_mode_addr(bus, mode)?;
        let mut data: u8 = bus.read_byte(addr as u32);
        data = data.wrapping_sub(1);
        self.set_nz_flags(data);
        bus.write_byte(addr as u32, data);
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
        bus: &mut dyn SimBus,
        opt_mode: Option<AddrMode>,
    ) -> Result<(), SimBreak> {
        if let Some(mode) = opt_mode {
            self.read_mode_addr(bus, mode)?; // ignore the result
        }
        Ok(())
    }

    fn op_undocumented_sbc(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        self.op_sbc(bus, mode)
    }

    fn op_undocumented_sbx(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
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

    fn branch_if(
        &mut self,
        bus: &mut dyn SimBus,
        condition: bool,
    ) -> Result<(), SimBreak> {
        let offset: i8 = self.read_immediate_byte(bus) as i8;
        if condition {
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        Ok(())
    }

    fn compare_to(
        &mut self,
        bus: &mut dyn SimBus,
        lhs: u8,
        mode: AddrMode,
    ) -> Result<(), SimBreak> {
        let rhs: u8 = self.read_mode_data(bus, mode)?;
        self.set_flag(PROC_FLAG_C, lhs >= rhs);
        self.set_nz_flags(lhs.wrapping_sub(rhs));
        Ok(())
    }

    fn pull_byte(&mut self, bus: &mut dyn SimBus) -> Result<u8, SimBreak> {
        self.reg_s = self.reg_s.wrapping_add(1);
        let addr: u16 = 0x0100 | (self.reg_s as u16);
        Ok(bus.read_byte(addr as u32))
    }

    fn pull_word(&mut self, bus: &mut dyn SimBus) -> Result<u16, SimBreak> {
        let lo: u8 = self.pull_byte(bus)?;
        let hi: u8 = self.pull_byte(bus)?;
        Ok(((hi as u16) << 8) | (lo as u16))
    }

    fn push_byte(
        &mut self,
        bus: &mut dyn SimBus,
        data: u8,
    ) -> Result<(), SimBreak> {
        let addr: u16 = 0x0100 | (self.reg_s as u16);
        bus.write_byte(addr as u32, data);
        self.reg_s = self.reg_s.wrapping_sub(1);
        Ok(())
    }

    fn push_word(
        &mut self,
        bus: &mut dyn SimBus,
        data: u16,
    ) -> Result<(), SimBreak> {
        let lo: u8 = (data & 0xff) as u8;
        let hi: u8 = (data >> 8) as u8;
        self.push_byte(bus, hi)?;
        self.push_byte(bus, lo)
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

    fn read_immediate_byte(&mut self, bus: &mut dyn SimBus) -> u8 {
        let value = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        value
    }

    fn read_immediate_word(&mut self, bus: &mut dyn SimBus) -> u16 {
        let lo = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        let hi = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        ((hi as u16) << 8) | (lo as u16)
    }

    fn read_zeropage_word(&mut self, bus: &mut dyn SimBus, addr: u8) -> u16 {
        let lo = bus.read_byte(addr as u32);
        let hi = bus.read_byte(addr.wrapping_add(1) as u32);
        ((hi as u16) << 8) | (lo as u16)
    }

    fn read_mode_addr(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<u16, SimBreak> {
        match mode {
            AddrMode::Immediate => {
                let addr: u16 = self.pc;
                self.pc = self.pc.wrapping_add(1);
                Ok(addr)
            }
            AddrMode::Absolute => Ok(self.read_immediate_word(bus)),
            AddrMode::AbsoluteIndirect => {
                let lo_ptr: u16 = self.read_immediate_word(bus);
                // The 6502 doesn't perform the carry when adding 1 to the base
                // address to read the hi byte of the pointer.  Therefore, for
                // example, JMP ($xxFF) will read the destination address from
                // $xxFF and $xx00.
                let hi_ptr: u16 = (lo_ptr & 0xff00) | ((lo_ptr + 1) & 0x00ff);
                let lo = bus.read_byte(lo_ptr as u32);
                let hi = bus.read_byte(hi_ptr as u32);
                Ok(((hi as u16) << 8) | (lo as u16))
            }
            AddrMode::XIndexedAbsolute => {
                let base: u16 = self.read_immediate_word(bus);
                // The value at the base address, ignoring the index offset, is
                // read and discarded before the final address is read. This
                // may cause side effects in hardware registers.
                bus.read_byte(base as u32);
                Ok(base.wrapping_add(self.reg_x as u16))
            }
            AddrMode::YIndexedAbsolute => {
                let base: u16 = self.read_immediate_word(bus);
                // The value at the base address, ignoring the index offset, is
                // read and discarded before the final address is read. This
                // may cause side effects in hardware registers.
                bus.read_byte(base as u32);
                Ok(base.wrapping_add(self.reg_y as u16))
            }
            AddrMode::ZeroPage => Ok(self.read_immediate_byte(bus) as u16),
            AddrMode::XIndexedZeroPage => {
                let base: u8 = self.read_immediate_byte(bus);
                let addr: u8 = base.wrapping_add(self.reg_x);
                Ok(addr as u16)
            }
            AddrMode::YIndexedZeroPage => {
                let base: u8 = self.read_immediate_byte(bus);
                let addr: u8 = base.wrapping_add(self.reg_y);
                Ok(addr as u16)
            }
            AddrMode::XIndexedZeroPageIndirect => {
                let base: u8 = self.read_immediate_byte(bus);
                let ptr: u8 = base.wrapping_add(self.reg_x);
                Ok(self.read_zeropage_word(bus, ptr))
            }
            AddrMode::ZeroPageIndirectYIndexed => {
                let ptr: u8 = self.read_immediate_byte(bus);
                let base: u16 = self.read_zeropage_word(bus, ptr);
                let addr: u16 = base.wrapping_add(self.reg_y as u16);
                Ok(addr)
            }
        }
    }

    fn read_mode_data(
        &mut self,
        bus: &mut dyn SimBus,
        mode: AddrMode,
    ) -> Result<u8, SimBreak> {
        let addr = self.read_mode_addr(bus, mode)?;
        Ok(bus.read_byte(addr as u32))
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
        "MOS 6502".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, addr: u32) -> (usize, String) {
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

    fn set_pc(&mut self, addr: u32) {
        self.pc = (addr & 0xffff) as u16;
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["A", "X", "Y", "P", "S"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "A" => Some(u32::from(self.reg_a)),
            "X" => Some(u32::from(self.reg_x)),
            "Y" => Some(u32::from(self.reg_y)),
            "P" => Some(u32::from(self.reg_p)),
            "S" => Some(u32::from(self.reg_s)),
            _ => None,
        }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        match name {
            "A" => self.reg_a = (value & 0xff) as u8,
            "X" => self.reg_x = (value & 0xff) as u8,
            "Y" => self.reg_y = (value & 0xff) as u8,
            "P" => self.reg_p = (value & u32::from(REG_P_MASK)) as u8,
            "S" => self.reg_s = (value & 0xff) as u8,
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let opcode = bus.read_byte(self.pc as u32);
        self.pc = self.pc.wrapping_add(1);
        match opcode {
            0x00 => self.op_brk(bus),
            0x01 => self.op_ora(bus, AddrMode::XIndexedZeroPageIndirect),
            0x02 | 0x12 | 0x22 | 0x32 | 0x42 | 0x52 | 0x62 | 0x72 | 0x92
            | 0xb2 | 0xd2 | 0xf2 => self.op_undocumented_jam(opcode),
            0x04 | 0x44 | 0x64 => {
                self.op_undocumented_nop(bus, Some(AddrMode::ZeroPage))
            }
            0x05 => self.op_ora(bus, AddrMode::ZeroPage),
            0x06 => self.op_asl(bus, AddrMode::ZeroPage),
            0x08 => self.op_php(bus),
            0x09 => self.op_ora(bus, AddrMode::Immediate),
            0x0a => self.op_asl_a(),
            0x0b | 0x2b => self.op_undocumented_anc(bus, AddrMode::Immediate),
            0x0c => self.op_undocumented_nop(bus, Some(AddrMode::Absolute)),
            0x0d => self.op_ora(bus, AddrMode::Absolute),
            0x0e => self.op_asl(bus, AddrMode::Absolute),
            0x10 => self.op_bpl(bus),
            0x11 => self.op_ora(bus, AddrMode::ZeroPageIndirectYIndexed),
            0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 => {
                self.op_undocumented_nop(bus, Some(AddrMode::XIndexedZeroPage))
            }
            0x15 => self.op_ora(bus, AddrMode::XIndexedZeroPage),
            0x16 => self.op_asl(bus, AddrMode::XIndexedZeroPage),
            0x18 => self.op_clc(),
            0x19 => self.op_ora(bus, AddrMode::YIndexedAbsolute),
            0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa => {
                self.op_undocumented_nop(bus, None)
            }
            0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc => {
                self.op_undocumented_nop(bus, Some(AddrMode::XIndexedAbsolute))
            }
            0x1d => self.op_ora(bus, AddrMode::XIndexedAbsolute),
            0x1e => self.op_asl(bus, AddrMode::XIndexedAbsolute),
            0x20 => self.op_jsr(bus, AddrMode::Absolute),
            0x21 => self.op_and(bus, AddrMode::XIndexedZeroPageIndirect),
            0x24 => self.op_bit(bus, AddrMode::ZeroPage),
            0x25 => self.op_and(bus, AddrMode::ZeroPage),
            0x26 => self.op_rol(bus, AddrMode::ZeroPage),
            0x28 => self.op_plp(bus),
            0x29 => self.op_and(bus, AddrMode::Immediate),
            0x2a => self.op_rol_a(),
            0x2c => self.op_bit(bus, AddrMode::Absolute),
            0x2d => self.op_and(bus, AddrMode::Absolute),
            0x2e => self.op_rol(bus, AddrMode::Absolute),
            0x30 => self.op_bmi(bus),
            0x31 => self.op_and(bus, AddrMode::ZeroPageIndirectYIndexed),
            0x35 => self.op_and(bus, AddrMode::XIndexedZeroPage),
            0x36 => self.op_rol(bus, AddrMode::XIndexedZeroPage),
            0x38 => self.op_sec(),
            0x39 => self.op_and(bus, AddrMode::YIndexedAbsolute),
            0x3d => self.op_and(bus, AddrMode::XIndexedAbsolute),
            0x3e => self.op_rol(bus, AddrMode::XIndexedAbsolute),
            0x40 => self.op_rti(bus),
            0x41 => self.op_eor(bus, AddrMode::XIndexedZeroPageIndirect),
            0x45 => self.op_eor(bus, AddrMode::ZeroPage),
            0x46 => self.op_lsr(bus, AddrMode::ZeroPage),
            0x48 => self.op_pha(bus),
            0x49 => self.op_eor(bus, AddrMode::Immediate),
            0x4a => self.op_lsr_a(),
            0x4b => self.op_undocumented_alr(bus, AddrMode::Immediate),
            0x4c => self.op_jmp(bus, AddrMode::Absolute),
            0x4d => self.op_eor(bus, AddrMode::Absolute),
            0x4e => self.op_lsr(bus, AddrMode::Absolute),
            0x50 => self.op_bvc(bus),
            0x51 => self.op_eor(bus, AddrMode::ZeroPageIndirectYIndexed),
            0x55 => self.op_eor(bus, AddrMode::XIndexedZeroPage),
            0x56 => self.op_lsr(bus, AddrMode::XIndexedZeroPage),
            0x58 => self.op_cli(),
            0x59 => self.op_eor(bus, AddrMode::YIndexedAbsolute),
            0x5d => self.op_eor(bus, AddrMode::XIndexedAbsolute),
            0x5e => self.op_lsr(bus, AddrMode::XIndexedAbsolute),
            0x60 => self.op_rts(bus),
            0x61 => self.op_adc(bus, AddrMode::XIndexedZeroPageIndirect),
            0x65 => self.op_adc(bus, AddrMode::ZeroPage),
            0x66 => self.op_ror(bus, AddrMode::ZeroPage),
            0x68 => self.op_pla(bus),
            0x69 => self.op_adc(bus, AddrMode::Immediate),
            0x6a => self.op_ror_a(),
            0x6b => self.op_undocumented_arr(bus, AddrMode::Immediate),
            0x6c => self.op_jmp(bus, AddrMode::AbsoluteIndirect),
            0x6d => self.op_adc(bus, AddrMode::Absolute),
            0x6e => self.op_ror(bus, AddrMode::Absolute),
            0x70 => self.op_bvs(bus),
            0x71 => self.op_adc(bus, AddrMode::ZeroPageIndirectYIndexed),
            0x75 => self.op_adc(bus, AddrMode::XIndexedZeroPage),
            0x76 => self.op_ror(bus, AddrMode::XIndexedZeroPage),
            0x78 => self.op_sei(),
            0x79 => self.op_adc(bus, AddrMode::YIndexedAbsolute),
            0x7d => self.op_adc(bus, AddrMode::XIndexedAbsolute),
            0x7e => self.op_ror(bus, AddrMode::XIndexedAbsolute),
            0x80 | 0x82 | 0x89 | 0xc2 | 0xe2 => {
                self.op_undocumented_nop(bus, Some(AddrMode::Immediate))
            }
            0x81 => self.op_sta(bus, AddrMode::XIndexedZeroPageIndirect),
            0x84 => self.op_sty(bus, AddrMode::ZeroPage),
            0x85 => self.op_sta(bus, AddrMode::ZeroPage),
            0x86 => self.op_stx(bus, AddrMode::ZeroPage),
            0x88 => self.op_dey(),
            0x8a => self.op_txa(),
            0x8c => self.op_sty(bus, AddrMode::Absolute),
            0x8d => self.op_sta(bus, AddrMode::Absolute),
            0x8e => self.op_stx(bus, AddrMode::Absolute),
            0x90 => self.op_bcc(bus),
            0x91 => self.op_sta(bus, AddrMode::ZeroPageIndirectYIndexed),
            0x94 => self.op_sty(bus, AddrMode::XIndexedZeroPage),
            0x95 => self.op_sta(bus, AddrMode::XIndexedZeroPage),
            0x96 => self.op_stx(bus, AddrMode::YIndexedZeroPage),
            0x98 => self.op_tya(),
            0x99 => self.op_sta(bus, AddrMode::YIndexedAbsolute),
            0x9a => self.op_txs(),
            0x9d => self.op_sta(bus, AddrMode::XIndexedAbsolute),
            0xa0 => self.op_ldy(bus, AddrMode::Immediate),
            0xa1 => self.op_lda(bus, AddrMode::XIndexedZeroPageIndirect),
            0xa2 => self.op_ldx(bus, AddrMode::Immediate),
            0xa4 => self.op_ldy(bus, AddrMode::ZeroPage),
            0xa5 => self.op_lda(bus, AddrMode::ZeroPage),
            0xa6 => self.op_ldx(bus, AddrMode::ZeroPage),
            0xa8 => self.op_tay(),
            0xa9 => self.op_lda(bus, AddrMode::Immediate),
            0xaa => self.op_tax(),
            0xac => self.op_ldy(bus, AddrMode::Absolute),
            0xad => self.op_lda(bus, AddrMode::Absolute),
            0xae => self.op_ldx(bus, AddrMode::Absolute),
            0xb0 => self.op_bcs(bus),
            0xb1 => self.op_lda(bus, AddrMode::ZeroPageIndirectYIndexed),
            0xb4 => self.op_ldy(bus, AddrMode::XIndexedZeroPage),
            0xb5 => self.op_lda(bus, AddrMode::XIndexedZeroPage),
            0xb6 => self.op_ldx(bus, AddrMode::YIndexedZeroPage),
            0xb8 => self.op_clv(),
            0xb9 => self.op_lda(bus, AddrMode::YIndexedAbsolute),
            0xba => self.op_tsx(),
            0xbc => self.op_ldy(bus, AddrMode::XIndexedAbsolute),
            0xbd => self.op_lda(bus, AddrMode::XIndexedAbsolute),
            0xbe => self.op_ldx(bus, AddrMode::YIndexedAbsolute),
            0xc0 => self.op_cpy(bus, AddrMode::Immediate),
            0xc1 => self.op_cmp(bus, AddrMode::XIndexedZeroPageIndirect),
            0xc3 => self
                .op_undocumented_dcp(bus, AddrMode::XIndexedZeroPageIndirect),
            0xc4 => self.op_cpy(bus, AddrMode::ZeroPage),
            0xc5 => self.op_cmp(bus, AddrMode::ZeroPage),
            0xc6 => self.op_dec(bus, AddrMode::ZeroPage),
            0xc7 => self.op_undocumented_dcp(bus, AddrMode::ZeroPage),
            0xc8 => self.op_iny(),
            0xc9 => self.op_cmp(bus, AddrMode::Immediate),
            0xca => self.op_dex(),
            0xcb => self.op_undocumented_sbx(bus, AddrMode::Immediate),
            0xcc => self.op_cpy(bus, AddrMode::Absolute),
            0xcd => self.op_cmp(bus, AddrMode::Absolute),
            0xce => self.op_dec(bus, AddrMode::Absolute),
            0xcf => self.op_undocumented_dcp(bus, AddrMode::Absolute),
            0xd0 => self.op_bne(bus),
            0xd1 => self.op_cmp(bus, AddrMode::ZeroPageIndirectYIndexed),
            0xd3 => self
                .op_undocumented_dcp(bus, AddrMode::ZeroPageIndirectYIndexed),
            0xd5 => self.op_cmp(bus, AddrMode::XIndexedZeroPage),
            0xd6 => self.op_dec(bus, AddrMode::XIndexedZeroPage),
            0xd7 => self.op_undocumented_dcp(bus, AddrMode::XIndexedZeroPage),
            0xd8 => self.op_cld(),
            0xd9 => self.op_cmp(bus, AddrMode::YIndexedAbsolute),
            0xdb => self.op_undocumented_dcp(bus, AddrMode::YIndexedAbsolute),
            0xdd => self.op_cmp(bus, AddrMode::XIndexedAbsolute),
            0xde => self.op_dec(bus, AddrMode::XIndexedAbsolute),
            0xdf => self.op_undocumented_dcp(bus, AddrMode::XIndexedAbsolute),
            0xe0 => self.op_cpx(bus, AddrMode::Immediate),
            0xe1 => self.op_sbc(bus, AddrMode::XIndexedZeroPageIndirect),
            0xe4 => self.op_cpx(bus, AddrMode::ZeroPage),
            0xe5 => self.op_sbc(bus, AddrMode::ZeroPage),
            0xe6 => self.op_inc(bus, AddrMode::ZeroPage),
            0xe8 => self.op_inx(),
            0xe9 => self.op_sbc(bus, AddrMode::Immediate),
            0xea => self.op_nop(),
            0xeb => self.op_undocumented_sbc(bus, AddrMode::Immediate),
            0xec => self.op_cpx(bus, AddrMode::Absolute),
            0xed => self.op_sbc(bus, AddrMode::Absolute),
            0xee => self.op_inc(bus, AddrMode::Absolute),
            0xf0 => self.op_beq(bus),
            0xf1 => self.op_sbc(bus, AddrMode::ZeroPageIndirectYIndexed),
            0xf5 => self.op_sbc(bus, AddrMode::XIndexedZeroPage),
            0xf6 => self.op_inc(bus, AddrMode::XIndexedZeroPage),
            0xf8 => self.op_sed(),
            0xf9 => self.op_sbc(bus, AddrMode::YIndexedAbsolute),
            0xfd => self.op_sbc(bus, AddrMode::XIndexedAbsolute),
            0xfe => self.op_inc(bus, AddrMode::XIndexedAbsolute),
            // TODO: implement remaining undocumented opcodes
            _ => Err(SimBreak::HaltOpcode("unimplemented", opcode)),
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{Mos6502, REG_P_MASK, SimProc};
    use crate::bus::null_bus;

    #[test]
    fn get_registers() {
        let proc = Mos6502::new(&mut *null_bus());
        for &register in proc.register_names() {
            assert!(proc.get_register(register).is_some());
        }
    }

    #[test]
    fn set_registers() {
        let mut proc = Mos6502::new(&mut *null_bus());
        for &register in proc.register_names() {
            proc.set_register(register, 0);
            assert_eq!(proc.get_register(register), Some(0));
            let value = u32::from(REG_P_MASK);
            proc.set_register(register, value);
            assert_eq!(proc.get_register(register), Some(value));
        }
    }

    #[test]
    fn set_p_register() {
        let mut proc = Mos6502::new(&mut *null_bus());
        proc.set_register("P", 0);
        assert_eq!(proc.get_register("P"), Some(0));
        // Invalid bits should get masked off of the P register.
        proc.set_register("P", 0xff);
        assert_eq!(proc.get_register("P"), Some(u32::from(REG_P_MASK)));
    }
}

//===========================================================================//
