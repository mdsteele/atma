use super::util::{pack, unpack, watch};
use crate::addr::Addr;
use crate::bus::{SimBus, WatchKind};
use crate::dis::sm83::{
    Condition, Instruction, Operation, Prefixed, Reg8, Reg16,
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

#[derive(Clone, Copy)]
enum Microcode {
    Adc,                // A += (DATA + carry), set flags
    And,                // A &= DATA, set flags
    Call(Condition),    // if condition met, push new microcode to call ADDR
    Compare,            // compare A to DATA and set flags
    DecodeOpcode,       // decode DATA as opcode, push new microcode
    DecodePrefixed,     // decode DATA as prefixed, push new microcode
    Decrement,          // DATA -= 1, set flags
    GetPcHi,            // DATA = PC.hi
    GetPcLo,            // DATA = PC.lo
    GetReg(Reg8),       // DATA = reg
    GetRegLo(Reg16),    // DATA = reg.lo
    GetRegHi(Reg16),    // DATA = reg.hi
    Increment,          // DATA += 1, set flags
    JumpAbs(Condition), // if condition met, PC = ADDR
    JumpRel(Condition), // if condition met, PC += (DATA as i8)
    JumpTo(u16),        // PC = value
    MakeAddrAbs,        // ADDR = pack(DATA, TEMP)
    MakeAddrHp,         // ADDR = pack(0xff, DATA)
    OffsetSp(Reg16),    // reg = SP + DATA, set flags
    Or,                 // A |= DATA, set flags
    Pop,                // ADDR = SP, SP += 1, DATA = [ADDR], watch(ADDR, Read)
    PrefixedBit(u8),    // test specified bit in DATA, set flags
    PrefixedRes(u8),    // reset specified bit in DATA
    PrefixedSet(u8),    // set specified bit in DATA
    PrefixedSwap,       // swap nibbles of DATA, set flags
    Push,               // push Write2, SP -= 1, ADDR = SP, watch(ADDR, Write)
    Read,               // DATA = [ADDR], watch(ADDR, Read)
    ReadAtPc,           // DATA = [PC], watch(PC, Read), PC += 1
    Sbc,                // A -= (DATA + carry), set flags
    SetReg(Reg8),       // reg = DATA (or for Reg8::M*, watch and push Write2)
    SetRegHi(Reg16),    // reg.hi = DATA
    SetRegLo(Reg16),    // reg.lo = DATA
    SetTemp,            // TEMP = DATA
    Write,              // push Write2, watch(ADDR, Write)
    Write2,             // [ADDR] = DATA
    Xor,                // A ^= DATA, set flags
}

//===========================================================================//

/// A simulated Sharp SM83 processor.
pub struct SharpSm83 {
    pc: u16,
    sp: u16,
    addr: u16,
    data: u8,
    temp: u8,
    reg_a: u8,
    reg_f: u8,
    reg_b: u8,
    reg_c: u8,
    reg_d: u8,
    reg_e: u8,
    reg_h: u8,
    reg_l: u8,
    ime: Ime,
    microcode: Vec<Microcode>,
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
            pc: 0x0100,
            sp: 0xffff,
            addr: 0,
            data: 0,
            temp: 0,
            reg_a: 0xff,
            reg_f: REG_F_MASK,
            reg_b: 0,
            reg_c: 0,
            reg_d: 0,
            reg_e: 0,
            reg_h: 0,
            reg_l: 0,
            ime: Ime::Disabled,
            microcode: Vec::new(),
        }
    }

    fn execute_microcode(
        &mut self,
        bus: &mut dyn SimBus,
        microcode: Microcode,
    ) -> Result<(), SimBreak> {
        match microcode {
            Microcode::Adc => self.exec_adc(),
            Microcode::And => self.exec_and(),
            Microcode::Call(cond) => self.exec_call(cond),
            Microcode::Compare => self.exec_compare(),
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::DecodePrefixed => self.exec_decode_prefixed(),
            Microcode::Decrement => self.exec_decrement(),
            Microcode::GetPcHi => self.exec_get_pc_hi(),
            Microcode::GetPcLo => self.exec_get_pc_lo(),
            Microcode::GetReg(reg) => self.exec_get_reg(bus, reg),
            Microcode::GetRegHi(reg) => self.exec_get_reg_hi(reg),
            Microcode::GetRegLo(reg) => self.exec_get_reg_lo(reg),
            Microcode::Increment => self.exec_increment(),
            Microcode::JumpAbs(cond) => self.exec_jump_abs(cond),
            Microcode::JumpRel(cond) => self.exec_jump_rel(cond),
            Microcode::JumpTo(dest) => self.exec_jump_to(dest),
            Microcode::MakeAddrAbs => self.exec_make_addr_abs(),
            Microcode::MakeAddrHp => self.exec_make_addr_hp(),
            Microcode::OffsetSp(reg) => self.exec_offset_sp(reg),
            Microcode::Or => self.exec_or(),
            Microcode::Pop => self.exec_pop(bus),
            Microcode::PrefixedBit(bit) => self.exec_prefixed_bit(bit),
            Microcode::PrefixedRes(bit) => self.exec_prefixed_res(bit),
            Microcode::PrefixedSet(bit) => self.exec_prefixed_set(bit),
            Microcode::PrefixedSwap => self.exec_prefixed_swap(),
            Microcode::Push => self.exec_push(bus),
            Microcode::Read => self.exec_read(bus),
            Microcode::ReadAtPc => self.exec_read_at_pc(bus),
            Microcode::Sbc => self.exec_sbc(),
            Microcode::SetReg(reg) => self.exec_set_reg(bus, reg),
            Microcode::SetRegHi(reg) => self.exec_set_reg_hi(reg),
            Microcode::SetRegLo(reg) => self.exec_set_reg_lo(reg),
            Microcode::SetTemp => self.exec_set_temp(),
            Microcode::Write => self.exec_write(bus),
            Microcode::Write2 => self.exec_write2(bus),
            Microcode::Xor => self.exec_xor(),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let opcode = self.data;
        match Operation::from_opcode(opcode) {
            Operation::AdcAI8 => self.decode_op_adc_a_i8(),
            Operation::AdcAR8(reg) => self.decode_op_adc_a_r8(reg),
            Operation::AddHlR16(reg) => self.decode_op_add_hl_r16(reg),
            Operation::AddAI8 => self.decode_op_add_a_i8(),
            Operation::AddAR8(reg) => self.decode_op_add_a_r8(reg),
            Operation::AddSpI8 => self.decode_op_add_sp_i8(),
            Operation::AndAI8 => self.decode_op_and_a_i8(),
            Operation::AndAR8(reg) => self.decode_op_and_a_r8(reg),
            Operation::CallM16(cond) => self.decode_op_call(cond),
            Operation::Ccf => self.decode_op_ccf(),
            Operation::CpAI8 => self.decode_op_cp_a_i8(),
            Operation::CpAR8(reg) => self.decode_op_cp_a_r8(reg),
            Operation::Cpl => self.decode_op_cpl(),
            Operation::Daa => self.decode_op_daa(),
            Operation::DecR16(reg) => self.decode_op_dec_r16(reg),
            Operation::DecR8(reg) => self.decode_op_dec_r8(reg),
            Operation::Di => self.decode_op_di(),
            Operation::Ei => self.decode_op_ei(),
            Operation::Halt => {
                return Err(SimBreak::HaltOpcode("HALT", opcode));
            }
            Operation::IncR16(reg) => self.decode_op_inc_r16(reg),
            Operation::IncR8(reg) => self.decode_op_inc_r8(reg),
            Operation::Invalid => return self.op_invalid(opcode),
            Operation::JpI16(cond) => self.decode_op_jp_i16(cond),
            Operation::JpHl => self.decode_op_jp_hl(),
            Operation::JrI8(cond) => self.decode_op_jr_i8(cond),
            Operation::LdAM16 => self.decode_op_ld_a_m16(),
            Operation::LdAMhld => self.decode_op_ld_a_mhld(),
            Operation::LdAMhli => self.decode_op_ld_a_mhli(),
            Operation::LdHlSpI8 => self.decode_op_ld_hl_sp_i8(),
            Operation::LdM16A => self.decode_op_ld_m16_a(),
            Operation::LdM16Sp => todo!("LD [a16], SP"),
            Operation::LdMhldA => self.decode_op_ld_mhld_a(),
            Operation::LdMhliA => self.decode_op_ld_mhli_a(),
            Operation::LdR16I16(reg) => self.decode_op_ld_r16_i16(reg),
            Operation::LdR8I8(reg) => self.decode_op_ld_r8_i8(reg),
            Operation::LdR8R8(r1, r2) => self.decode_op_ld_r8_r8(r1, r2),
            Operation::LdSpHl => self.decode_op_ld_sp_hl(),
            Operation::LdhAM8 => self.decode_op_ldh_a_m8(),
            Operation::LdhAMc => self.decode_op_ldh_a_mc(),
            Operation::LdhM8A => self.decode_op_ldh_m8_a(),
            Operation::LdhMcA => self.decode_op_ldh_mc_a(),
            Operation::Nop => {}
            Operation::OrAI8 => self.decode_op_or_a_i8(),
            Operation::OrAR8(reg) => self.decode_op_or_a_r8(reg),
            Operation::Pop(reg) => self.decode_op_pop_r16(reg),
            Operation::Prefix => self.decode_op_prefix(),
            Operation::Push(reg) => self.decode_op_push_r16(reg),
            Operation::Ret(cond) => self.decode_op_ret(cond),
            Operation::Reti => self.decode_op_reti(),
            Operation::Rla => todo!("RLA"),
            Operation::Rlca => todo!("RLCA"),
            Operation::Rra => todo!("RRA"),
            Operation::Rrca => todo!("RRCA"),
            Operation::Rst(zp) => self.decode_op_rst(zp),
            Operation::SbcAI8 => self.decode_op_sbc_a_i8(),
            Operation::SbcAR8(reg) => self.decode_op_sbc_a_r8(reg),
            Operation::Scf => self.decode_op_scf(),
            Operation::Stop => {
                return Err(SimBreak::HaltOpcode("STOP", opcode));
            }
            Operation::SubAI8 => self.decode_op_sub_a_i8(),
            Operation::SubAR8(reg) => self.decode_op_sub_a_r8(reg),
            Operation::XorAI8 => self.decode_op_xor_a_i8(),
            Operation::XorAR8(reg) => self.decode_op_xor_a_r8(reg),
        };
        Ok(())
    }

    fn exec_decode_prefixed(&mut self) -> Result<(), SimBreak> {
        let (prefixed, reg) = Prefixed::decode(self.data);
        self.microcode.push(Microcode::SetReg(reg));
        self.microcode.push(match prefixed {
            Prefixed::Rl => todo!("RL"),
            Prefixed::Rlc => todo!("RLC"),
            Prefixed::Rr => todo!("RR"),
            Prefixed::Rrc => todo!("RRC"),
            Prefixed::Sla => todo!("SLA"),
            Prefixed::Sra => todo!("SRA"),
            Prefixed::Srl => todo!("SRL"),
            Prefixed::Swap => Microcode::PrefixedSwap,
            Prefixed::Bit(bit) => Microcode::PrefixedBit(bit),
            Prefixed::Res(bit) => Microcode::PrefixedRes(bit),
            Prefixed::Set(bit) => Microcode::PrefixedSet(bit),
        });
        self.microcode.push(Microcode::GetReg(reg));
        Ok(())
    }

    fn exec_adc(&mut self) -> Result<(), SimBreak> {
        let lhs = i16::from(self.reg_a);
        let rhs = i16::from(self.data);
        let carry = i16::from(self.get_flag(PROC_FLAG_C));
        self.set_flag(PROC_FLAG_H, (lhs & 0xf) + (rhs & 0xf) + carry > 0xf);
        let result = lhs + rhs + carry;
        self.set_flag(PROC_FLAG_C, result >= 0x100);
        self.reg_a = result as u8;
        self.set_flag(PROC_FLAG_Z, self.reg_a == 0);
        self.set_flag(PROC_FLAG_N, false);
        Ok(())
    }

    fn exec_and(&mut self) -> Result<(), SimBreak> {
        self.reg_a &= self.data;
        self.set_flag(PROC_FLAG_Z, self.reg_a == 0);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, true);
        self.set_flag(PROC_FLAG_C, false);
        Ok(())
    }

    fn exec_call(&mut self, cond: Condition) -> Result<(), SimBreak> {
        if self.condition_met(cond) {
            self.microcode.push(Microcode::JumpTo(self.addr));
            self.microcode.push(Microcode::Push);
            self.microcode.push(Microcode::GetPcLo);
            self.microcode.push(Microcode::Push);
            self.microcode.push(Microcode::GetPcHi);
        }
        Ok(())
    }

    fn exec_compare(&mut self) -> Result<(), SimBreak> {
        let lhs = self.reg_a;
        let rhs = self.data;
        self.set_flag(PROC_FLAG_H, (lhs & 0xf) < (rhs & 0xf));
        self.set_flag(PROC_FLAG_C, lhs < rhs);
        self.set_flag(PROC_FLAG_Z, lhs == rhs);
        self.set_flag(PROC_FLAG_N, true);
        Ok(())
    }

    fn exec_decrement(&mut self) -> Result<(), SimBreak> {
        self.set_flag(PROC_FLAG_H, (self.data & 0xf) == 0);
        self.data = self.data.wrapping_sub(1);
        self.set_flag(PROC_FLAG_Z, self.data == 0);
        self.set_flag(PROC_FLAG_N, true);
        Ok(())
    }

    fn exec_get_pc_hi(&mut self) -> Result<(), SimBreak> {
        self.data = (self.pc >> 8) as u8;
        Ok(())
    }

    fn exec_get_pc_lo(&mut self) -> Result<(), SimBreak> {
        self.data = self.pc as u8;
        Ok(())
    }

    fn exec_get_reg(
        &mut self,
        bus: &mut dyn SimBus,
        reg: Reg8,
    ) -> Result<(), SimBreak> {
        let addr = Addr::from(match reg {
            Reg8::A => {
                self.data = self.reg_a;
                return Ok(());
            }
            Reg8::B => {
                self.data = self.reg_b;
                return Ok(());
            }
            Reg8::C => {
                self.data = self.reg_c;
                return Ok(());
            }
            Reg8::D => {
                self.data = self.reg_d;
                return Ok(());
            }
            Reg8::E => {
                self.data = self.reg_e;
                return Ok(());
            }
            Reg8::H => {
                self.data = self.reg_h;
                return Ok(());
            }
            Reg8::L => {
                self.data = self.reg_l;
                return Ok(());
            }
            Reg8::Mbc => self.get_r16(Reg16::Bc),
            Reg8::Mde => self.get_r16(Reg16::De),
            Reg8::Mhl => self.get_r16(Reg16::Hl),
        });
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_get_reg_hi(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        self.data = match reg {
            Reg16::Af => self.reg_a,
            Reg16::Bc => self.reg_b,
            Reg16::De => self.reg_d,
            Reg16::Hl => self.reg_h,
            Reg16::Sp => (self.sp >> 8) as u8,
        };
        Ok(())
    }

    fn exec_get_reg_lo(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        self.data = match reg {
            Reg16::Af => self.reg_f,
            Reg16::Bc => self.reg_c,
            Reg16::De => self.reg_e,
            Reg16::Hl => self.reg_l,
            Reg16::Sp => self.sp as u8,
        };
        Ok(())
    }

    fn exec_increment(&mut self) -> Result<(), SimBreak> {
        self.data = self.data.wrapping_add(1);
        self.set_flag(PROC_FLAG_Z, self.data == 0);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, (self.data & 0xf) == 0);
        Ok(())
    }

    fn exec_jump_abs(&mut self, cond: Condition) -> Result<(), SimBreak> {
        if self.condition_met(cond) {
            self.pc = self.addr;
        }
        Ok(())
    }

    fn exec_jump_rel(&mut self, cond: Condition) -> Result<(), SimBreak> {
        if self.condition_met(cond) {
            let offset = self.data as i8;
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        Ok(())
    }

    fn exec_jump_to(&mut self, dest: u16) -> Result<(), SimBreak> {
        self.pc = dest;
        Ok(())
    }

    fn exec_make_addr_abs(&mut self) -> Result<(), SimBreak> {
        self.addr = pack(self.data, self.temp);
        Ok(())
    }

    fn exec_make_addr_hp(&mut self) -> Result<(), SimBreak> {
        self.addr = pack(0xff, self.data);
        Ok(())
    }

    fn exec_offset_sp(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        self.set_flag(
            PROC_FLAG_H,
            (self.sp & 0xf) + u16::from(self.data & 0xf) > 0xf,
        );
        self.set_flag(
            PROC_FLAG_C,
            (self.sp & 0xff) + u16::from(self.data) > 0xff,
        );
        self.set_flag(PROC_FLAG_Z, false);
        self.set_flag(PROC_FLAG_N, false);
        let offset = self.data as i8;
        let result = self.sp.wrapping_add(offset as u16);
        self.set_r16(reg, result);
        Ok(())
    }

    fn exec_or(&mut self) -> Result<(), SimBreak> {
        self.reg_a |= self.data;
        self.set_flag(PROC_FLAG_Z, self.reg_a == 0);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, false);
        self.set_flag(PROC_FLAG_C, false);
        Ok(())
    }

    fn exec_pop(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.addr = self.sp;
        self.sp = self.sp.wrapping_add(1);
        self.exec_read(bus)
    }

    fn exec_push(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.sp = self.sp.wrapping_sub(1);
        self.addr = self.sp;
        self.exec_write(bus)
    }

    fn exec_prefixed_bit(&mut self, bit: u8) -> Result<(), SimBreak> {
        self.set_flag(PROC_FLAG_Z, (self.data & (1 << bit)) == 0);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, true);
        Ok(())
    }

    fn exec_prefixed_res(&mut self, bit: u8) -> Result<(), SimBreak> {
        self.data &= !(1 << bit);
        Ok(())
    }

    fn exec_prefixed_set(&mut self, bit: u8) -> Result<(), SimBreak> {
        self.data |= 1 << bit;
        Ok(())
    }

    fn exec_prefixed_swap(&mut self) -> Result<(), SimBreak> {
        self.data = self.data.rotate_right(4);
        self.set_flag(PROC_FLAG_Z, self.data == 0);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, false);
        self.set_flag(PROC_FLAG_C, false);
        Ok(())
    }

    fn exec_read(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let addr = Addr::from(self.addr);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_read_at_pc(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let addr = Addr::from(self.pc);
        self.pc = self.pc.wrapping_add(1);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_sbc(&mut self) -> Result<(), SimBreak> {
        let lhs = i16::from(self.reg_a);
        let rhs = i16::from(self.data);
        let carry = i16::from(self.get_flag(PROC_FLAG_C));
        self.set_flag(PROC_FLAG_H, (lhs & 0xf) - (rhs & 0xf) - carry < 0);
        let result = lhs - rhs - carry;
        self.set_flag(PROC_FLAG_C, result < 0);
        self.reg_a = result as u8;
        self.set_flag(PROC_FLAG_Z, self.reg_a == 0);
        self.set_flag(PROC_FLAG_N, true);
        Ok(())
    }

    fn exec_set_reg(
        &mut self,
        bus: &mut dyn SimBus,
        reg: Reg8,
    ) -> Result<(), SimBreak> {
        match reg {
            Reg8::A => {
                self.reg_a = self.data;
                return Ok(());
            }
            Reg8::B => {
                self.reg_b = self.data;
                return Ok(());
            }
            Reg8::C => {
                self.reg_c = self.data;
                return Ok(());
            }
            Reg8::D => {
                self.reg_d = self.data;
                return Ok(());
            }
            Reg8::E => {
                self.reg_e = self.data;
                return Ok(());
            }
            Reg8::H => {
                self.reg_h = self.data;
                return Ok(());
            }
            Reg8::L => {
                self.reg_l = self.data;
                return Ok(());
            }
            Reg8::Mbc => self.addr = self.get_r16(Reg16::Bc),
            Reg8::Mde => self.addr = self.get_r16(Reg16::De),
            Reg8::Mhl => self.addr = self.get_r16(Reg16::Hl),
        }
        self.microcode.push(Microcode::Write2);
        watch(bus, Addr::from(self.addr), WatchKind::Write)
    }

    fn exec_set_reg_hi(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        match reg {
            Reg16::Af => self.reg_a = self.data,
            Reg16::Bc => self.reg_b = self.data,
            Reg16::De => self.reg_d = self.data,
            Reg16::Hl => self.reg_h = self.data,
            Reg16::Sp => self.sp = pack(self.data, self.sp as u8),
        }
        Ok(())
    }

    fn exec_set_reg_lo(&mut self, reg: Reg16) -> Result<(), SimBreak> {
        match reg {
            Reg16::Af => self.reg_f = self.data,
            Reg16::Bc => self.reg_c = self.data,
            Reg16::De => self.reg_e = self.data,
            Reg16::Hl => self.reg_l = self.data,
            Reg16::Sp => self.sp = (self.sp & 0xff00) | u16::from(self.data),
        }
        Ok(())
    }

    fn exec_set_temp(&mut self) -> Result<(), SimBreak> {
        self.temp = self.data;
        Ok(())
    }

    fn exec_write(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.microcode.push(Microcode::Write2);
        watch(bus, Addr::from(self.addr), WatchKind::Write)
    }

    fn exec_write2(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        bus.write_byte(Addr::from(self.addr), self.data);
        Ok(())
    }

    fn exec_xor(&mut self) -> Result<(), SimBreak> {
        self.reg_a ^= self.data;
        self.set_flag(PROC_FLAG_Z, self.reg_a == 0);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, false);
        self.set_flag(PROC_FLAG_C, false);
        Ok(())
    }

    fn decode_op_adc_a_i8(&mut self) {
        self.microcode.push(Microcode::Adc);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_adc_a_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::Adc);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn decode_op_add_hl_r16(&mut self, reg: Reg16) {
        let lhs = u32::from(self.get_r16(Reg16::Hl));
        let rhs = u32::from(self.get_r16(reg));
        let result = lhs + rhs;
        self.set_r16(Reg16::Hl, result as u16);
        self.set_flag(PROC_FLAG_N, false);
        self.set_flag(PROC_FLAG_H, (lhs & 0xfff) + (rhs & 0xfff) > 0xfff);
        self.set_flag(PROC_FLAG_C, result > 0xffff);
    }

    fn decode_op_add_a_i8(&mut self) {
        self.set_flag(PROC_FLAG_C, false);
        self.decode_op_adc_a_i8();
    }

    fn decode_op_add_a_r8(&mut self, reg: Reg8) {
        self.set_flag(PROC_FLAG_C, false);
        self.decode_op_adc_a_r8(reg);
    }

    fn decode_op_add_sp_i8(&mut self) {
        self.microcode.push(Microcode::OffsetSp(Reg16::Sp));
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_and_a_i8(&mut self) {
        self.microcode.push(Microcode::And);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_and_a_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::And);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn decode_op_call(&mut self, cond: Condition) {
        self.microcode.push(Microcode::Call(cond));
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::ReadAtPc);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ccf(&mut self) {
        self.reg_f &= !(PROC_FLAG_N | PROC_FLAG_H);
        self.reg_f ^= PROC_FLAG_C;
    }

    fn decode_op_cp_a_i8(&mut self) {
        self.microcode.push(Microcode::Compare);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_cp_a_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::Compare);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn decode_op_cpl(&mut self) {
        self.reg_a = !self.reg_a;
        self.reg_f |= PROC_FLAG_N | PROC_FLAG_H;
    }

    fn decode_op_daa(&mut self) {
        let mut adjust: i16 = 0;
        if self.get_flag(PROC_FLAG_N) {
            if self.get_flag(PROC_FLAG_H) {
                adjust += 0x06;
            }
            if self.get_flag(PROC_FLAG_C) {
                adjust += 0x60;
            }
            let result = i16::from(self.reg_a) - adjust;
            self.set_flag(PROC_FLAG_C, result < 0);
            self.reg_a = result as u8;
        } else {
            if self.get_flag(PROC_FLAG_H) || (self.reg_a & 0x0f) > 0x09 {
                adjust += 0x06;
            }
            if self.get_flag(PROC_FLAG_C) || self.reg_a > 0x99 {
                adjust += 0x60;
                self.set_flag(PROC_FLAG_C, true)
            }
            if self.get_flag(PROC_FLAG_C) {
                adjust += 1;
            }
            let result = i16::from(self.reg_a) + adjust;
            self.set_flag(PROC_FLAG_C, result > 0xff);
            self.reg_a = result as u8;
        }
        self.set_flag(PROC_FLAG_Z, self.reg_a == 0);
        self.set_flag(PROC_FLAG_H, false);
    }

    fn decode_op_dec_r16(&mut self, reg: Reg16) {
        self.set_r16(reg, self.get_r16(reg).wrapping_sub(1));
    }

    fn decode_op_dec_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::SetReg(reg));
        self.microcode.push(Microcode::Decrement);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn decode_op_di(&mut self) {
        self.ime = Ime::Disabled;
    }

    fn decode_op_ei(&mut self) {
        self.ime = Ime::Pending2;
    }

    fn decode_op_inc_r16(&mut self, reg: Reg16) {
        self.set_r16(reg, self.get_r16(reg).wrapping_add(1));
    }

    fn decode_op_inc_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::SetReg(reg));
        self.microcode.push(Microcode::Increment);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn op_invalid(&mut self, opcode: u8) -> Result<(), SimBreak> {
        // Invalid instructions hard-lock the processor until power off.
        // See https://gbdev.io/pandocs/CPU_Instruction_Set.html and
        // https://gist.github.com/SonoSooS/c0055300670d678b5ae8433e20bea595
        self.pc = self.pc.wrapping_sub(1); // keep PC at invalid instruction
        Err(SimBreak::HaltOpcode("invalid", opcode))
    }

    fn decode_op_jp_i16(&mut self, cond: Condition) {
        self.microcode.push(Microcode::JumpAbs(cond));
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::ReadAtPc);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_jp_hl(&mut self) {
        self.pc = self.get_r16(Reg16::Hl);
    }

    fn decode_op_jr_i8(&mut self, cond: Condition) {
        self.microcode.push(Microcode::JumpRel(cond));
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ld_a_m16(&mut self) {
        self.microcode.push(Microcode::SetReg(Reg8::A));
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::ReadAtPc);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ld_a_mhld(&mut self) {
        self.microcode.push(Microcode::SetReg(Reg8::A));
        self.microcode.push(Microcode::Read);
        self.addr = self.get_r16(Reg16::Hl);
        self.set_r16(Reg16::Hl, self.addr.wrapping_sub(1));
    }

    fn decode_op_ld_a_mhli(&mut self) {
        self.microcode.push(Microcode::SetReg(Reg8::A));
        self.microcode.push(Microcode::Read);
        self.addr = self.get_r16(Reg16::Hl);
        self.set_r16(Reg16::Hl, self.addr.wrapping_add(1));
    }

    fn decode_op_ld_hl_sp_i8(&mut self) {
        self.microcode.push(Microcode::OffsetSp(Reg16::Hl));
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ld_m16_a(&mut self) {
        self.microcode.push(Microcode::Write);
        self.microcode.push(Microcode::GetReg(Reg8::A));
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::ReadAtPc);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ld_mhld_a(&mut self) {
        self.microcode.push(Microcode::Write);
        self.data = self.reg_a;
        self.addr = self.get_r16(Reg16::Hl);
        self.set_r16(Reg16::Hl, self.addr.wrapping_sub(1));
    }

    fn decode_op_ld_mhli_a(&mut self) {
        self.microcode.push(Microcode::Write);
        self.data = self.reg_a;
        self.addr = self.get_r16(Reg16::Hl);
        self.set_r16(Reg16::Hl, self.addr.wrapping_add(1));
    }

    fn decode_op_ld_r8_r8(&mut self, dst: Reg8, src: Reg8) {
        self.microcode.push(Microcode::SetReg(dst));
        self.microcode.push(Microcode::GetReg(src));
    }

    fn decode_op_ld_r8_i8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::SetReg(reg));
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ld_r16_i16(&mut self, reg: Reg16) {
        self.microcode.push(Microcode::SetRegHi(reg));
        self.microcode.push(Microcode::ReadAtPc);
        self.microcode.push(Microcode::SetRegLo(reg));
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ld_sp_hl(&mut self) {
        self.sp = pack(self.reg_h, self.reg_l);
    }

    fn decode_op_ldh_a_mc(&mut self) {
        self.microcode.push(Microcode::SetReg(Reg8::A));
        self.microcode.push(Microcode::Read);
        self.addr = pack(0xff, self.reg_c);
    }

    fn decode_op_ldh_a_m8(&mut self) {
        self.microcode.push(Microcode::SetReg(Reg8::A));
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::MakeAddrHp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_ldh_mc_a(&mut self) {
        self.microcode.push(Microcode::Write);
        self.addr = pack(0xff, self.reg_c);
        self.data = self.reg_a;
    }

    fn decode_op_ldh_m8_a(&mut self) {
        self.microcode.push(Microcode::Write);
        self.microcode.push(Microcode::GetReg(Reg8::A));
        self.microcode.push(Microcode::MakeAddrHp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_or_a_i8(&mut self) {
        self.microcode.push(Microcode::Or);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_or_a_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::Or);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn decode_op_pop_r16(&mut self, reg: Reg16) {
        self.microcode.push(Microcode::SetRegHi(reg));
        self.microcode.push(Microcode::Pop);
        self.microcode.push(Microcode::SetRegLo(reg));
        self.microcode.push(Microcode::Pop);
    }

    fn decode_op_prefix(&mut self) {
        self.microcode.push(Microcode::DecodePrefixed);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_push_r16(&mut self, reg: Reg16) {
        self.microcode.push(Microcode::Push);
        self.microcode.push(Microcode::GetRegLo(reg));
        self.microcode.push(Microcode::Push);
        self.microcode.push(Microcode::GetRegHi(reg));
    }

    fn decode_op_ret(&mut self, cond: Condition) {
        if self.condition_met(cond) {
            self.microcode.push(Microcode::JumpAbs(Condition::Always));
            self.microcode.push(Microcode::MakeAddrAbs);
            self.microcode.push(Microcode::Pop);
            self.microcode.push(Microcode::SetTemp);
            self.microcode.push(Microcode::Pop);
        }
    }

    fn decode_op_reti(&mut self) {
        self.microcode.push(Microcode::JumpAbs(Condition::Always));
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::Pop);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Pop);
        self.ime = Ime::Pending1;
    }

    fn decode_op_rst(&mut self, zp: u8) {
        self.microcode.push(Microcode::JumpTo(u16::from(zp)));
        self.microcode.push(Microcode::Push);
        self.microcode.push(Microcode::GetPcLo);
        self.microcode.push(Microcode::Push);
        self.microcode.push(Microcode::GetPcHi);
    }

    fn decode_op_sbc_a_i8(&mut self) {
        self.microcode.push(Microcode::Sbc);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_sbc_a_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::Sbc);
        self.microcode.push(Microcode::GetReg(reg));
    }

    fn decode_op_scf(&mut self) {
        self.reg_f &= !(PROC_FLAG_N | PROC_FLAG_H);
        self.reg_f |= PROC_FLAG_C;
    }

    fn decode_op_sub_a_i8(&mut self) {
        self.set_flag(PROC_FLAG_C, false);
        self.decode_op_sbc_a_i8();
    }

    fn decode_op_sub_a_r8(&mut self, reg: Reg8) {
        self.set_flag(PROC_FLAG_C, false);
        self.decode_op_sbc_a_r8(reg);
    }

    fn decode_op_xor_a_i8(&mut self) {
        self.microcode.push(Microcode::Xor);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_xor_a_r8(&mut self, reg: Reg8) {
        self.microcode.push(Microcode::Xor);
        self.microcode.push(Microcode::GetReg(reg));
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

    fn get_flag(&mut self, flag: u8) -> bool {
        (self.reg_f & flag) != 0
    }

    fn set_flag(&mut self, flag: u8, value: bool) {
        if value {
            self.reg_f |= flag;
        } else {
            self.reg_f &= !flag;
        }
    }
}

impl SimProc for SharpSm83 {
    fn description(&self) -> String {
        "Sharp SM83".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, pc: Addr) -> (u32, String) {
        let pc = pc.as_u16();
        let instruction = Instruction::decode(bus, pc);
        (instruction.size(), instruction.format(pc, bus))
    }

    fn pc(&self) -> Addr {
        Addr::from(self.pc)
    }

    fn set_pc(&mut self, addr: Addr) {
        self.pc = addr.as_u16();
        match self.ime {
            Ime::Pending1 | Ime::Pending2 => self.ime = Ime::Enabled,
            Ime::Disabled | Ime::Enabled => {}
        }
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &[
            "A", "F", "B", "C", "D", "E", "H", "L", "BC", "DE", "HL", "SP",
            "DATA",
        ]
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
            "BC" => Some(u32::from(pack(self.reg_b, self.reg_c))),
            "DE" => Some(u32::from(pack(self.reg_d, self.reg_e))),
            "HL" => Some(u32::from(pack(self.reg_h, self.reg_l))),
            "SP" => Some(u32::from(self.sp)),
            "DATA" => Some(u32::from(self.data)),
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
            "BC" => (self.reg_b, self.reg_c) = unpack((value & 0xffff) as u16),
            "DE" => (self.reg_d, self.reg_e) = unpack((value & 0xffff) as u16),
            "HL" => (self.reg_h, self.reg_l) = unpack((value & 0xffff) as u16),
            "SP" => self.sp = value as u16,
            "DATA" => self.data = value as u8,
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        if self.microcode.is_empty() {
            self.microcode.push(Microcode::DecodeOpcode);
            self.microcode.push(Microcode::ReadAtPc);
        }
        while let Some(microcode) = self.microcode.pop() {
            self.execute_microcode(bus, microcode)?;
        }
        if let Ime::Pending1 = self.ime {
            self.ime = Ime::Enabled;
        } else if let Ime::Pending2 = self.ime {
            self.ime = Ime::Pending1;
        }
        watch(bus, Addr::from(self.pc), WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        !self.microcode.is_empty()
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
