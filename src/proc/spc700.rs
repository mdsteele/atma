use super::util::{unpack, watch};
use crate::bus::{SimBus, WatchKind};
use crate::dis::spc700::{AddrMode, Instruction, Operation, Reg};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

const VECTOR_BREAK: u16 = 0xffde;
const VECTOR_RESET: u16 = 0xfffe;

const PROC_FLAG_N: u8 = 0b1000_0000;
const PROC_FLAG_V: u8 = 0b0100_0000;
const PROC_FLAG_P: u8 = 0b0010_0000;
const PROC_FLAG_B: u8 = 0b0001_0000;
const PROC_FLAG_H: u8 = 0b0000_1000;
const PROC_FLAG_I: u8 = 0b0000_0100;
const PROC_FLAG_Z: u8 = 0b0000_0010;
const PROC_FLAG_C: u8 = 0b0000_0001;

//===========================================================================//

#[derive(Clone, Copy)]
enum Microcode {
    Call,         // push new microcode to call ADDR
    DecodeOpcode, // decode DATA as opcode, push new microcode
    FinishWrite,  // [ADDR] = DATA
    GetReg(Reg),  // DATA = reg
    GetTemp,      // DATA = TEMP
    IncAddr,      // ADDR += 1
    IncAddrLo,    // ADDR = (ADDR & 0xff00) | ((ADDR + 1) & 0x00ff)
    Index(Reg),   // ADDR += reg
    IndexLo(Reg), // ADDR = (ADDR & 0xff00) | ((ADDR + reg) & 0x00ff)
    Jump,         // PC = ADDR
    JumpTo(u16),  // PC = value
    MakeAddrAbs,  // ADDR = (DATA << 8) | TEMP
    MakeAddrDp,   // ADDR = (P << 8) | DATA
    MakeAddrHp,   // ADDR = 0xff00 | DATA
    MakeAddrImm,  // ADDR = PC, PC += 1
    Pop,          // SP += 1, ADDR = SP, DATA = [ADDR], watch(ADDR, Write)
    Push(u8),     // DATA = value, ADDR = SP--, exec_write
    Read,         // DATA = [ADDR], watch(ADDR, Read)
    ReadAtPc,     // DATA = [PC], watch(PC, Read), PC += 1
    SetReg(Reg),  // reg = DATA
    SetTemp,      // TEMP = DATA
    UpdateNz,     // update NZ flags from DATA
    VectorBreak,  // ADDR = VECTOR_BREAK, set B, clear I
    Write,        // watch(ADDR, Write), push FinishWrite microcode
}

//===========================================================================//

/// A simulated SPC-700 processor.
pub struct Spc700 {
    pc: u16,
    addr: u16,
    data: u8,
    temp: u8,
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    reg_sp: u8,
    reg_psw: u8,
    microcode: Vec<Microcode>,
}

impl Default for Spc700 {
    fn default() -> Spc700 {
        Spc700::new()
    }
}

impl Spc700 {
    /// Returns a new simulated SPC-700 processor.
    pub fn new() -> Spc700 {
        Spc700 {
            pc: 0,
            addr: VECTOR_RESET,
            data: 0,
            temp: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            reg_sp: 0,
            reg_psw: 0,
            microcode: vec![
                Microcode::Jump,
                Microcode::MakeAddrAbs,
                Microcode::Read,
                Microcode::IncAddr,
                Microcode::SetTemp,
                Microcode::Read,
            ],
        }
    }

    fn execute_microcode(
        &mut self,
        bus: &mut dyn SimBus,
        microcode: Microcode,
    ) -> Result<(), SimBreak> {
        match microcode {
            Microcode::Call => self.exec_call(),
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::FinishWrite => self.exec_finish_write(bus),
            Microcode::GetReg(reg) => self.exec_get_reg(reg),
            Microcode::GetTemp => self.exec_get_temp(),
            Microcode::IncAddr => self.exec_inc_addr(),
            Microcode::IncAddrLo => self.exec_inc_addr_lo(),
            Microcode::Index(reg) => self.exec_index(reg),
            Microcode::IndexLo(reg) => self.exec_index_lo(reg),
            Microcode::Jump => self.exec_jump(),
            Microcode::JumpTo(addr) => self.exec_jump_to(addr),
            Microcode::MakeAddrAbs => self.exec_make_addr_abs(),
            Microcode::MakeAddrDp => self.exec_make_addr_dp(),
            Microcode::MakeAddrHp => self.exec_make_addr_hp(),
            Microcode::MakeAddrImm => self.exec_make_addr_imm(),
            Microcode::Pop => self.exec_pop(bus),
            Microcode::Push(value) => self.exec_push(bus, value),
            Microcode::Read => self.exec_read(bus),
            Microcode::ReadAtPc => self.exec_read_at_pc(bus),
            Microcode::SetReg(reg) => self.exec_set_reg(reg),
            Microcode::SetTemp => self.exec_set_temp(),
            Microcode::UpdateNz => self.exec_update_nz(),
            Microcode::VectorBreak => self.exec_vector_break(),
            Microcode::Write => self.exec_write(bus),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let opcode = self.data;
        let operation = Operation::from_opcode(opcode);
        match operation {
            Operation::Brk => self.decode_op_brk(),
            Operation::Call(mode) => self.decode_op_call(mode),
            Operation::Clrc => self.decode_op_clrc(),
            Operation::Clrp => self.decode_op_clrp(),
            Operation::Clrv => self.decode_op_clrv(),
            Operation::Di => self.decode_op_di(),
            Operation::Div => self.decode_op_div(),
            Operation::Ei => self.decode_op_ei(),
            Operation::Jmp(mode) => self.decode_op_jmp(mode),
            Operation::MovAddrAddr(d, s) => self.decode_op_mov_addr_addr(d, s),
            Operation::MovAddrReg(a, r) => self.decode_op_mov_addr_reg(a, r),
            Operation::MovRegAddr(r, a) => self.decode_op_mov_reg_addr(r, a),
            Operation::MovRegReg(d, s) => self.decode_op_mov_reg_reg(d, s),
            Operation::Mul => self.decode_op_mul(),
            Operation::Nop => {}
            Operation::Notc => self.decode_op_notc(),
            Operation::Pcall(mode) => self.decode_op_pcall(mode),
            Operation::Pop(reg) => self.decode_op_pop(reg),
            Operation::Push(reg) => self.decode_op_push(reg),
            Operation::Setc => self.decode_op_setc(),
            Operation::Setp => self.decode_op_setp(),
            Operation::Sleep => {
                return Err(SimBreak::HaltOpcode("SLEEP", opcode));
            }
            Operation::Stop => {
                return Err(SimBreak::HaltOpcode("STOP", opcode));
            }
            Operation::Tcall(hp) => self.decode_op_tcall(hp),
            Operation::Xcn => self.decode_op_xcn(),
            _ => todo!("operation={operation:?}"),
        }
        Ok(())
    }

    fn decode_addr_mode(&mut self, addr_mode: AddrMode) {
        match addr_mode {
            AddrMode::Immediate | AddrMode::Relative => {
                self.microcode.push(Microcode::MakeAddrImm);
            }
            AddrMode::Absolute => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::XIndexedAbsolute => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddr);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::Index(Reg::X));
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::YIndexedAbsolute => {
                self.microcode.push(Microcode::Index(Reg::Y));
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::XIndexedAbsoluteIndirect => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddrLo);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::Index(Reg::X));
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::HighPage => {
                self.microcode.push(Microcode::MakeAddrHp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::DirectPage => {
                self.microcode.push(Microcode::MakeAddrDp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::DirectPageX => {
                self.microcode.push(Microcode::MakeAddrDp);
                self.microcode.push(Microcode::GetReg(Reg::X));
            }
            AddrMode::DirectPageXInc => todo!(),
            AddrMode::XIndexedDirectPage => {
                self.microcode.push(Microcode::IndexLo(Reg::X));
                self.microcode.push(Microcode::MakeAddrDp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::YIndexedDirectPage => {
                self.microcode.push(Microcode::IndexLo(Reg::Y));
                self.microcode.push(Microcode::MakeAddrDp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::XIndexedDirectPageIndirect => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddrLo);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IndexLo(Reg::X));
                self.microcode.push(Microcode::MakeAddrDp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::DirectPageIndirectYIndexed => {
                self.microcode.push(Microcode::Index(Reg::Y));
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddrLo);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::MakeAddrDp);
                self.microcode.push(Microcode::ReadAtPc);
            }
        }
    }

    fn exec_call(&mut self) -> Result<(), SimBreak> {
        self.microcode.push(Microcode::JumpTo(self.addr));
        self.microcode.push(Microcode::Push(self.pc as u8));
        self.microcode.push(Microcode::Push((self.pc >> 8) as u8));
        Ok(())
    }

    fn exec_finish_write(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        bus.write_byte(u32::from(self.addr), self.data);
        Ok(())
    }

    fn exec_get_reg(&mut self, reg: Reg) -> Result<(), SimBreak> {
        self.data = self.get_reg(reg);
        Ok(())
    }

    fn exec_get_temp(&mut self) -> Result<(), SimBreak> {
        self.data = self.temp;
        Ok(())
    }

    fn exec_inc_addr(&mut self) -> Result<(), SimBreak> {
        self.addr = self.addr.wrapping_add(1);
        Ok(())
    }

    fn exec_inc_addr_lo(&mut self) -> Result<(), SimBreak> {
        self.addr = (self.addr & 0xff00) | (self.addr.wrapping_add(1) & 0xff);
        Ok(())
    }

    fn exec_index(&mut self, reg: Reg) -> Result<(), SimBreak> {
        self.addr = self.addr.wrapping_add(u16::from(self.get_reg(reg)));
        Ok(())
    }

    fn exec_index_lo(&mut self, reg: Reg) -> Result<(), SimBreak> {
        let lo = (self.addr as u8).wrapping_add(self.get_reg(reg));
        self.addr = (self.addr & 0xff00) | u16::from(lo);
        Ok(())
    }

    fn exec_jump(&mut self) -> Result<(), SimBreak> {
        self.pc = self.addr;
        Ok(())
    }

    fn exec_jump_to(&mut self, addr: u16) -> Result<(), SimBreak> {
        self.pc = addr;
        Ok(())
    }

    fn exec_make_addr_abs(&mut self) -> Result<(), SimBreak> {
        self.addr = (u16::from(self.data) << 8) | u16::from(self.temp);
        Ok(())
    }

    fn exec_make_addr_dp(&mut self) -> Result<(), SimBreak> {
        self.addr = u16::from(self.data);
        if self.get_flag(PROC_FLAG_P) {
            self.addr |= 0x0100;
        }
        Ok(())
    }

    fn exec_make_addr_hp(&mut self) -> Result<(), SimBreak> {
        self.addr = 0xff00 | u16::from(self.data);
        Ok(())
    }

    fn exec_make_addr_imm(&mut self) -> Result<(), SimBreak> {
        self.addr = self.pc;
        self.pc = self.pc.wrapping_add(1);
        Ok(())
    }

    fn exec_pop(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.reg_sp = self.reg_sp.wrapping_add(1);
        self.addr = 0x0100 | u16::from(self.reg_sp);
        self.exec_read(bus)
    }

    fn exec_push(&mut self, bus: &mut dyn SimBus, value: u8) -> Result<(), SimBreak> {
        self.data = value;
        self.addr = 0x0100 | u16::from(self.reg_sp);
        self.reg_sp = self.reg_sp.wrapping_sub(1);
        self.exec_write(bus)
    }

    fn exec_read(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        let addr = u32::from(self.addr);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_read_at_pc(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let addr = u32::from(self.pc);
        self.pc = self.pc.wrapping_add(1);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_set_reg(&mut self, reg: Reg) -> Result<(), SimBreak> {
        self.set_reg(reg, self.data);
        Ok(())
    }

    fn exec_set_temp(&mut self) -> Result<(), SimBreak> {
        self.temp = self.data;
        Ok(())
    }

    fn exec_update_nz(&mut self) -> Result<(), SimBreak> {
        self.update_nz_flags(self.data);
        Ok(())
    }

    fn exec_vector_break(&mut self) -> Result<(), SimBreak> {
        self.addr = VECTOR_BREAK;
        self.set_flag(PROC_FLAG_I, false);
        self.set_flag(PROC_FLAG_B, true);
        Ok(())
    }

    fn exec_write(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.microcode.push(Microcode::FinishWrite);
        watch(bus, u32::from(self.addr), WatchKind::Write)
    }

    fn decode_op_brk(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::IncAddr);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::VectorBreak);
        self.microcode.push(Microcode::Push(self.reg_psw));
        self.microcode.push(Microcode::Push(self.pc as u8));
        self.microcode.push(Microcode::Push((self.pc >> 8) as u8));
    }

    fn decode_op_call(&mut self, mode: AddrMode) {
        self.microcode.push(Microcode::Call);
        self.decode_addr_mode(mode);
    }

    fn decode_op_clrc(&mut self) {
        self.set_flag(PROC_FLAG_C, false);
    }

    fn decode_op_clrp(&mut self) {
        self.set_flag(PROC_FLAG_P, false);
    }

    fn decode_op_clrv(&mut self) {
        self.set_flag(PROC_FLAG_V, false);
    }

    fn decode_op_di(&mut self) {
        self.set_flag(PROC_FLAG_I, false);
    }

    fn decode_op_div(&mut self) {
        // This implementation comes from "Anomie's SPC700 Doc".
        self.set_flag(PROC_FLAG_H, (self.reg_x & 0x0f) <= (self.reg_y & 0x0f));
        let mut yva = (u32::from(self.reg_y) << 8) | u32::from(self.reg_a);
        let x = u32::from(self.reg_x);
        for _ in 0..9 {
            yva <<= 1;
            if (yva & 0x20000) != 0 {
                yva = (yva | 1) & 0x1ffff;
            }
            if yva >= x {
                yva ^= 1;
            }
            if (yva & 1) != 0 {
                yva = yva.wrapping_sub(x) & 0x1ffff;
            }
        }
        self.reg_a = yva as u8;
        self.set_flag(PROC_FLAG_V, (yva & 0x100) != 0);
        self.reg_y = (yva >> 9) as u8;
        self.data = self.reg_y;
        self.update_nz_flags(self.data);
    }

    fn decode_op_ei(&mut self) {
        self.set_flag(PROC_FLAG_I, true);
    }

    fn decode_op_jmp(&mut self, mode: AddrMode) {
        self.microcode.push(Microcode::Jump);
        self.decode_addr_mode(mode);
    }

    fn decode_op_mov_addr_addr(
        &mut self,
        dst_mode: AddrMode,
        src_mode: AddrMode,
    ) {
        self.microcode.push(Microcode::Write);
        self.microcode.push(Microcode::GetTemp);
        self.decode_addr_mode(dst_mode);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(src_mode);
    }

    fn decode_op_mov_addr_reg(&mut self, dst_mode: AddrMode, src_reg: Reg) {
        self.microcode.push(Microcode::Write);
        self.microcode.push(Microcode::GetReg(src_reg));
        self.decode_addr_mode(dst_mode);
    }

    fn decode_op_mov_reg_addr(&mut self, dst_reg: Reg, src_mode: AddrMode) {
        self.microcode.push(Microcode::SetReg(dst_reg));
        self.microcode.push(Microcode::UpdateNz);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(src_mode);
    }

    fn decode_op_mov_reg_reg(&mut self, dst_reg: Reg, src_reg: Reg) {
        self.data = self.get_reg(src_reg);
        if dst_reg != Reg::Sp {
            self.update_nz_flags(self.data);
        }
        self.set_reg(dst_reg, self.data);
    }

    fn decode_op_mul(&mut self) {
        let product = u16::from(self.reg_y) * u16::from(self.reg_a);
        (self.reg_y, self.reg_a) = unpack(product);
        self.data = self.reg_y;
        self.update_nz_flags(self.data);
    }

    fn decode_op_notc(&mut self) {
        self.reg_psw ^= PROC_FLAG_C;
    }

    fn decode_op_pcall(&mut self, mode: AddrMode) {
        self.decode_op_call(mode);
    }

    fn decode_op_pop(&mut self, reg: Reg) {
        self.microcode.push(Microcode::SetReg(reg));
        self.microcode.push(Microcode::Pop);
    }

    fn decode_op_push(&mut self, reg: Reg) {
        self.microcode.push(Microcode::Push(self.get_reg(reg)));
    }

    fn decode_op_setc(&mut self) {
        self.set_flag(PROC_FLAG_C, true);
    }

    fn decode_op_setp(&mut self) {
        self.set_flag(PROC_FLAG_P, true);
    }

    fn decode_op_tcall(&mut self, hp: u8) {
        self.microcode.push(Microcode::Call);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::IncAddr);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Read);
        self.addr = 0xff00 | u16::from(hp);
    }

    fn decode_op_xcn(&mut self) {
        self.data = self.reg_a.rotate_right(4);
        self.update_nz_flags(self.data);
        self.reg_a = self.data;
    }

    fn get_reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::A => self.reg_a,
            Reg::X => self.reg_x,
            Reg::Y => self.reg_y,
            Reg::Sp => self.reg_sp,
            Reg::Psw => self.reg_psw,
        }
    }

    fn set_reg(&mut self, reg: Reg, value: u8) {
        match reg {
            Reg::A => self.reg_a = value,
            Reg::X => self.reg_x = value,
            Reg::Y => self.reg_y = value,
            Reg::Sp => self.reg_sp = value,
            Reg::Psw => self.reg_psw = value,
        }
    }

    fn get_flag(&self, flag: u8) -> bool {
        (self.reg_psw & flag) != 0
    }

    fn set_flag(&mut self, flag: u8, value: bool) {
        if value {
            self.reg_psw |= flag;
        } else {
            self.reg_psw &= !flag;
        }
    }

    fn update_nz_flags(&mut self, value: u8) {
        self.reg_psw &= !(PROC_FLAG_N | PROC_FLAG_Z);
        if value == 0 {
            self.reg_psw |= PROC_FLAG_Z;
        }
        if value >= 0x80 {
            self.reg_psw |= PROC_FLAG_N;
        }
    }
}

impl SimProc for Spc700 {
    fn description(&self) -> String {
        "SPC-700".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, pc: u32) -> (u32, String) {
        let pc = pc as u16;
        let instruction = Instruction::decode(bus, pc);
        (instruction.size(), instruction.format(bus, pc))
    }

    fn pc(&self) -> u32 {
        self.pc as u32
    }

    fn set_pc(&mut self, addr: u32) {
        self.pc = addr as u16;
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["A", "X", "Y", "SP", "PSW", "DATA"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "A" => Some(u32::from(self.reg_a)),
            "X" => Some(u32::from(self.reg_x)),
            "Y" => Some(u32::from(self.reg_y)),
            "SP" => Some(u32::from(self.reg_sp)),
            "PSW" => Some(u32::from(self.reg_psw)),
            "DATA" => Some(u32::from(self.data)),
            _ => None,
        }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        match name {
            "A" => self.reg_a = value as u8,
            "X" => self.reg_x = value as u8,
            "Y" => self.reg_y = value as u8,
            "SP" => self.reg_sp = value as u8,
            "PSW" => self.reg_psw = value as u8,
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
        watch(bus, u32::from(self.pc), WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        !self.microcode.is_empty()
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{SimProc, Spc700};

    #[test]
    fn get_registers() {
        let proc = Spc700::new();
        for &register in proc.register_names() {
            assert!(proc.get_register(register).is_some());
        }
    }

    #[test]
    fn set_registers() {
        let mut proc = Spc700::new();
        for &register in proc.register_names() {
            proc.set_register(register, 0x12);
            assert_eq!(proc.get_register(register), Some(0x12));
            proc.set_register(register, 0xcd);
            assert_eq!(proc.get_register(register), Some(0xcd));
        }
    }
}

//===========================================================================//
