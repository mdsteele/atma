use super::util::watch;
use crate::bus::{SimBus, WatchKind};
use crate::dis::w65c816::{AddrMode, Instruction, Mnemonic, Operation};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

const VECTOR_EMULATED_RESET: u32 = 0x00fffc;

const PROC_FLAG_N: u8 = 0b1000_0000;
const PROC_FLAG_V: u8 = 0b0100_0000;
const PROC_FLAG_M: u8 = 0b0010_0000;
const PROC_FLAG_X: u8 = 0b0001_0000;
const PROC_FLAG_D: u8 = 0b0000_1000;
const PROC_FLAG_I: u8 = 0b0000_0100;
const PROC_FLAG_Z: u8 = 0b0000_0010;
const PROC_FLAG_C: u8 = 0b0000_0001;

//===========================================================================//

#[derive(Clone, Copy)]
enum BankReg {
    Program,
    Data,
}

//===========================================================================//

#[derive(Clone, Copy)]
enum Microcode {
    DecodeOpcode,         // decode DATA as opcode, push new microcode
    FinishWrite,          // [ADDR] = DATA
    GetZero,              // DATA = 0
    IncAddr,              // ADDR += 1
    Jump,                 // PC = ADDR
    MakeAddrAbs(BankReg), // ADDR = (bank << 16) | (DATA << 8) | TEMP0
    MakeAddrDirect,       // ADDR = D + DATA
    MakeAddrLong,         // ADDR = (DATA << 16) | (TEMP1 << 8) | TEMP0
    ReadAtPc,             // DATA = [PC], watch(PC, Read), PC16 += 1
    ReadByte,             // DATA = [ADDR], watch(ADDR, Read)
    ReadIndex,            // DATA = [ADDR], watch(ADDR, Read), push microcode
    SetRegX,              // X = (DATA << 8) | TEMP0
    SetRegY,              // Y = (DATA << 8) | TEMP0
    SetTemp0,             // TEMP0 = DATA
    SetTemp1,             // TEMP1 = DATA
    Write,                // watch(ADDR, Write), push FinishWrite microcode
}

//===========================================================================//

/// A simulated WDC 65C816 processor.
pub struct Wdc65c816 {
    pc16: u16,  // lower 16 bits of the 24-bit PC
    pbr: u8,    // Program Bank Register (upper 8 bits of the 24-bit PC)
    dbr: u8,    // Data Bank Register
    reg_a: u16, // Accumulator
    reg_d: u16, // Direct page register
    reg_p: u8,  // Processor status register
    reg_s: u16, // Stack pointer
    reg_x: u16, // X index register
    reg_y: u16, // Y index register
    emulation: bool,
    data: u8,
    temp0: u8,
    temp1: u8,
    addr: u32,
    microcode: Vec<Microcode>,
}

impl Default for Wdc65c816 {
    fn default() -> Wdc65c816 {
        Wdc65c816::new()
    }
}

impl Wdc65c816 {
    /// Returns a new simulated WDC 65C816 processor.
    pub fn new() -> Wdc65c816 {
        // See W65C816S datasheet, section 2.25 for initialization values.  In
        // particular:
        // * D, DBR, PBR are all initialized to zero.
        // * Emulation mode is enabled, which means that the high byte of S is
        //   forced to 0x01 and the high bytes of X and Y are forced to zero.
        // * The M, X, I, and C flags are set, and the D flag is cleared.
        Wdc65c816 {
            pc16: 0,
            pbr: 0,
            dbr: 0,
            reg_a: 0,
            reg_d: 0,
            reg_p: PROC_FLAG_M | PROC_FLAG_X | PROC_FLAG_I | PROC_FLAG_C,
            reg_s: 0x0100,
            reg_x: 0,
            reg_y: 0,
            emulation: true,
            data: 0,
            temp0: 0,
            temp1: 0,
            addr: VECTOR_EMULATED_RESET,
            microcode: vec![
                Microcode::Jump,
                Microcode::MakeAddrAbs(BankReg::Program),
                Microcode::ReadByte,
                Microcode::IncAddr,
                Microcode::SetTemp0,
                Microcode::ReadByte,
            ],
        }
    }

    fn execute_microcode(
        &mut self,
        bus: &mut dyn SimBus,
        microcode: Microcode,
    ) -> Result<(), SimBreak> {
        match microcode {
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::FinishWrite => self.exec_finish_write(bus),
            Microcode::GetZero => self.exec_get_zero(),
            Microcode::IncAddr => self.exec_inc_addr(),
            Microcode::Jump => self.exec_jump(),
            Microcode::MakeAddrAbs(reg) => self.exec_make_addr_abs(reg),
            Microcode::MakeAddrDirect => self.exec_make_addr_direct(),
            Microcode::MakeAddrLong => self.exec_make_addr_long(),
            Microcode::ReadAtPc => self.exec_read_at_pc(bus),
            Microcode::ReadByte => self.exec_read_byte(bus),
            Microcode::ReadIndex => self.exec_read_index(bus),
            Microcode::SetRegX => self.exec_set_reg_x(),
            Microcode::SetRegY => self.exec_set_reg_y(),
            Microcode::SetTemp0 => self.exec_set_temp0(),
            Microcode::SetTemp1 => self.exec_set_temp1(),
            Microcode::Write => self.exec_write(bus),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let flag_m = self.get_flag(PROC_FLAG_M);
        let flag_x = self.get_flag(PROC_FLAG_X);
        let operation = Operation::from_opcode(self.data, flag_m, flag_x);
        let addr_mode = operation.addr_mode;
        match operation.mnemonic {
            Mnemonic::Clc => self.decode_op_clc(),
            Mnemonic::Cld => self.decode_op_cld(),
            Mnemonic::Clv => self.decode_op_clv(),
            Mnemonic::Jmp => self.decode_op_jmp(addr_mode),
            Mnemonic::Ldx => self.decode_op_ldx(addr_mode),
            Mnemonic::Ldy => self.decode_op_ldy(addr_mode),
            Mnemonic::Sec => self.decode_op_sec(),
            Mnemonic::Sed => self.decode_op_sed(),
            Mnemonic::Stz => self.decode_op_stz(addr_mode),
            Mnemonic::Xce => self.decode_op_xce(),
            _ => todo!("{}", operation.mnemonic),
        }
        Ok(())
    }

    fn decode_addr_mode(&mut self, addr_mode: AddrMode, bank_reg: BankReg) {
        match addr_mode {
            AddrMode::Implied | AddrMode::Accumulator => {}
            AddrMode::ImmediateByte | AddrMode::Relative => {
                self.addr = self.pc();
                self.pc16 = self.pc16.wrapping_add(1);
            }
            AddrMode::ImmediateWord
            | AddrMode::RelativeLong
            | AddrMode::BlockMove => {
                self.addr = self.pc();
                self.pc16 = self.pc16.wrapping_add(2);
            }
            AddrMode::Absolute => {
                self.microcode.push(Microcode::MakeAddrAbs(bank_reg));
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp0);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::AbsoluteLong => {
                self.microcode.push(Microcode::MakeAddrLong);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp1);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp0);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::DirectPage => {
                self.microcode.push(Microcode::MakeAddrDirect);
                self.microcode.push(Microcode::ReadAtPc);
            }
            _ => todo!("{addr_mode:?}"),
        }
    }

    fn exec_finish_write(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        bus.write_byte(self.addr, self.data);
        Ok(())
    }

    fn exec_get_zero(&mut self) -> Result<(), SimBreak> {
        self.data = 0;
        Ok(())
    }

    fn exec_inc_addr(&mut self) -> Result<(), SimBreak> {
        self.addr = self.addr.wrapping_add(1) & 0xffffff;
        Ok(())
    }

    fn exec_jump(&mut self) -> Result<(), SimBreak> {
        self.pc16 = self.addr as u16;
        self.pbr = (self.addr >> 16) as u8;
        Ok(())
    }

    fn exec_make_addr_abs(&mut self, reg: BankReg) -> Result<(), SimBreak> {
        let bank = match reg {
            BankReg::Program => self.pbr,
            BankReg::Data => self.dbr,
        };
        self.addr = (u32::from(bank) << 16)
            | (u32::from(self.data) << 8)
            | u32::from(self.temp0);
        Ok(())
    }

    fn exec_make_addr_direct(&mut self) -> Result<(), SimBreak> {
        self.addr = u32::from(self.reg_d.wrapping_add(u16::from(self.data)));
        Ok(())
    }

    fn exec_make_addr_long(&mut self) -> Result<(), SimBreak> {
        self.addr = (u32::from(self.data) << 16)
            | (u32::from(self.temp1) << 8)
            | u32::from(self.temp0);
        Ok(())
    }

    fn exec_read_at_pc(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let addr = self.pc();
        self.pc16 = self.pc16.wrapping_add(1);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_read_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        self.data = bus.read_byte(self.addr);
        watch(bus, self.addr, WatchKind::Read)
    }

    fn exec_read_index(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        if self.get_flag(PROC_FLAG_X) {
            self.microcode.push(Microcode::GetZero);
        } else {
            self.microcode.push(Microcode::ReadByte);
            self.microcode.push(Microcode::IncAddr);
        }
        self.microcode.push(Microcode::SetTemp0);
        self.exec_read_byte(bus)
    }

    fn exec_set_reg_x(&mut self) -> Result<(), SimBreak> {
        self.set_reg_x((u16::from(self.data) << 8) | u16::from(self.temp0));
        self.update_nz_flags(self.reg_x, self.get_flag(PROC_FLAG_X));
        Ok(())
    }

    fn exec_set_reg_y(&mut self) -> Result<(), SimBreak> {
        self.set_reg_y((u16::from(self.data) << 8) | u16::from(self.temp0));
        self.update_nz_flags(self.reg_y, self.get_flag(PROC_FLAG_X));
        Ok(())
    }

    fn exec_set_temp0(&mut self) -> Result<(), SimBreak> {
        self.temp0 = self.data;
        Ok(())
    }

    fn exec_set_temp1(&mut self) -> Result<(), SimBreak> {
        self.temp1 = self.data;
        Ok(())
    }

    fn exec_write(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.microcode.push(Microcode::FinishWrite);
        watch(bus, self.addr, WatchKind::Write)
    }

    fn decode_op_clc(&mut self) {
        self.set_flag(PROC_FLAG_C, false);
    }

    fn decode_op_cld(&mut self) {
        self.set_flag(PROC_FLAG_D, false);
    }

    fn decode_op_clv(&mut self) {
        self.set_flag(PROC_FLAG_V, false);
    }

    fn decode_op_jmp(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Jump);
        self.decode_addr_mode(addr_mode, BankReg::Program);
    }

    fn decode_op_ldx(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegX);
        self.microcode.push(Microcode::ReadIndex);
        self.decode_addr_mode(addr_mode, BankReg::Data);
    }

    fn decode_op_ldy(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegY);
        self.microcode.push(Microcode::ReadIndex);
        self.decode_addr_mode(addr_mode, BankReg::Data);
    }

    fn decode_op_sec(&mut self) {
        self.set_flag(PROC_FLAG_C, true);
    }

    fn decode_op_sed(&mut self) {
        self.set_flag(PROC_FLAG_D, true);
    }

    fn decode_op_stz(&mut self, addr_mode: AddrMode) {
        if self.get_flag(PROC_FLAG_M) {
            self.microcode.push(Microcode::Write);
            self.microcode.push(Microcode::GetZero);
        } else {
            todo!("16-bit STZ")
        }
        self.decode_addr_mode(addr_mode, BankReg::Data);
    }

    fn decode_op_xce(&mut self) {
        let emulation = self.get_flag(PROC_FLAG_C);
        self.set_flag(PROC_FLAG_C, self.emulation);
        self.emulation = emulation;
        self.force_registers();
    }

    fn get_flag(&self, flag: u8) -> bool {
        (self.reg_p & flag) != 0
    }

    fn set_flag(&mut self, flag: u8, value: bool) {
        self.set_reg_p(if value {
            self.reg_p | flag
        } else {
            self.reg_p & !flag
        });
    }

    fn update_nz_flags(&mut self, mut value: u16, is_8bit: bool) {
        self.reg_p &= !(PROC_FLAG_N | PROC_FLAG_Z);
        let threshold = if is_8bit {
            value &= 0x00ff;
            0x0080
        } else {
            0x8000
        };
        if value == 0 {
            self.reg_p |= PROC_FLAG_Z;
        }
        if value >= threshold {
            self.reg_p |= PROC_FLAG_N;
        }
    }

    fn set_reg_x(&mut self, value: u16) {
        self.reg_x = value;
        self.force_registers();
    }

    fn set_reg_y(&mut self, value: u16) {
        self.reg_y = value;
        self.force_registers();
    }

    fn set_reg_p(&mut self, value: u8) {
        self.reg_p = value;
        self.force_registers();
    }

    fn force_registers(&mut self) {
        // From http://www.6502.org/tutorials/65c816opcodes.html#4: "When the e
        // flag is 1, the SH register is forced to $01, the m flag is forced to
        // 1, and the x flag is forced to 1."
        if self.emulation {
            self.reg_s = 0x0100 | (self.reg_s & 0x00ff);
            self.reg_p |= PROC_FLAG_M | PROC_FLAG_X;
        }
        // From http://www.6502.org/tutorials/65c816opcodes.html#4: "when the x
        // flag is 1 (8-bit index registers), the XH register and the YH
        // register are both forced to $00...this is not the case for the m
        // flag. The B accumulator is not forced to zero when the m flag is 1."
        if (self.reg_p & PROC_FLAG_X) != 0 {
            self.reg_x &= 0x00ff;
            self.reg_y &= 0x00ff;
        }
    }
}

impl SimProc for Wdc65c816 {
    fn description(&self) -> String {
        "WDC 65C816".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, pc: u32) -> (u32, String) {
        let flag_m = self.get_flag(PROC_FLAG_M);
        let flag_x = self.get_flag(PROC_FLAG_X);
        let instruction = Instruction::decode(bus, pc, flag_m, flag_x);
        (instruction.size(), instruction.format(bus, pc, self.reg_d, self.dbr))
    }

    fn pc(&self) -> u32 {
        (u32::from(self.pbr) << 16) | u32::from(self.pc16)
    }

    fn set_pc(&mut self, addr: u32) {
        self.pc16 = addr as u16;
        self.pbr = (addr >> 16) as u8;
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["A", "X", "Y", "D", "S", "DBR", "P", "DATA"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "A" => Some(u32::from(self.reg_a)),
            "X" => Some(u32::from(self.reg_x)),
            "Y" => Some(u32::from(self.reg_y)),
            "D" => Some(u32::from(self.reg_d)),
            "S" => Some(u32::from(self.reg_s)),
            "DBR" => Some(u32::from(self.dbr)),
            "P" => Some(u32::from(self.reg_p)),
            "DATA" => Some(u32::from(self.data)),
            _ => None,
        }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        match name {
            "A" => todo!("A"),
            "X" => self.set_reg_x(value as u16),
            "Y" => self.set_reg_y(value as u16),
            "D" => todo!("D"),
            "S" => todo!("S"),
            "DBR" => todo!("DBR"),
            "P" => self.set_reg_p(value as u8),
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
        watch(bus, self.pc(), WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        !self.microcode.is_empty()
    }
}

//===========================================================================//
