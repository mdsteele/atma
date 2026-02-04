use super::util::watch;
use crate::addr::Addr;
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
    Pbr,
    Dbr,
    Zero,
}

//===========================================================================//

#[derive(Clone, Copy)]
enum Microcode {
    Branch,               // if TEMP0 != 0 then PC16 += (DATA as i8) and clear
    DecodeOpcode,         // decode DATA as opcode, push new microcode
    DoMvn,                // X += 1, Y += 1, clear microcode if C == 0, C -= 1
    DoMvp,                // X -= 1, Y -= 1, clear microcode if C == 0, C -= 1
    DoRep,                // P &= !DATA
    DoSep,                // P |= DATA
    FinishWrite,          // [ADDR] = DATA
    GetPbr,               // DATA = PBR
    GetPc16Hi(u8),        // DATA = (PC16 + offset) >> 8
    GetPc16Lo(u8),        // DATA = (PC16 + offset) & 0xff
    GetRegCHi,            // DATA = C >> 8
    GetRegCLo,            // DATA = C & 0xff
    GetRegDHi,            // DATA = D >> 8
    GetRegDLo,            // DATA = D & 0xff
    GetRegXHi,            // DATA = X >> 8
    GetRegXLo,            // DATA = X & 0xff
    GetRegYHi,            // DATA = Y >> 8
    GetRegYLo,            // DATA = Y & 0xff
    GetTemp0,             // DATA = TEMP0
    GetTemp2,             // DATA = TEMP2
    GetZero,              // DATA = 0
    IncAddrAbs,           // ADDR = (0xff0000 & ADDR) | (0xffff & (ADDR + 1))
    IncAddrLong,          // ADDR += 1
    IncPc16(u8),          // PC16 += offset
    Jump,                 // PC = ADDR, clear microcode
    MakeAddrAbs(BankReg), // ADDR = (bank << 16) | (DATA << 8) | TEMP0
    MakeAddrDirect,       // ADDR = D + DATA
    MakeAddrLong,         // ADDR = (DATA << 16) | (TEMP1 << 8) | TEMP0
    MakeAddrMoveDst,      // ADDR = (TEMP0 << 16) | Y, DBR = TEMP0
    MakeAddrMoveSrc,      // ADDR = (TEMP1 << 16) | X
    MakeAddrPc(u8),       // ADDR = PC + offset
    PullByte,             // ADDR = ++S, exec_read_byte
    PushByte,             // ADDR = S--, exec_write_byte
    ReadByte,             // DATA = [ADDR], watch(ADDR, Read)
    ReadWord(u8),         // DATA = [ADDR], watch(ADDR, Read), push microcode
    SetRegA,              // A = (DATA << 8) | TEMP0 (or just lower byte if M)
    SetRegD,              // D = (DATA << 8) | TEMP0
    SetRegX,              // X = (DATA << 8) | TEMP0
    SetRegY,              // Y = (DATA << 8) | TEMP0
    SetTemp0,             // TEMP0 = DATA
    SetTemp1,             // TEMP1 = DATA
    SetTemp2,             // TEMP2 = DATA
    WriteByte,            // watch(ADDR, Write), push FinishWrite microcode
}

//===========================================================================//

/// A simulated WDC 65C816 processor.
pub struct Wdc65c816 {
    pc16: u16,  // lower 16 bits of the 24-bit PC
    pbr: u8,    // Program Bank Register (upper 8 bits of the 24-bit PC)
    dbr: u8,    // Data Bank Register
    reg_c: u16, // 16-bit accumulator
    reg_d: u16, // Direct page register
    reg_p: u8,  // Processor status register
    reg_s: u16, // Stack pointer
    reg_x: u16, // X index register
    reg_y: u16, // Y index register
    emulation: bool,
    data: u8,
    temp0: u8,
    temp1: u8,
    temp2: u8,
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
            reg_c: 0,
            reg_d: 0,
            reg_p: PROC_FLAG_M | PROC_FLAG_X | PROC_FLAG_I | PROC_FLAG_C,
            reg_s: 0x0100,
            reg_x: 0,
            reg_y: 0,
            emulation: true,
            data: 0,
            temp0: 0,
            temp1: 0,
            temp2: 0,
            addr: VECTOR_EMULATED_RESET,
            microcode: vec![
                Microcode::Jump,
                Microcode::MakeAddrAbs(BankReg::Zero),
                Microcode::ReadByte,
                Microcode::IncAddrLong,
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
            Microcode::Branch => self.exec_branch(),
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::DoMvn => self.exec_do_mvn(),
            Microcode::DoMvp => self.exec_do_mvp(),
            Microcode::DoRep => self.exec_do_rep(),
            Microcode::DoSep => self.exec_do_sep(),
            Microcode::FinishWrite => self.exec_finish_write(bus),
            Microcode::GetPbr => self.exec_get_pbr(),
            Microcode::GetPc16Hi(offset) => self.exec_get_pc16_hi(offset),
            Microcode::GetPc16Lo(offset) => self.exec_get_pc16_lo(offset),
            Microcode::GetRegCHi => self.exec_get_reg_c_hi(),
            Microcode::GetRegCLo => self.exec_get_reg_c_lo(),
            Microcode::GetRegDHi => self.exec_get_reg_d_hi(),
            Microcode::GetRegDLo => self.exec_get_reg_d_lo(),
            Microcode::GetRegXHi => self.exec_get_reg_x_hi(),
            Microcode::GetRegXLo => self.exec_get_reg_x_lo(),
            Microcode::GetRegYHi => self.exec_get_reg_y_hi(),
            Microcode::GetRegYLo => self.exec_get_reg_y_lo(),
            Microcode::GetTemp0 => self.exec_get_temp0(),
            Microcode::GetTemp2 => self.exec_get_temp2(),
            Microcode::GetZero => self.exec_get_zero(),
            Microcode::IncAddrAbs => self.exec_inc_addr_abs(),
            Microcode::IncAddrLong => self.exec_inc_addr_long(),
            Microcode::IncPc16(offset) => self.exec_inc_pc16(offset),
            Microcode::Jump => self.exec_jump(),
            Microcode::MakeAddrAbs(reg) => self.exec_make_addr_abs(reg),
            Microcode::MakeAddrDirect => self.exec_make_addr_direct(),
            Microcode::MakeAddrLong => self.exec_make_addr_long(),
            Microcode::MakeAddrMoveDst => self.exec_make_addr_move_dst(),
            Microcode::MakeAddrMoveSrc => self.exec_make_addr_move_src(),
            Microcode::MakeAddrPc(offset) => self.exec_make_addr_pc(offset),
            Microcode::PullByte => self.exec_pull_byte(bus),
            Microcode::PushByte => self.exec_push_byte(bus),
            Microcode::ReadByte => self.exec_read_byte(bus),
            Microcode::ReadWord(flag) => self.exec_read_word(bus, flag),
            Microcode::SetRegA => self.exec_set_reg_a(),
            Microcode::SetRegD => self.exec_set_reg_d(),
            Microcode::SetRegX => self.exec_set_reg_x(),
            Microcode::SetRegY => self.exec_set_reg_y(),
            Microcode::SetTemp0 => self.exec_set_temp0(),
            Microcode::SetTemp1 => self.exec_set_temp1(),
            Microcode::SetTemp2 => self.exec_set_temp2(),
            Microcode::WriteByte => self.exec_write_byte(bus),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let opcode = self.data;
        let flag_m = self.get_flag(PROC_FLAG_M);
        let flag_x = self.get_flag(PROC_FLAG_X);
        let Operation { mnemonic, addr_mode } =
            Operation::from_opcode(opcode, flag_m, flag_x);
        self.microcode.push(Microcode::IncPc16(addr_mode.instruction_size()));
        match mnemonic {
            Mnemonic::Bcc => self.decode_op_bcc(),
            Mnemonic::Bcs => self.decode_op_bcs(),
            Mnemonic::Beq => self.decode_op_beq(),
            Mnemonic::Bmi => self.decode_op_bmi(),
            Mnemonic::Bne => self.decode_op_bne(),
            Mnemonic::Bpl => self.decode_op_bpl(),
            Mnemonic::Bvc => self.decode_op_bvc(),
            Mnemonic::Bvs => self.decode_op_bvs(),
            Mnemonic::Clc => self.decode_op_clc(),
            Mnemonic::Cld => self.decode_op_cld(),
            Mnemonic::Cli => self.decode_op_cli(),
            Mnemonic::Clv => self.decode_op_clv(),
            Mnemonic::Dex => self.decode_op_dex(),
            Mnemonic::Dey => self.decode_op_dey(),
            Mnemonic::Inx => self.decode_op_inx(),
            Mnemonic::Iny => self.decode_op_iny(),
            Mnemonic::Jml => self.decode_op_jml(addr_mode),
            Mnemonic::Jmp => self.decode_op_jmp(addr_mode),
            Mnemonic::Jsl => self.decode_op_jsl(addr_mode),
            Mnemonic::Jsr => self.decode_op_jsr(addr_mode),
            Mnemonic::Lda => self.decode_op_lda(addr_mode),
            Mnemonic::Ldx => self.decode_op_ldx(addr_mode),
            Mnemonic::Ldy => self.decode_op_ldy(addr_mode),
            Mnemonic::Mvn => self.decode_op_mvn(addr_mode),
            Mnemonic::Mvp => self.decode_op_mvp(addr_mode),
            Mnemonic::Nop => self.decode_op_nop(),
            Mnemonic::Pea => self.decode_op_pea(addr_mode),
            Mnemonic::Pei => self.decode_op_pei(addr_mode),
            Mnemonic::Per => self.decode_op_per(addr_mode),
            Mnemonic::Phd => self.decode_op_phd(),
            Mnemonic::Phk => self.decode_op_phk(),
            Mnemonic::Php => self.decode_op_php(),
            Mnemonic::Pld => self.decode_op_pld(),
            Mnemonic::Rep => self.decode_op_rep(addr_mode),
            Mnemonic::Rtl => self.decode_op_rtl(),
            Mnemonic::Rts => self.decode_op_rts(),
            Mnemonic::Sec => self.decode_op_sec(),
            Mnemonic::Sed => self.decode_op_sed(),
            Mnemonic::Sei => self.decode_op_sei(),
            Mnemonic::Sep => self.decode_op_sep(addr_mode),
            Mnemonic::Sta => self.decode_op_sta(addr_mode),
            Mnemonic::Stp | Mnemonic::Wai => {
                self.microcode.clear(); // cancel the IncPc16
                return Err(SimBreak::HaltOpcode(mnemonic.string(), opcode));
            }
            Mnemonic::Stx => self.decode_op_stx(addr_mode),
            Mnemonic::Sty => self.decode_op_sty(addr_mode),
            Mnemonic::Stz => self.decode_op_stz(addr_mode),
            Mnemonic::Tcd => self.decode_op_tcd(),
            Mnemonic::Txs => self.decode_op_txs(),
            Mnemonic::Wdm => self.decode_op_wdm(addr_mode),
            Mnemonic::Xce => self.decode_op_xce(),
            _ => todo!("{mnemonic} {addr_mode:?} at PC=${:06x}", self.pc()),
        }
        Ok(())
    }

    fn decode_addr_mode(&mut self, addr_mode: AddrMode, bank_reg: BankReg) {
        match addr_mode {
            AddrMode::Implied | AddrMode::Accumulator => {}
            AddrMode::ImmediateByte
            | AddrMode::Relative
            | AddrMode::ImmediateWord
            | AddrMode::RelativeLong => {
                self.microcode.push(Microcode::MakeAddrPc(1));
            }
            AddrMode::Absolute => {
                self.microcode.push(Microcode::MakeAddrAbs(bank_reg));
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(2));
                self.microcode.push(Microcode::SetTemp0);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(1));
            }
            AddrMode::AbsoluteLong => {
                self.microcode.push(Microcode::MakeAddrLong);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(3));
                self.microcode.push(Microcode::SetTemp1);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(2));
                self.microcode.push(Microcode::SetTemp0);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(1));
            }
            AddrMode::BlockMove => {
                self.microcode.push(Microcode::MakeAddrMoveSrc);
                self.microcode.push(Microcode::SetTemp1);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(2));
                self.microcode.push(Microcode::SetTemp0);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(1));
            }
            AddrMode::DirectPage => {
                self.microcode.push(Microcode::MakeAddrDirect);
                self.microcode.push(Microcode::ReadByte);
                self.microcode.push(Microcode::MakeAddrPc(1));
            }
            _ => todo!("{addr_mode:?}"),
        }
    }

    fn decode_branch(&mut self, condition: u8) {
        self.microcode.push(Microcode::Branch);
        self.microcode.push(Microcode::ReadByte);
        self.microcode.push(Microcode::MakeAddrPc(1));
        self.temp0 = condition;
    }

    fn exec_branch(&mut self) -> Result<(), SimBreak> {
        if self.temp0 != 0 {
            self.pc16 = self.pc16.wrapping_add(2);
            let offset = self.data as i8;
            self.pc16 = self.pc16.wrapping_add(offset as u16);
            self.microcode.clear(); // don't IncPc16 at end of instruction
        }
        Ok(())
    }

    fn exec_do_mvn(&mut self) -> Result<(), SimBreak> {
        self.reg_x = self.reg_x.wrapping_add(1);
        self.reg_y = self.reg_y.wrapping_add(1);
        if self.reg_c != 0 {
            self.microcode.clear(); // don't IncPc16 at end of instruction
        }
        self.reg_c = self.reg_c.wrapping_sub(1);
        self.force_registers();
        Ok(())
    }

    fn exec_do_mvp(&mut self) -> Result<(), SimBreak> {
        self.reg_x = self.reg_x.wrapping_sub(1);
        self.reg_y = self.reg_y.wrapping_sub(1);
        if self.reg_c != 0 {
            self.microcode.clear(); // don't IncPc16 at end of instruction
        }
        self.reg_c = self.reg_c.wrapping_sub(1);
        self.force_registers();
        Ok(())
    }

    fn exec_do_rep(&mut self) -> Result<(), SimBreak> {
        self.set_reg_p(self.reg_p & !self.data);
        Ok(())
    }

    fn exec_do_sep(&mut self) -> Result<(), SimBreak> {
        self.set_reg_p(self.reg_p | self.data);
        Ok(())
    }

    fn exec_finish_write(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        bus.write_byte(Addr::from(self.addr), self.data);
        Ok(())
    }

    fn exec_get_pbr(&mut self) -> Result<(), SimBreak> {
        self.data = self.pbr;
        Ok(())
    }

    fn exec_get_pc16_hi(&mut self, offset: u8) -> Result<(), SimBreak> {
        self.data = (self.pc16.wrapping_add(u16::from(offset)) >> 8) as u8;
        Ok(())
    }

    fn exec_get_pc16_lo(&mut self, offset: u8) -> Result<(), SimBreak> {
        self.data = self.pc16.wrapping_add(u16::from(offset)) as u8;
        Ok(())
    }

    fn exec_get_reg_c_hi(&mut self) -> Result<(), SimBreak> {
        self.data = (self.reg_c >> 8) as u8;
        Ok(())
    }

    fn exec_get_reg_c_lo(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_c as u8;
        Ok(())
    }

    fn exec_get_reg_d_hi(&mut self) -> Result<(), SimBreak> {
        self.data = (self.reg_d >> 8) as u8;
        Ok(())
    }

    fn exec_get_reg_d_lo(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_d as u8;
        Ok(())
    }

    fn exec_get_reg_x_hi(&mut self) -> Result<(), SimBreak> {
        self.data = (self.reg_x >> 8) as u8;
        Ok(())
    }

    fn exec_get_reg_x_lo(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_x as u8;
        Ok(())
    }

    fn exec_get_reg_y_hi(&mut self) -> Result<(), SimBreak> {
        self.data = (self.reg_y >> 8) as u8;
        Ok(())
    }

    fn exec_get_reg_y_lo(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_y as u8;
        Ok(())
    }

    fn exec_get_temp0(&mut self) -> Result<(), SimBreak> {
        self.data = self.temp0;
        Ok(())
    }

    fn exec_get_temp2(&mut self) -> Result<(), SimBreak> {
        self.data = self.temp2;
        Ok(())
    }

    fn exec_get_zero(&mut self) -> Result<(), SimBreak> {
        self.data = 0;
        Ok(())
    }

    fn exec_inc_addr_abs(&mut self) -> Result<(), SimBreak> {
        self.addr =
            (self.addr & 0xff0000) | (self.addr.wrapping_add(1) & 0xffff);
        Ok(())
    }

    fn exec_inc_addr_long(&mut self) -> Result<(), SimBreak> {
        self.addr = self.addr.wrapping_add(1) & 0xffffff;
        Ok(())
    }

    fn exec_inc_pc16(&mut self, offset: u8) -> Result<(), SimBreak> {
        self.pc16 = self.pc16.wrapping_add(u16::from(offset));
        Ok(())
    }

    fn exec_jump(&mut self) -> Result<(), SimBreak> {
        self.pc16 = self.addr as u16;
        self.pbr = (self.addr >> 16) as u8;
        self.microcode.clear(); // don't IncPc16 at end of instruction
        Ok(())
    }

    fn exec_make_addr_abs(&mut self, reg: BankReg) -> Result<(), SimBreak> {
        let bank = match reg {
            BankReg::Pbr => self.pbr,
            BankReg::Dbr => self.dbr,
            BankReg::Zero => 0,
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

    fn exec_make_addr_move_dst(&mut self) -> Result<(), SimBreak> {
        self.addr = (u32::from(self.temp0) << 16) | u32::from(self.reg_y);
        self.dbr = self.temp0;
        Ok(())
    }

    fn exec_make_addr_move_src(&mut self) -> Result<(), SimBreak> {
        self.addr = (u32::from(self.temp1) << 16) | u32::from(self.reg_x);
        Ok(())
    }

    fn exec_make_addr_pc(&mut self, offset: u8) -> Result<(), SimBreak> {
        self.addr = (u32::from(self.pbr) << 16)
            | u32::from(self.pc16.wrapping_add(u16::from(offset)));
        Ok(())
    }

    fn exec_pull_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        self.set_reg_s(self.reg_s.wrapping_add(1));
        self.addr = u32::from(self.reg_s);
        self.exec_read_byte(bus)
    }

    fn exec_push_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        self.addr = u32::from(self.reg_s);
        self.set_reg_s(self.reg_s.wrapping_sub(1));
        self.exec_write_byte(bus)
    }

    fn exec_read_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let addr = Addr::from(self.addr);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_read_word(
        &mut self,
        bus: &mut dyn SimBus,
        flag: u8,
    ) -> Result<(), SimBreak> {
        if self.get_flag(flag) {
            self.microcode.push(Microcode::GetZero);
        } else {
            self.microcode.push(Microcode::ReadByte);
            // TODO: This should IncAddrAbs instead for AddrMode::ImmediateWord
            self.microcode.push(Microcode::IncAddrLong);
        }
        self.microcode.push(Microcode::SetTemp0);
        self.exec_read_byte(bus)
    }

    fn exec_set_reg_a(&mut self) -> Result<(), SimBreak> {
        self.set_reg_a((u16::from(self.data) << 8) | u16::from(self.temp0));
        self.update_nz_flags(self.reg_c, self.get_flag(PROC_FLAG_M));
        Ok(())
    }

    fn exec_set_reg_d(&mut self) -> Result<(), SimBreak> {
        self.reg_d = (u16::from(self.data) << 8) | u16::from(self.temp0);
        self.update_nz_flags(self.reg_d, false);
        Ok(())
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

    fn exec_set_temp2(&mut self) -> Result<(), SimBreak> {
        self.temp2 = self.data;
        Ok(())
    }

    fn exec_write_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        self.microcode.push(Microcode::FinishWrite);
        watch(bus, Addr::from(self.addr), WatchKind::Write)
    }

    fn decode_op_bcc(&mut self) {
        self.decode_branch(PROC_FLAG_C & !self.reg_p);
    }

    fn decode_op_bcs(&mut self) {
        self.decode_branch(PROC_FLAG_C & self.reg_p);
    }

    fn decode_op_beq(&mut self) {
        self.decode_branch(PROC_FLAG_Z & self.reg_p);
    }

    fn decode_op_bmi(&mut self) {
        self.decode_branch(PROC_FLAG_N & self.reg_p);
    }

    fn decode_op_bne(&mut self) {
        self.decode_branch(PROC_FLAG_Z & !self.reg_p);
    }

    fn decode_op_bpl(&mut self) {
        self.decode_branch(PROC_FLAG_N & !self.reg_p);
    }

    fn decode_op_bvc(&mut self) {
        self.decode_branch(PROC_FLAG_V & !self.reg_p);
    }

    fn decode_op_bvs(&mut self) {
        self.decode_branch(PROC_FLAG_V & self.reg_p);
    }

    fn decode_op_clc(&mut self) {
        self.set_flag(PROC_FLAG_C, false);
    }

    fn decode_op_cld(&mut self) {
        self.set_flag(PROC_FLAG_D, false);
    }

    fn decode_op_cli(&mut self) {
        self.set_flag(PROC_FLAG_I, false);
    }

    fn decode_op_clv(&mut self) {
        self.set_flag(PROC_FLAG_V, false);
    }

    fn decode_op_dex(&mut self) {
        self.set_reg_x(self.reg_x.wrapping_sub(1));
        self.update_nz_flags(self.reg_x, self.get_flag(PROC_FLAG_X));
    }

    fn decode_op_dey(&mut self) {
        self.set_reg_y(self.reg_y.wrapping_sub(1));
        self.update_nz_flags(self.reg_y, self.get_flag(PROC_FLAG_X));
    }

    fn decode_op_inx(&mut self) {
        self.set_reg_x(self.reg_x.wrapping_add(1));
        self.update_nz_flags(self.reg_x, self.get_flag(PROC_FLAG_X));
    }

    fn decode_op_iny(&mut self) {
        self.set_reg_y(self.reg_y.wrapping_add(1));
        self.update_nz_flags(self.reg_y, self.get_flag(PROC_FLAG_X));
    }

    fn decode_op_jml(&mut self, addr_mode: AddrMode) {
        self.decode_op_jmp(addr_mode);
    }

    fn decode_op_jmp(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Jump);
        self.decode_addr_mode(addr_mode, BankReg::Pbr);
    }

    fn decode_op_jsl(&mut self, addr_mode: AddrMode) {
        // For the order of memory operations here, see Table 5-7 from
        // https://www.westerndesigncenter.com/wdc/documentation/w65c816s.pdf
        debug_assert_eq!(addr_mode, AddrMode::AbsoluteLong);
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::MakeAddrLong);
        self.microcode.push(Microcode::GetTemp2);
        self.microcode.push(Microcode::PushByte); // cycle 8
        self.microcode.push(Microcode::GetPc16Lo(3));
        self.microcode.push(Microcode::PushByte); // cycle 7
        self.microcode.push(Microcode::GetPc16Hi(3));
        self.microcode.push(Microcode::SetTemp2);
        self.microcode.push(Microcode::ReadByte); // cycle 6
        self.microcode.push(Microcode::MakeAddrPc(3));
        self.microcode.push(Microcode::PushByte); // cycle 4
        self.microcode.push(Microcode::GetPbr);
        self.microcode.push(Microcode::SetTemp1);
        self.microcode.push(Microcode::ReadByte); // cycle 3
        self.microcode.push(Microcode::MakeAddrPc(2));
        self.microcode.push(Microcode::SetTemp0);
        self.microcode.push(Microcode::ReadByte); // cycle 2
        self.microcode.push(Microcode::MakeAddrPc(1));
    }

    fn decode_op_jsr(&mut self, addr_mode: AddrMode) {
        // For the order of memory operations here, see Table 5-7 from
        // https://www.westerndesigncenter.com/wdc/documentation/w65c816s.pdf
        match addr_mode {
            AddrMode::Absolute => {
                self.microcode.push(Microcode::Jump);
                self.microcode.push(Microcode::MakeAddrAbs(BankReg::Pbr));
                self.microcode.push(Microcode::GetTemp2);
                self.microcode.push(Microcode::PushByte); // cycle 6
                self.microcode.push(Microcode::GetPc16Lo(2));
                self.microcode.push(Microcode::PushByte); // cycle 5
                self.microcode.push(Microcode::GetPc16Hi(2));
                self.microcode.push(Microcode::SetTemp2);
                self.microcode.push(Microcode::ReadByte); // cycle 3
                self.microcode.push(Microcode::MakeAddrPc(2));
                self.microcode.push(Microcode::SetTemp0);
                self.microcode.push(Microcode::ReadByte); // cycle 2
                self.microcode.push(Microcode::MakeAddrPc(1));
            }
            AddrMode::AbsoluteXIndexedIndirect => {
                todo!("decode_op_jsr(AbsoluteXIndexedIndirect)");
            }
            _ => unreachable!(),
        }
    }

    fn decode_op_lda(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegA);
        self.microcode.push(Microcode::ReadWord(PROC_FLAG_M));
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_ldx(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegX);
        self.microcode.push(Microcode::ReadWord(PROC_FLAG_X));
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_ldy(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegY);
        self.microcode.push(Microcode::ReadWord(PROC_FLAG_X));
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_mvn(&mut self, addr_mode: AddrMode) {
        debug_assert_eq!(addr_mode, AddrMode::BlockMove);
        self.decode_move(addr_mode, Microcode::DoMvn);
    }

    fn decode_op_mvp(&mut self, addr_mode: AddrMode) {
        debug_assert_eq!(addr_mode, AddrMode::BlockMove);
        self.decode_move(addr_mode, Microcode::DoMvp);
    }

    fn decode_move(&mut self, addr_mode: AddrMode, finish: Microcode) {
        debug_assert_eq!(addr_mode, AddrMode::BlockMove);
        self.microcode.push(finish);
        self.microcode.push(Microcode::WriteByte);
        self.microcode.push(Microcode::MakeAddrMoveDst);
        self.microcode.push(Microcode::ReadByte);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_nop(&mut self) {}

    fn decode_op_pea(&mut self, addr_mode: AddrMode) {
        debug_assert_eq!(addr_mode, AddrMode::Absolute);
        self.decode_push_addr(addr_mode);
    }

    fn decode_op_pei(&mut self, addr_mode: AddrMode) {
        debug_assert_eq!(addr_mode, AddrMode::DirectPageIndirect);
        self.decode_push_addr(addr_mode);
    }

    fn decode_op_per(&mut self, addr_mode: AddrMode) {
        debug_assert_eq!(addr_mode, AddrMode::RelativeLong);
        self.decode_push_addr(addr_mode);
    }

    fn decode_push_addr(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::PushByte);
        self.microcode.push(Microcode::GetTemp0);
        self.microcode.push(Microcode::PushByte);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_phd(&mut self) {
        self.microcode.push(Microcode::PushByte);
        self.microcode.push(Microcode::GetRegDLo);
        self.microcode.push(Microcode::PushByte);
        self.microcode.push(Microcode::GetRegDHi);
    }

    fn decode_op_phk(&mut self) {
        self.microcode.push(Microcode::PushByte);
        self.data = self.pbr;
    }

    fn decode_op_php(&mut self) {
        self.microcode.push(Microcode::PushByte);
        self.data = self.reg_p;
    }

    fn decode_op_pld(&mut self) {
        self.microcode.push(Microcode::SetRegD);
        self.microcode.push(Microcode::PullByte);
        self.microcode.push(Microcode::SetTemp0);
        self.microcode.push(Microcode::PullByte);
    }

    fn decode_op_rep(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::DoRep);
        self.microcode.push(Microcode::ReadByte);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_rtl(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::IncAddrAbs);
        self.microcode.push(Microcode::MakeAddrLong);
        self.microcode.push(Microcode::PullByte);
        self.microcode.push(Microcode::SetTemp1);
        self.microcode.push(Microcode::PullByte);
        self.microcode.push(Microcode::SetTemp0);
        self.microcode.push(Microcode::PullByte);
    }

    fn decode_op_rts(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::IncAddrAbs);
        self.microcode.push(Microcode::MakeAddrAbs(BankReg::Pbr));
        self.microcode.push(Microcode::PullByte);
        self.microcode.push(Microcode::SetTemp0);
        self.microcode.push(Microcode::PullByte);
    }

    fn decode_op_sec(&mut self) {
        self.set_flag(PROC_FLAG_C, true);
    }

    fn decode_op_sed(&mut self) {
        self.set_flag(PROC_FLAG_D, true);
    }

    fn decode_op_sei(&mut self) {
        self.set_flag(PROC_FLAG_I, true);
    }

    fn decode_op_sep(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::DoSep);
        self.microcode.push(Microcode::ReadByte);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_sta(&mut self, addr_mode: AddrMode) {
        if !self.get_flag(PROC_FLAG_M) {
            self.microcode.push(Microcode::WriteByte);
            self.microcode.push(Microcode::GetRegCHi);
            self.microcode.push(Microcode::IncAddrLong);
        }
        self.microcode.push(Microcode::WriteByte);
        self.microcode.push(Microcode::GetRegCLo);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_stx(&mut self, addr_mode: AddrMode) {
        if !self.get_flag(PROC_FLAG_X) {
            self.microcode.push(Microcode::WriteByte);
            self.microcode.push(Microcode::GetRegXHi);
            self.microcode.push(Microcode::IncAddrLong);
        }
        self.microcode.push(Microcode::WriteByte);
        self.microcode.push(Microcode::GetRegXLo);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_sty(&mut self, addr_mode: AddrMode) {
        if !self.get_flag(PROC_FLAG_X) {
            self.microcode.push(Microcode::WriteByte);
            self.microcode.push(Microcode::GetRegYHi);
            self.microcode.push(Microcode::IncAddrLong);
        }
        self.microcode.push(Microcode::WriteByte);
        self.microcode.push(Microcode::GetRegYLo);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_stz(&mut self, addr_mode: AddrMode) {
        if !self.get_flag(PROC_FLAG_M) {
            self.microcode.push(Microcode::WriteByte);
            self.microcode.push(Microcode::GetZero);
            self.microcode.push(Microcode::IncAddrLong);
        }
        self.microcode.push(Microcode::WriteByte);
        self.microcode.push(Microcode::GetZero);
        self.decode_addr_mode(addr_mode, BankReg::Dbr);
    }

    fn decode_op_tcd(&mut self) {
        self.reg_d = self.reg_c;
        self.update_nz_flags(self.reg_d, false);
    }

    fn decode_op_txs(&mut self) {
        self.reg_s = self.reg_x;
        self.force_registers();
    }

    fn decode_op_wdm(&mut self, addr_mode: AddrMode) {
        debug_assert_eq!(addr_mode, AddrMode::ImmediateByte);
        // From http://www.6502.org/tutorials/65c816opcodes.html#6.7: "On the
        // 65C816, [WDM] acts like a 2-byte, 2-cycle NOP (note that the actual
        // NOP instruction is only 1 byte). The second byte is read, but
        // ignored."
        self.microcode.push(Microcode::ReadByte);
        self.microcode.push(Microcode::MakeAddrPc(1));
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

    fn set_reg_a(&mut self, value: u16) {
        if self.get_flag(PROC_FLAG_M) {
            self.reg_c = (self.reg_c & 0xff00) | (value & 0x00ff);
        } else {
            self.reg_c = value;
        }
    }

    fn set_reg_s(&mut self, value: u16) {
        self.reg_s = value;
        self.force_registers();
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

    fn disassemble(&self, bus: &dyn SimBus, pc: Addr) -> (u32, String) {
        let pc = pc.as_u32();
        let flag_m = self.get_flag(PROC_FLAG_M);
        let flag_x = self.get_flag(PROC_FLAG_X);
        let instruction = Instruction::decode(bus, pc, flag_m, flag_x);
        (instruction.size(), instruction.format(bus, pc, self.reg_d, self.dbr))
    }

    fn pc(&self) -> Addr {
        (Addr::from(self.pbr) << 16) | Addr::from(self.pc16)
    }

    fn set_pc(&mut self, addr: Addr) {
        self.pc16 = addr.as_u16();
        self.pbr = (addr >> 16).as_u8();
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["A", "B", "C", "X", "Y", "D", "S", "DBR", "P", "DATA"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "A" => Some(u32::from(if self.get_flag(PROC_FLAG_M) {
                self.reg_c & 0x00ff
            } else {
                self.reg_c
            })),
            "B" => Some(u32::from(self.reg_c >> 8)),
            "C" => Some(u32::from(self.reg_c)),
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
            "A" => self.set_reg_a(value as u16),
            "B" => {
                self.reg_c &= 0x00ff;
                self.reg_c |= (value as u16) << 8;
            }
            "C" => self.reg_c = value as u16,
            "X" => self.set_reg_x(value as u16),
            "Y" => self.set_reg_y(value as u16),
            "D" => self.reg_d = value as u16,
            "S" => self.set_reg_s(value as u16),
            "DBR" => self.dbr = value as u8,
            "P" => self.set_reg_p(value as u8),
            "DATA" => self.data = value as u8,
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        if self.microcode.is_empty() {
            self.microcode.push(Microcode::DecodeOpcode);
            self.microcode.push(Microcode::ReadByte);
            self.addr = self.pc().as_u32();
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
