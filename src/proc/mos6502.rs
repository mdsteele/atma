use super::util::watch;
use crate::bus::{BusPeeker, SimBus, WatchKind};
use crate::dis::mos6502::{
    AddrMode, Operation, decode_opcode, disassemble_instruction,
    format_instruction,
};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

const VECTOR_IRQ: u16 = 0xfffe;
const VECTOR_RESET: u16 = 0xfffc;

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

#[derive(Clone, Copy)]
enum Microcode {
    Add,          // A += DATA + C, update NVZC flags appropriately
    BitTest,      // update NV from DATA and Z from (DATA & A)
    BitwiseAnd,   // A &= DATA, update NZ flags from A
    BitwiseOr,    // A |= DATA, update NZ flags from A
    BitwiseXor,   // A ^= DATA, update NZ flags from A
    Branch,       // if TEMP != 0 then PC += (DATA as i8)
    Compare,      // compare DATA to TEMP, update NZC flags appropriately
    DecodeOpcode, // decode DATA as opcode, push new microcode
    Decrement,    // DATA -= 1, set_nz_flags(DATA)
    GetPcHi,      // DATA = PC >> 8
    GetPcLo,      // DATA = PC & 0xff
    GetRegA,      // DATA = A
    GetRegP,      // DATA = P | !REG_P_MASK
    GetRegX,      // DATA = X
    GetRegY,      // DATA = Y
    IncAddr,      // ADDR += 1
    IncAddrLo,    // ADDR = (ADDR & 0xff00) | ((ADDR + 1) & 0x00ff)
    Increment,    // DATA += 1, set_nz_flags(DATA)
    IndexX,       // ADDR += X
    IndexXLo,     // ADDR = (ADDR & 0xff00) | ((ADDR + X) & 0x00ff)
    IndexY,       // ADDR += Y
    IndexYLo,     // ADDR = (ADDR & 0xff00) | ((ADDR + Y) & 0x00ff)
    Jump,         // PC = ADDR
    MakeAddrAbs,  // ADDR = (DATA << 8) | TEMP
    MakeAddrZp,   // ADDR = DATA
    Pull,         // S += 1, DATA = [SP], watch(SP, Read)
    Push1,        // ADDR = SP, watch(ADDR, Write)
    Push2,        // [ADDR] = DATA, S -= 1
    Read,         // DATA = [ADDR], watch(ADDR, Read)
    ReadAtPc,     // DATA = [PC], watch(PC, Read), PC += 1
    SetRegA,      // A = DATA, update NZ flags from DATA
    SetRegP,      // P = DATA & REG_P_MASK
    SetRegX,      // X = DATA, update NZ flags from DATA
    SetRegY,      // Y = DATA, update NZ flags from DATA
    SetTemp,      // TEMP = DATA
    Subtract,     // A -= DATA + 1 - C, update NVZC flags appropriately
    RotateLeft,   // rotate DATA left with C, update NZC flags
    RotateRight,  // rotate DATA right with C, update NZC flags
    VectorIrq,    // ADDR = VECTOR_IRQ, P |= PROC_FLAG_I
    Write1,       // watch(ADDR, Write)
    Write2,       // [ADDR] = DATA
}

//===========================================================================//

/// A simulated MOS 6502 processor.
pub struct Mos6502 {
    pc: u16,
    addr: u16,
    data: u8,
    temp: u8,
    reg_s: u8,
    reg_p: u8,
    reg_a: u8,
    reg_x: u8,
    reg_y: u8,
    microcode: Vec<Microcode>,
}

impl Default for Mos6502 {
    fn default() -> Mos6502 {
        Mos6502::new()
    }
}

impl Mos6502 {
    /// Returns a new simulated MOS 6502 processor.
    pub fn new() -> Mos6502 {
        Mos6502 {
            pc: 0,
            addr: VECTOR_RESET,
            data: 0,
            temp: 0,
            reg_s: 0,
            reg_p: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
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
            Microcode::Add => self.exec_add(),
            Microcode::BitTest => self.exec_bit_test(),
            Microcode::BitwiseAnd => self.exec_bitwise_and(),
            Microcode::BitwiseOr => self.exec_bitwise_or(),
            Microcode::BitwiseXor => self.exec_bitwise_xor(),
            Microcode::Branch => self.exec_branch(),
            Microcode::Compare => self.exec_compare(),
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::Decrement => self.exec_decrement(),
            Microcode::GetPcHi => self.exec_get_pc_hi(),
            Microcode::GetPcLo => self.exec_get_pc_lo(),
            Microcode::GetRegA => self.exec_get_reg_a(),
            Microcode::GetRegP => self.exec_get_reg_p(),
            Microcode::GetRegX => self.exec_get_reg_x(),
            Microcode::GetRegY => self.exec_get_reg_y(),
            Microcode::IncAddr => self.exec_inc_addr(),
            Microcode::IncAddrLo => self.exec_inc_addr_lo(),
            Microcode::Increment => self.exec_increment(),
            Microcode::IndexX => self.exec_index_x(),
            Microcode::IndexXLo => self.exec_index_x_lo(),
            Microcode::IndexY => self.exec_index_y(),
            Microcode::IndexYLo => self.exec_index_y_lo(),
            Microcode::Jump => self.exec_jump(),
            Microcode::MakeAddrAbs => self.exec_make_addr_abs(),
            Microcode::MakeAddrZp => self.exec_make_addr_zp(),
            Microcode::Pull => self.exec_pull(bus),
            Microcode::Push1 => self.exec_push1(bus),
            Microcode::Push2 => self.exec_push2(bus),
            Microcode::Read => self.exec_read(bus),
            Microcode::ReadAtPc => self.exec_read_at_pc(bus),
            Microcode::RotateLeft => self.exec_rotate_left(),
            Microcode::RotateRight => self.exec_rotate_right(),
            Microcode::SetRegA => self.exec_set_reg_a(),
            Microcode::SetRegP => self.exec_set_reg_p(),
            Microcode::SetRegX => self.exec_set_reg_x(),
            Microcode::SetRegY => self.exec_set_reg_y(),
            Microcode::SetTemp => self.exec_set_temp(),
            Microcode::Subtract => self.exec_subtract(),
            Microcode::VectorIrq => self.exec_vector_irq(),
            Microcode::Write1 => self.exec_write1(bus),
            Microcode::Write2 => self.exec_write2(bus),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let (operation, addr_mode) = decode_opcode(self.data);
        match operation {
            Operation::Adc => self.decode_op_adc(addr_mode),
            Operation::And => self.decode_op_and(addr_mode),
            Operation::Asl => self.decode_op_asl(addr_mode),
            Operation::Bcc => self.decode_op_bcc(),
            Operation::Bcs => self.decode_op_bcs(),
            Operation::Beq => self.decode_op_beq(),
            Operation::Bit => self.decode_op_bit(addr_mode),
            Operation::Bmi => self.decode_op_bmi(),
            Operation::Bne => self.decode_op_bne(),
            Operation::Bpl => self.decode_op_bpl(),
            Operation::Brk => self.decode_op_brk(),
            Operation::Bvc => self.decode_op_bvc(),
            Operation::Bvs => self.decode_op_bvs(),
            Operation::Clc => self.decode_op_clc(),
            Operation::Cld => self.decode_op_cld(),
            Operation::Cli => self.decode_op_cli(),
            Operation::Clv => self.decode_op_clv(),
            Operation::Cmp => self.decode_op_cmp(addr_mode),
            Operation::Cpx => self.decode_op_cpx(addr_mode),
            Operation::Cpy => self.decode_op_cpy(addr_mode),
            Operation::Dec => self.decode_op_dec(addr_mode),
            Operation::Dex => self.decode_op_dex(),
            Operation::Dey => self.decode_op_dey(),
            Operation::Eor => self.decode_op_eor(addr_mode),
            Operation::Inc => self.decode_op_inc(addr_mode),
            Operation::Inx => self.decode_op_inx(),
            Operation::Iny => self.decode_op_iny(),
            Operation::Jmp => self.decode_op_jmp(addr_mode),
            Operation::Jsr => self.decode_op_jsr(),
            Operation::Lda => self.decode_op_lda(addr_mode),
            Operation::Ldx => self.decode_op_ldx(addr_mode),
            Operation::Ldy => self.decode_op_ldy(addr_mode),
            Operation::Lsr => self.decode_op_lsr(addr_mode),
            Operation::Nop => {}
            Operation::Ora => self.decode_op_ora(addr_mode),
            Operation::Pha => self.decode_op_pha(),
            Operation::Php => self.decode_op_php(),
            Operation::Pla => self.decode_op_pla(),
            Operation::Plp => self.decode_op_plp(),
            Operation::Rol => self.decode_op_rol(addr_mode),
            Operation::Ror => self.decode_op_ror(addr_mode),
            Operation::Rti => self.decode_op_rti(),
            Operation::Rts => self.decode_op_rts(),
            Operation::Sbc => self.decode_op_sbc(addr_mode),
            Operation::Sec => self.decode_op_sec(),
            Operation::Sed => self.decode_op_sed(),
            Operation::Sei => self.decode_op_sei(),
            Operation::Sta => self.decode_op_sta(addr_mode),
            Operation::Stx => self.decode_op_stx(addr_mode),
            Operation::Sty => self.decode_op_sty(addr_mode),
            Operation::Tax => self.decode_op_tax(),
            Operation::Tay => self.decode_op_tay(),
            Operation::Tsx => self.decode_op_tsx(),
            Operation::Txa => self.decode_op_txa(),
            Operation::Txs => self.decode_op_txs(),
            Operation::Tya => self.decode_op_tya(),
            Operation::UndocNop => self.decode_op_undoc_nop(addr_mode),
            // TODO: Support more undocumented opcodes.
            _ => {
                return Err(SimBreak::HaltOpcode(
                    operation.mnemonic(),
                    self.data,
                ));
            }
        }
        Ok(())
    }

    fn decode_addr_mode(&mut self, addr_mode: AddrMode) {
        match addr_mode {
            AddrMode::Implied | AddrMode::Accumulator => {}
            AddrMode::Immediate | AddrMode::Relative => {
                self.addr = self.pc;
                self.pc += 1;
            }
            AddrMode::Absolute => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::AbsoluteIndirect => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddrLo);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::XIndexedAbsolute => {
                self.microcode.push(Microcode::IndexX);
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::YIndexedAbsolute => {
                self.microcode.push(Microcode::IndexY);
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::ReadAtPc);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::ZeroPage => {
                self.microcode.push(Microcode::MakeAddrZp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::XIndexedZeroPage => {
                self.microcode.push(Microcode::IndexXLo);
                self.microcode.push(Microcode::MakeAddrZp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::YIndexedZeroPage => {
                self.microcode.push(Microcode::IndexYLo);
                self.microcode.push(Microcode::MakeAddrZp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::XIndexedZeroPageIndirect => {
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddrLo);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IndexXLo);
                self.microcode.push(Microcode::MakeAddrZp);
                self.microcode.push(Microcode::ReadAtPc);
            }
            AddrMode::ZeroPageIndirectYIndexed => {
                self.microcode.push(Microcode::IndexY);
                self.microcode.push(Microcode::MakeAddrAbs);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::IncAddrLo);
                self.microcode.push(Microcode::SetTemp);
                self.microcode.push(Microcode::Read);
                self.microcode.push(Microcode::MakeAddrZp);
                self.microcode.push(Microcode::ReadAtPc);
            }
        }
    }

    fn decode_branch(&mut self, condition: u8) {
        self.microcode.push(Microcode::Branch);
        self.microcode.push(Microcode::ReadAtPc);
        self.temp = condition;
    }

    fn decode_compare(&mut self, addr_mode: AddrMode, get_reg: Microcode) {
        self.microcode.push(Microcode::Compare);
        self.microcode.push(get_reg);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_adc(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Add);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_and(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::BitwiseAnd);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_asl(&mut self, addr_mode: AddrMode) {
        self.reg_p &= !PROC_FLAG_C;
        self.decode_op_rol(addr_mode);
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

    fn decode_op_bit(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::BitTest);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
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

    fn decode_op_brk(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::IncAddr);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Read);
        self.microcode.push(Microcode::VectorIrq);
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetRegP);
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetPcLo);
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetPcHi);
        self.pc += 1;
    }

    fn decode_op_bvc(&mut self) {
        self.decode_branch(PROC_FLAG_V & !self.reg_p);
    }

    fn decode_op_bvs(&mut self) {
        self.decode_branch(PROC_FLAG_V & self.reg_p);
    }

    fn decode_op_clc(&mut self) {
        self.reg_p &= !PROC_FLAG_C;
    }

    fn decode_op_cld(&mut self) {
        self.reg_p &= !PROC_FLAG_D;
    }

    fn decode_op_cli(&mut self) {
        self.reg_p &= !PROC_FLAG_I;
    }

    fn decode_op_clv(&mut self) {
        self.reg_p &= !PROC_FLAG_V;
    }

    fn decode_op_cmp(&mut self, addr_mode: AddrMode) {
        self.decode_compare(addr_mode, Microcode::GetRegA);
    }

    fn decode_op_cpx(&mut self, addr_mode: AddrMode) {
        self.decode_compare(addr_mode, Microcode::GetRegX);
    }

    fn decode_op_cpy(&mut self, addr_mode: AddrMode) {
        self.decode_compare(addr_mode, Microcode::GetRegY);
    }

    fn decode_op_dec(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Write2);
        self.microcode.push(Microcode::Write1);
        self.microcode.push(Microcode::Decrement);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_dex(&mut self) {
        self.data = self.reg_x.wrapping_sub(1);
        self.set_nz_flags(self.data);
        self.reg_x = self.data;
    }

    fn decode_op_dey(&mut self) {
        self.data = self.reg_y.wrapping_sub(1);
        self.set_nz_flags(self.data);
        self.reg_y = self.data;
    }

    fn decode_op_eor(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::BitwiseXor);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_inc(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Write2);
        self.microcode.push(Microcode::Write1);
        self.microcode.push(Microcode::Increment);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_inx(&mut self) {
        self.data = self.reg_x.wrapping_add(1);
        self.set_nz_flags(self.data);
        self.reg_x = self.data;
    }

    fn decode_op_iny(&mut self) {
        self.data = self.reg_y.wrapping_add(1);
        self.set_nz_flags(self.data);
        self.reg_y = self.data;
    }

    fn decode_op_jmp(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Jump);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_jsr(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::ReadAtPc);
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetPcLo);
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetPcHi);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::ReadAtPc);
    }

    fn decode_op_lda(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegA);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_ldx(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegX);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_ldy(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::SetRegY);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_lsr(&mut self, addr_mode: AddrMode) {
        self.reg_p &= !PROC_FLAG_C;
        self.decode_op_ror(addr_mode);
    }

    fn decode_op_ora(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::BitwiseOr);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_pha(&mut self) {
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetRegA);
    }

    fn decode_op_php(&mut self) {
        self.microcode.push(Microcode::Push2);
        self.microcode.push(Microcode::Push1);
        self.microcode.push(Microcode::GetRegP);
    }

    fn decode_op_pla(&mut self) {
        self.microcode.push(Microcode::SetRegA);
        self.microcode.push(Microcode::Pull);
    }

    fn decode_op_plp(&mut self) {
        self.microcode.push(Microcode::SetRegP);
        self.microcode.push(Microcode::Pull);
    }

    fn decode_op_rol(&mut self, addr_mode: AddrMode) {
        if let AddrMode::Accumulator = addr_mode {
            self.data = self.rotate_left(self.reg_a);
            self.reg_a = self.data;
        } else {
            self.microcode.push(Microcode::Write2);
            self.microcode.push(Microcode::Write1);
            self.microcode.push(Microcode::RotateLeft);
            self.microcode.push(Microcode::Read);
            self.decode_addr_mode(addr_mode);
        }
    }

    fn decode_op_ror(&mut self, addr_mode: AddrMode) {
        if let AddrMode::Accumulator = addr_mode {
            self.data = self.rotate_right(self.reg_a);
            self.reg_a = self.data;
        } else {
            self.microcode.push(Microcode::Write2);
            self.microcode.push(Microcode::Write1);
            self.microcode.push(Microcode::RotateRight);
            self.microcode.push(Microcode::Read);
            self.decode_addr_mode(addr_mode);
        }
    }

    fn decode_op_rti(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::Pull);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Pull);
        self.microcode.push(Microcode::SetRegP);
        self.microcode.push(Microcode::Pull);
    }

    fn decode_op_rts(&mut self) {
        self.microcode.push(Microcode::Jump);
        self.microcode.push(Microcode::IncAddr);
        self.microcode.push(Microcode::MakeAddrAbs);
        self.microcode.push(Microcode::Pull);
        self.microcode.push(Microcode::SetTemp);
        self.microcode.push(Microcode::Pull);
    }

    fn decode_op_sbc(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Subtract);
        self.microcode.push(Microcode::Read);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_sec(&mut self) {
        self.reg_p |= PROC_FLAG_C;
    }

    fn decode_op_sed(&mut self) {
        self.reg_p |= PROC_FLAG_D;
    }

    fn decode_op_sei(&mut self) {
        self.reg_p |= PROC_FLAG_I;
    }

    fn decode_op_sta(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Write2);
        self.microcode.push(Microcode::Write1);
        self.microcode.push(Microcode::GetRegA);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_stx(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Write2);
        self.microcode.push(Microcode::Write1);
        self.microcode.push(Microcode::GetRegX);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_sty(&mut self, addr_mode: AddrMode) {
        self.microcode.push(Microcode::Write2);
        self.microcode.push(Microcode::Write1);
        self.microcode.push(Microcode::GetRegY);
        self.decode_addr_mode(addr_mode);
    }

    fn decode_op_tax(&mut self) {
        self.data = self.reg_a;
        self.set_nz_flags(self.data);
        self.reg_x = self.data;
    }

    fn decode_op_tay(&mut self) {
        self.data = self.reg_a;
        self.set_nz_flags(self.data);
        self.reg_y = self.data;
    }

    fn decode_op_tsx(&mut self) {
        self.data = self.reg_s;
        self.set_nz_flags(self.data);
        self.reg_x = self.data;
    }

    fn decode_op_txa(&mut self) {
        self.data = self.reg_x;
        self.set_nz_flags(self.data);
        self.reg_a = self.data;
    }

    fn decode_op_txs(&mut self) {
        self.data = self.reg_x;
        // TXS does not update processor flags.
        self.reg_s = self.data;
    }

    fn decode_op_tya(&mut self) {
        self.data = self.reg_y;
        self.set_nz_flags(self.data);
        self.reg_a = self.data;
    }

    fn decode_op_undoc_nop(&mut self, addr_mode: AddrMode) {
        if !matches!(addr_mode, AddrMode::Implied) {
            self.microcode.push(Microcode::Read);
            self.decode_addr_mode(addr_mode);
        }
    }

    fn exec_add(&mut self) -> Result<(), SimBreak> {
        self.add(self.data);
        Ok(())
    }

    fn exec_bit_test(&mut self) -> Result<(), SimBreak> {
        self.reg_p &= !(PROC_FLAG_N | PROC_FLAG_V | PROC_FLAG_Z);
        self.reg_p |= self.data & (PROC_FLAG_N | PROC_FLAG_V);
        if self.data & self.reg_a == 0 {
            self.reg_p |= PROC_FLAG_Z;
        }
        Ok(())
    }

    fn exec_bitwise_and(&mut self) -> Result<(), SimBreak> {
        self.reg_a &= self.data;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn exec_bitwise_or(&mut self) -> Result<(), SimBreak> {
        self.reg_a |= self.data;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn exec_bitwise_xor(&mut self) -> Result<(), SimBreak> {
        self.reg_a ^= self.data;
        self.set_nz_flags(self.reg_a);
        Ok(())
    }

    fn exec_branch(&mut self) -> Result<(), SimBreak> {
        if self.temp != 0 {
            let offset = self.data as i8;
            self.pc = self.pc.wrapping_add(offset as u16);
        }
        Ok(())
    }

    fn exec_compare(&mut self) -> Result<(), SimBreak> {
        let lhs = self.data;
        let rhs = self.temp;
        self.set_flag(PROC_FLAG_C, lhs >= rhs);
        self.set_nz_flags(lhs.wrapping_sub(rhs));
        Ok(())
    }

    fn exec_decrement(&mut self) -> Result<(), SimBreak> {
        self.data = self.data.wrapping_sub(1);
        self.set_nz_flags(self.data);
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

    fn exec_get_reg_a(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_a;
        Ok(())
    }

    fn exec_get_reg_p(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_p | !REG_P_MASK;
        Ok(())
    }

    fn exec_get_reg_x(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_x;
        Ok(())
    }

    fn exec_get_reg_y(&mut self) -> Result<(), SimBreak> {
        self.data = self.reg_y;
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

    fn exec_increment(&mut self) -> Result<(), SimBreak> {
        self.data = self.data.wrapping_add(1);
        self.set_nz_flags(self.data);
        Ok(())
    }

    fn exec_index_x(&mut self) -> Result<(), SimBreak> {
        self.addr = self.addr.wrapping_add(u16::from(self.reg_x));
        Ok(())
    }

    fn exec_index_x_lo(&mut self) -> Result<(), SimBreak> {
        let x = u16::from(self.reg_x);
        self.addr = (self.addr & 0xff00) | (self.addr.wrapping_add(x) & 0xff);
        Ok(())
    }

    fn exec_index_y(&mut self) -> Result<(), SimBreak> {
        self.addr = self.addr.wrapping_add(u16::from(self.reg_y));
        Ok(())
    }

    fn exec_index_y_lo(&mut self) -> Result<(), SimBreak> {
        let y = u16::from(self.reg_y);
        self.addr = (self.addr & 0xff00) | (self.addr.wrapping_add(y) & 0xff);
        Ok(())
    }

    fn exec_jump(&mut self) -> Result<(), SimBreak> {
        self.pc = self.addr;
        Ok(())
    }

    fn exec_make_addr_abs(&mut self) -> Result<(), SimBreak> {
        self.addr = (u16::from(self.data) << 8) | u16::from(self.temp);
        Ok(())
    }

    fn exec_make_addr_zp(&mut self) -> Result<(), SimBreak> {
        self.addr = u16::from(self.data);
        Ok(())
    }

    fn exec_pull(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.reg_s = self.reg_s.wrapping_add(1);
        let addr = 0x0100 | u32::from(self.reg_s);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_push1(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        self.addr = 0x0100 | u16::from(self.reg_s);
        watch(bus, u32::from(self.addr), WatchKind::Write)
    }

    fn exec_push2(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        bus.write_byte(u32::from(self.addr), self.data);
        self.reg_s = self.reg_s.wrapping_sub(1);
        Ok(())
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
        self.pc += 1;
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_rotate_left(&mut self) -> Result<(), SimBreak> {
        self.data = self.rotate_left(self.data);
        Ok(())
    }

    fn exec_rotate_right(&mut self) -> Result<(), SimBreak> {
        self.data = self.rotate_right(self.data);
        Ok(())
    }

    fn exec_set_reg_a(&mut self) -> Result<(), SimBreak> {
        self.set_nz_flags(self.data);
        self.reg_a = self.data;
        Ok(())
    }

    fn exec_set_reg_p(&mut self) -> Result<(), SimBreak> {
        self.reg_p = self.data;
        Ok(())
    }

    fn exec_set_reg_x(&mut self) -> Result<(), SimBreak> {
        self.set_nz_flags(self.data);
        self.reg_x = self.data;
        Ok(())
    }

    fn exec_set_reg_y(&mut self) -> Result<(), SimBreak> {
        self.set_nz_flags(self.data);
        self.reg_y = self.data;
        Ok(())
    }

    fn exec_set_temp(&mut self) -> Result<(), SimBreak> {
        self.temp = self.data;
        Ok(())
    }

    fn exec_subtract(&mut self) -> Result<(), SimBreak> {
        self.add(!self.data);
        Ok(())
    }

    fn exec_vector_irq(&mut self) -> Result<(), SimBreak> {
        self.addr = VECTOR_IRQ;
        self.reg_p |= PROC_FLAG_I;
        Ok(())
    }

    fn exec_write1(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        watch(bus, u32::from(self.addr), WatchKind::Write)
    }

    fn exec_write2(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        bus.write_byte(u32::from(self.addr), self.data);
        Ok(())
    }

    fn add(&mut self, rhs: u8) {
        let lhs: u8 = self.reg_a;
        let sum: u8 = if self.get_flag(PROC_FLAG_D) {
            0 // TODO: implement decimal mode
        } else {
            let mut sum: u16 = (lhs as u16) + (rhs as u16);
            if self.get_flag(PROC_FLAG_C) {
                sum += 1;
            }
            self.set_flag(PROC_FLAG_C, sum >= 0x100);
            sum as u8
        };
        self.set_flag(PROC_FLAG_V, ((lhs ^ sum) & (rhs ^ sum) & 0x80) != 0);
        self.reg_a = sum;
        self.set_nz_flags(self.reg_a);
    }

    fn get_flag(&self, flag: u8) -> bool {
        (self.reg_p & flag) != 0
    }

    fn rotate_left(&mut self, input: u8) -> u8 {
        let carry_in = (self.reg_p & PROC_FLAG_C) != 0;
        let carry_out = (input & 0x80) != 0;
        let mut output = input << 1;
        if carry_in {
            output |= 0x01;
        }
        self.set_flag(PROC_FLAG_C, carry_out);
        self.set_nz_flags(output);
        output
    }

    fn rotate_right(&mut self, input: u8) -> u8 {
        let carry_in = (self.reg_p & PROC_FLAG_C) != 0;
        let carry_out = (input & 0x01) != 0;
        let mut output = input >> 1;
        if carry_in {
            output |= 0x80;
        }
        self.set_flag(PROC_FLAG_C, carry_out);
        self.set_nz_flags(output);
        output
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
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &["A", "X", "Y", "S", "P", "DATA"]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "A" => Some(u32::from(self.reg_a)),
            "X" => Some(u32::from(self.reg_x)),
            "Y" => Some(u32::from(self.reg_y)),
            "P" => Some(u32::from(self.reg_p)),
            "S" => Some(u32::from(self.reg_s)),
            "DATA" => Some(u32::from(self.data)),
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
            "DATA" => self.data = (value & 0xff) as u8,
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
    use super::{Mos6502, REG_P_MASK, SimProc};

    #[test]
    fn get_registers() {
        let proc = Mos6502::new();
        for &register in proc.register_names() {
            assert!(proc.get_register(register).is_some());
        }
    }

    #[test]
    fn set_registers() {
        let mut proc = Mos6502::new();
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
        let mut proc = Mos6502::new();
        proc.set_register("P", 0);
        assert_eq!(proc.get_register("P"), Some(0));
        // Invalid bits should get masked off of the P register.
        proc.set_register("P", 0xff);
        assert_eq!(proc.get_register("P"), Some(u32::from(REG_P_MASK)));
    }
}

//===========================================================================//
