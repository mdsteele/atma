//! Facilities for disassembling 65C816 machine code.

use crate::addr::Addr;
use crate::bus::SimBus;
use std::fmt;

//===========================================================================//

/// An operation mnemonic (ignoring the addressing mode) for a 65C816
/// processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mnemonic {
    /// An ADC (add to accumulator with carry) operation.
    Adc,
    /// An AND (binary AND with accumulator) operation.
    And,
    /// An ASL (arithmetic shift left) operation.
    Asl,
    /// A BCC (branch if carry flag clear) operation.
    Bcc,
    /// A BCS (branch if carry flag set) operation.
    Bcs,
    /// A BEQ (branch if equal to zero) operation.
    Beq,
    /// A BIT (bit test) operation.
    Bit,
    /// A BMI (branch if minus) operation.
    Bmi,
    /// A BNE (branch if not equal to zero) operation.
    Bne,
    /// A BPL (branch if plus) operation.
    Bpl,
    /// A BRA (branch always) operation.
    Bra,
    /// A BRK (break) operation.
    Brk,
    /// A BRA (branch always long) operation.
    Brl,
    /// A BVC (branch if overflow flag clear) operation.
    Bvc,
    /// A BVS (branch if overflow flag set) operation.
    Bvs,
    /// A CLC (clear carry flag) operation.
    Clc,
    /// A CLD (clear decimal-mode flag) operation.
    Cld,
    /// A CLI (clear interrupt-disable flag) operation.
    Cli,
    /// A CLV (clear overflow flag) operation.
    Clv,
    /// A CMP (compare accumulator) operation.
    Cmp,
    /// A COP (coprocessor empowerment) operation.
    Cop,
    /// A CPX (compare index X) operation.
    Cpx,
    /// A CPY (compare index Y) operation.
    Cpy,
    /// A DEC (decrement memory) operation.
    Dec,
    /// A DEX (decrement index X) operation.
    Dex,
    /// A DEY (decrement index Y) operation.
    Dey,
    /// An EOR (exclusive OR with accumulator) operation.
    Eor,
    /// An INC (increment memory) operation.
    Inc,
    /// An INX (increment index X) operation.
    Inx,
    /// An INY (increment index Y) operation.
    Iny,
    /// A JML (jump to address long) operation.
    Jml,
    /// A JMP (jump to address) operation.
    Jmp,
    /// A JSL (jump to subroutine long) operation.
    Jsl,
    /// A JSR (jump to subroutine) operation.
    Jsr,
    /// An LDA (load accumulator) operation.
    Lda,
    /// An LDX (load index X) operation.
    Ldx,
    /// An LDY (load index Y) operation.
    Ldy,
    /// An LSR (logical shift right) operation.
    Lsr,
    /// An MVN (move memory negative) operation.
    Mvn,
    /// An MVP (move memory positive) operation.
    Mvp,
    /// A NOP (no operation) operation.
    Nop,
    /// An ORA (binary OR with accumulator) operation.
    Ora,
    /// A PEA (push effective address) operation.
    Pea,
    /// A PEI (push effective indirect address) operation.
    Pei,
    /// A PER (push effective relative address) operation.
    Per,
    /// A PHA (push accumulator) operation.
    Pha,
    /// A PHB (push data bank register) operation.
    Phb,
    /// A PHD (push direct register) operation.
    Phd,
    /// A PHK (push K register) operation.
    Phk,
    /// A PHP (push processor status) operation.
    Php,
    /// A PHX (push X register) operation.
    Phx,
    /// A PHY (push Y register) operation.
    Phy,
    /// A PLA (pull accumulator) operation.
    Pla,
    /// A PLB (pull data bank register) operation.
    Plb,
    /// A PLD (pull direct register) operation.
    Pld,
    /// A PLP (pull processor status) operation.
    Plp,
    /// A PLX (pull X register) operation.
    Plx,
    /// A PLY (pull Y register) operation.
    Ply,
    /// A REP (reset processor status bits) operation.
    Rep,
    /// An ROL (rotate left) operation.
    Rol,
    /// An ROR (rotate right) operation.
    Ror,
    /// An RTI (return from interrupt) operation.
    Rti,
    /// An RTL (return from subroutine long) operation.
    Rtl,
    /// An RTS (return from subroutine) operation.
    Rts,
    /// An SBC (subtract from accumulator with borrow) operation.
    Sbc,
    /// An SEC (set carry flag) operation.
    Sec,
    /// An SED (set decimal-mode flag) operation.
    Sed,
    /// An SEI (set interrupt-disable flag) operation.
    Sei,
    /// An SEP (set processor status bits) operation.
    Sep,
    /// An STA (store accumulator) operation.
    Sta,
    /// An STP (stop the clock) operation.
    Stp,
    /// An STX (store index X) operation.
    Stx,
    /// An STY (store index Y) operation.
    Sty,
    /// An STZ (store zero) operation.
    Stz,
    /// A TAX (transfer accumulator to index X) operation.
    Tax,
    /// A TAY (transfer accumulator to index Y) operation.
    Tay,
    /// A TCD (transfer C accumulator to direct register) operation.
    Tcd,
    /// A TCS (transfer C accumulator to stack pointer) operation.
    Tcs,
    /// A TDC (transfer direct register to C accumulator) operation.
    Tdc,
    /// A TRB (test and reset memory bits) operation.
    Trb,
    /// A TSB (test and set memory bits) operation.
    Tsb,
    /// A TDC (transfer stack pointer to C accumulator) operation.
    Tsc,
    /// A TSX (transfer stack pointer to index X) operation.
    Tsx,
    /// A TXA (transfer index X to accumulator) operation.
    Txa,
    /// A TXY (transfer index X to index Y) operation.
    Txy,
    /// A TXS (transfer index X to stack pointer) operation.
    Txs,
    /// A TYA (transfer index Y to accumulator) operation.
    Tya,
    /// A TYX (transfer index Y to index X) operation.
    Tyx,
    /// A WAI (wait for interrupt) operation.
    Wai,
    /// A WDM (William D. Mensch, reserved for future expansion) operation.
    Wdm,
    /// An XBA (exchange B and A accumulator) operation.
    Xba,
    /// An XCE (exchange carry and emulatation flags) operation.
    Xce,
}

impl Mnemonic {
    /// Returns the assembler string for this mnemonic.
    pub fn string(self) -> &'static str {
        match self {
            Mnemonic::Adc => "ADC",
            Mnemonic::And => "AND",
            Mnemonic::Asl => "ASL",
            Mnemonic::Bcc => "BCC",
            Mnemonic::Bcs => "BCS",
            Mnemonic::Beq => "BEQ",
            Mnemonic::Bit => "BIT",
            Mnemonic::Bmi => "BMI",
            Mnemonic::Bne => "BNE",
            Mnemonic::Bpl => "BPL",
            Mnemonic::Bra => "BRA",
            Mnemonic::Brk => "BRK",
            Mnemonic::Brl => "BRL",
            Mnemonic::Bvc => "BVC",
            Mnemonic::Bvs => "BVS",
            Mnemonic::Clc => "CLC",
            Mnemonic::Cld => "CLD",
            Mnemonic::Cli => "CLI",
            Mnemonic::Clv => "CLV",
            Mnemonic::Cmp => "CMP",
            Mnemonic::Cop => "COP",
            Mnemonic::Cpx => "CPX",
            Mnemonic::Cpy => "CPY",
            Mnemonic::Dec => "DEC",
            Mnemonic::Dex => "DEX",
            Mnemonic::Dey => "DEY",
            Mnemonic::Eor => "EOR",
            Mnemonic::Inc => "INC",
            Mnemonic::Inx => "INX",
            Mnemonic::Iny => "INY",
            Mnemonic::Jml => "JML",
            Mnemonic::Jmp => "JMP",
            Mnemonic::Jsl => "JSL",
            Mnemonic::Jsr => "JSR",
            Mnemonic::Lda => "LDA",
            Mnemonic::Ldx => "LDX",
            Mnemonic::Ldy => "LDY",
            Mnemonic::Lsr => "LSR",
            Mnemonic::Mvn => "MVN",
            Mnemonic::Mvp => "MVP",
            Mnemonic::Nop => "NOP",
            Mnemonic::Ora => "ORA",
            Mnemonic::Pea => "PEA",
            Mnemonic::Pei => "PEI",
            Mnemonic::Per => "PER",
            Mnemonic::Pha => "PHA",
            Mnemonic::Phb => "PHB",
            Mnemonic::Phd => "PHD",
            Mnemonic::Phk => "PHK",
            Mnemonic::Php => "PHP",
            Mnemonic::Phx => "PHX",
            Mnemonic::Phy => "PHY",
            Mnemonic::Pla => "PLA",
            Mnemonic::Plb => "PLB",
            Mnemonic::Pld => "PLD",
            Mnemonic::Plp => "PLP",
            Mnemonic::Plx => "PLX",
            Mnemonic::Ply => "PLY",
            Mnemonic::Rep => "REP",
            Mnemonic::Rol => "ROL",
            Mnemonic::Ror => "ROR",
            Mnemonic::Rti => "RTI",
            Mnemonic::Rtl => "RTL",
            Mnemonic::Rts => "RTS",
            Mnemonic::Sbc => "SBC",
            Mnemonic::Sec => "SEC",
            Mnemonic::Sed => "SED",
            Mnemonic::Sei => "SEI",
            Mnemonic::Sep => "SEP",
            Mnemonic::Sta => "STA",
            Mnemonic::Stp => "STP",
            Mnemonic::Stx => "STX",
            Mnemonic::Sty => "STY",
            Mnemonic::Stz => "STZ",
            Mnemonic::Tax => "TAX",
            Mnemonic::Tay => "TAY",
            Mnemonic::Tcd => "TCD",
            Mnemonic::Tcs => "TCS",
            Mnemonic::Tdc => "TDC",
            Mnemonic::Trb => "TRB",
            Mnemonic::Tsb => "TSB",
            Mnemonic::Tsc => "TSC",
            Mnemonic::Tsx => "TSX",
            Mnemonic::Txa => "TXA",
            Mnemonic::Txy => "TXY",
            Mnemonic::Txs => "TXS",
            Mnemonic::Tya => "TYA",
            Mnemonic::Tyx => "TYX",
            Mnemonic::Wai => "WAI",
            Mnemonic::Wdm => "WDM",
            Mnemonic::Xba => "XBA",
            Mnemonic::Xce => "XCE",
        }
    }

    /// Returns true if operations with this mnemonic use the program bank
    /// register for absolute addresses, or false if they use the data bank
    /// register.
    fn uses_program_bank(self) -> bool {
        match self {
            Mnemonic::Adc => false,
            Mnemonic::And => false,
            Mnemonic::Asl => false,
            Mnemonic::Bcc => true,
            Mnemonic::Bcs => true,
            Mnemonic::Beq => true,
            Mnemonic::Bit => false,
            Mnemonic::Bmi => true,
            Mnemonic::Bne => true,
            Mnemonic::Bpl => true,
            Mnemonic::Bra => true,
            Mnemonic::Brk => false,
            Mnemonic::Brl => true,
            Mnemonic::Bvc => true,
            Mnemonic::Bvs => true,
            Mnemonic::Clc => false,
            Mnemonic::Cld => false,
            Mnemonic::Cli => false,
            Mnemonic::Clv => false,
            Mnemonic::Cmp => false,
            Mnemonic::Cop => false,
            Mnemonic::Cpx => false,
            Mnemonic::Cpy => false,
            Mnemonic::Dec => false,
            Mnemonic::Dex => false,
            Mnemonic::Dey => false,
            Mnemonic::Eor => false,
            Mnemonic::Inc => false,
            Mnemonic::Inx => false,
            Mnemonic::Iny => false,
            Mnemonic::Jml => true,
            Mnemonic::Jmp => true,
            Mnemonic::Jsl => true,
            Mnemonic::Jsr => true,
            Mnemonic::Lda => false,
            Mnemonic::Ldx => false,
            Mnemonic::Ldy => false,
            Mnemonic::Lsr => false,
            Mnemonic::Mvn => false,
            Mnemonic::Mvp => false,
            Mnemonic::Nop => false,
            Mnemonic::Ora => false,
            Mnemonic::Pea => false,
            Mnemonic::Pei => false,
            Mnemonic::Per => true,
            Mnemonic::Pha => false,
            Mnemonic::Phb => false,
            Mnemonic::Phd => false,
            Mnemonic::Phk => false,
            Mnemonic::Php => false,
            Mnemonic::Phx => false,
            Mnemonic::Phy => false,
            Mnemonic::Pla => false,
            Mnemonic::Plb => false,
            Mnemonic::Pld => false,
            Mnemonic::Plp => false,
            Mnemonic::Plx => false,
            Mnemonic::Ply => false,
            Mnemonic::Rep => false,
            Mnemonic::Rol => false,
            Mnemonic::Ror => false,
            Mnemonic::Rti => true,
            Mnemonic::Rtl => true,
            Mnemonic::Rts => true,
            Mnemonic::Sbc => false,
            Mnemonic::Sec => false,
            Mnemonic::Sed => false,
            Mnemonic::Sei => false,
            Mnemonic::Sep => false,
            Mnemonic::Sta => false,
            Mnemonic::Stp => false,
            Mnemonic::Stx => false,
            Mnemonic::Sty => false,
            Mnemonic::Stz => false,
            Mnemonic::Tax => false,
            Mnemonic::Tay => false,
            Mnemonic::Tcd => false,
            Mnemonic::Tcs => false,
            Mnemonic::Tdc => false,
            Mnemonic::Trb => false,
            Mnemonic::Tsb => false,
            Mnemonic::Tsc => false,
            Mnemonic::Tsx => false,
            Mnemonic::Txa => false,
            Mnemonic::Txy => false,
            Mnemonic::Txs => false,
            Mnemonic::Tya => false,
            Mnemonic::Tyx => false,
            Mnemonic::Wai => false,
            Mnemonic::Wdm => false,
            Mnemonic::Xba => false,
            Mnemonic::Xce => false,
        }
    }
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.string())
    }
}

//===========================================================================//

/// An addressing mode for a 65C816 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AddrMode {
    /// No additional arguments to the opcode.
    Implied,
    /// Operate on the A register.
    Accumulator,
    /// Operate on a constant 8-bit value immediately following the opcode.
    ImmediateByte,
    /// Operate on a constant 16-bit value immediately following the opcode.
    ImmediateWord,
    /// Operate on the destination and source banks specified in the two
    /// constant bytes immediately following the opcode.
    BlockMove,
    /// Operate on the bank-relative 16-bit address following the opcode.
    Absolute,
    /// Treat the 16-bit address following the opcode as a pointer to the
    /// 16-bit address to operate on.
    AbsoluteIndirect,
    /// Treat the 16-bit address following the opcode as a pointer to the
    /// 24-bit address to operate on.
    AbsoluteIndirectLong,
    /// Operate on a the absolute 16-bit address following the opcode, offset
    /// by index X.
    AbsoluteXIndexed,
    /// Operate on 16-bit address that is stored in memory, at the 16-bit
    /// bank-relative address following the opcode offset by index X.
    AbsoluteXIndexedIndirect,
    /// Operate on a the absolute 16-bit address following the opcode, offset
    /// by index Y.
    AbsoluteYIndexed,
    /// Operate on the absolute 24-bit address following the opcode.
    AbsoluteLong,
    /// Operate on a the absolute 24-bit address following the opcode, offset
    /// by index X.
    AbsoluteLongXIndexed,
    /// Operate on a the 8-bit direct page address following the opcode.
    DirectPage,
    /// Operate on a address equal to the 16-bit address stored at the 8-bit
    /// direct page address following the opcode.
    DirectPageIndirect,
    /// Operate on a address equal to the 24-bit address stored at the 8-bit
    /// direct page address following the opcode.
    DirectPageIndirectLong,
    /// Operate on the 8-bit direct page address following the opcode, offset
    /// by index X.
    DirectPageXIndexed,
    /// Operate on the 8-bit direct page address following the opcode, offset
    /// by index Y.
    DirectPageYIndexed,
    /// Operate on the 16-bit address that is stored in memory, at the 8-bit
    /// direct page address following the opcode offset by index X.
    DirectPageXIndexedIndirect,
    /// Operate on a address equal to the 16-bit address stored at the 8-bit
    /// direct page address following the opcode, offset by index Y.
    DirectPageIndirectYIndexed,
    /// Operate on a address equal to the 24-bit address stored at the 8-bit
    /// direct page address following the opcode, offset by index Y.
    DirectPageIndirectLongYIndexed,
    /// Operate on an address that is offset from the end of this instruction
    /// by the signed 8-bit value following the opcode.
    Relative,
    /// Operate on an address that is offset from the end of this instruction
    /// by the signed 16-bit value following the opcode.
    RelativeLong,
    /// Operate on an address offset from the stack pointer by the unsigned
    /// 8-bit value following the opcode.
    StackRelative,
    /// Operate on a address equal to the 16-bit address stored at a location
    /// offset from the stack pointer by the unsigned 8-bit value following the
    /// opcode.
    StackRelativeIndirectYIndexed,
}

impl AddrMode {
    fn immediate(flag: bool) -> AddrMode {
        if flag { AddrMode::ImmediateByte } else { AddrMode::ImmediateWord }
    }

    /// Returns the length of an instruction that uses this addressing mode, in
    /// bytes.
    pub fn instruction_size(self) -> u8 {
        match self {
            AddrMode::Absolute => 3,
            AddrMode::AbsoluteIndirect => 3,
            AddrMode::AbsoluteIndirectLong => 3,
            AddrMode::AbsoluteLong => 4,
            AddrMode::AbsoluteLongXIndexed => 4,
            AddrMode::AbsoluteXIndexed => 3,
            AddrMode::AbsoluteXIndexedIndirect => 3,
            AddrMode::AbsoluteYIndexed => 3,
            AddrMode::Accumulator => 1,
            AddrMode::BlockMove => 3,
            AddrMode::DirectPage => 2,
            AddrMode::DirectPageIndirect => 2,
            AddrMode::DirectPageIndirectLong => 2,
            AddrMode::DirectPageIndirectLongYIndexed => 2,
            AddrMode::DirectPageIndirectYIndexed => 2,
            AddrMode::DirectPageXIndexed => 2,
            AddrMode::DirectPageXIndexedIndirect => 2,
            AddrMode::DirectPageYIndexed => 2,
            AddrMode::ImmediateByte => 2,
            AddrMode::ImmediateWord => 3,
            AddrMode::Implied => 1,
            AddrMode::Relative => 2,
            AddrMode::RelativeLong => 3,
            AddrMode::StackRelative => 2,
            AddrMode::StackRelativeIndirectYIndexed => 2,
        }
    }
}

//===========================================================================//

/// An addressing mode and argument value for a 65C816 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    /// No additional arguments to the opcode.
    Implied,
    /// Operate on the A register.
    Accumulator,
    /// Operate on the given constant 8-bit value.
    ImmediateByte(u8),
    /// Operate on the given constant 16-bit value.
    ImmediateWord(u16),
    /// Operate on the given destination and source banks.
    BlockMove(u8, u8),
    /// Operate on the given 16-bit address.
    Absolute(u16),
    /// Operate on the 16-bit address in memory that the given 16-bit address
    /// points to.
    AbsoluteIndirect(u16),
    /// Operate on the 24-bit address in memory that the given 16-bit address
    /// points to.
    AbsoluteIndirectLong(u16),
    /// Operate on the given 16-bit address, offset by index X.
    AbsoluteXIndexed(u16),
    /// Operate on the 16-bit address in memory that is offset from the given
    /// 16-bit address by index X.
    AbsoluteXIndexedIndirect(u16),
    /// Operate on the given 16-bit address, offset by index Y.
    AbsoluteYIndexed(u16),
    /// Operate on the given 24-bit address.
    AbsoluteLong(u32),
    /// Operate on the given 24-bit address, offset by index Y.
    AbsoluteLongXIndexed(u32),
    /// Operate on the given direct page address.
    DirectPage(u8),
    /// Operate on the 16-bit address that is stored in memory at the given
    /// direct page address.
    DirectPageIndirect(u8),
    /// Operate on the 24-bit address that is stored in memory at the given
    /// direct page address.
    DirectPageIndirectLong(u8),
    /// Operate on the given direct page address, offset by index X.
    DirectPageXIndexed(u8),
    /// Operate on the given direct page address, offset by index Y.
    DirectPageYIndexed(u8),
    /// Operate on the 16-bit address in memory that is offset from the given
    /// direct page address by index X.
    DirectPageXIndexedIndirect(u8),
    /// Operate on an address equal to the 16-bit address stored at the given
    /// direct page address, offset by index Y.
    DirectPageIndirectYIndexed(u8),
    /// Operate on an address equal to the 24-bit address stored at the given
    /// direct page address, offset by index Y.
    DirectPageIndirectLongYIndexed(u8),
    /// Operate on an address that is offset (by the given signed 8-bit offset)
    /// from the address of the following instruction.
    Relative(i8),
    /// Operate on an address that is offset (by the given signed 16-bit
    /// offset) from the address of the following instruction.
    RelativeLong(i16),
    /// Operate on the address that is offset from the stack pointer by the
    /// given unsigned 8-bit offset.
    StackRelative(u8),
    /// Operate on an address equal to the 16-bit address stored at the given
    /// offset from the stack pointer, offset by index Y.
    StackRelativeIndirectYIndexed(u8),
}

impl Operand {
    /// Returns the size of this operand, in bytes.
    pub fn size(self) -> u32 {
        match self {
            Operand::Implied => 0,
            Operand::Accumulator => 0,
            Operand::ImmediateByte(_) => 1,
            Operand::ImmediateWord(_) => 2,
            Operand::BlockMove(_, _) => 2,
            Operand::Absolute(_) => 2,
            Operand::AbsoluteIndirect(_) => 2,
            Operand::AbsoluteIndirectLong(_) => 2,
            Operand::AbsoluteXIndexed(_) => 2,
            Operand::AbsoluteXIndexedIndirect(_) => 2,
            Operand::AbsoluteYIndexed(_) => 2,
            Operand::AbsoluteLong(_) => 3,
            Operand::AbsoluteLongXIndexed(_) => 3,
            Operand::DirectPage(_) => 1,
            Operand::DirectPageIndirect(_) => 1,
            Operand::DirectPageIndirectLong(_) => 1,
            Operand::DirectPageXIndexed(_) => 1,
            Operand::DirectPageYIndexed(_) => 1,
            Operand::DirectPageXIndexedIndirect(_) => 1,
            Operand::DirectPageIndirectYIndexed(_) => 1,
            Operand::DirectPageIndirectLongYIndexed(_) => 1,
            Operand::Relative(_) => 1,
            Operand::RelativeLong(_) => 2,
            Operand::StackRelative(_) => 1,
            Operand::StackRelativeIndirectYIndexed(_) => 1,
        }
    }

    /// Formats this operand.  `pc` gives the address of the start of the
    /// instruction.  `dpr` gives the address stored in the direct page
    /// register.  `bank` gives the current program or data bank (whichever is
    /// relevant to this instruction).
    fn format(self, bus: &dyn SimBus, pc: u16, dpr: u16, bank: u8) -> String {
        match self {
            Operand::Implied => String::new(),
            Operand::Accumulator => " A".to_string(),
            Operand::ImmediateByte(byte) => format!(" #${byte:02x}"),
            Operand::ImmediateWord(word) => format!(" #${word:04x}"),
            Operand::BlockMove(dst, src) => {
                format!(" #${src:02x}, #${dst:02x}")
            }
            Operand::Absolute(abs) => {
                format!(" {}", format_abs(bus, bank, abs))
            }
            Operand::AbsoluteIndirect(abs) => {
                format!(" ({})", format_abs(bus, bank, abs))
            }
            Operand::AbsoluteIndirectLong(abs) => {
                format!(" [{}]", format_abs(bus, bank, abs))
            }
            Operand::AbsoluteXIndexed(abs) => {
                format!(" {}, X", format_abs(bus, bank, abs))
            }
            Operand::AbsoluteXIndexedIndirect(abs) => {
                format!(" ({}, X)", format_abs(bus, bank, abs))
            }
            Operand::AbsoluteYIndexed(abs) => {
                format!(" {}, Y", format_abs(bus, bank, abs))
            }
            Operand::AbsoluteLong(long) => {
                format!(" {}", format_long(bus, long))
            }
            Operand::AbsoluteLongXIndexed(long) => {
                format!(" {}, X", format_long(bus, long))
            }
            Operand::DirectPage(dp) => format!(" {}", format_dp(bus, dpr, dp)),
            Operand::DirectPageIndirect(dp) => {
                format!(" ({})", format_dp(bus, dpr, dp))
            }
            Operand::DirectPageIndirectLong(dp) => {
                format!(" [{}]", format_dp(bus, dpr, dp))
            }
            Operand::DirectPageXIndexed(dp) => {
                format!(" {}, X", format_dp(bus, dpr, dp))
            }
            Operand::DirectPageYIndexed(dp) => {
                format!(" {}, Y", format_dp(bus, dpr, dp))
            }
            Operand::DirectPageXIndexedIndirect(dp) => {
                format!(" ({}, X)", format_dp(bus, dpr, dp))
            }
            Operand::DirectPageIndirectYIndexed(dp) => {
                format!(" ({}), Y", format_dp(bus, dpr, dp))
            }
            Operand::DirectPageIndirectLongYIndexed(dp) => {
                format!(" [{}], Y", format_dp(bus, dpr, dp))
            }
            Operand::Relative(offset) => {
                let dest = pc.wrapping_add(2).wrapping_add(offset as u16);
                format!(" {}", format_abs(bus, bank, dest))
            }
            Operand::RelativeLong(offset) => {
                let dest = pc.wrapping_add(3).wrapping_add(offset as u16);
                format!(" {}", format_abs(bus, bank, dest))
            }
            Operand::StackRelative(offset) => format!(" ${offset:02x}, S"),
            Operand::StackRelativeIndirectYIndexed(offset) => {
                format!(" (${offset:02x}, S), Y")
            }
        }
    }
}

fn format_dp(bus: &dyn SimBus, dpr: u16, dp: u8) -> String {
    let addr = Addr::from(dpr.wrapping_add(u16::from(dp)));
    match bus.label_at(addr) {
        Some(label) => label.to_string(),
        None => format!("${dp:02x}"),
    }
}

fn format_abs(bus: &dyn SimBus, bank: u8, abs: u16) -> String {
    let addr = (Addr::from(bank) << 16) | Addr::from(abs);
    match bus.label_at(addr) {
        Some(label) => label.to_string(),
        None => format!("${abs:04x}"),
    }
}

fn format_long(bus: &dyn SimBus, long: u32) -> String {
    let addr = Addr::from(long);
    match bus.label_at(addr) {
        Some(label) => label.to_string(),
        None => format!("${long:06x}"),
    }
}

//===========================================================================//

/// An operation (as defined by the instruction opcode, but without the
/// parameter values) that can be performed on a 65C816 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Operation {
    /// The kind of operation to be performed.
    pub mnemonic: Mnemonic,
    /// The addressing mode to use for this operation.
    pub addr_mode: AddrMode,
}

impl Operation {
    /// Decodes a 6502 opcode into its mnemonic and addressing mode. `flag_m`
    /// should be true if the accumulator is in 8-bit mode. `flag_x` should be
    /// true if the index registers are in 8-bit mode.
    pub fn from_opcode(opcode: u8, flag_m: bool, flag_x: bool) -> Operation {
        let (mnemonic, addr_mode) = match opcode {
            0x00 => (Mnemonic::Brk, AddrMode::ImmediateByte),
            0x10 => (Mnemonic::Bpl, AddrMode::Relative),
            0x20 => (Mnemonic::Jsr, AddrMode::Absolute),
            0x30 => (Mnemonic::Bmi, AddrMode::Relative),
            0x40 => (Mnemonic::Rti, AddrMode::Implied),
            0x50 => (Mnemonic::Bvc, AddrMode::Relative),
            0x60 => (Mnemonic::Rts, AddrMode::Implied),
            0x70 => (Mnemonic::Bvs, AddrMode::Relative),
            0x80 => (Mnemonic::Bra, AddrMode::Relative),
            0x90 => (Mnemonic::Bcc, AddrMode::Relative),
            0xa0 => (Mnemonic::Ldy, AddrMode::immediate(flag_x)),
            0xb0 => (Mnemonic::Bcs, AddrMode::Relative),
            0xc0 => (Mnemonic::Cpy, AddrMode::immediate(flag_x)),
            0xd0 => (Mnemonic::Bne, AddrMode::Relative),
            0xe0 => (Mnemonic::Cpx, AddrMode::immediate(flag_x)),
            0xf0 => (Mnemonic::Beq, AddrMode::Relative),

            0x01 => (Mnemonic::Ora, AddrMode::DirectPageXIndexedIndirect),
            0x11 => (Mnemonic::Ora, AddrMode::DirectPageIndirectYIndexed),
            0x21 => (Mnemonic::And, AddrMode::DirectPageXIndexedIndirect),
            0x31 => (Mnemonic::And, AddrMode::DirectPageIndirectYIndexed),
            0x41 => (Mnemonic::Eor, AddrMode::DirectPageXIndexedIndirect),
            0x51 => (Mnemonic::Eor, AddrMode::DirectPageIndirectYIndexed),
            0x61 => (Mnemonic::Adc, AddrMode::DirectPageXIndexedIndirect),
            0x71 => (Mnemonic::Adc, AddrMode::DirectPageIndirectYIndexed),
            0x81 => (Mnemonic::Sta, AddrMode::DirectPageXIndexedIndirect),
            0x91 => (Mnemonic::Sta, AddrMode::DirectPageIndirectYIndexed),
            0xa1 => (Mnemonic::Lda, AddrMode::DirectPageXIndexedIndirect),
            0xb1 => (Mnemonic::Lda, AddrMode::DirectPageIndirectYIndexed),
            0xc1 => (Mnemonic::Cmp, AddrMode::DirectPageXIndexedIndirect),
            0xd1 => (Mnemonic::Cmp, AddrMode::DirectPageIndirectYIndexed),
            0xe1 => (Mnemonic::Sbc, AddrMode::DirectPageXIndexedIndirect),
            0xf1 => (Mnemonic::Sbc, AddrMode::DirectPageIndirectYIndexed),

            0x02 => (Mnemonic::Cop, AddrMode::ImmediateByte),
            0x12 => (Mnemonic::Ora, AddrMode::DirectPageIndirect),
            0x22 => (Mnemonic::Jsl, AddrMode::AbsoluteLong),
            0x32 => (Mnemonic::And, AddrMode::DirectPageIndirect),
            0x42 => (Mnemonic::Wdm, AddrMode::ImmediateByte),
            0x52 => (Mnemonic::Eor, AddrMode::DirectPageIndirect),
            0x62 => (Mnemonic::Per, AddrMode::RelativeLong),
            0x72 => (Mnemonic::Adc, AddrMode::DirectPageIndirect),
            0x82 => (Mnemonic::Brl, AddrMode::RelativeLong),
            0x92 => (Mnemonic::Sta, AddrMode::DirectPageIndirect),
            0xa2 => (Mnemonic::Ldx, AddrMode::immediate(flag_x)),
            0xb2 => (Mnemonic::Lda, AddrMode::DirectPageIndirect),
            0xc2 => (Mnemonic::Rep, AddrMode::ImmediateByte),
            0xd2 => (Mnemonic::Cmp, AddrMode::DirectPageIndirect),
            0xe2 => (Mnemonic::Sep, AddrMode::ImmediateByte),
            0xf2 => (Mnemonic::Sbc, AddrMode::DirectPageIndirect),

            0x03 => (Mnemonic::Ora, AddrMode::StackRelative),
            0x13 => (Mnemonic::Ora, AddrMode::StackRelativeIndirectYIndexed),
            0x23 => (Mnemonic::And, AddrMode::StackRelative),
            0x33 => (Mnemonic::And, AddrMode::StackRelativeIndirectYIndexed),
            0x43 => (Mnemonic::Eor, AddrMode::StackRelative),
            0x53 => (Mnemonic::Eor, AddrMode::StackRelativeIndirectYIndexed),
            0x63 => (Mnemonic::Adc, AddrMode::StackRelative),
            0x73 => (Mnemonic::Adc, AddrMode::StackRelativeIndirectYIndexed),
            0x83 => (Mnemonic::Sta, AddrMode::StackRelative),
            0x93 => (Mnemonic::Sta, AddrMode::StackRelativeIndirectYIndexed),
            0xa3 => (Mnemonic::Lda, AddrMode::StackRelative),
            0xb3 => (Mnemonic::Lda, AddrMode::StackRelativeIndirectYIndexed),
            0xc3 => (Mnemonic::Cmp, AddrMode::StackRelative),
            0xd3 => (Mnemonic::Cmp, AddrMode::StackRelativeIndirectYIndexed),
            0xe3 => (Mnemonic::Sbc, AddrMode::StackRelative),
            0xf3 => (Mnemonic::Sbc, AddrMode::StackRelativeIndirectYIndexed),

            0x04 => (Mnemonic::Tsb, AddrMode::DirectPage),
            0x14 => (Mnemonic::Trb, AddrMode::DirectPage),
            0x24 => (Mnemonic::Bit, AddrMode::DirectPage),
            0x34 => (Mnemonic::Bit, AddrMode::DirectPageXIndexed),
            0x44 => (Mnemonic::Mvp, AddrMode::BlockMove),
            0x54 => (Mnemonic::Mvn, AddrMode::BlockMove),
            0x64 => (Mnemonic::Stz, AddrMode::DirectPage),
            0x74 => (Mnemonic::Stz, AddrMode::DirectPageXIndexed),
            0x84 => (Mnemonic::Sty, AddrMode::DirectPage),
            0x94 => (Mnemonic::Sty, AddrMode::DirectPageXIndexed),
            0xa4 => (Mnemonic::Ldy, AddrMode::DirectPage),
            0xb4 => (Mnemonic::Ldy, AddrMode::DirectPageXIndexed),
            0xc4 => (Mnemonic::Cpy, AddrMode::DirectPage),
            0xd4 => (Mnemonic::Pei, AddrMode::DirectPageIndirect),
            0xe4 => (Mnemonic::Cpx, AddrMode::DirectPage),
            0xf4 => (Mnemonic::Pea, AddrMode::Absolute),

            0x05 => (Mnemonic::Ora, AddrMode::DirectPage),
            0x15 => (Mnemonic::Ora, AddrMode::DirectPageXIndexed),
            0x25 => (Mnemonic::And, AddrMode::DirectPage),
            0x35 => (Mnemonic::And, AddrMode::DirectPageXIndexed),
            0x45 => (Mnemonic::Eor, AddrMode::DirectPage),
            0x55 => (Mnemonic::Eor, AddrMode::DirectPageXIndexed),
            0x65 => (Mnemonic::Adc, AddrMode::DirectPage),
            0x75 => (Mnemonic::Adc, AddrMode::DirectPageXIndexed),
            0x85 => (Mnemonic::Sta, AddrMode::DirectPage),
            0x95 => (Mnemonic::Sta, AddrMode::DirectPageXIndexed),
            0xa5 => (Mnemonic::Lda, AddrMode::DirectPage),
            0xb5 => (Mnemonic::Lda, AddrMode::DirectPageXIndexed),
            0xc5 => (Mnemonic::Cmp, AddrMode::DirectPage),
            0xd5 => (Mnemonic::Cmp, AddrMode::DirectPageXIndexed),
            0xe5 => (Mnemonic::Sbc, AddrMode::DirectPage),
            0xf5 => (Mnemonic::Sbc, AddrMode::DirectPageXIndexed),

            0x06 => (Mnemonic::Asl, AddrMode::DirectPage),
            0x16 => (Mnemonic::Asl, AddrMode::DirectPageXIndexed),
            0x26 => (Mnemonic::Rol, AddrMode::DirectPage),
            0x36 => (Mnemonic::Rol, AddrMode::DirectPageXIndexed),
            0x46 => (Mnemonic::Lsr, AddrMode::DirectPage),
            0x56 => (Mnemonic::Lsr, AddrMode::DirectPageXIndexed),
            0x66 => (Mnemonic::Ror, AddrMode::DirectPage),
            0x76 => (Mnemonic::Ror, AddrMode::DirectPageXIndexed),
            0x86 => (Mnemonic::Stx, AddrMode::DirectPage),
            0x96 => (Mnemonic::Stx, AddrMode::DirectPageYIndexed),
            0xa6 => (Mnemonic::Ldx, AddrMode::DirectPage),
            0xb6 => (Mnemonic::Ldx, AddrMode::DirectPageYIndexed),
            0xc6 => (Mnemonic::Dec, AddrMode::DirectPage),
            0xd6 => (Mnemonic::Dec, AddrMode::DirectPageXIndexed),
            0xe6 => (Mnemonic::Inc, AddrMode::DirectPage),
            0xf6 => (Mnemonic::Inc, AddrMode::DirectPageXIndexed),

            0x07 => (Mnemonic::Ora, AddrMode::DirectPageIndirectLong),
            0x17 => (Mnemonic::Ora, AddrMode::DirectPageIndirectLongYIndexed),
            0x27 => (Mnemonic::And, AddrMode::DirectPageIndirectLong),
            0x37 => (Mnemonic::And, AddrMode::DirectPageIndirectLongYIndexed),
            0x47 => (Mnemonic::Eor, AddrMode::DirectPageIndirectLong),
            0x57 => (Mnemonic::Eor, AddrMode::DirectPageIndirectLongYIndexed),
            0x67 => (Mnemonic::Adc, AddrMode::DirectPageIndirectLong),
            0x77 => (Mnemonic::Adc, AddrMode::DirectPageIndirectLongYIndexed),
            0x87 => (Mnemonic::Sta, AddrMode::DirectPageIndirectLong),
            0x97 => (Mnemonic::Sta, AddrMode::DirectPageIndirectLongYIndexed),
            0xa7 => (Mnemonic::Lda, AddrMode::DirectPageIndirectLong),
            0xb7 => (Mnemonic::Lda, AddrMode::DirectPageIndirectLongYIndexed),
            0xc7 => (Mnemonic::Cmp, AddrMode::DirectPageIndirectLong),
            0xd7 => (Mnemonic::Cmp, AddrMode::DirectPageIndirectLongYIndexed),
            0xe7 => (Mnemonic::Sbc, AddrMode::DirectPageIndirectLong),
            0xf7 => (Mnemonic::Sbc, AddrMode::DirectPageIndirectLongYIndexed),

            0x08 => (Mnemonic::Php, AddrMode::Implied),
            0x18 => (Mnemonic::Clc, AddrMode::Implied),
            0x28 => (Mnemonic::Plp, AddrMode::Implied),
            0x38 => (Mnemonic::Sec, AddrMode::Implied),
            0x48 => (Mnemonic::Pha, AddrMode::Implied),
            0x58 => (Mnemonic::Cli, AddrMode::Implied),
            0x68 => (Mnemonic::Pla, AddrMode::Implied),
            0x78 => (Mnemonic::Sei, AddrMode::Implied),
            0x88 => (Mnemonic::Dey, AddrMode::Implied),
            0x98 => (Mnemonic::Tya, AddrMode::Implied),
            0xa8 => (Mnemonic::Tay, AddrMode::Implied),
            0xb8 => (Mnemonic::Clv, AddrMode::Implied),
            0xc8 => (Mnemonic::Iny, AddrMode::Implied),
            0xd8 => (Mnemonic::Cld, AddrMode::Implied),
            0xe8 => (Mnemonic::Inx, AddrMode::Implied),
            0xf8 => (Mnemonic::Sed, AddrMode::Implied),

            0x09 => (Mnemonic::Ora, AddrMode::immediate(flag_m)),
            0x19 => (Mnemonic::Ora, AddrMode::AbsoluteYIndexed),
            0x29 => (Mnemonic::And, AddrMode::immediate(flag_m)),
            0x39 => (Mnemonic::And, AddrMode::AbsoluteYIndexed),
            0x49 => (Mnemonic::Eor, AddrMode::immediate(flag_m)),
            0x59 => (Mnemonic::Eor, AddrMode::AbsoluteYIndexed),
            0x69 => (Mnemonic::Adc, AddrMode::immediate(flag_m)),
            0x79 => (Mnemonic::Adc, AddrMode::AbsoluteYIndexed),
            0x89 => (Mnemonic::Bit, AddrMode::immediate(flag_m)),
            0x99 => (Mnemonic::Sta, AddrMode::AbsoluteYIndexed),
            0xa9 => (Mnemonic::Lda, AddrMode::immediate(flag_m)),
            0xb9 => (Mnemonic::Lda, AddrMode::AbsoluteYIndexed),
            0xc9 => (Mnemonic::Cmp, AddrMode::immediate(flag_m)),
            0xd9 => (Mnemonic::Cmp, AddrMode::AbsoluteYIndexed),
            0xe9 => (Mnemonic::Sbc, AddrMode::immediate(flag_m)),
            0xf9 => (Mnemonic::Sbc, AddrMode::AbsoluteYIndexed),

            0x0a => (Mnemonic::Asl, AddrMode::Accumulator),
            0x1a => (Mnemonic::Inc, AddrMode::Accumulator),
            0x2a => (Mnemonic::Rol, AddrMode::Accumulator),
            0x3a => (Mnemonic::Dec, AddrMode::Accumulator),
            0x4a => (Mnemonic::Lsr, AddrMode::Accumulator),
            0x5a => (Mnemonic::Phy, AddrMode::Implied),
            0x6a => (Mnemonic::Ror, AddrMode::Accumulator),
            0x7a => (Mnemonic::Ply, AddrMode::Implied),
            0x8a => (Mnemonic::Txa, AddrMode::Implied),
            0x9a => (Mnemonic::Txs, AddrMode::Implied),
            0xaa => (Mnemonic::Tax, AddrMode::Implied),
            0xba => (Mnemonic::Tsx, AddrMode::Implied),
            0xca => (Mnemonic::Dex, AddrMode::Implied),
            0xda => (Mnemonic::Phx, AddrMode::Implied),
            0xea => (Mnemonic::Nop, AddrMode::Implied),
            0xfa => (Mnemonic::Plx, AddrMode::Implied),

            0x0b => (Mnemonic::Phd, AddrMode::Implied),
            0x1b => (Mnemonic::Tcs, AddrMode::Implied),
            0x2b => (Mnemonic::Pld, AddrMode::Implied),
            0x3b => (Mnemonic::Tsc, AddrMode::Implied),
            0x4b => (Mnemonic::Phk, AddrMode::Implied),
            0x5b => (Mnemonic::Tcd, AddrMode::Implied),
            0x6b => (Mnemonic::Rtl, AddrMode::Implied),
            0x7b => (Mnemonic::Tdc, AddrMode::Implied),
            0x8b => (Mnemonic::Phb, AddrMode::Implied),
            0x9b => (Mnemonic::Txy, AddrMode::Implied),
            0xab => (Mnemonic::Plb, AddrMode::Implied),
            0xbb => (Mnemonic::Tyx, AddrMode::Implied),
            0xcb => (Mnemonic::Wai, AddrMode::Implied),
            0xdb => (Mnemonic::Stp, AddrMode::Implied),
            0xeb => (Mnemonic::Xba, AddrMode::Implied),
            0xfb => (Mnemonic::Xce, AddrMode::Implied),

            0x0c => (Mnemonic::Tsb, AddrMode::Absolute),
            0x1c => (Mnemonic::Trb, AddrMode::Absolute),
            0x2c => (Mnemonic::Bit, AddrMode::Absolute),
            0x3c => (Mnemonic::Bit, AddrMode::AbsoluteXIndexed),
            0x4c => (Mnemonic::Jmp, AddrMode::Absolute),
            0x5c => (Mnemonic::Jml, AddrMode::AbsoluteLong),
            0x6c => (Mnemonic::Jmp, AddrMode::AbsoluteIndirect),
            0x7c => (Mnemonic::Jmp, AddrMode::AbsoluteXIndexedIndirect),
            0x8c => (Mnemonic::Sty, AddrMode::Absolute),
            0x9c => (Mnemonic::Stz, AddrMode::Absolute),
            0xac => (Mnemonic::Ldy, AddrMode::Absolute),
            0xbc => (Mnemonic::Ldy, AddrMode::AbsoluteXIndexed),
            0xcc => (Mnemonic::Cpy, AddrMode::Absolute),
            0xdc => (Mnemonic::Jml, AddrMode::AbsoluteIndirectLong),
            0xec => (Mnemonic::Cpx, AddrMode::Absolute),
            0xfc => (Mnemonic::Jsr, AddrMode::AbsoluteXIndexedIndirect),

            0x0d => (Mnemonic::Ora, AddrMode::Absolute),
            0x1d => (Mnemonic::Ora, AddrMode::AbsoluteXIndexed),
            0x2d => (Mnemonic::And, AddrMode::Absolute),
            0x3d => (Mnemonic::And, AddrMode::AbsoluteXIndexed),
            0x4d => (Mnemonic::Eor, AddrMode::Absolute),
            0x5d => (Mnemonic::Eor, AddrMode::AbsoluteXIndexed),
            0x6d => (Mnemonic::Adc, AddrMode::Absolute),
            0x7d => (Mnemonic::Adc, AddrMode::AbsoluteXIndexed),
            0x8d => (Mnemonic::Sta, AddrMode::Absolute),
            0x9d => (Mnemonic::Sta, AddrMode::AbsoluteXIndexed),
            0xad => (Mnemonic::Lda, AddrMode::Absolute),
            0xbd => (Mnemonic::Lda, AddrMode::AbsoluteXIndexed),
            0xcd => (Mnemonic::Cmp, AddrMode::Absolute),
            0xdd => (Mnemonic::Cmp, AddrMode::AbsoluteXIndexed),
            0xed => (Mnemonic::Sbc, AddrMode::Absolute),
            0xfd => (Mnemonic::Sbc, AddrMode::AbsoluteXIndexed),

            0x0e => (Mnemonic::Asl, AddrMode::Absolute),
            0x1e => (Mnemonic::Asl, AddrMode::AbsoluteXIndexed),
            0x2e => (Mnemonic::Rol, AddrMode::Absolute),
            0x3e => (Mnemonic::Rol, AddrMode::AbsoluteXIndexed),
            0x4e => (Mnemonic::Lsr, AddrMode::Absolute),
            0x5e => (Mnemonic::Lsr, AddrMode::AbsoluteXIndexed),
            0x6e => (Mnemonic::Ror, AddrMode::Absolute),
            0x7e => (Mnemonic::Ror, AddrMode::AbsoluteXIndexed),
            0x8e => (Mnemonic::Stx, AddrMode::Absolute),
            0x9e => (Mnemonic::Stz, AddrMode::AbsoluteXIndexed),
            0xae => (Mnemonic::Ldx, AddrMode::Absolute),
            0xbe => (Mnemonic::Ldx, AddrMode::AbsoluteYIndexed),
            0xce => (Mnemonic::Dec, AddrMode::Absolute),
            0xde => (Mnemonic::Dec, AddrMode::AbsoluteXIndexed),
            0xee => (Mnemonic::Inc, AddrMode::Absolute),
            0xfe => (Mnemonic::Inc, AddrMode::AbsoluteXIndexed),

            0x0f => (Mnemonic::Ora, AddrMode::AbsoluteLong),
            0x1f => (Mnemonic::Ora, AddrMode::AbsoluteLongXIndexed),
            0x2f => (Mnemonic::And, AddrMode::AbsoluteLong),
            0x3f => (Mnemonic::And, AddrMode::AbsoluteLongXIndexed),
            0x4f => (Mnemonic::Eor, AddrMode::AbsoluteLong),
            0x5f => (Mnemonic::Eor, AddrMode::AbsoluteLongXIndexed),
            0x6f => (Mnemonic::Adc, AddrMode::AbsoluteLong),
            0x7f => (Mnemonic::Adc, AddrMode::AbsoluteLongXIndexed),
            0x8f => (Mnemonic::Sta, AddrMode::AbsoluteLong),
            0x9f => (Mnemonic::Sta, AddrMode::AbsoluteLongXIndexed),
            0xaf => (Mnemonic::Lda, AddrMode::AbsoluteLong),
            0xbf => (Mnemonic::Lda, AddrMode::AbsoluteLongXIndexed),
            0xcf => (Mnemonic::Cmp, AddrMode::AbsoluteLong),
            0xdf => (Mnemonic::Cmp, AddrMode::AbsoluteLongXIndexed),
            0xef => (Mnemonic::Sbc, AddrMode::AbsoluteLong),
            0xff => (Mnemonic::Sbc, AddrMode::AbsoluteLongXIndexed),
        };
        Operation { mnemonic, addr_mode }
    }
}

//===========================================================================//

/// A complete instruction, including parameter values, for a 65C816 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Instruction {
    /// The kind of operation to be performed.
    pub mnemonic: Mnemonic,
    /// The addressing mode parameter value.
    pub operand: Operand,
}

impl Instruction {
    /// Returns the size of this instruction, in bytes.
    pub fn size(self) -> u32 {
        1 + self.operand.size()
    }

    /// Decodes a single 65C816 instruction at the given 24-bit starting
    /// address. `flag_m` should be true if the accumulator is in 8-bit
    /// mode. `flag_x` should be true if the index registers are in 8-bit mode.
    pub fn decode(
        bus: &dyn SimBus,
        pc: u32,
        flag_m: bool,
        flag_x: bool,
    ) -> Instruction {
        let opcode = bus.peek_byte(Addr::from(pc & 0xffffff));
        let operation = Operation::from_opcode(opcode, flag_m, flag_x);
        let operand = match operation.addr_mode {
            AddrMode::Implied => Operand::Implied,
            AddrMode::Accumulator => Operand::Accumulator,
            AddrMode::ImmediateByte => {
                Operand::ImmediateByte(next_byte(bus, pc))
            }
            AddrMode::ImmediateWord => {
                Operand::ImmediateWord(next_word(bus, pc))
            }
            AddrMode::BlockMove => {
                Operand::BlockMove(next_byte(bus, pc), third_byte(bus, pc))
            }
            AddrMode::Absolute => Operand::Absolute(next_word(bus, pc)),
            AddrMode::AbsoluteIndirect => {
                Operand::AbsoluteIndirect(next_word(bus, pc))
            }
            AddrMode::AbsoluteIndirectLong => {
                Operand::AbsoluteIndirectLong(next_word(bus, pc))
            }
            AddrMode::AbsoluteXIndexed => {
                Operand::AbsoluteXIndexed(next_word(bus, pc))
            }
            AddrMode::AbsoluteXIndexedIndirect => {
                Operand::AbsoluteXIndexedIndirect(next_word(bus, pc))
            }
            AddrMode::AbsoluteYIndexed => {
                Operand::AbsoluteYIndexed(next_word(bus, pc))
            }
            AddrMode::AbsoluteLong => {
                Operand::AbsoluteLong(next_long(bus, pc))
            }
            AddrMode::AbsoluteLongXIndexed => {
                Operand::AbsoluteLongXIndexed(next_long(bus, pc))
            }
            AddrMode::DirectPage => Operand::DirectPage(next_byte(bus, pc)),
            AddrMode::DirectPageIndirect => {
                Operand::DirectPageIndirect(next_byte(bus, pc))
            }
            AddrMode::DirectPageIndirectLong => {
                Operand::DirectPageIndirectLong(next_byte(bus, pc))
            }
            AddrMode::DirectPageXIndexed => {
                Operand::DirectPageXIndexed(next_byte(bus, pc))
            }
            AddrMode::DirectPageYIndexed => {
                Operand::DirectPageYIndexed(next_byte(bus, pc))
            }
            AddrMode::DirectPageXIndexedIndirect => {
                Operand::DirectPageXIndexedIndirect(next_byte(bus, pc))
            }
            AddrMode::DirectPageIndirectYIndexed => {
                Operand::DirectPageIndirectYIndexed(next_byte(bus, pc))
            }
            AddrMode::DirectPageIndirectLongYIndexed => {
                Operand::DirectPageIndirectLongYIndexed(next_byte(bus, pc))
            }
            AddrMode::Relative => Operand::Relative(next_byte(bus, pc) as i8),
            AddrMode::RelativeLong => {
                Operand::RelativeLong(next_word(bus, pc) as i16)
            }
            AddrMode::StackRelative => {
                Operand::StackRelative(next_byte(bus, pc))
            }
            AddrMode::StackRelativeIndirectYIndexed => {
                Operand::StackRelativeIndirectYIndexed(next_byte(bus, pc))
            }
        };
        Instruction { mnemonic: operation.mnemonic, operand }
    }

    /// Formats the decoded 65C816 instruction as a human-readable string.
    /// `pc` specifies the 24-bit address of the start of the instruction.
    /// `bus` is required for providing labels for addresses; if no labels are
    /// needed, a `new_open_bus` can be used.  `dpr` specifies the 16-bit
    /// direct page register value, while `dbr` specifies the 8-bit data bank
    /// register value; these are also used for determining labels, so if the
    /// bus won't have any labels, zero can be passed for these.
    pub fn format(
        self,
        bus: &dyn SimBus,
        pc: u32,
        dpr: u16,
        dbr: u8,
    ) -> String {
        let bank: u8 = if self.mnemonic.uses_program_bank() {
            (pc >> 16) as u8
        } else {
            dbr
        };
        format!(
            "{}{}",
            self.mnemonic,
            self.operand.format(bus, pc as u16, dpr, bank)
        )
    }
}

// From http://www.6502.org/tutorials/65c816opcodes.html#4: "...instruction
// execution wraps at bank boundaries.  This is true even if the bank boundary
// occurs in the middle of the instruction."  Therefore, in the below functions
// for getting subsequent after the instruction opcode, we perform wrapping
// addition on the bottom 16 bits of the PC.

fn next_byte(bus: &dyn SimBus, pc: u32) -> u8 {
    bus.peek_byte(Addr::from((pc & 0xff0000) | (pc.wrapping_add(1) & 0xffff)))
}

fn third_byte(bus: &dyn SimBus, pc: u32) -> u8 {
    bus.peek_byte(Addr::from((pc & 0xff0000) | (pc.wrapping_add(2) & 0xffff)))
}

fn fourth_byte(bus: &dyn SimBus, pc: u32) -> u8 {
    bus.peek_byte(Addr::from((pc & 0xff0000) | (pc.wrapping_add(3) & 0xffff)))
}

fn next_word(bus: &dyn SimBus, pc: u32) -> u16 {
    let lo = next_byte(bus, pc);
    let hi = third_byte(bus, pc);
    (u16::from(hi) << 8) | u16::from(lo)
}

fn next_long(bus: &dyn SimBus, pc: u32) -> u32 {
    let lo = next_byte(bus, pc);
    let hi = third_byte(bus, pc);
    let bank = fourth_byte(bus, pc);
    (u32::from(bank) << 16) | (u32::from(hi) << 8) | u32::from(lo)
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::Instruction;
    use crate::addr::Addr;
    use crate::bus::{LabeledBus, SimBus, new_rom_bus};
    use std::collections::HashMap;

    fn make_test_bus(code: &[u8]) -> Box<dyn SimBus> {
        let mut rom = vec![0u8; 1 << 4];
        rom[..code.len()].copy_from_slice(code);
        new_rom_bus(rom.into_boxed_slice())
    }

    fn disassemble(code: &[u8]) -> String {
        disassemble_with_bus(&*make_test_bus(code))
    }

    fn disassemble_with_label(code: &[u8], addr: u32, label: &str) -> String {
        let labels = HashMap::from([(label.to_string(), Addr::from(addr))]);
        let bus = LabeledBus::new(make_test_bus(code), labels);
        disassemble_with_bus(&bus)
    }

    fn disassemble_with_bus(bus: &dyn SimBus) -> String {
        Instruction::decode(bus, 0, false, false).format(bus, 0, 0, 0)
    }

    #[test]
    fn disassemble_addr_mode_absolute() {
        assert_eq!(disassemble(&[0x6d, 0x34, 0x12]), "ADC $1234");
        assert_eq!(disassemble(&[0x2d, 0x34, 0x12]), "AND $1234");
        assert_eq!(disassemble(&[0x0e, 0x34, 0x12]), "ASL $1234");
        assert_eq!(disassemble(&[0x2c, 0x34, 0x12]), "BIT $1234");
        assert_eq!(disassemble(&[0xcd, 0x34, 0x12]), "CMP $1234");
        assert_eq!(disassemble(&[0xec, 0x34, 0x12]), "CPX $1234");
        assert_eq!(disassemble(&[0xcc, 0x34, 0x12]), "CPY $1234");
        assert_eq!(disassemble(&[0xce, 0x34, 0x12]), "DEC $1234");
        assert_eq!(disassemble(&[0x4d, 0x34, 0x12]), "EOR $1234");
        assert_eq!(disassemble(&[0xee, 0x34, 0x12]), "INC $1234");
        assert_eq!(disassemble(&[0x4c, 0x34, 0x12]), "JMP $1234");
        assert_eq!(disassemble(&[0x20, 0x34, 0x12]), "JSR $1234");
        assert_eq!(disassemble(&[0xad, 0x34, 0x12]), "LDA $1234");
        assert_eq!(disassemble(&[0xae, 0x34, 0x12]), "LDX $1234");
        assert_eq!(disassemble(&[0xac, 0x34, 0x12]), "LDY $1234");
        assert_eq!(disassemble(&[0x4e, 0x34, 0x12]), "LSR $1234");
        assert_eq!(disassemble(&[0x0d, 0x34, 0x12]), "ORA $1234");
        assert_eq!(disassemble(&[0xf4, 0x34, 0x12]), "PEA $1234");
        assert_eq!(disassemble(&[0x2e, 0x34, 0x12]), "ROL $1234");
        assert_eq!(disassemble(&[0x6e, 0x34, 0x12]), "ROR $1234");
        assert_eq!(disassemble(&[0xed, 0x34, 0x12]), "SBC $1234");
        assert_eq!(disassemble(&[0x8d, 0x34, 0x12]), "STA $1234");
        assert_eq!(disassemble(&[0x8e, 0x34, 0x12]), "STX $1234");
        assert_eq!(disassemble(&[0x8c, 0x34, 0x12]), "STY $1234");
        assert_eq!(disassemble(&[0x9c, 0x34, 0x12]), "STZ $1234");
        assert_eq!(disassemble(&[0x1c, 0x34, 0x12]), "TRB $1234");
        assert_eq!(disassemble(&[0x0c, 0x34, 0x12]), "TSB $1234");
    }

    #[test]
    fn disassemble_addr_mode_absolute_indirect() {
        assert_eq!(disassemble(&[0x6c, 0xef, 0xbe]), "JMP ($beef)");
        assert_eq!(
            disassemble_with_label(&[0x6c, 0xef, 0xbe], 0xbeef, "foo"),
            "JMP (foo)"
        );
    }

    #[test]
    fn disassemble_addr_mode_absolute_indirect_long() {
        assert_eq!(disassemble(&[0xdc, 0xef, 0xbe]), "JML [$beef]");
        assert_eq!(
            disassemble_with_label(&[0xdc, 0xef, 0xbe], 0xbeef, "foo"),
            "JML [foo]"
        );
    }

    #[test]
    fn disassemble_addr_mode_absolute_long() {
        assert_eq!(disassemble(&[0x6f, 0x56, 0x34, 0x12]), "ADC $123456");
        assert_eq!(disassemble(&[0x2f, 0x56, 0x34, 0x12]), "AND $123456");
        assert_eq!(disassemble(&[0xcf, 0x56, 0x34, 0x12]), "CMP $123456");
        assert_eq!(disassemble(&[0x4f, 0x56, 0x34, 0x12]), "EOR $123456");
        assert_eq!(disassemble(&[0x5c, 0x56, 0x34, 0x12]), "JML $123456");
        assert_eq!(disassemble(&[0x22, 0x56, 0x34, 0x12]), "JSL $123456");
        assert_eq!(disassemble(&[0xaf, 0x56, 0x34, 0x12]), "LDA $123456");
        assert_eq!(disassemble(&[0x0f, 0x56, 0x34, 0x12]), "ORA $123456");
        assert_eq!(disassemble(&[0xef, 0x56, 0x34, 0x12]), "SBC $123456");
        assert_eq!(disassemble(&[0x8f, 0x56, 0x34, 0x12]), "STA $123456");
    }

    #[test]
    fn disassemble_addr_mode_absolute_long_x_indexed() {
        assert_eq!(disassemble(&[0x7f, 0x56, 0x34, 0x12]), "ADC $123456, X");
        assert_eq!(disassemble(&[0x3f, 0x56, 0x34, 0x12]), "AND $123456, X");
        assert_eq!(disassemble(&[0xdf, 0x56, 0x34, 0x12]), "CMP $123456, X");
        assert_eq!(disassemble(&[0x5f, 0x56, 0x34, 0x12]), "EOR $123456, X");
        assert_eq!(disassemble(&[0xbf, 0x56, 0x34, 0x12]), "LDA $123456, X");
        assert_eq!(disassemble(&[0x1f, 0x56, 0x34, 0x12]), "ORA $123456, X");
        assert_eq!(disassemble(&[0xff, 0x56, 0x34, 0x12]), "SBC $123456, X");
        assert_eq!(disassemble(&[0x9f, 0x56, 0x34, 0x12]), "STA $123456, X");
    }

    #[test]
    fn disassemble_addr_mode_absolute_x_indexed() {
        assert_eq!(disassemble(&[0x7d, 0x34, 0x12]), "ADC $1234, X");
        assert_eq!(disassemble(&[0x3d, 0x34, 0x12]), "AND $1234, X");
        assert_eq!(disassemble(&[0x1e, 0x34, 0x12]), "ASL $1234, X");
        assert_eq!(disassemble(&[0x3c, 0x34, 0x12]), "BIT $1234, X");
        assert_eq!(disassemble(&[0xdd, 0x34, 0x12]), "CMP $1234, X");
        assert_eq!(disassemble(&[0xde, 0x34, 0x12]), "DEC $1234, X");
        assert_eq!(disassemble(&[0x5d, 0x34, 0x12]), "EOR $1234, X");
        assert_eq!(disassemble(&[0xfe, 0x34, 0x12]), "INC $1234, X");
        assert_eq!(disassemble(&[0xbd, 0x34, 0x12]), "LDA $1234, X");
        assert_eq!(disassemble(&[0xbc, 0x34, 0x12]), "LDY $1234, X");
        assert_eq!(disassemble(&[0x5e, 0x34, 0x12]), "LSR $1234, X");
        assert_eq!(disassemble(&[0x1d, 0x34, 0x12]), "ORA $1234, X");
        assert_eq!(disassemble(&[0x3e, 0x34, 0x12]), "ROL $1234, X");
        assert_eq!(disassemble(&[0x7e, 0x34, 0x12]), "ROR $1234, X");
        assert_eq!(disassemble(&[0xfd, 0x34, 0x12]), "SBC $1234, X");
        assert_eq!(disassemble(&[0x9d, 0x34, 0x12]), "STA $1234, X");
        assert_eq!(disassemble(&[0x9e, 0x34, 0x12]), "STZ $1234, X");
    }

    #[test]
    fn disassemble_addr_mode_absolute_x_indexed_indirect() {
        assert_eq!(disassemble(&[0x7c, 0x34, 0x12]), "JMP ($1234, X)");
        assert_eq!(disassemble(&[0xfc, 0x34, 0x12]), "JSR ($1234, X)");
    }

    #[test]
    fn disassemble_addr_mode_absolute_y_indexed() {
        assert_eq!(disassemble(&[0x79, 0x34, 0x12]), "ADC $1234, Y");
        assert_eq!(disassemble(&[0x39, 0x34, 0x12]), "AND $1234, Y");
        assert_eq!(disassemble(&[0xd9, 0x34, 0x12]), "CMP $1234, Y");
        assert_eq!(disassemble(&[0x59, 0x34, 0x12]), "EOR $1234, Y");
        assert_eq!(disassemble(&[0xb9, 0x34, 0x12]), "LDA $1234, Y");
        assert_eq!(disassemble(&[0xbe, 0x34, 0x12]), "LDX $1234, Y");
        assert_eq!(disassemble(&[0x19, 0x34, 0x12]), "ORA $1234, Y");
        assert_eq!(disassemble(&[0xf9, 0x34, 0x12]), "SBC $1234, Y");
        assert_eq!(disassemble(&[0x99, 0x34, 0x12]), "STA $1234, Y");
    }

    #[test]
    fn disassemble_addr_mode_accumulator() {
        assert_eq!(disassemble(&[0x0a]), "ASL A");
        assert_eq!(disassemble(&[0x3a]), "DEC A");
        assert_eq!(disassemble(&[0x1a]), "INC A");
        assert_eq!(disassemble(&[0x4a]), "LSR A");
        assert_eq!(disassemble(&[0x2a]), "ROL A");
        assert_eq!(disassemble(&[0x6a]), "ROR A");
    }

    #[test]
    fn disassemble_addr_mode_block_move() {
        assert_eq!(disassemble(&[0x54, 0x34, 0x12]), "MVN #$12, #$34");
        assert_eq!(disassemble(&[0x44, 0x34, 0x12]), "MVP #$12, #$34");
    }

    #[test]
    fn disassemble_addr_mode_direct_page() {
        assert_eq!(disassemble(&[0x65, 0x12]), "ADC $12");
        assert_eq!(disassemble(&[0x25, 0x12]), "AND $12");
        assert_eq!(disassemble(&[0x06, 0x12]), "ASL $12");
        assert_eq!(disassemble(&[0x24, 0x12]), "BIT $12");
        assert_eq!(disassemble(&[0xc5, 0x12]), "CMP $12");
        assert_eq!(disassemble(&[0xe4, 0x12]), "CPX $12");
        assert_eq!(disassemble(&[0xc4, 0x12]), "CPY $12");
        assert_eq!(disassemble(&[0xc6, 0x12]), "DEC $12");
        assert_eq!(disassemble(&[0x45, 0x12]), "EOR $12");
        assert_eq!(disassemble(&[0xe6, 0x12]), "INC $12");
        assert_eq!(disassemble(&[0xa5, 0x12]), "LDA $12");
        assert_eq!(disassemble(&[0xa6, 0x12]), "LDX $12");
        assert_eq!(disassemble(&[0xa4, 0x12]), "LDY $12");
        assert_eq!(disassemble(&[0x46, 0x12]), "LSR $12");
        assert_eq!(disassemble(&[0x05, 0x12]), "ORA $12");
        assert_eq!(disassemble(&[0x26, 0x12]), "ROL $12");
        assert_eq!(disassemble(&[0x66, 0x12]), "ROR $12");
        assert_eq!(disassemble(&[0xe5, 0x12]), "SBC $12");
        assert_eq!(disassemble(&[0x85, 0x12]), "STA $12");
        assert_eq!(disassemble(&[0x86, 0x12]), "STX $12");
        assert_eq!(disassemble(&[0x84, 0x12]), "STY $12");
        assert_eq!(disassemble(&[0x64, 0x12]), "STZ $12");
        assert_eq!(disassemble(&[0x14, 0x12]), "TRB $12");
        assert_eq!(disassemble(&[0x04, 0x12]), "TSB $12");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_indirect() {
        assert_eq!(disassemble(&[0x72, 0x12]), "ADC ($12)");
        assert_eq!(disassemble(&[0x32, 0x12]), "AND ($12)");
        assert_eq!(disassemble(&[0xd2, 0x12]), "CMP ($12)");
        assert_eq!(disassemble(&[0x52, 0x12]), "EOR ($12)");
        assert_eq!(disassemble(&[0xb2, 0x12]), "LDA ($12)");
        assert_eq!(disassemble(&[0x12, 0x12]), "ORA ($12)");
        assert_eq!(disassemble(&[0xd4, 0x12]), "PEI ($12)");
        assert_eq!(disassemble(&[0xf2, 0x12]), "SBC ($12)");
        assert_eq!(disassemble(&[0x92, 0x12]), "STA ($12)");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_indirect_long() {
        assert_eq!(disassemble(&[0x67, 0x12]), "ADC [$12]");
        assert_eq!(disassemble(&[0x27, 0x12]), "AND [$12]");
        assert_eq!(disassemble(&[0xc7, 0x12]), "CMP [$12]");
        assert_eq!(disassemble(&[0x47, 0x12]), "EOR [$12]");
        assert_eq!(disassemble(&[0xa7, 0x12]), "LDA [$12]");
        assert_eq!(disassemble(&[0x07, 0x12]), "ORA [$12]");
        assert_eq!(disassemble(&[0xe7, 0x12]), "SBC [$12]");
        assert_eq!(disassemble(&[0x87, 0x12]), "STA [$12]");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_indirect_long_y_indexed() {
        assert_eq!(disassemble(&[0x77, 0x12]), "ADC [$12], Y");
        assert_eq!(disassemble(&[0x37, 0x12]), "AND [$12], Y");
        assert_eq!(disassemble(&[0xd7, 0x12]), "CMP [$12], Y");
        assert_eq!(disassemble(&[0x57, 0x12]), "EOR [$12], Y");
        assert_eq!(disassemble(&[0xb7, 0x12]), "LDA [$12], Y");
        assert_eq!(disassemble(&[0x17, 0x12]), "ORA [$12], Y");
        assert_eq!(disassemble(&[0xf7, 0x12]), "SBC [$12], Y");
        assert_eq!(disassemble(&[0x97, 0x12]), "STA [$12], Y");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_indirect_y_indexed() {
        assert_eq!(disassemble(&[0x71, 0x12]), "ADC ($12), Y");
        assert_eq!(disassemble(&[0x31, 0x12]), "AND ($12), Y");
        assert_eq!(disassemble(&[0xd1, 0x12]), "CMP ($12), Y");
        assert_eq!(disassemble(&[0x51, 0x12]), "EOR ($12), Y");
        assert_eq!(disassemble(&[0xb1, 0x12]), "LDA ($12), Y");
        assert_eq!(disassemble(&[0x11, 0x12]), "ORA ($12), Y");
        assert_eq!(disassemble(&[0xf1, 0x12]), "SBC ($12), Y");
        assert_eq!(disassemble(&[0x91, 0x12]), "STA ($12), Y");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_x_indexed() {
        assert_eq!(disassemble(&[0x75, 0x12]), "ADC $12, X");
        assert_eq!(disassemble(&[0x35, 0x12]), "AND $12, X");
        assert_eq!(disassemble(&[0x16, 0x12]), "ASL $12, X");
        assert_eq!(disassemble(&[0x34, 0x12]), "BIT $12, X");
        assert_eq!(disassemble(&[0xd5, 0x12]), "CMP $12, X");
        assert_eq!(disassemble(&[0xd6, 0x12]), "DEC $12, X");
        assert_eq!(disassemble(&[0x55, 0x12]), "EOR $12, X");
        assert_eq!(disassemble(&[0xf6, 0x12]), "INC $12, X");
        assert_eq!(disassemble(&[0xb5, 0x12]), "LDA $12, X");
        assert_eq!(disassemble(&[0xb4, 0x12]), "LDY $12, X");
        assert_eq!(disassemble(&[0x56, 0x12]), "LSR $12, X");
        assert_eq!(disassemble(&[0x15, 0x12]), "ORA $12, X");
        assert_eq!(disassemble(&[0x36, 0x12]), "ROL $12, X");
        assert_eq!(disassemble(&[0x76, 0x12]), "ROR $12, X");
        assert_eq!(disassemble(&[0xf5, 0x12]), "SBC $12, X");
        assert_eq!(disassemble(&[0x95, 0x12]), "STA $12, X");
        assert_eq!(disassemble(&[0x94, 0x12]), "STY $12, X");
        assert_eq!(disassemble(&[0x74, 0x12]), "STZ $12, X");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_x_indexed_indirect() {
        assert_eq!(disassemble(&[0x61, 0x12]), "ADC ($12, X)");
        assert_eq!(disassemble(&[0x21, 0x12]), "AND ($12, X)");
        assert_eq!(disassemble(&[0xc1, 0x12]), "CMP ($12, X)");
        assert_eq!(disassemble(&[0x41, 0x12]), "EOR ($12, X)");
        assert_eq!(disassemble(&[0xa1, 0x12]), "LDA ($12, X)");
        assert_eq!(disassemble(&[0x01, 0x12]), "ORA ($12, X)");
        assert_eq!(disassemble(&[0xe1, 0x12]), "SBC ($12, X)");
        assert_eq!(disassemble(&[0x81, 0x12]), "STA ($12, X)");
    }

    #[test]
    fn disassemble_addr_mode_direct_page_y_indexed() {
        assert_eq!(disassemble(&[0xb6, 0x12]), "LDX $12, Y");
        assert_eq!(disassemble(&[0x96, 0x12]), "STX $12, Y");
    }

    #[test]
    fn disassemble_addr_mode_implied() {
        assert_eq!(disassemble(&[0x18]), "CLC");
        assert_eq!(disassemble(&[0xd8]), "CLD");
        assert_eq!(disassemble(&[0x58]), "CLI");
        assert_eq!(disassemble(&[0xb8]), "CLV");
        assert_eq!(disassemble(&[0xca]), "DEX");
        assert_eq!(disassemble(&[0x88]), "DEY");
        assert_eq!(disassemble(&[0xe8]), "INX");
        assert_eq!(disassemble(&[0xc8]), "INY");
        assert_eq!(disassemble(&[0xea]), "NOP");
        assert_eq!(disassemble(&[0x48]), "PHA");
        assert_eq!(disassemble(&[0x8b]), "PHB");
        assert_eq!(disassemble(&[0x0b]), "PHD");
        assert_eq!(disassemble(&[0x4b]), "PHK");
        assert_eq!(disassemble(&[0x08]), "PHP");
        assert_eq!(disassemble(&[0xda]), "PHX");
        assert_eq!(disassemble(&[0x5a]), "PHY");
        assert_eq!(disassemble(&[0x68]), "PLA");
        assert_eq!(disassemble(&[0xab]), "PLB");
        assert_eq!(disassemble(&[0x2b]), "PLD");
        assert_eq!(disassemble(&[0x28]), "PLP");
        assert_eq!(disassemble(&[0xfa]), "PLX");
        assert_eq!(disassemble(&[0x7a]), "PLY");
        assert_eq!(disassemble(&[0x40]), "RTI");
        assert_eq!(disassemble(&[0x6b]), "RTL");
        assert_eq!(disassemble(&[0x60]), "RTS");
        assert_eq!(disassemble(&[0x38]), "SEC");
        assert_eq!(disassemble(&[0xf8]), "SED");
        assert_eq!(disassemble(&[0x78]), "SEI");
        assert_eq!(disassemble(&[0xdb]), "STP");
        assert_eq!(disassemble(&[0xaa]), "TAX");
        assert_eq!(disassemble(&[0xa8]), "TAY");
        assert_eq!(disassemble(&[0x5b]), "TCD");
        assert_eq!(disassemble(&[0x1b]), "TCS");
        assert_eq!(disassemble(&[0x7b]), "TDC");
        assert_eq!(disassemble(&[0x3b]), "TSC");
        assert_eq!(disassemble(&[0xba]), "TSX");
        assert_eq!(disassemble(&[0x8a]), "TXA");
        assert_eq!(disassemble(&[0x9a]), "TXS");
        assert_eq!(disassemble(&[0x9b]), "TXY");
        assert_eq!(disassemble(&[0x98]), "TYA");
        assert_eq!(disassemble(&[0xbb]), "TYX");
        assert_eq!(disassemble(&[0xcb]), "WAI");
        assert_eq!(disassemble(&[0xeb]), "XBA");
        assert_eq!(disassemble(&[0xfb]), "XCE");
    }

    #[test]
    fn disassemble_addr_mode_relative() {
        assert_eq!(disassemble(&[0x90, 0x10]), "BCC $0012");
        assert_eq!(disassemble(&[0xb0, 0x0e]), "BCS $0010");
        assert_eq!(disassemble(&[0xf0, 0x80]), "BEQ $ff82");
        assert_eq!(disassemble(&[0x30, 0xf0]), "BMI $fff2");
        assert_eq!(disassemble(&[0xd0, 0x81]), "BNE $ff83");
        assert_eq!(disassemble(&[0x10, 0x7f]), "BPL $0081");
        assert_eq!(disassemble(&[0x50, 0xfe]), "BVC $0000");
        assert_eq!(disassemble(&[0x70, 0xfd]), "BVS $ffff");
        assert_eq!(
            disassemble_with_label(&[0x90, 0x10], 0x12, "foo"),
            "BCC foo"
        );
    }

    #[test]
    fn disassemble_addr_mode_relative_long() {
        assert_eq!(disassemble(&[0x82, 0xfd, 0x01]), "BRL $0200");
        assert_eq!(disassemble(&[0x62, 0xfd, 0xff]), "PER $0000");
    }

    #[test]
    fn disassemble_addr_mode_stack_relative() {
        assert_eq!(disassemble(&[0x63, 0x12]), "ADC $12, S");
        assert_eq!(disassemble(&[0x23, 0x12]), "AND $12, S");
        assert_eq!(disassemble(&[0xc3, 0x12]), "CMP $12, S");
        assert_eq!(disassemble(&[0x43, 0x12]), "EOR $12, S");
        assert_eq!(disassemble(&[0xa3, 0x12]), "LDA $12, S");
        assert_eq!(disassemble(&[0x03, 0x12]), "ORA $12, S");
        assert_eq!(disassemble(&[0xe3, 0x12]), "SBC $12, S");
        assert_eq!(disassemble(&[0x83, 0x12]), "STA $12, S");
    }

    #[test]
    fn disassemble_addr_mode_stack_relative_indirect_y_indexed() {
        assert_eq!(disassemble(&[0x73, 0x12]), "ADC ($12, S), Y");
        assert_eq!(disassemble(&[0x33, 0x12]), "AND ($12, S), Y");
        assert_eq!(disassemble(&[0xd3, 0x12]), "CMP ($12, S), Y");
        assert_eq!(disassemble(&[0x53, 0x12]), "EOR ($12, S), Y");
        assert_eq!(disassemble(&[0xb3, 0x12]), "LDA ($12, S), Y");
        assert_eq!(disassemble(&[0x13, 0x12]), "ORA ($12, S), Y");
        assert_eq!(disassemble(&[0xf3, 0x12]), "SBC ($12, S), Y");
        assert_eq!(disassemble(&[0x93, 0x12]), "STA ($12, S), Y");
    }
}

//===========================================================================//
