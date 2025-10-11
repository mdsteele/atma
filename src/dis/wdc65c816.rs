//! Facilities for disassembling 65C816 machine code.

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
    Immediate,
    /// Operate on a constant 8-bit or 16-bit value (depending on the
    /// accumulator size) immediately following the opcode.
    ImmediateM,
    /// Operate on a constant 8-bit or 16-bit value (depending on the
    /// index register size) immediately following the opcode.
    ImmediateX,
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
    DirectPageIndirectYIndexedLong,
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

//===========================================================================//
