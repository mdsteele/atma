//! Facilities for disassembling 6502 machine code.

use crate::bus::SimBus;
use std::fmt;

//===========================================================================//

/// An operation (as defined by the instruction opcode, but without the
/// parameter values) that can be performed on a 6502 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Operation {
    /// The kind of operation to be performed.
    pub mnemonic: Mnemonic,
    /// The addressing mode to use for this operation.
    pub addr_mode: AddrMode,
}

impl Operation {
    /// Decodes a 6502 opcode into its mnemonic and addressing mode.
    pub fn from_opcode(opcode: u8) -> Operation {
        let (mnemonic, addr_mode) = match opcode {
            0x00 => (Mnemonic::Brk, AddrMode::Immediate),
            0x01 => (Mnemonic::Ora, AddrMode::XIndexedZeroPageIndirect),
            0x02 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x03 => (Mnemonic::UndocSlo, AddrMode::XIndexedZeroPageIndirect),
            0x04 => (Mnemonic::UndocNop, AddrMode::ZeroPage),
            0x05 => (Mnemonic::Ora, AddrMode::ZeroPage),
            0x06 => (Mnemonic::Asl, AddrMode::ZeroPage),
            0x07 => (Mnemonic::UndocSlo, AddrMode::ZeroPage),
            0x08 => (Mnemonic::Php, AddrMode::Implied),
            0x09 => (Mnemonic::Ora, AddrMode::Immediate),
            0x0a => (Mnemonic::Asl, AddrMode::Accumulator),
            0x0b => (Mnemonic::UndocAnc, AddrMode::Immediate),
            0x0c => (Mnemonic::UndocNop, AddrMode::Absolute),
            0x0d => (Mnemonic::Ora, AddrMode::Absolute),
            0x0e => (Mnemonic::Asl, AddrMode::Absolute),
            0x0f => (Mnemonic::UndocSlo, AddrMode::Absolute),
            0x10 => (Mnemonic::Bpl, AddrMode::Relative),
            0x11 => (Mnemonic::Ora, AddrMode::ZeroPageIndirectYIndexed),
            0x12 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x13 => (Mnemonic::UndocSlo, AddrMode::ZeroPageIndirectYIndexed),
            0x14 => (Mnemonic::UndocNop, AddrMode::XIndexedZeroPage),
            0x15 => (Mnemonic::Ora, AddrMode::XIndexedZeroPage),
            0x16 => (Mnemonic::Asl, AddrMode::XIndexedZeroPage),
            0x17 => (Mnemonic::UndocSlo, AddrMode::XIndexedZeroPage),
            0x18 => (Mnemonic::Clc, AddrMode::Implied),
            0x19 => (Mnemonic::Ora, AddrMode::YIndexedAbsolute),
            0x1a => (Mnemonic::UndocNop, AddrMode::Implied),
            0x1b => (Mnemonic::UndocSlo, AddrMode::YIndexedAbsolute),
            0x1c => (Mnemonic::UndocNop, AddrMode::XIndexedAbsolute),
            0x1d => (Mnemonic::Ora, AddrMode::XIndexedAbsolute),
            0x1e => (Mnemonic::Asl, AddrMode::XIndexedAbsolute),
            0x1f => (Mnemonic::UndocSlo, AddrMode::XIndexedAbsolute),
            0x20 => (Mnemonic::Jsr, AddrMode::Absolute),
            0x21 => (Mnemonic::And, AddrMode::XIndexedZeroPageIndirect),
            0x22 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x23 => (Mnemonic::UndocRla, AddrMode::XIndexedZeroPageIndirect),
            0x24 => (Mnemonic::Bit, AddrMode::ZeroPage),
            0x25 => (Mnemonic::And, AddrMode::ZeroPage),
            0x26 => (Mnemonic::Rol, AddrMode::ZeroPage),
            0x27 => (Mnemonic::UndocRla, AddrMode::ZeroPage),
            0x28 => (Mnemonic::Plp, AddrMode::Implied),
            0x29 => (Mnemonic::And, AddrMode::Immediate),
            0x2a => (Mnemonic::Rol, AddrMode::Accumulator),
            0x2b => (Mnemonic::UndocAnc, AddrMode::Immediate),
            0x2c => (Mnemonic::Bit, AddrMode::Absolute),
            0x2d => (Mnemonic::And, AddrMode::Absolute),
            0x2e => (Mnemonic::Rol, AddrMode::Absolute),
            0x2f => (Mnemonic::UndocRla, AddrMode::Absolute),
            0x30 => (Mnemonic::Bmi, AddrMode::Relative),
            0x31 => (Mnemonic::And, AddrMode::ZeroPageIndirectYIndexed),
            0x32 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x33 => (Mnemonic::UndocRla, AddrMode::ZeroPageIndirectYIndexed),
            0x34 => (Mnemonic::UndocNop, AddrMode::XIndexedZeroPage),
            0x35 => (Mnemonic::And, AddrMode::XIndexedZeroPage),
            0x36 => (Mnemonic::Rol, AddrMode::XIndexedZeroPage),
            0x37 => (Mnemonic::UndocRla, AddrMode::XIndexedZeroPage),
            0x38 => (Mnemonic::Sec, AddrMode::Implied),
            0x39 => (Mnemonic::And, AddrMode::YIndexedAbsolute),
            0x3a => (Mnemonic::UndocNop, AddrMode::Implied),
            0x3b => (Mnemonic::UndocRla, AddrMode::YIndexedAbsolute),
            0x3c => (Mnemonic::UndocNop, AddrMode::XIndexedAbsolute),
            0x3d => (Mnemonic::And, AddrMode::XIndexedAbsolute),
            0x3e => (Mnemonic::Rol, AddrMode::XIndexedAbsolute),
            0x3f => (Mnemonic::UndocRla, AddrMode::XIndexedAbsolute),
            0x40 => (Mnemonic::Rti, AddrMode::Implied),
            0x41 => (Mnemonic::Eor, AddrMode::XIndexedZeroPageIndirect),
            0x42 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x43 => (Mnemonic::UndocSre, AddrMode::XIndexedZeroPageIndirect),
            0x44 => (Mnemonic::UndocNop, AddrMode::ZeroPage),
            0x45 => (Mnemonic::Eor, AddrMode::ZeroPage),
            0x46 => (Mnemonic::Lsr, AddrMode::ZeroPage),
            0x47 => (Mnemonic::UndocSre, AddrMode::ZeroPage),
            0x48 => (Mnemonic::Pha, AddrMode::Implied),
            0x49 => (Mnemonic::Eor, AddrMode::Immediate),
            0x4a => (Mnemonic::Lsr, AddrMode::Accumulator),
            0x4b => (Mnemonic::UndocAsr, AddrMode::Immediate),
            0x4c => (Mnemonic::Jmp, AddrMode::Absolute),
            0x4d => (Mnemonic::Eor, AddrMode::Absolute),
            0x4e => (Mnemonic::Lsr, AddrMode::Absolute),
            0x4f => (Mnemonic::UndocSre, AddrMode::Absolute),
            0x50 => (Mnemonic::Bvc, AddrMode::Relative),
            0x51 => (Mnemonic::Eor, AddrMode::ZeroPageIndirectYIndexed),
            0x52 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x53 => (Mnemonic::UndocSre, AddrMode::ZeroPageIndirectYIndexed),
            0x54 => (Mnemonic::UndocNop, AddrMode::XIndexedZeroPage),
            0x55 => (Mnemonic::Eor, AddrMode::XIndexedZeroPage),
            0x56 => (Mnemonic::Lsr, AddrMode::XIndexedZeroPage),
            0x57 => (Mnemonic::UndocSre, AddrMode::XIndexedZeroPage),
            0x58 => (Mnemonic::Cli, AddrMode::Implied),
            0x59 => (Mnemonic::Eor, AddrMode::YIndexedAbsolute),
            0x5a => (Mnemonic::UndocNop, AddrMode::Implied),
            0x5b => (Mnemonic::UndocSre, AddrMode::YIndexedAbsolute),
            0x5c => (Mnemonic::UndocNop, AddrMode::XIndexedAbsolute),
            0x5d => (Mnemonic::Eor, AddrMode::XIndexedAbsolute),
            0x5e => (Mnemonic::Lsr, AddrMode::XIndexedAbsolute),
            0x5f => (Mnemonic::UndocSre, AddrMode::XIndexedAbsolute),
            0x60 => (Mnemonic::Rts, AddrMode::Implied),
            0x61 => (Mnemonic::Adc, AddrMode::XIndexedZeroPageIndirect),
            0x62 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x63 => (Mnemonic::UndocRra, AddrMode::XIndexedZeroPageIndirect),
            0x64 => (Mnemonic::UndocNop, AddrMode::ZeroPage),
            0x65 => (Mnemonic::Adc, AddrMode::ZeroPage),
            0x66 => (Mnemonic::Ror, AddrMode::ZeroPage),
            0x67 => (Mnemonic::UndocRra, AddrMode::ZeroPage),
            0x68 => (Mnemonic::Pla, AddrMode::Implied),
            0x69 => (Mnemonic::Adc, AddrMode::Immediate),
            0x6a => (Mnemonic::Ror, AddrMode::Accumulator),
            0x6b => (Mnemonic::UndocArr, AddrMode::Immediate),
            0x6c => (Mnemonic::Jmp, AddrMode::AbsoluteIndirect),
            0x6d => (Mnemonic::Adc, AddrMode::Absolute),
            0x6e => (Mnemonic::Ror, AddrMode::Absolute),
            0x6f => (Mnemonic::UndocRra, AddrMode::Absolute),
            0x70 => (Mnemonic::Bvs, AddrMode::Relative),
            0x71 => (Mnemonic::Adc, AddrMode::ZeroPageIndirectYIndexed),
            0x72 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x73 => (Mnemonic::UndocRra, AddrMode::ZeroPageIndirectYIndexed),
            0x74 => (Mnemonic::UndocNop, AddrMode::XIndexedZeroPage),
            0x75 => (Mnemonic::Adc, AddrMode::XIndexedZeroPage),
            0x76 => (Mnemonic::Ror, AddrMode::XIndexedZeroPage),
            0x77 => (Mnemonic::UndocRra, AddrMode::XIndexedZeroPage),
            0x78 => (Mnemonic::Sei, AddrMode::Implied),
            0x79 => (Mnemonic::Adc, AddrMode::YIndexedAbsolute),
            0x7a => (Mnemonic::UndocNop, AddrMode::Implied),
            0x7b => (Mnemonic::UndocRra, AddrMode::YIndexedAbsolute),
            0x7c => (Mnemonic::UndocNop, AddrMode::XIndexedAbsolute),
            0x7d => (Mnemonic::Adc, AddrMode::XIndexedAbsolute),
            0x7e => (Mnemonic::Ror, AddrMode::XIndexedAbsolute),
            0x7f => (Mnemonic::UndocRra, AddrMode::XIndexedAbsolute),
            0x80 => (Mnemonic::UndocNop, AddrMode::Immediate),
            0x81 => (Mnemonic::Sta, AddrMode::XIndexedZeroPageIndirect),
            0x82 => (Mnemonic::UndocNop, AddrMode::Immediate),
            0x83 => (Mnemonic::UndocSax, AddrMode::XIndexedZeroPageIndirect),
            0x84 => (Mnemonic::Sty, AddrMode::ZeroPage),
            0x85 => (Mnemonic::Sta, AddrMode::ZeroPage),
            0x86 => (Mnemonic::Stx, AddrMode::ZeroPage),
            0x87 => (Mnemonic::UndocSax, AddrMode::ZeroPage),
            0x88 => (Mnemonic::Dey, AddrMode::Implied),
            0x89 => (Mnemonic::UndocNop, AddrMode::Immediate),
            0x8a => (Mnemonic::Txa, AddrMode::Implied),
            0x8b => (Mnemonic::UndocXaa, AddrMode::Immediate),
            0x8c => (Mnemonic::Sty, AddrMode::Absolute),
            0x8d => (Mnemonic::Sta, AddrMode::Absolute),
            0x8e => (Mnemonic::Stx, AddrMode::Absolute),
            0x8f => (Mnemonic::UndocSax, AddrMode::Absolute),
            0x90 => (Mnemonic::Bcc, AddrMode::Relative),
            0x91 => (Mnemonic::Sta, AddrMode::ZeroPageIndirectYIndexed),
            0x92 => (Mnemonic::UndocJam, AddrMode::Implied),
            0x93 => (Mnemonic::UndocSha, AddrMode::ZeroPageIndirectYIndexed),
            0x94 => (Mnemonic::Sty, AddrMode::XIndexedZeroPage),
            0x95 => (Mnemonic::Sta, AddrMode::XIndexedZeroPage),
            0x96 => (Mnemonic::Stx, AddrMode::YIndexedZeroPage),
            0x97 => (Mnemonic::UndocSax, AddrMode::YIndexedZeroPage),
            0x98 => (Mnemonic::Tya, AddrMode::Implied),
            0x99 => (Mnemonic::Sta, AddrMode::YIndexedAbsolute),
            0x9a => (Mnemonic::Txs, AddrMode::Implied),
            0x9b => (Mnemonic::UndocShs, AddrMode::YIndexedAbsolute),
            0x9c => (Mnemonic::UndocShy, AddrMode::XIndexedAbsolute),
            0x9d => (Mnemonic::Sta, AddrMode::XIndexedAbsolute),
            0x9e => (Mnemonic::UndocShx, AddrMode::YIndexedAbsolute),
            0x9f => (Mnemonic::UndocSha, AddrMode::YIndexedAbsolute),
            0xa0 => (Mnemonic::Ldy, AddrMode::Immediate),
            0xa1 => (Mnemonic::Lda, AddrMode::XIndexedZeroPageIndirect),
            0xa2 => (Mnemonic::Ldx, AddrMode::Immediate),
            0xa3 => (Mnemonic::UndocLax, AddrMode::XIndexedZeroPageIndirect),
            0xa4 => (Mnemonic::Ldy, AddrMode::ZeroPage),
            0xa5 => (Mnemonic::Lda, AddrMode::ZeroPage),
            0xa6 => (Mnemonic::Ldx, AddrMode::ZeroPage),
            0xa7 => (Mnemonic::UndocLax, AddrMode::ZeroPage),
            0xa8 => (Mnemonic::Tay, AddrMode::Implied),
            0xa9 => (Mnemonic::Lda, AddrMode::Immediate),
            0xaa => (Mnemonic::Tax, AddrMode::Implied),
            0xab => (Mnemonic::UndocLax, AddrMode::Immediate),
            0xac => (Mnemonic::Ldy, AddrMode::Absolute),
            0xad => (Mnemonic::Lda, AddrMode::Absolute),
            0xae => (Mnemonic::Ldx, AddrMode::Absolute),
            0xaf => (Mnemonic::UndocLax, AddrMode::Absolute),
            0xb0 => (Mnemonic::Bcs, AddrMode::Relative),
            0xb1 => (Mnemonic::Lda, AddrMode::ZeroPageIndirectYIndexed),
            0xb2 => (Mnemonic::UndocJam, AddrMode::Implied),
            0xb3 => (Mnemonic::UndocLax, AddrMode::ZeroPageIndirectYIndexed),
            0xb4 => (Mnemonic::Ldy, AddrMode::XIndexedZeroPage),
            0xb5 => (Mnemonic::Lda, AddrMode::XIndexedZeroPage),
            0xb6 => (Mnemonic::Ldx, AddrMode::YIndexedZeroPage),
            0xb7 => (Mnemonic::UndocLax, AddrMode::YIndexedZeroPage),
            0xb8 => (Mnemonic::Clv, AddrMode::Implied),
            0xb9 => (Mnemonic::Lda, AddrMode::YIndexedAbsolute),
            0xba => (Mnemonic::Tsx, AddrMode::Implied),
            0xbb => (Mnemonic::UndocLas, AddrMode::YIndexedAbsolute),
            0xbc => (Mnemonic::Ldy, AddrMode::XIndexedAbsolute),
            0xbd => (Mnemonic::Lda, AddrMode::XIndexedAbsolute),
            0xbe => (Mnemonic::Ldx, AddrMode::YIndexedAbsolute),
            0xbf => (Mnemonic::UndocLax, AddrMode::YIndexedAbsolute),
            0xc0 => (Mnemonic::Cpy, AddrMode::Immediate),
            0xc1 => (Mnemonic::Cmp, AddrMode::XIndexedZeroPageIndirect),
            0xc2 => (Mnemonic::UndocNop, AddrMode::Immediate),
            0xc3 => (Mnemonic::UndocDcp, AddrMode::XIndexedZeroPageIndirect),
            0xc4 => (Mnemonic::Cpy, AddrMode::ZeroPage),
            0xc5 => (Mnemonic::Cmp, AddrMode::ZeroPage),
            0xc6 => (Mnemonic::Dec, AddrMode::ZeroPage),
            0xc7 => (Mnemonic::UndocDcp, AddrMode::ZeroPage),
            0xc8 => (Mnemonic::Iny, AddrMode::Implied),
            0xc9 => (Mnemonic::Cmp, AddrMode::Immediate),
            0xca => (Mnemonic::Dex, AddrMode::Implied),
            0xcb => (Mnemonic::UndocSbx, AddrMode::Immediate),
            0xcc => (Mnemonic::Cpy, AddrMode::Absolute),
            0xcd => (Mnemonic::Cmp, AddrMode::Absolute),
            0xce => (Mnemonic::Dec, AddrMode::Absolute),
            0xcf => (Mnemonic::UndocDcp, AddrMode::Absolute),
            0xd0 => (Mnemonic::Bne, AddrMode::Relative),
            0xd1 => (Mnemonic::Cmp, AddrMode::ZeroPageIndirectYIndexed),
            0xd2 => (Mnemonic::UndocJam, AddrMode::Implied),
            0xd3 => (Mnemonic::UndocDcp, AddrMode::ZeroPageIndirectYIndexed),
            0xd4 => (Mnemonic::UndocNop, AddrMode::XIndexedZeroPage),
            0xd5 => (Mnemonic::Cmp, AddrMode::XIndexedZeroPage),
            0xd6 => (Mnemonic::Dec, AddrMode::XIndexedZeroPage),
            0xd7 => (Mnemonic::UndocDcp, AddrMode::XIndexedZeroPage),
            0xd8 => (Mnemonic::Cld, AddrMode::Implied),
            0xd9 => (Mnemonic::Cmp, AddrMode::YIndexedAbsolute),
            0xda => (Mnemonic::UndocNop, AddrMode::Implied),
            0xdb => (Mnemonic::UndocDcp, AddrMode::YIndexedAbsolute),
            0xdc => (Mnemonic::UndocNop, AddrMode::XIndexedAbsolute),
            0xdd => (Mnemonic::Cmp, AddrMode::XIndexedAbsolute),
            0xde => (Mnemonic::Dec, AddrMode::XIndexedAbsolute),
            0xdf => (Mnemonic::UndocDcp, AddrMode::XIndexedAbsolute),
            0xe0 => (Mnemonic::Cpx, AddrMode::Immediate),
            0xe1 => (Mnemonic::Sbc, AddrMode::XIndexedZeroPageIndirect),
            0xe2 => (Mnemonic::UndocNop, AddrMode::Immediate),
            0xe3 => (Mnemonic::UndocIsc, AddrMode::XIndexedZeroPageIndirect),
            0xe4 => (Mnemonic::Cpx, AddrMode::ZeroPage),
            0xe5 => (Mnemonic::Sbc, AddrMode::ZeroPage),
            0xe6 => (Mnemonic::Inc, AddrMode::ZeroPage),
            0xe7 => (Mnemonic::UndocIsc, AddrMode::ZeroPage),
            0xe8 => (Mnemonic::Inx, AddrMode::Implied),
            0xe9 => (Mnemonic::Sbc, AddrMode::Immediate),
            0xea => (Mnemonic::Nop, AddrMode::Implied),
            0xeb => (Mnemonic::UndocSbc, AddrMode::Immediate),
            0xec => (Mnemonic::Cpx, AddrMode::Absolute),
            0xed => (Mnemonic::Sbc, AddrMode::Absolute),
            0xee => (Mnemonic::Inc, AddrMode::Absolute),
            0xef => (Mnemonic::UndocIsc, AddrMode::Absolute),
            0xf0 => (Mnemonic::Beq, AddrMode::Relative),
            0xf1 => (Mnemonic::Sbc, AddrMode::ZeroPageIndirectYIndexed),
            0xf2 => (Mnemonic::UndocJam, AddrMode::Implied),
            0xf3 => (Mnemonic::UndocIsc, AddrMode::ZeroPageIndirectYIndexed),
            0xf4 => (Mnemonic::UndocNop, AddrMode::XIndexedZeroPage),
            0xf5 => (Mnemonic::Sbc, AddrMode::XIndexedZeroPage),
            0xf6 => (Mnemonic::Inc, AddrMode::XIndexedZeroPage),
            0xf7 => (Mnemonic::UndocIsc, AddrMode::XIndexedZeroPage),
            0xf8 => (Mnemonic::Sed, AddrMode::Implied),
            0xf9 => (Mnemonic::Sbc, AddrMode::YIndexedAbsolute),
            0xfa => (Mnemonic::UndocNop, AddrMode::Implied),
            0xfb => (Mnemonic::UndocIsc, AddrMode::YIndexedAbsolute),
            0xfc => (Mnemonic::UndocNop, AddrMode::XIndexedAbsolute),
            0xfd => (Mnemonic::Sbc, AddrMode::XIndexedAbsolute),
            0xfe => (Mnemonic::Inc, AddrMode::XIndexedAbsolute),
            0xff => (Mnemonic::UndocIsc, AddrMode::XIndexedAbsolute),
        };
        Operation { mnemonic, addr_mode }
    }
}

//===========================================================================//

/// An operation mnemonic (ignoring the addressing mode) for a 6502 processor.
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
    /// A BRK (break) operation.
    Brk,
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
    /// A NOP (no operation) operation.
    Nop,
    /// An ORA (binary OR with accumulator) operation.
    Ora,
    /// A PHA (push accumulator) operation.
    Pha,
    /// A PHP (push processor status) operation.
    Php,
    /// A PLA (pull accumulator) operation.
    Pla,
    /// A PLP (pull processor status) operation.
    Plp,
    /// An ROL (rotate left) operation.
    Rol,
    /// An ROR (rotate right) operation.
    Ror,
    /// An RTI (return from interrupt) operation.
    Rti,
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
    /// An STA (store accumulator) operation.
    Sta,
    /// An STX (store index X) operation.
    Stx,
    /// An STY (store index Y) operation.
    Sty,
    /// A TAX (transfer accumulator to index X) operation.
    Tax,
    /// A TAY (transfer accumulator to index Y) operation.
    Tay,
    /// A TSX (transfer stack pointer to index X) operation.
    Tsx,
    /// A TXA (transfer accumulator to index Y) operation.
    Txa,
    /// A TXS (transfer index X to stack pointer) operation.
    Txs,
    /// A TYA (transfer index Y to A) operation.
    Tya,
    /// An undocumented ANC (AND then set carry from negative flag) operation.
    UndocAnc,
    /// An undocumented ARR (AND then rotate right) operation.
    UndocArr,
    /// An undocumented ASR (AND then logical shift right) operation.
    UndocAsr,
    /// An undocumented DCP (decrement memory then compare with accumulator)
    /// operation.
    UndocDcp,
    /// An undocumented ISC (increment memory then subtract with borrow)
    /// operation.
    UndocIsc,
    /// An undocumented JAM (halt processor) operation.
    UndocJam,
    /// An undocumented LAS (load then AND with stack pointer) operation.
    UndocLas,
    /// An undocumented LAX (load accumulator and index X) operation.
    UndocLax,
    /// An undocumented NOP (no operation) operation.
    UndocNop,
    /// An undocumented RLA (rotate left then binary AND) operation.
    UndocRla,
    /// An undocumented RRA (rotate right then add with carry) operation.
    UndocRra,
    /// An undocumented SAX (store accumulator AND index X) operation.
    UndocSax,
    /// An undocumented SBC (subtract from accumulator with borrow) operation.
    UndocSbc,
    /// An undocumented SBX (subtract from accumulator AND index X) operation.
    UndocSbx,
    /// An undocumented SHA (store accumulator AND index X AND value)
    /// operation.
    UndocSha,
    /// An undocumented SHS (transfer accumulator AND index X to stack pointer
    /// and store) operation.
    UndocShs,
    /// An undocumented SHX (store index X AND value) operation.
    UndocShx,
    /// An undocumented SHY (store index Y AND value) operation.
    UndocShy,
    /// An undocumented SLO (arithmetic shift left then OR with accumulator)
    /// operation.
    UndocSlo,
    /// An undocumented SRE (logical shift right then exclusive OR with
    /// accumulator) operation.
    UndocSre,
    /// An undocumented XAA (non-deterministic) operation.
    UndocXaa,
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
            Mnemonic::Brk => "BRK",
            Mnemonic::Bvc => "BVC",
            Mnemonic::Bvs => "BVS",
            Mnemonic::Clc => "CLC",
            Mnemonic::Cld => "CLD",
            Mnemonic::Cli => "CLI",
            Mnemonic::Clv => "CLV",
            Mnemonic::Cmp => "CMP",
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
            Mnemonic::Jsr => "JSR",
            Mnemonic::Lda => "LDA",
            Mnemonic::Ldx => "LDX",
            Mnemonic::Ldy => "LDY",
            Mnemonic::Lsr => "LSR",
            Mnemonic::Nop => "NOP",
            Mnemonic::Ora => "ORA",
            Mnemonic::Pha => "PHA",
            Mnemonic::Php => "PHP",
            Mnemonic::Pla => "PLA",
            Mnemonic::Plp => "PLP",
            Mnemonic::Rol => "ROL",
            Mnemonic::Ror => "ROR",
            Mnemonic::Rti => "RTI",
            Mnemonic::Rts => "RTS",
            Mnemonic::Sbc => "SBC",
            Mnemonic::Sec => "SEC",
            Mnemonic::Sed => "SED",
            Mnemonic::Sei => "SEI",
            Mnemonic::Sta => "STA",
            Mnemonic::Stx => "STX",
            Mnemonic::Sty => "STY",
            Mnemonic::Tax => "TAX",
            Mnemonic::Tay => "TAY",
            Mnemonic::Tsx => "TSX",
            Mnemonic::Txa => "TXA",
            Mnemonic::Txs => "TXS",
            Mnemonic::Tya => "TYA",
            Mnemonic::UndocAnc => "anc",
            Mnemonic::UndocArr => "arr",
            Mnemonic::UndocAsr => "asr",
            Mnemonic::UndocDcp => "dcp",
            Mnemonic::UndocIsc => "isc",
            Mnemonic::UndocJam => "jam",
            Mnemonic::UndocLas => "las",
            Mnemonic::UndocLax => "lax",
            Mnemonic::UndocNop => "nop",
            Mnemonic::UndocRla => "rla",
            Mnemonic::UndocRra => "rra",
            Mnemonic::UndocSax => "sax",
            Mnemonic::UndocSbc => "sbc",
            Mnemonic::UndocSbx => "sbx",
            Mnemonic::UndocSha => "sha",
            Mnemonic::UndocShs => "shs",
            Mnemonic::UndocShx => "shx",
            Mnemonic::UndocShy => "shy",
            Mnemonic::UndocSlo => "slo",
            Mnemonic::UndocSre => "sre",
            Mnemonic::UndocXaa => "xaa",
        }
    }
}

impl fmt::Display for Mnemonic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.string())
    }
}

//===========================================================================//

/// An addressing mode for a 6502 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AddrMode {
    /// No additional arguments to the opcode.
    Implied,
    /// Operate on the A register.
    Accumulator,
    /// Operate on a constant byte immediately following the opcode.
    Immediate,
    /// Operate on an address that is offset (by the signed byte following the
    /// opcode) from the address of this instruction.
    Relative,
    /// Operate on the absolute 16-bit address following the opcode.
    Absolute,
    /// Treat the 16-bit address following the opcode as a pointer to the
    /// address to operate on.
    AbsoluteIndirect,
    /// Operate on a the absolute 16-bit address following the opcode, offset
    /// by index X.
    XIndexedAbsolute,
    /// Operate on a the absolute 16-bit address following the opcode, offset
    /// by index Y.
    YIndexedAbsolute,
    /// Operate on a the 8-bit zero page address following the opcode.
    ZeroPage,
    /// Operate on the 8-bit zero page address following the opcode, offset by
    /// index X.
    XIndexedZeroPage,
    /// Operate on the 8-bit zero page address following the opcode, offset by
    /// index Y.
    YIndexedZeroPage,
    /// Operate on the 16-bit address that is stored in memory, at the 8-bit
    /// zero page address following the opcode offset by index X.
    XIndexedZeroPageIndirect,
    /// Operate on a address equal to the 16-bit address stored at the 8-bit
    /// zero page address following the opcode, offset by index Y.
    ZeroPageIndirectYIndexed,
}

//===========================================================================//

/// An addressing mode and argument value for a 6502 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    /// No additional arguments to the opcode.
    Implied,
    /// Operate on the A register.
    Accumulator,
    /// Operate on the given constant byte.
    Immediate(u8),
    /// Operate on an address that is offset (by the given signed byte) from
    /// the address of this instruction.
    Relative(i8),
    /// Operate on the given absolute address.
    Absolute(u16),
    /// Operate on the address in memory that the given absolute address points
    /// to.
    AbsoluteIndirect(u16),
    /// Operate on the given absolute address, offset by index X.
    XIndexedAbsolute(u16),
    /// Operate on the given absolute address, offset by index Y.
    YIndexedAbsolute(u16),
    /// Operate on the given zero page address.
    ZeroPage(u8),
    /// Operate on the given zero page address, offset by index X.
    XIndexedZeroPage(u8),
    /// Operate on the given zero page address, offset by index Y.
    YIndexedZeroPage(u8),
    /// Operate on the address in memory that is offset from the given zero
    /// page address by index X.
    XIndexedZeroPageIndirect(u8),
    /// Operate on an address in memory equal to the address stored at the
    /// given zero page address, offset by index Y.
    ZeroPageIndirectYIndexed(u8),
}

impl Operand {
    /// Returns the size of this operand, in bytes.
    pub fn size(self) -> u32 {
        match self {
            Operand::Implied => 0,
            Operand::Accumulator => 0,
            Operand::Immediate(_) => 1,
            Operand::Relative(_) => 1,
            Operand::Absolute(_) => 2,
            Operand::AbsoluteIndirect(_) => 2,
            Operand::XIndexedAbsolute(_) => 2,
            Operand::YIndexedAbsolute(_) => 2,
            Operand::ZeroPage(_) => 1,
            Operand::XIndexedZeroPage(_) => 1,
            Operand::YIndexedZeroPage(_) => 1,
            Operand::XIndexedZeroPageIndirect(_) => 1,
            Operand::ZeroPageIndirectYIndexed(_) => 1,
        }
    }

    /// Formats this operand.  `pc` gives the address of the start of the
    /// instruction.
    fn format(self, bus: &dyn SimBus, pc: u16) -> String {
        match self {
            Operand::Implied => String::new(),
            Operand::Accumulator => " A".to_string(),
            Operand::Immediate(byte) => format!(" #${byte:02x}"),
            Operand::Relative(offset) => {
                let dest = pc.wrapping_add(2).wrapping_add(offset as u16);
                format!(" {}", format_abs(dest, bus))
            }
            Operand::Absolute(abs) => format!(" {}", format_abs(abs, bus)),
            Operand::AbsoluteIndirect(abs) => {
                format!(" ({})", format_abs(abs, bus))
            }
            Operand::XIndexedAbsolute(abs) => {
                format!(" {}, X", format_abs(abs, bus))
            }
            Operand::YIndexedAbsolute(abs) => {
                format!(" {}, Y", format_abs(abs, bus))
            }
            Operand::ZeroPage(zp) => format!(" {}", format_zp(zp, bus)),
            Operand::XIndexedZeroPage(zp) => {
                format!(" {}, X", format_zp(zp, bus))
            }
            Operand::YIndexedZeroPage(zp) => {
                format!(" {}, Y", format_zp(zp, bus))
            }
            Operand::XIndexedZeroPageIndirect(zp) => {
                format!(" ({}, X)", format_zp(zp, bus))
            }
            Operand::ZeroPageIndirectYIndexed(zp) => {
                format!(" ({}), Y", format_zp(zp, bus))
            }
        }
    }
}

fn format_zp(zp: u8, bus: &dyn SimBus) -> String {
    match bus.label_at(zp as u32) {
        Some(label) => label.to_string(),
        None => format!("${zp:02x}"),
    }
}

fn format_abs(abs: u16, bus: &dyn SimBus) -> String {
    match bus.label_at(abs as u32) {
        Some(label) => label.to_string(),
        None => format!("${abs:04x}"),
    }
}

//===========================================================================//

/// A complete instruction, including parameter values, for a 6502 processor.
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

    /// Decodes a single 6502 instruction at the given starting address.
    pub fn decode(bus: &dyn SimBus, pc: u16) -> Instruction {
        let opcode = bus.peek_byte(u32::from(pc));
        let operation = Operation::from_opcode(opcode);
        let operand = match operation.addr_mode {
            AddrMode::Implied => Operand::Implied,
            AddrMode::Accumulator => Operand::Accumulator,
            AddrMode::Immediate => Operand::Immediate(next_byte(bus, pc)),
            AddrMode::Relative => Operand::Relative(next_byte(bus, pc) as i8),
            AddrMode::Absolute => Operand::Absolute(next_word(bus, pc)),
            AddrMode::AbsoluteIndirect => {
                Operand::AbsoluteIndirect(next_word(bus, pc))
            }
            AddrMode::XIndexedAbsolute => {
                Operand::XIndexedAbsolute(next_word(bus, pc))
            }
            AddrMode::YIndexedAbsolute => {
                Operand::YIndexedAbsolute(next_word(bus, pc))
            }
            AddrMode::ZeroPage => Operand::ZeroPage(next_byte(bus, pc)),
            AddrMode::XIndexedZeroPage => {
                Operand::XIndexedZeroPage(next_byte(bus, pc))
            }
            AddrMode::YIndexedZeroPage => {
                Operand::YIndexedZeroPage(next_byte(bus, pc))
            }
            AddrMode::XIndexedZeroPageIndirect => {
                Operand::XIndexedZeroPageIndirect(next_byte(bus, pc))
            }
            AddrMode::ZeroPageIndirectYIndexed => {
                Operand::ZeroPageIndirectYIndexed(next_byte(bus, pc))
            }
        };
        Instruction { mnemonic: operation.mnemonic, operand }
    }

    /// Formats the decoded 6502 instruction as a human-readable string.  `pc`
    /// specifies the address of the start of the instruction.  `bus` is
    /// required for providing labels for addresses; if no labels are needed, a
    /// `new_open_bus` can be used.
    pub fn format(self, bus: &dyn SimBus, pc: u16) -> String {
        format!("{}{}", self.mnemonic, self.operand.format(bus, pc))
    }
}

fn next_byte(bus: &dyn SimBus, pc: u16) -> u8 {
    bus.peek_byte(u32::from(pc.wrapping_add(1)))
}

fn next_word(bus: &dyn SimBus, pc: u16) -> u16 {
    let lo = bus.peek_byte(u32::from(pc.wrapping_add(1)));
    let hi = bus.peek_byte(u32::from(pc.wrapping_add(2)));
    (u16::from(hi) << 8) | u16::from(lo)
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::Instruction;
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
        let labels = HashMap::from([(label.to_string(), addr)]);
        let bus = LabeledBus::new(make_test_bus(code), labels);
        disassemble_with_bus(&bus)
    }

    fn disassemble_with_bus(bus: &dyn SimBus) -> String {
        Instruction::decode(bus, 0).format(bus, 0)
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
        assert_eq!(disassemble(&[0x08]), "PHP");
        assert_eq!(disassemble(&[0x68]), "PLA");
        assert_eq!(disassemble(&[0x28]), "PLP");
        assert_eq!(disassemble(&[0x40]), "RTI");
        assert_eq!(disassemble(&[0x60]), "RTS");
        assert_eq!(disassemble(&[0x38]), "SEC");
        assert_eq!(disassemble(&[0xf8]), "SED");
        assert_eq!(disassemble(&[0x78]), "SEI");
        assert_eq!(disassemble(&[0xaa]), "TAX");
        assert_eq!(disassemble(&[0xa8]), "TAY");
        assert_eq!(disassemble(&[0xba]), "TSX");
        assert_eq!(disassemble(&[0x8a]), "TXA");
        assert_eq!(disassemble(&[0x9a]), "TXS");
        assert_eq!(disassemble(&[0x98]), "TYA");
    }

    #[test]
    fn disassemble_addr_mode_accumulator() {
        assert_eq!(disassemble(&[0x0a]), "ASL A");
        assert_eq!(disassemble(&[0x4a]), "LSR A");
        assert_eq!(disassemble(&[0x2a]), "ROL A");
        assert_eq!(disassemble(&[0x6a]), "ROR A");
    }

    #[test]
    fn disassemble_addr_mode_immediate() {
        assert_eq!(disassemble(&[0x69, 0x03]), "ADC #$03");
        assert_eq!(disassemble(&[0x29, 0xf0]), "AND #$f0");
        assert_eq!(disassemble(&[0xc9, 0x80]), "CMP #$80");
        assert_eq!(disassemble(&[0xe0, 0x10]), "CPX #$10");
        assert_eq!(disassemble(&[0xc0, 0x20]), "CPY #$20");
        assert_eq!(disassemble(&[0x49, 0xff]), "EOR #$ff");
        assert_eq!(disassemble(&[0xa9, 0x00]), "LDA #$00");
        assert_eq!(disassemble(&[0xa2, 0xff]), "LDX #$ff");
        assert_eq!(disassemble(&[0xa0, 0x47]), "LDY #$47");
        assert_eq!(disassemble(&[0x09, 0x54]), "ORA #$54");
        assert_eq!(disassemble(&[0xe9, 0x01]), "SBC #$01");
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
    fn disassemble_addr_mode_absolute() {
        assert_eq!(disassemble(&[0x6d, 0x34, 0x12]), "ADC $1234");
        assert_eq!(disassemble(&[0x2d, 0x78, 0x56]), "AND $5678");
        assert_eq!(disassemble(&[0x0e, 0xbc, 0x9a]), "ASL $9abc");
        assert_eq!(disassemble(&[0x2c, 0xfe, 0xca]), "BIT $cafe");
        assert_eq!(disassemble(&[0xcd, 0xf0, 0xde]), "CMP $def0");
        assert_eq!(disassemble(&[0xec, 0x41, 0x31]), "CPX $3141");
        assert_eq!(disassemble(&[0xcc, 0x26, 0x59]), "CPY $5926");
        assert_eq!(disassemble(&[0xce, 0x22, 0x11]), "DEC $1122");
        assert_eq!(disassemble(&[0x4d, 0x44, 0x33]), "EOR $3344");
        assert_eq!(disassemble(&[0xee, 0x66, 0x55]), "INC $5566");
        assert_eq!(disassemble(&[0x4c, 0xad, 0xde]), "JMP $dead");
        assert_eq!(disassemble(&[0x20, 0xef, 0xbe]), "JSR $beef");
        assert_eq!(disassemble(&[0xad, 0x88, 0x77]), "LDA $7788");
        assert_eq!(disassemble(&[0xae, 0x77, 0x88]), "LDX $8877");
        assert_eq!(disassemble(&[0xac, 0xaa, 0x99]), "LDY $99aa");
        assert_eq!(disassemble(&[0x4e, 0xcc, 0xbb]), "LSR $bbcc");
        assert_eq!(disassemble(&[0x0d, 0xee, 0xdd]), "ORA $ddee");
        assert_eq!(disassemble(&[0x2e, 0x00, 0xff]), "ROL $ff00");
        assert_eq!(disassemble(&[0x6e, 0xed, 0x0f]), "ROR $0fed");
        assert_eq!(disassemble(&[0xed, 0xa9, 0xbc]), "SBC $bca9");
        assert_eq!(disassemble(&[0x8d, 0x65, 0x87]), "STA $8765");
        assert_eq!(disassemble(&[0x8e, 0x21, 0x43]), "STX $4321");
        assert_eq!(disassemble(&[0x8c, 0xcd, 0xab]), "STY $abcd");
        assert_eq!(
            disassemble_with_label(&[0x8c, 0xcd, 0xab], 0xabcd, "foo"),
            "STY foo"
        );
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
    fn disassemble_addr_mode_x_indexed_absolute() {
        assert_eq!(disassemble(&[0x7d, 0x34, 0x12]), "ADC $1234, X");
        assert_eq!(disassemble(&[0x3d, 0x78, 0x56]), "AND $5678, X");
        assert_eq!(disassemble(&[0x1e, 0xbc, 0x9a]), "ASL $9abc, X");
        assert_eq!(disassemble(&[0xdd, 0xf0, 0xde]), "CMP $def0, X");
        assert_eq!(disassemble(&[0xde, 0x22, 0x11]), "DEC $1122, X");
        assert_eq!(disassemble(&[0x5d, 0x44, 0x33]), "EOR $3344, X");
        assert_eq!(disassemble(&[0xfe, 0x66, 0x55]), "INC $5566, X");
        assert_eq!(disassemble(&[0xbd, 0x88, 0x77]), "LDA $7788, X");
        assert_eq!(disassemble(&[0xbc, 0xaa, 0x99]), "LDY $99aa, X");
        assert_eq!(disassemble(&[0x5e, 0xcc, 0xbb]), "LSR $bbcc, X");
        assert_eq!(disassemble(&[0x1d, 0xee, 0xdd]), "ORA $ddee, X");
        assert_eq!(disassemble(&[0x3e, 0x00, 0xff]), "ROL $ff00, X");
        assert_eq!(disassemble(&[0x7e, 0xed, 0x0f]), "ROR $0fed, X");
        assert_eq!(disassemble(&[0xfd, 0xa9, 0xbc]), "SBC $bca9, X");
        assert_eq!(disassemble(&[0x9d, 0x65, 0x87]), "STA $8765, X");
        assert_eq!(
            disassemble_with_label(&[0x9d, 0x65, 0x87], 0x8765, "foo"),
            "STA foo, X"
        );
    }

    #[test]
    fn disassemble_addr_mode_y_indexed_absolute() {
        assert_eq!(disassemble(&[0x79, 0x34, 0x12]), "ADC $1234, Y");
        assert_eq!(disassemble(&[0x39, 0x78, 0x56]), "AND $5678, Y");
        assert_eq!(disassemble(&[0xd9, 0xf0, 0xde]), "CMP $def0, Y");
        assert_eq!(disassemble(&[0x59, 0x44, 0x33]), "EOR $3344, Y");
        assert_eq!(disassemble(&[0xb9, 0x88, 0x77]), "LDA $7788, Y");
        assert_eq!(disassemble(&[0xbe, 0xaa, 0x99]), "LDX $99aa, Y");
        assert_eq!(disassemble(&[0x19, 0xee, 0xdd]), "ORA $ddee, Y");
        assert_eq!(disassemble(&[0xf9, 0xa9, 0xbc]), "SBC $bca9, Y");
        assert_eq!(disassemble(&[0x99, 0x65, 0x87]), "STA $8765, Y");
        assert_eq!(
            disassemble_with_label(&[0x99, 0x65, 0x87], 0x8765, "foo"),
            "STA foo, Y"
        );
    }

    #[test]
    fn disassemble_addr_mode_zero_page() {
        assert_eq!(disassemble(&[0x65, 0xef]), "ADC $ef");
        assert_eq!(disassemble(&[0x25, 0x12]), "AND $12");
        assert_eq!(disassemble(&[0x06, 0x00]), "ASL $00");
        assert_eq!(disassemble(&[0x24, 0xb1]), "BIT $b1");
        assert_eq!(disassemble(&[0xc5, 0x09]), "CMP $09");
        assert_eq!(disassemble(&[0xe4, 0x0a]), "CPX $0a");
        assert_eq!(disassemble(&[0xc4, 0x0f]), "CPY $0f");
        assert_eq!(disassemble(&[0xc6, 0xff]), "DEC $ff");
        assert_eq!(disassemble(&[0x45, 0xee]), "EOR $ee");
        assert_eq!(disassemble(&[0xe6, 0xb2]), "INC $b2");
        assert_eq!(disassemble(&[0xa5, 0x22]), "LDA $22");
        assert_eq!(disassemble(&[0xa6, 0x33]), "LDX $33");
        assert_eq!(disassemble(&[0xa4, 0x44]), "LDY $44");
        assert_eq!(disassemble(&[0x46, 0x49]), "LSR $49");
        assert_eq!(disassemble(&[0x05, 0xfa]), "ORA $fa");
        assert_eq!(disassemble(&[0x26, 0xc9]), "ROL $c9");
        assert_eq!(disassemble(&[0x66, 0xbb]), "ROR $bb");
        assert_eq!(disassemble(&[0xe5, 0xcd]), "SBC $cd");
        assert_eq!(disassemble(&[0x85, 0x22]), "STA $22");
        assert_eq!(disassemble(&[0x86, 0x33]), "STX $33");
        assert_eq!(disassemble(&[0x84, 0x44]), "STY $44");
        assert_eq!(
            disassemble_with_label(&[0x84, 0x44], 0x44, "foo"),
            "STY foo"
        );
    }

    #[test]
    fn disassemble_addr_mode_x_indexed_zero_page() {
        assert_eq!(disassemble(&[0x75, 0x01]), "ADC $01, X");
        assert_eq!(disassemble(&[0x35, 0x0f]), "AND $0f, X");
        assert_eq!(disassemble(&[0x16, 0x40]), "ASL $40, X");
        assert_eq!(disassemble(&[0xd5, 0xef]), "CMP $ef, X");
        assert_eq!(disassemble(&[0xd6, 0x70]), "DEC $70, X");
        assert_eq!(disassemble(&[0x55, 0xbe]), "EOR $be, X");
        assert_eq!(disassemble(&[0xf6, 0x88]), "INC $88, X");
        assert_eq!(disassemble(&[0xb5, 0x10]), "LDA $10, X");
        assert_eq!(disassemble(&[0xb4, 0x11]), "LDY $11, X");
        assert_eq!(disassemble(&[0x56, 0x12]), "LSR $12, X");
        assert_eq!(disassemble(&[0x15, 0x13]), "ORA $13, X");
        assert_eq!(disassemble(&[0x36, 0x14]), "ROL $14, X");
        assert_eq!(disassemble(&[0x76, 0x15]), "ROR $15, X");
        assert_eq!(disassemble(&[0xf5, 0x16]), "SBC $16, X");
        assert_eq!(disassemble(&[0x95, 0x2a]), "STA $2a, X");
        assert_eq!(disassemble(&[0x94, 0x20]), "STY $20, X");
        assert_eq!(
            disassemble_with_label(&[0x94, 0x20], 0x20, "foo"),
            "STY foo, X"
        );
    }

    #[test]
    fn disassemble_addr_mode_y_indexed_zero_page() {
        assert_eq!(disassemble(&[0xb6, 0x1a]), "LDX $1a, Y");
        assert_eq!(disassemble(&[0x96, 0x2a]), "STX $2a, Y");
        assert_eq!(
            disassemble_with_label(&[0x96, 0x2a], 0x2a, "foo"),
            "STX foo, Y"
        );
    }

    #[test]
    fn disassemble_addr_mode_x_indexed_zero_page_indirect() {
        assert_eq!(disassemble(&[0x61, 0x12]), "ADC ($12, X)");
        assert_eq!(disassemble(&[0x21, 0x34]), "AND ($34, X)");
        assert_eq!(disassemble(&[0xc1, 0x56]), "CMP ($56, X)");
        assert_eq!(disassemble(&[0x41, 0x78]), "EOR ($78, X)");
        assert_eq!(disassemble(&[0xa1, 0x9a]), "LDA ($9a, X)");
        assert_eq!(disassemble(&[0x01, 0xbc]), "ORA ($bc, X)");
        assert_eq!(disassemble(&[0xe1, 0xde]), "SBC ($de, X)");
        assert_eq!(disassemble(&[0x81, 0xf0]), "STA ($f0, X)");
        assert_eq!(
            disassemble_with_label(&[0x81, 0xf0], 0xf0, "foo"),
            "STA (foo, X)"
        );
    }

    #[test]
    fn disassemble_addr_mode_zero_page_indirect_y_indexed() {
        assert_eq!(disassemble(&[0x71, 0x12]), "ADC ($12), Y");
        assert_eq!(disassemble(&[0x31, 0x34]), "AND ($34), Y");
        assert_eq!(disassemble(&[0xd1, 0x56]), "CMP ($56), Y");
        assert_eq!(disassemble(&[0x51, 0x78]), "EOR ($78), Y");
        assert_eq!(disassemble(&[0xb1, 0x9a]), "LDA ($9a), Y");
        assert_eq!(disassemble(&[0x11, 0xbc]), "ORA ($bc), Y");
        assert_eq!(disassemble(&[0xf1, 0xde]), "SBC ($de), Y");
        assert_eq!(disassemble(&[0x91, 0xf0]), "STA ($f0), Y");
        assert_eq!(
            disassemble_with_label(&[0x91, 0xf0], 0xf0, "foo"),
            "STA (foo), Y"
        );
    }

    #[test]
    fn disassemble_undocumented_addr_mode_implied() {
        assert_eq!(disassemble(&[0x02]), "jam");
        assert_eq!(disassemble(&[0x12]), "jam");
        assert_eq!(disassemble(&[0x22]), "jam");
        assert_eq!(disassemble(&[0x32]), "jam");
        assert_eq!(disassemble(&[0x42]), "jam");
        assert_eq!(disassemble(&[0x52]), "jam");
        assert_eq!(disassemble(&[0x62]), "jam");
        assert_eq!(disassemble(&[0x72]), "jam");
        assert_eq!(disassemble(&[0x92]), "jam");
        assert_eq!(disassemble(&[0xb2]), "jam");
        assert_eq!(disassemble(&[0xd2]), "jam");
        assert_eq!(disassemble(&[0xf2]), "jam");
        assert_eq!(disassemble(&[0x1a]), "nop");
        assert_eq!(disassemble(&[0x3a]), "nop");
        assert_eq!(disassemble(&[0x5a]), "nop");
        assert_eq!(disassemble(&[0x7a]), "nop");
        assert_eq!(disassemble(&[0xda]), "nop");
        assert_eq!(disassemble(&[0xfa]), "nop");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_immediate() {
        assert_eq!(disassemble(&[0x0b, 0x12]), "anc #$12");
        assert_eq!(disassemble(&[0x2b, 0x34]), "anc #$34");
        assert_eq!(disassemble(&[0x6b, 0x56]), "arr #$56");
        assert_eq!(disassemble(&[0x4b, 0x78]), "asr #$78");
        assert_eq!(disassemble(&[0xab, 0x9a]), "lax #$9a");
        assert_eq!(disassemble(&[0x80, 0x12]), "nop #$12");
        assert_eq!(disassemble(&[0x82, 0x34]), "nop #$34");
        assert_eq!(disassemble(&[0x89, 0x56]), "nop #$56");
        assert_eq!(disassemble(&[0xc2, 0x78]), "nop #$78");
        assert_eq!(disassemble(&[0xe2, 0x9a]), "nop #$9a");
        assert_eq!(disassemble(&[0xeb, 0x12]), "sbc #$12");
        assert_eq!(disassemble(&[0xcb, 0x34]), "sbx #$34");
        assert_eq!(disassemble(&[0x8b, 0x56]), "xaa #$56");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_absolute() {
        assert_eq!(disassemble(&[0xcf, 0x34, 0x12]), "dcp $1234");
        assert_eq!(disassemble(&[0xef, 0x34, 0x12]), "isc $1234");
        assert_eq!(disassemble(&[0xaf, 0x34, 0x12]), "lax $1234");
        assert_eq!(disassemble(&[0x0c, 0x34, 0x12]), "nop $1234");
        assert_eq!(disassemble(&[0x2f, 0x34, 0x12]), "rla $1234");
        assert_eq!(disassemble(&[0x6f, 0x34, 0x12]), "rra $1234");
        assert_eq!(disassemble(&[0x8f, 0x34, 0x12]), "sax $1234");
        assert_eq!(disassemble(&[0x0f, 0x34, 0x12]), "slo $1234");
        assert_eq!(disassemble(&[0x4f, 0x34, 0x12]), "sre $1234");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_x_indexed_absolute() {
        assert_eq!(disassemble(&[0xdf, 0x34, 0x12]), "dcp $1234, X");
        assert_eq!(disassemble(&[0xff, 0x34, 0x12]), "isc $1234, X");
        assert_eq!(disassemble(&[0x1c, 0x34, 0x12]), "nop $1234, X");
        assert_eq!(disassemble(&[0x3c, 0x34, 0x12]), "nop $1234, X");
        assert_eq!(disassemble(&[0x5c, 0x34, 0x12]), "nop $1234, X");
        assert_eq!(disassemble(&[0x7c, 0x34, 0x12]), "nop $1234, X");
        assert_eq!(disassemble(&[0xdc, 0x34, 0x12]), "nop $1234, X");
        assert_eq!(disassemble(&[0xfc, 0x34, 0x12]), "nop $1234, X");
        assert_eq!(disassemble(&[0x3f, 0x34, 0x12]), "rla $1234, X");
        assert_eq!(disassemble(&[0x7f, 0x34, 0x12]), "rra $1234, X");
        assert_eq!(disassemble(&[0x9c, 0x34, 0x12]), "shy $1234, X");
        assert_eq!(disassemble(&[0x1f, 0x34, 0x12]), "slo $1234, X");
        assert_eq!(disassemble(&[0x5f, 0x34, 0x12]), "sre $1234, X");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_y_indexed_absolute() {
        assert_eq!(disassemble(&[0xdb, 0x34, 0x12]), "dcp $1234, Y");
        assert_eq!(disassemble(&[0xfb, 0x34, 0x12]), "isc $1234, Y");
        assert_eq!(disassemble(&[0xbb, 0x34, 0x12]), "las $1234, Y");
        assert_eq!(disassemble(&[0xbf, 0x34, 0x12]), "lax $1234, Y");
        assert_eq!(disassemble(&[0x3b, 0x34, 0x12]), "rla $1234, Y");
        assert_eq!(disassemble(&[0x7b, 0x34, 0x12]), "rra $1234, Y");
        assert_eq!(disassemble(&[0x9f, 0x34, 0x12]), "sha $1234, Y");
        assert_eq!(disassemble(&[0x9b, 0x34, 0x12]), "shs $1234, Y");
        assert_eq!(disassemble(&[0x9e, 0x34, 0x12]), "shx $1234, Y");
        assert_eq!(disassemble(&[0x1b, 0x34, 0x12]), "slo $1234, Y");
        assert_eq!(disassemble(&[0x5b, 0x34, 0x12]), "sre $1234, Y");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_zero_page() {
        assert_eq!(disassemble(&[0xc7, 0x12]), "dcp $12");
        assert_eq!(disassemble(&[0xe7, 0x12]), "isc $12");
        assert_eq!(disassemble(&[0xa7, 0x12]), "lax $12");
        assert_eq!(disassemble(&[0x04, 0x12]), "nop $12");
        assert_eq!(disassemble(&[0x44, 0x12]), "nop $12");
        assert_eq!(disassemble(&[0x64, 0x12]), "nop $12");
        assert_eq!(disassemble(&[0x27, 0x12]), "rla $12");
        assert_eq!(disassemble(&[0x67, 0x12]), "rra $12");
        assert_eq!(disassemble(&[0x87, 0x12]), "sax $12");
        assert_eq!(disassemble(&[0x07, 0x12]), "slo $12");
        assert_eq!(disassemble(&[0x47, 0x12]), "sre $12");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_x_indexed_zero_page() {
        assert_eq!(disassemble(&[0xd7, 0x12]), "dcp $12, X");
        assert_eq!(disassemble(&[0xf7, 0x12]), "isc $12, X");
        assert_eq!(disassemble(&[0x14, 0x12]), "nop $12, X");
        assert_eq!(disassemble(&[0x34, 0x12]), "nop $12, X");
        assert_eq!(disassemble(&[0x54, 0x12]), "nop $12, X");
        assert_eq!(disassemble(&[0x74, 0x12]), "nop $12, X");
        assert_eq!(disassemble(&[0xd4, 0x12]), "nop $12, X");
        assert_eq!(disassemble(&[0xf4, 0x12]), "nop $12, X");
        assert_eq!(disassemble(&[0x37, 0x12]), "rla $12, X");
        assert_eq!(disassemble(&[0x77, 0x12]), "rra $12, X");
        assert_eq!(disassemble(&[0x17, 0x12]), "slo $12, X");
        assert_eq!(disassemble(&[0x57, 0x12]), "sre $12, X");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_y_indexed_zero_page() {
        assert_eq!(disassemble(&[0xb7, 0x12]), "lax $12, Y");
        assert_eq!(disassemble(&[0x97, 0x12]), "sax $12, Y");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_x_indexed_zero_page_indirect() {
        assert_eq!(disassemble(&[0xc3, 0x12]), "dcp ($12, X)");
        assert_eq!(disassemble(&[0xe3, 0x12]), "isc ($12, X)");
        assert_eq!(disassemble(&[0xa3, 0x12]), "lax ($12, X)");
        assert_eq!(disassemble(&[0x23, 0x12]), "rla ($12, X)");
        assert_eq!(disassemble(&[0x63, 0x12]), "rra ($12, X)");
        assert_eq!(disassemble(&[0x83, 0x12]), "sax ($12, X)");
        assert_eq!(disassemble(&[0x03, 0x12]), "slo ($12, X)");
        assert_eq!(disassemble(&[0x43, 0x12]), "sre ($12, X)");
    }

    #[test]
    fn disassemble_undocumented_addr_mode_zero_page_indirect_y_indexed() {
        assert_eq!(disassemble(&[0xd3, 0x12]), "dcp ($12), Y");
        assert_eq!(disassemble(&[0xf3, 0x12]), "isc ($12), Y");
        assert_eq!(disassemble(&[0xb3, 0x12]), "lax ($12), Y");
        assert_eq!(disassemble(&[0x33, 0x12]), "rla ($12), Y");
        assert_eq!(disassemble(&[0x73, 0x12]), "rra ($12), Y");
        assert_eq!(disassemble(&[0x93, 0x12]), "sha ($12), Y");
        assert_eq!(disassemble(&[0x13, 0x12]), "slo ($12), Y");
        assert_eq!(disassemble(&[0x53, 0x12]), "sre ($12), Y");
    }
}

//===========================================================================//
