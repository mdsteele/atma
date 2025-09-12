//! Facilities for disassembling 6502 machine code.

use crate::bus::SimBus;
use std::io::{self, Read};

//===========================================================================//

/// An operation (ignoring the addressing mode) that can be executed by a 6502
/// processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operation {
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
}

impl Operation {
    /// Returns the assembler mnemonic for this operation.
    pub fn mnemonic(self) -> &'static str {
        match self {
            Operation::Adc => "ADC",
            Operation::And => "AND",
            Operation::Asl => "ASL",
            Operation::Bcc => "BCC",
            Operation::Bcs => "BCS",
            Operation::Beq => "BEQ",
            Operation::Bit => "BIT",
            Operation::Bmi => "BMI",
            Operation::Bne => "BNE",
            Operation::Bpl => "BPL",
            Operation::Brk => "BRK",
            Operation::Bvc => "BVC",
            Operation::Bvs => "BVS",
            Operation::Clc => "CLC",
            Operation::Cld => "CLD",
            Operation::Cli => "CLI",
            Operation::Clv => "CLV",
            Operation::Cmp => "CMP",
            Operation::Cpx => "CPX",
            Operation::Cpy => "CPY",
            Operation::Dec => "DEC",
            Operation::Dex => "DEX",
            Operation::Dey => "DEY",
            Operation::Eor => "EOR",
            Operation::Inc => "INC",
            Operation::Inx => "INX",
            Operation::Iny => "INY",
            Operation::Jmp => "JMP",
            Operation::Jsr => "JSR",
            Operation::Lda => "LDA",
            Operation::Ldx => "LDX",
            Operation::Ldy => "LDY",
            Operation::Lsr => "LSR",
            Operation::Nop => "NOP",
            Operation::Ora => "ORA",
            Operation::Pha => "PHA",
            Operation::Php => "PHP",
            Operation::Pla => "PLA",
            Operation::Plp => "PLP",
            Operation::Rol => "ROL",
            Operation::Ror => "ROR",
            Operation::Rti => "RTI",
            Operation::Rts => "RTS",
            Operation::Sbc => "SBC",
            Operation::Sec => "SEC",
            Operation::Sed => "SED",
            Operation::Sei => "SEI",
            Operation::Sta => "STA",
            Operation::Stx => "STX",
            Operation::Sty => "STY",
            Operation::Tax => "TAX",
            Operation::Tay => "TAY",
            Operation::Tsx => "TSX",
            Operation::Txa => "TXA",
            Operation::Txs => "TXS",
            Operation::Tya => "TYA",
        }
    }
}

/// An instruction addressing mode for a 6502 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AddrMode {
    /// No additional arguments to the opcode.
    Implied,
    /// Operate on a constant byte immediately following the opcode.
    Immediate,
    /// Operate on an address that is offset (by the signed byte following the
    /// opcode) from the address of this instruction.
    Relative,
    /// Operate on a the absolute 16-bit address following the opcode.
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

/// An addressing mode and argument value for a 6502 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    /// No additional arguments to the opcode.
    Implied,
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
    /// The size of this operand, in bytes.
    pub fn size(self) -> usize {
        match self {
            Operand::Implied => 0,
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
    fn format(self, pc: u16, bus: &dyn SimBus) -> String {
        match self {
            Operand::Implied => String::new(),
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

/// Decodes a 6502 opcode into its operation and addressing mode.
pub fn decode_opcode(opcode: u8) -> (Operation, AddrMode) {
    match opcode {
        0x00 => (Operation::Brk, AddrMode::Immediate),
        0x01 => (Operation::Ora, AddrMode::XIndexedZeroPageIndirect),
        0x05 => (Operation::Ora, AddrMode::ZeroPage),
        0x06 => (Operation::Asl, AddrMode::ZeroPage),
        0x08 => (Operation::Php, AddrMode::Implied),
        0x09 => (Operation::Ora, AddrMode::Immediate),
        0x0a => (Operation::Asl, AddrMode::Implied),
        0x0d => (Operation::Ora, AddrMode::Absolute),
        0x0e => (Operation::Asl, AddrMode::Absolute),
        0x10 => (Operation::Bpl, AddrMode::Relative),
        0x11 => (Operation::Ora, AddrMode::ZeroPageIndirectYIndexed),
        0x15 => (Operation::Ora, AddrMode::XIndexedZeroPage),
        0x16 => (Operation::Asl, AddrMode::XIndexedZeroPage),
        0x18 => (Operation::Clc, AddrMode::Implied),
        0x19 => (Operation::Ora, AddrMode::YIndexedAbsolute),
        0x1d => (Operation::Ora, AddrMode::XIndexedAbsolute),
        0x1e => (Operation::Asl, AddrMode::XIndexedAbsolute),
        0x20 => (Operation::Jsr, AddrMode::Absolute),
        0x21 => (Operation::And, AddrMode::XIndexedZeroPageIndirect),
        0x24 => (Operation::Bit, AddrMode::ZeroPage),
        0x25 => (Operation::And, AddrMode::ZeroPage),
        0x26 => (Operation::Rol, AddrMode::ZeroPage),
        0x28 => (Operation::Plp, AddrMode::Implied),
        0x29 => (Operation::And, AddrMode::Immediate),
        0x2a => (Operation::Rol, AddrMode::Implied),
        0x2c => (Operation::Bit, AddrMode::Absolute),
        0x2d => (Operation::And, AddrMode::Absolute),
        0x2e => (Operation::Rol, AddrMode::Absolute),
        0x30 => (Operation::Bmi, AddrMode::Relative),
        0x31 => (Operation::And, AddrMode::ZeroPageIndirectYIndexed),
        0x35 => (Operation::And, AddrMode::XIndexedZeroPage),
        0x36 => (Operation::Rol, AddrMode::XIndexedZeroPage),
        0x38 => (Operation::Sec, AddrMode::Implied),
        0x39 => (Operation::And, AddrMode::YIndexedAbsolute),
        0x3d => (Operation::And, AddrMode::XIndexedAbsolute),
        0x3e => (Operation::Rol, AddrMode::XIndexedAbsolute),
        0x40 => (Operation::Rti, AddrMode::Implied),
        0x41 => (Operation::Eor, AddrMode::XIndexedZeroPageIndirect),
        0x45 => (Operation::Eor, AddrMode::ZeroPage),
        0x46 => (Operation::Lsr, AddrMode::ZeroPage),
        0x48 => (Operation::Pha, AddrMode::Implied),
        0x49 => (Operation::Eor, AddrMode::Immediate),
        0x4a => (Operation::Lsr, AddrMode::Implied),
        0x4c => (Operation::Jmp, AddrMode::Absolute),
        0x4d => (Operation::Eor, AddrMode::Absolute),
        0x4e => (Operation::Lsr, AddrMode::Absolute),
        0x50 => (Operation::Bvc, AddrMode::Relative),
        0x51 => (Operation::Eor, AddrMode::ZeroPageIndirectYIndexed),
        0x55 => (Operation::Eor, AddrMode::XIndexedZeroPage),
        0x56 => (Operation::Lsr, AddrMode::XIndexedZeroPage),
        0x58 => (Operation::Cli, AddrMode::Implied),
        0x59 => (Operation::Eor, AddrMode::YIndexedAbsolute),
        0x5d => (Operation::Eor, AddrMode::XIndexedAbsolute),
        0x5e => (Operation::Lsr, AddrMode::XIndexedAbsolute),
        0x60 => (Operation::Rts, AddrMode::Implied),
        0x61 => (Operation::Adc, AddrMode::XIndexedZeroPageIndirect),
        0x65 => (Operation::Adc, AddrMode::ZeroPage),
        0x66 => (Operation::Ror, AddrMode::ZeroPage),
        0x68 => (Operation::Pla, AddrMode::Implied),
        0x69 => (Operation::Adc, AddrMode::Immediate),
        0x6a => (Operation::Ror, AddrMode::Implied),
        0x6c => (Operation::Jmp, AddrMode::AbsoluteIndirect),
        0x6d => (Operation::Adc, AddrMode::Absolute),
        0x6e => (Operation::Ror, AddrMode::Absolute),
        0x70 => (Operation::Bvs, AddrMode::Relative),
        0x71 => (Operation::Adc, AddrMode::ZeroPageIndirectYIndexed),
        0x75 => (Operation::Adc, AddrMode::XIndexedZeroPage),
        0x76 => (Operation::Ror, AddrMode::XIndexedZeroPage),
        0x78 => (Operation::Sei, AddrMode::Implied),
        0x79 => (Operation::Adc, AddrMode::YIndexedAbsolute),
        0x7d => (Operation::Adc, AddrMode::XIndexedAbsolute),
        0x7e => (Operation::Ror, AddrMode::XIndexedAbsolute),
        0x81 => (Operation::Sta, AddrMode::XIndexedZeroPageIndirect),
        0x84 => (Operation::Sty, AddrMode::ZeroPage),
        0x85 => (Operation::Sta, AddrMode::ZeroPage),
        0x86 => (Operation::Stx, AddrMode::ZeroPage),
        0x88 => (Operation::Dey, AddrMode::Implied),
        0x8a => (Operation::Txa, AddrMode::Implied),
        0x8c => (Operation::Sty, AddrMode::Absolute),
        0x8d => (Operation::Sta, AddrMode::Absolute),
        0x8e => (Operation::Stx, AddrMode::Absolute),
        0x90 => (Operation::Bcc, AddrMode::Relative),
        0x91 => (Operation::Sta, AddrMode::ZeroPageIndirectYIndexed),
        0x94 => (Operation::Sty, AddrMode::XIndexedZeroPage),
        0x95 => (Operation::Sta, AddrMode::XIndexedZeroPage),
        0x96 => (Operation::Stx, AddrMode::YIndexedZeroPage),
        0x98 => (Operation::Tya, AddrMode::Implied),
        0x99 => (Operation::Sta, AddrMode::YIndexedAbsolute),
        0x9a => (Operation::Txs, AddrMode::Implied),
        0x9d => (Operation::Sta, AddrMode::XIndexedAbsolute),
        0xa0 => (Operation::Ldy, AddrMode::Immediate),
        0xa1 => (Operation::Lda, AddrMode::XIndexedZeroPageIndirect),
        0xa2 => (Operation::Ldx, AddrMode::Immediate),
        0xa4 => (Operation::Ldy, AddrMode::ZeroPage),
        0xa5 => (Operation::Lda, AddrMode::ZeroPage),
        0xa6 => (Operation::Ldx, AddrMode::ZeroPage),
        0xa8 => (Operation::Tay, AddrMode::Implied),
        0xa9 => (Operation::Lda, AddrMode::Immediate),
        0xaa => (Operation::Tax, AddrMode::Implied),
        0xac => (Operation::Ldy, AddrMode::Absolute),
        0xad => (Operation::Lda, AddrMode::Absolute),
        0xae => (Operation::Ldx, AddrMode::Absolute),
        0xb0 => (Operation::Bcs, AddrMode::Relative),
        0xb1 => (Operation::Lda, AddrMode::ZeroPageIndirectYIndexed),
        0xb4 => (Operation::Ldy, AddrMode::XIndexedZeroPage),
        0xb5 => (Operation::Lda, AddrMode::XIndexedZeroPage),
        0xb6 => (Operation::Ldx, AddrMode::YIndexedZeroPage),
        0xb8 => (Operation::Clv, AddrMode::Implied),
        0xb9 => (Operation::Lda, AddrMode::YIndexedAbsolute),
        0xba => (Operation::Tsx, AddrMode::Implied),
        0xbc => (Operation::Ldy, AddrMode::XIndexedAbsolute),
        0xbd => (Operation::Lda, AddrMode::XIndexedAbsolute),
        0xbe => (Operation::Ldx, AddrMode::YIndexedAbsolute),
        0xc0 => (Operation::Cpy, AddrMode::Immediate),
        0xc1 => (Operation::Cmp, AddrMode::XIndexedZeroPageIndirect),
        0xc4 => (Operation::Cpy, AddrMode::ZeroPage),
        0xc5 => (Operation::Cmp, AddrMode::ZeroPage),
        0xc6 => (Operation::Dec, AddrMode::ZeroPage),
        0xc8 => (Operation::Iny, AddrMode::Implied),
        0xc9 => (Operation::Cmp, AddrMode::Immediate),
        0xca => (Operation::Dex, AddrMode::Implied),
        0xcc => (Operation::Cpy, AddrMode::Absolute),
        0xcd => (Operation::Cmp, AddrMode::Absolute),
        0xce => (Operation::Dec, AddrMode::Absolute),
        0xd0 => (Operation::Bne, AddrMode::Relative),
        0xd1 => (Operation::Cmp, AddrMode::ZeroPageIndirectYIndexed),
        0xd5 => (Operation::Cmp, AddrMode::XIndexedZeroPage),
        0xd6 => (Operation::Dec, AddrMode::XIndexedZeroPage),
        0xd8 => (Operation::Cld, AddrMode::Implied),
        0xd9 => (Operation::Cmp, AddrMode::YIndexedAbsolute),
        0xdd => (Operation::Cmp, AddrMode::XIndexedAbsolute),
        0xde => (Operation::Dec, AddrMode::XIndexedAbsolute),
        0xe0 => (Operation::Cpx, AddrMode::Immediate),
        0xe1 => (Operation::Sbc, AddrMode::XIndexedZeroPageIndirect),
        0xe4 => (Operation::Cpx, AddrMode::ZeroPage),
        0xe5 => (Operation::Sbc, AddrMode::ZeroPage),
        0xe6 => (Operation::Inc, AddrMode::ZeroPage),
        0xe8 => (Operation::Inx, AddrMode::Implied),
        0xe9 => (Operation::Sbc, AddrMode::Immediate),
        0xea => (Operation::Nop, AddrMode::Implied),
        0xec => (Operation::Cpx, AddrMode::Absolute),
        0xed => (Operation::Sbc, AddrMode::Absolute),
        0xee => (Operation::Inc, AddrMode::Absolute),
        0xf0 => (Operation::Beq, AddrMode::Relative),
        0xf1 => (Operation::Sbc, AddrMode::ZeroPageIndirectYIndexed),
        0xf5 => (Operation::Sbc, AddrMode::XIndexedZeroPage),
        0xf6 => (Operation::Inc, AddrMode::XIndexedZeroPage),
        0xf8 => (Operation::Sed, AddrMode::Implied),
        0xf9 => (Operation::Sbc, AddrMode::YIndexedAbsolute),
        0xfd => (Operation::Sbc, AddrMode::XIndexedAbsolute),
        0xfe => (Operation::Inc, AddrMode::XIndexedAbsolute),
        // TODO: implement remaining opcodes
        _ => panic!("unimplemented opcode: ${opcode:02x}"),
    }
}

/// Reads and disassembles a single 6502 instruction.
pub fn disassemble_instruction<R: Read>(
    reader: &mut R,
) -> io::Result<(u8, Operation, Operand)> {
    let opcode = read_byte(reader)?;
    let (operation, mode) = decode_opcode(opcode);
    let operand = match mode {
        AddrMode::Implied => Operand::Implied,
        AddrMode::Immediate => Operand::Immediate(read_byte(reader)?),
        AddrMode::Relative => Operand::Relative(read_byte(reader)? as i8),
        AddrMode::Absolute => Operand::Absolute(read_word(reader)?),
        AddrMode::AbsoluteIndirect => {
            Operand::AbsoluteIndirect(read_word(reader)?)
        }
        AddrMode::XIndexedAbsolute => {
            Operand::XIndexedAbsolute(read_word(reader)?)
        }
        AddrMode::YIndexedAbsolute => {
            Operand::YIndexedAbsolute(read_word(reader)?)
        }
        AddrMode::ZeroPage => Operand::ZeroPage(read_byte(reader)?),
        AddrMode::XIndexedZeroPage => {
            Operand::XIndexedZeroPage(read_byte(reader)?)
        }
        AddrMode::YIndexedZeroPage => {
            Operand::YIndexedZeroPage(read_byte(reader)?)
        }
        AddrMode::XIndexedZeroPageIndirect => {
            Operand::XIndexedZeroPageIndirect(read_byte(reader)?)
        }
        AddrMode::ZeroPageIndirectYIndexed => {
            Operand::ZeroPageIndirectYIndexed(read_byte(reader)?)
        }
    };
    Ok((opcode, operation, operand))
}

/// Formats a disassembled 6502 instruction as a human-readable string.  `addr`
/// specifies the address of the start of the instruction.  `bus` is required
/// for providing labels for addresses; if no labels are needed, a `null_bus()`
/// can be used.
pub fn format_instruction(
    operation: Operation,
    operand: Operand,
    addr: u16,
    bus: &dyn SimBus,
) -> String {
    format!("{}{}", operation.mnemonic(), operand.format(addr, bus))
}

fn read_byte<R: Read>(reader: &mut R) -> io::Result<u8> {
    let mut buffer = [0u8; 1];
    reader.read_exact(&mut buffer)?;
    Ok(buffer[0])
}

fn read_word<R: Read>(reader: &mut R) -> io::Result<u16> {
    let mut buffer = [0u8; 2];
    reader.read_exact(&mut buffer)?;
    Ok(((buffer[1] as u16) << 8) | (buffer[0] as u16))
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{disassemble_instruction, format_instruction};
    use crate::bus::{LabeledBus, SimBus, new_open_bus};
    use std::collections::HashMap;

    fn disassemble(code: &[u8]) -> String {
        disassemble_with_bus(code, &*new_open_bus(16))
    }

    fn disassemble_with_label(code: &[u8], addr: u32, label: &str) -> String {
        let labels = HashMap::from([(label.to_string(), addr)]);
        let bus = LabeledBus::new(new_open_bus(16), labels);
        disassemble_with_bus(code, &bus)
    }

    fn disassemble_with_bus(mut code: &[u8], bus: &dyn SimBus) -> String {
        let (_opcode, operation, operand) =
            disassemble_instruction(&mut code).unwrap();
        assert!(code.is_empty());
        format_instruction(operation, operand, 0x0000, bus)
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
        assert_eq!(disassemble(&[0x0a]), "ASL");
        assert_eq!(disassemble(&[0x4a]), "LSR");
        assert_eq!(disassemble(&[0x2a]), "ROL");
        assert_eq!(disassemble(&[0x6a]), "ROR");
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
}

//===========================================================================//
