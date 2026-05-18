//! Facilities for disassembling SuperFX machine code.

use crate::addr::Addr;
use crate::bus::SimBus;
use std::fmt;

//===========================================================================//

/// A register in a SuperFX processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Reg {
    /// R0, the default soruce/destination register.
    R0,
    /// R1, the X-coordinate for PLOT and RPIX instructions.
    R1,
    /// R2, the Y-coordinate for PLOT and RPIX instructions.
    R2,
    /// R3, a general-purpose register.
    R3,
    /// R4, the lower 16 bits of the result of an LMULT instruction.
    R4,
    /// R5, a general-purpose register.
    R5,
    /// R6, the multiplier for LMULT and FMULT instructions.
    R6,
    /// R7, the source for the high byte of MERGE instructions.
    R7,
    /// R8, the source for the low byte of MERGE instructions.
    R8,
    /// R9, a general-purpose register.
    R9,
    /// R10, a general-purpose register, conventionally used as a stack
    /// pointer.
    R10,
    /// R11, the destination of a LINK instruction.
    R11,
    /// R12, the counter for a LOOP instruction.
    R12,
    /// R13, the address for a LOOP instruction.
    R13,
    /// R14, the cartridge ROM address pointer for GETxx instructions.
    R14,
    /// R15, the program counter.
    R15,
}

impl Reg {
    fn for_opcode(opcode: u8) -> Reg {
        match opcode & 0x0f {
            0x0 => Reg::R0,
            0x1 => Reg::R1,
            0x2 => Reg::R2,
            0x3 => Reg::R3,
            0x4 => Reg::R4,
            0x5 => Reg::R5,
            0x6 => Reg::R6,
            0x7 => Reg::R7,
            0x8 => Reg::R8,
            0x9 => Reg::R9,
            0xa => Reg::R10,
            0xb => Reg::R11,
            0xc => Reg::R12,
            0xd => Reg::R13,
            0xe => Reg::R14,
            0xf => Reg::R15,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(self, f)
    }
}

//===========================================================================//

/// An operation that can be executed by a SuperFX processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operation {
    /// Adds the given register to the source register with carry, and stores
    /// the result in the destination register.
    Adc(Reg),
    /// Adds the given immediate value to the source register with carry, and
    /// stores the result in the destination register.
    Adci(u8),
    /// Adds the given register to the source register, and stores the result
    /// in the destination register.
    Add(Reg),
    /// Adds the given immediate value to the source register, and stores the
    /// result in the destination register.
    Addi(u8),
    /// Sets the ALT1 flag.
    Alt1,
    /// Sets the ALT2 flag.
    Alt2,
    /// Sets the ALT1 and ALT2 flags.
    Alt3,
    /// Bitwise-ANDs the given register with the source register, and stores
    /// the result in the destination register.
    And(Reg),
    /// Bitwise-ANDS the given immediate value with the source register, and
    /// stores the result in the destination register.
    Andi(u8),
    /// Performs a 1-bit arithmetic shift right on the source register, storing
    /// the result in the destination register.
    Asr,
    /// Branches if the carry flag is cleared.
    Bcc,
    /// Branches if the carry flag is set.
    Bcs,
    /// Branches if the zero flag is set.
    Beq,
    /// Branches if the sign flag is equal to the overflow flag, effectively
    /// checking for a signed greater-than-or-equal-to comparison.
    Bge,
    /// Bitwise-ANDs the complement of the given register with the source
    /// register, and stores the result in the destination register.
    Bic(Reg),
    /// Bitwise-ANDs the complement of given immediate value with the source
    /// register, and stores the result in the destination register.
    Bici(u8),
    /// Branches if the sign flag is not equal to the overflow flag,
    /// effectively checking for a signed less-than comparison.
    Blt,
    /// Branches if the sign flag is set.
    Bmi,
    /// Branches if the zero flag is cleared.
    Bne,
    /// Branches if the sign flag is cleared.
    Bpl,
    /// Branches always.
    Bra,
    /// Branches if the overflow flag is cleared.
    Bvc,
    /// Branches if the overflow flag is set.
    Bvs,
    /// Sets the Cache Base Register (CBR).
    Cache,
    /// Sets color and plot flags from the source register.
    Cmode,
    /// Compares the source register to the given register.
    Cmp(Reg),
    /// Stores the low byte of source register into the color register.
    Color,
    /// Decrements the given register.
    Dec(Reg),
    /// Performs signed division by 2 of the source register, storing the
    /// result in the destination register.
    Div2,
    /// Performs signed multiplication of the source register with R6, storing
    /// the upper 16 bits of the 32-bit result in the destination register,
    /// storing bit 15 in the carry flag, and discarding the lower 15 bits.
    Fmult,
    /// Set the source register (or move from this register to the destination
    /// register if the B flag has been set by a WITH instruction).
    From(Reg),
    /// Reads a byte from the ROM buffer, stores it in the low byte of the
    /// destination register, and zeroes the high byte of the destination
    /// register.
    Getb,
    /// Reads a byte from the ROM buffer and stores it in the high byte of the
    /// destination register, and stores the low byte of the source register in
    /// the low byte of the destination register.
    Getbh,
    /// Reads a byte from the ROM buffer and stores it in the low byte of the
    /// destination register, and stores the high byte of the source register
    /// in the high byte of the destination register.
    Getbl,
    /// Reads a byte from the ROM buffer, sign-extends it to 16 bits, and
    /// stores it in the destination register.
    Getbs,
    /// Reads a byte from the ROM buffer and stores it in the color register.
    Getc,
    /// Stores the high byte of the source register into the low byte of the
    /// destination register, and zeroes the high byte of the destination
    /// register.
    Hib,
    /// Sign-extends an immediate byte to 16 bits, and stores it in the given
    /// register.
    Ibt(Reg),
    /// Increments the given register.
    Inc(Reg),
    /// Stores an immediate word in the given register.
    Iwt(Reg),
    /// Jumps to the address in the given register (in the current program
    /// bank).
    Jmp(Reg),
    /// Loads the byte stored in cartridge RAM at the 16-bit address
    /// stored in the given register combined with the bank number stored in
    /// the RAMBR register, and stores it in the destination register.
    Ldb(Reg),
    /// Loads the 16-bit word stored in cartridge RAM at the 16-bit address
    /// stored in the given register combined with the bank number stored in
    /// the RAMBR register, and stores it in the destination register.
    Ldw(Reg),
    /// Adds the given immediate value to the address of the next instruction,
    /// and stores the result in R11.
    Link(u8),
    /// Jumps to the address stored in the source regsiter, in the program bank
    /// number equal to the lower byte of the given register.
    Ljmp(Reg),
    /// Loads the word stored in cartridge RAM at the 16-bit immediate address
    /// and stores it in the given register.
    Lm(Reg),
    /// Loads the word stored in cartridge RAM at the address equal to two
    /// times the 8-bit immediate value, and stores it in the given register.
    Lms(Reg),
    /// Performs signed multiplication of the source register with R6, storing
    /// the upper 16 bits of the 32-bit result in the destination register and
    /// the lower 16 bits in R4.
    Lmult,
    /// Stores the low byte of the source register into the low byte of the
    /// destination register, and zeroes the high byte of the destination
    /// register.
    Lob,
    /// Decrements R12, then jumps to R13 if R12 isn't zero.
    Loop,
    /// Performs a 1-bit logical shift right on the source register, storing
    /// the result in the destination register.
    Lsr,
    /// Stores the high byte of R7 into the high byte of the destination
    /// register, and the high byte of R8 into the low byte of the destination
    /// register.
    Merge,
    /// Performs signed multiplication of the low byte of the source register
    /// and the low byte of the given register, storing the signed result in
    /// the destination register.
    Mult(Reg),
    /// Performs signed multiplication of the low byte of the source register
    /// and the given immediate value, storing the signed result in the
    /// destination register.
    Multi(u8),
    /// Does nothing (other than restore the ALT0 state).
    Nop,
    /// Inverts the bits from the source register and stores the result in the
    /// destination register.
    Not,
    /// Bitwise-ORs the given register with the source register, and stores
    /// the result in the destination register.
    Or(Reg),
    /// Bitwise-ORS the given immediate value with the source register, and
    /// stores the result in the destination register.
    Ori(u8),
    /// Plots a pixel at position (R1, R2) with a color from the color
    /// register.
    Plot,
    /// Sets the RAMBR register to the low byte of the source register.
    Ramb,
    /// Performs a 1-bit rotate left (through the carry flag) of the source
    /// register, storing the result in the destination register.
    Rol,
    /// Sets the ROMBR register to the low byte of the source register.
    Romb,
    /// Performs a 1-bit rotate right (through the carry flag) of the source
    /// register, storing the result in the destination register.
    Ror,
    /// Reads a pixel at position (R1, R2).
    Rpix,
    /// Subtracts the given register from the source register with borrow, and
    /// stores the result in the destination register.
    Sbc(Reg),
    /// Stores the value of the source register to memory at the most recently
    /// used RAM address.
    Sbk,
    /// Sign-extends the low byte of the source register, storing the result in
    /// the destination register.
    Sex,
    /// Stores the value of the given register into cartridge RAM at the 16-bit
    /// immediate address.
    Sm(Reg),
    /// Stores the value of the given register into cartridge RAM at the
    /// address equal to two times the 8-bit immediate value.
    Sms(Reg),
    /// Clears the "go" flag, and generates an IRQ.
    Stop,
    /// Stores the low byte of the source register into cartridge RAM, starting
    /// at the 16-bit address stored in the given register combined with the
    /// bank number stored in the RAMBR register.
    Stb(Reg),
    /// Stores the 16-bit word in the source register into cartridge RAM,
    /// starting at the 16-bit address stored in the given register combined
    /// with the bank number stored in the RAMBR register.
    Stw(Reg),
    /// Subtracts the given register from the source register, and stores the
    /// result in the destination register.
    Sub(Reg),
    /// Subtracts the given immediate value from the source register, and
    /// stores the result in the destination register.
    Subi(u8),
    /// Swaps the high and low bytes from the source register and stores the
    /// result in the destination register.
    Swap,
    /// Set the destination register (or move from the source register to this
    /// register if the B flag has been set by a WITH instruction).
    To(Reg),
    /// Performs unsigned multiplication of the low byte of the source register
    /// and the low byte of the given register, storing the unsigned result in
    /// the destination register.
    Umult(Reg),
    /// Performs unsigned multiplication of the low byte of the source register
    /// and the given immediate value, storing the unsigned result in the
    /// destination register.
    Umulti(u8),
    /// Sets the source and destination register, and also sets the B flag
    /// (affecting subsequent FROM or TO instructions).
    With(Reg),
    /// Bitwise-XORs the given register with the source register, and stores
    /// the result in the destination register.
    Xor(Reg),
    /// Bitwise-XORS the given immediate value with the source register, and
    /// stores the result in the destination register.
    Xori(u8),
}

impl Operation {
    /// Decodes a SuperFX opcode.
    pub fn from_opcode(opcode: u8, alt: u8) -> Operation {
        let alt = alt & 0x03u8;
        match (opcode, alt) {
            (0x00, _) => Operation::Stop,
            (0x01, _) => Operation::Nop,
            (0x02, _) => Operation::Cache,
            (0x03, _) => Operation::Lsr,
            (0x04, _) => Operation::Rol,
            (0x05, _) => Operation::Bra,
            (0x06, _) => Operation::Bge,
            (0x07, _) => Operation::Blt,
            (0x08, _) => Operation::Bne,
            (0x09, _) => Operation::Beq,
            (0x0a, _) => Operation::Bpl,
            (0x0b, _) => Operation::Bmi,
            (0x0c, _) => Operation::Bcc,
            (0x0d, _) => Operation::Bcs,
            (0x0e, _) => Operation::Bvc,
            (0x0f, _) => Operation::Bvs,
            (0x10..0x20, _) => Operation::To(Reg::for_opcode(opcode)),
            (0x20..0x30, _) => Operation::With(Reg::for_opcode(opcode)),
            (0x30..0x3c, 0 | 2) => Operation::Stw(Reg::for_opcode(opcode)),
            (0x30..0x3c, _) => Operation::Stb(Reg::for_opcode(opcode)),
            (0x3c, _) => Operation::Loop,
            (0x3d, _) => Operation::Alt1,
            (0x3e, _) => Operation::Alt2,
            (0x3f, _) => Operation::Alt3,
            (0x40..0x4c, 0 | 2) => Operation::Ldw(Reg::for_opcode(opcode)),
            (0x40..0x4c, _) => Operation::Ldb(Reg::for_opcode(opcode)),
            (0x4c, 0 | 2) => Operation::Plot,
            (0x4c, _) => Operation::Rpix,
            (0x4d, _) => Operation::Swap,
            (0x4e, 0 | 2) => Operation::Color,
            (0x4e, _) => Operation::Cmode,
            (0x4f, _) => Operation::Not,
            (0x50..0x60, 0) => Operation::Add(Reg::for_opcode(opcode)),
            (0x50..0x60, 1) => Operation::Adc(Reg::for_opcode(opcode)),
            (0x50..0x60, 2) => Operation::Addi(opcode & 0x0f),
            (0x50..0x60, _) => Operation::Adci(opcode & 0x0f),
            (0x60..0x70, 0) => Operation::Sub(Reg::for_opcode(opcode)),
            (0x60..0x70, 1) => Operation::Sbc(Reg::for_opcode(opcode)),
            (0x60..0x70, 2) => Operation::Subi(opcode & 0x0f),
            (0x60..0x70, _) => Operation::Cmp(Reg::for_opcode(opcode)),
            (0x70, _) => Operation::Merge,
            (0x71..0x80, 0) => Operation::And(Reg::for_opcode(opcode)),
            (0x71..0x80, 1) => Operation::Bic(Reg::for_opcode(opcode)),
            (0x71..0x80, 2) => Operation::Andi(opcode & 0x0f),
            (0x71..0x80, _) => Operation::Bici(opcode & 0x0f),
            (0x80..0x90, 0) => Operation::Mult(Reg::for_opcode(opcode)),
            (0x80..0x90, 1) => Operation::Umult(Reg::for_opcode(opcode)),
            (0x80..0x90, 2) => Operation::Multi(opcode & 0x0f),
            (0x80..0x90, _) => Operation::Umulti(opcode & 0x0f),
            (0x90, _) => Operation::Sbk,
            (0x91..0x95, _) => Operation::Link(opcode & 0x0f),
            (0x95, _) => Operation::Sex,
            (0x96, 0 | 2) => Operation::Asr,
            (0x96, _) => Operation::Div2,
            (0x97, _) => Operation::Ror,
            (0x98..0x9e, 0 | 2) => Operation::Jmp(Reg::for_opcode(opcode)),
            (0x98..0x9e, _) => Operation::Ljmp(Reg::for_opcode(opcode)),
            (0x9e, _) => Operation::Lob,
            (0x9f, 0 | 2) => Operation::Fmult,
            (0x9f, _) => Operation::Lmult,
            (0xa0..0xb0, 0) => Operation::Ibt(Reg::for_opcode(opcode)),
            (0xa0..0xb0, 1) => Operation::Lms(Reg::for_opcode(opcode)),
            (0xa0..0xb0, _) => Operation::Sms(Reg::for_opcode(opcode)),
            (0xb0..0xc0, _) => Operation::From(Reg::for_opcode(opcode)),
            (0xc0, _) => Operation::Hib,
            (0xc1..0xd0, 0) => Operation::Or(Reg::for_opcode(opcode)),
            (0xc1..0xd0, 1) => Operation::Xor(Reg::for_opcode(opcode)),
            (0xc1..0xd0, 2) => Operation::Ori(opcode & 0x0f),
            (0xc1..0xd0, _) => Operation::Xori(opcode & 0x0f),
            (0xd0..0xdf, _) => Operation::Inc(Reg::for_opcode(opcode)),
            (0xdf, 0 | 1) => Operation::Getc,
            (0xdf, 2) => Operation::Ramb,
            (0xdf, _) => Operation::Romb,
            (0xe0..0xef, _) => Operation::Dec(Reg::for_opcode(opcode)),
            (0xef, 0) => Operation::Getb,
            (0xef, 1) => Operation::Getbh,
            (0xef, 2) => Operation::Getbl,
            (0xef, _) => Operation::Getbs,
            (0xf0.., 0) => Operation::Iwt(Reg::for_opcode(opcode)),
            (0xf0.., 1) => Operation::Lm(Reg::for_opcode(opcode)),
            (0xf0.., _) => Operation::Sm(Reg::for_opcode(opcode)),
        }
    }
}

//===========================================================================//

/// An argument value for a SuperFX processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    /// No operand.
    None,
    /// Relative branch destination.
    Branch(i8),
    /// Immediate byte.
    ImmediateByte(u8),
    /// Immediate word.
    ImmediateWord(u16),
    /// Address in the current RAM bank.
    RamAddr(u16),
    /// Short address in the current RAM bank.
    RamAddrShort(u8),
}

impl Operand {
    /// The size of this operand, in bytes.
    pub fn size(self) -> u32 {
        match self {
            Operand::None => 0,
            Operand::Branch(_) => 1,
            Operand::ImmediateByte(_) => 1,
            Operand::ImmediateWord(_) => 2,
            Operand::RamAddr(_) => 2,
            Operand::RamAddrShort(_) => 1,
        }
    }

    /// Formats this operand.  `pc` gives the address of the start of the
    /// instruction.  `ramb` specifies the RAM bank register value.
    fn format(self, bus: &dyn SimBus, pc: u32, ramb: u8) -> String {
        match self {
            Operand::None => String::new(),
            Operand::Branch(offset) => {
                let pbr = (pc >> 16) as u8;
                let pc16 = pc as u16;
                let dest = pc16.wrapping_add(2).wrapping_add(offset as u16);
                format_abs(bus, pbr, dest)
            }
            Operand::ImmediateByte(byte) => format!("${byte:02x}"),
            Operand::ImmediateWord(word) => format!("${word:04x}"),
            Operand::RamAddr(addr) => format_abs(bus, ramb, addr),
            Operand::RamAddrShort(half) => {
                format_abs(bus, ramb, u16::from(half) * 2)
            }
        }
    }
}

fn format_abs(bus: &dyn SimBus, bank: u8, abs: u16) -> String {
    let addr = (Addr::from(bank) << 16) | Addr::from(abs);
    match bus.label_at(addr) {
        Some(label) => label.to_string(),
        None => format!("${abs:04x}"),
    }
}

//===========================================================================//

/// A complete instruction, including parameter values, for a SuperFX
/// processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Instruction {
    /// The operation to be performed.
    pub operation: Operation,
    /// The parameter value.
    pub operand: Operand,
}

impl Instruction {
    /// Returns the size of this instruction, in bytes.
    pub fn size(self) -> u32 {
        1 + self.operand.size()
    }

    /// Reads and decodes a single SuperFX instruction.
    pub fn decode(bus: &dyn SimBus, pc: u32, alt: u8) -> Instruction {
        let opcode = bus.peek_byte(Addr::from(pc & 0x1fffff));
        let operation = Operation::from_opcode(opcode, alt);
        let operand = match operation {
            Operation::Bcc
            | Operation::Bcs
            | Operation::Beq
            | Operation::Bge
            | Operation::Blt
            | Operation::Bmi
            | Operation::Bne
            | Operation::Bpl
            | Operation::Bra
            | Operation::Bvc
            | Operation::Bvs => Operand::Branch(next_byte(bus, pc) as i8),
            Operation::Ibt(_) => Operand::ImmediateByte(next_byte(bus, pc)),
            Operation::Iwt(_) => Operand::ImmediateWord(next_word(bus, pc)),
            Operation::Lm(_) | Operation::Sm(_) => {
                Operand::RamAddr(next_word(bus, pc))
            }
            Operation::Lms(_) | Operation::Sms(_) => {
                Operand::RamAddrShort(next_byte(bus, pc))
            }
            _ => Operand::None,
        };
        Instruction { operation, operand }
    }

    /// Formats a disassembled SuperFX instruction as a human-readable string.
    /// `pc` specifies the address of the start of the instruction.  `bus` is
    /// required for providing labels for addresses; if no labels are needed, a
    /// `new_open_bus` can be used.  `ramb` specifies the RAM bank register
    /// value; this is also used for determining labels, so if the bus won't
    /// have any labels, then any value can be passed for this.
    pub fn format(self, bus: &dyn SimBus, pc: u32, ramb: u8) -> String {
        let operand = self.operand;
        match self.operation {
            Operation::Adc(reg) => format!("ADC {reg}"),
            Operation::Adci(imm) => format!("ADC #{imm}"),
            Operation::Add(reg) => format!("ADD {reg}"),
            Operation::Addi(imm) => format!("ADD #{imm}"),
            Operation::Alt1 => "ALT1".to_string(),
            Operation::Alt2 => "ALT2".to_string(),
            Operation::Alt3 => "ALT3".to_string(),
            Operation::And(reg) => format!("AND {reg}"),
            Operation::Andi(imm) => format!("AND #{imm}"),
            Operation::Asr => "ASR".to_string(),
            Operation::Bcc => format!("BCC {}", operand.format(bus, pc, ramb)),
            Operation::Bcs => format!("BCS {}", operand.format(bus, pc, ramb)),
            Operation::Beq => format!("BEQ {}", operand.format(bus, pc, ramb)),
            Operation::Bge => format!("BGE {}", operand.format(bus, pc, ramb)),
            Operation::Bic(reg) => format!("BIC {reg}"),
            Operation::Bici(imm) => format!("BIC #{imm}"),
            Operation::Blt => format!("BLT {}", operand.format(bus, pc, ramb)),
            Operation::Bmi => format!("BMI {}", operand.format(bus, pc, ramb)),
            Operation::Bne => format!("BNE {}", operand.format(bus, pc, ramb)),
            Operation::Bpl => format!("BPL {}", operand.format(bus, pc, ramb)),
            Operation::Bra => format!("BRA {}", operand.format(bus, pc, ramb)),
            Operation::Bvc => format!("BVC {}", operand.format(bus, pc, ramb)),
            Operation::Bvs => format!("BVS {}", operand.format(bus, pc, ramb)),
            Operation::Cache => "CACHE".to_string(),
            Operation::Cmode => "CMODE".to_string(),
            Operation::Cmp(reg) => format!("CMP {reg}"),
            Operation::Color => "COLOR".to_string(),
            Operation::Dec(reg) => format!("DEC {reg}"),
            Operation::Div2 => "DIV2".to_string(),
            Operation::Fmult => "FMULT".to_string(),
            Operation::From(reg) => format!("FROM {reg}"),
            Operation::Getb => "GETB".to_string(),
            Operation::Getbh => "GETBH".to_string(),
            Operation::Getbl => "GETBL".to_string(),
            Operation::Getbs => "GETBS".to_string(),
            Operation::Getc => "GETC".to_string(),
            Operation::Hib => "HIB".to_string(),
            Operation::Ibt(reg) => {
                format!("IBT {reg}, #{}", operand.format(bus, pc, ramb))
            }
            Operation::Iwt(reg) => {
                format!("IWT {reg}, #{}", operand.format(bus, pc, ramb))
            }
            Operation::Inc(reg) => format!("INC {reg}"),
            Operation::Jmp(reg) => format!("JMP {reg}"),
            Operation::Ldb(reg) => format!("LDB ({reg})"),
            Operation::Ldw(reg) => format!("LDW ({reg})"),
            Operation::Link(imm) => format!("LINK #{imm}"),
            Operation::Ljmp(reg) => format!("LJMP {reg}"),
            Operation::Lm(reg) => {
                format!("LM {reg}, ({})", operand.format(bus, pc, ramb))
            }
            Operation::Lms(reg) => {
                format!("LMS {reg}, ({})", operand.format(bus, pc, ramb))
            }
            Operation::Lmult => "LMULT".to_string(),
            Operation::Lob => "LOB".to_string(),
            Operation::Loop => "LOOP".to_string(),
            Operation::Lsr => "LSR".to_string(),
            Operation::Merge => "MERGE".to_string(),
            Operation::Mult(reg) => format!("MULT {reg}"),
            Operation::Multi(imm) => format!("MULT #{imm}"),
            Operation::Nop => "NOP".to_string(),
            Operation::Not => "NOT".to_string(),
            Operation::Or(reg) => format!("OR {reg}"),
            Operation::Ori(imm) => format!("OR #{imm}"),
            Operation::Plot => "PLOT".to_string(),
            Operation::Ramb => "RAMB".to_string(),
            Operation::Rol => "ROL".to_string(),
            Operation::Romb => "ROMB".to_string(),
            Operation::Ror => "ROR".to_string(),
            Operation::Rpix => "RPIX".to_string(),
            Operation::Sbc(reg) => format!("SBC {reg}"),
            Operation::Sbk => "SBK".to_string(),
            Operation::Sex => "SEX".to_string(),
            Operation::Sm(reg) => {
                format!("SM ({}), {reg}", operand.format(bus, pc, ramb))
            }
            Operation::Sms(reg) => {
                format!("SMS ({}), {reg}", operand.format(bus, pc, ramb))
            }
            Operation::Stop => "STOP".to_string(),
            Operation::Stb(reg) => format!("STB ({reg})"),
            Operation::Stw(reg) => format!("STW ({reg})"),
            Operation::Sub(reg) => format!("SUB {reg}"),
            Operation::Subi(imm) => format!("SUB #{imm}"),
            Operation::Swap => "SWAP".to_string(),
            Operation::To(reg) => format!("TO {reg}"),
            Operation::Umult(reg) => format!("UMULT {reg}"),
            Operation::Umulti(imm) => format!("UMULT #{imm}"),
            Operation::With(reg) => format!("WITH {reg}"),
            Operation::Xor(reg) => format!("XOR {reg}"),
            Operation::Xori(imm) => format!("XOR #{imm}"),
        }
    }
}

fn next_byte(bus: &dyn SimBus, pc: u32) -> u8 {
    bus.peek_byte(Addr::from((pc & 0xff0000) | (pc.wrapping_add(1) & 0xffff)))
}

fn third_byte(bus: &dyn SimBus, pc: u32) -> u8 {
    bus.peek_byte(Addr::from((pc & 0xff0000) | (pc.wrapping_add(2) & 0xffff)))
}

fn next_word(bus: &dyn SimBus, pc: u32) -> u16 {
    let lo = next_byte(bus, pc);
    let hi = third_byte(bus, pc);
    (u16::from(hi) << 8) | u16::from(lo)
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

    fn disassemble(code: &[u8], alt: u8) -> String {
        disassemble_with_bus(code, alt, &*make_test_bus(code))
    }

    fn disassemble_with_label(
        code: &[u8],
        alt: u8,
        addr: u32,
        label: &str,
    ) -> String {
        let labels = HashMap::from([(label.to_string(), Addr::from(addr))]);
        let bus = LabeledBus::new(make_test_bus(code), labels);
        disassemble_with_bus(code, alt, &bus)
    }

    fn disassemble_with_bus(code: &[u8], alt: u8, bus: &dyn SimBus) -> String {
        let instruction = Instruction::decode(bus, 0, alt);
        assert_eq!(instruction.size() as usize, code.len());
        instruction.format(bus, 0, 0x70)
    }

    #[test]
    fn disassemble_adc_immediate() {
        assert_eq!(disassemble(&[0x50], 3), "ADC #0");
        assert_eq!(disassemble(&[0x51], 3), "ADC #1");
        assert_eq!(disassemble(&[0x52], 3), "ADC #2");
        assert_eq!(disassemble(&[0x53], 3), "ADC #3");
        assert_eq!(disassemble(&[0x54], 3), "ADC #4");
        assert_eq!(disassemble(&[0x55], 3), "ADC #5");
        assert_eq!(disassemble(&[0x56], 3), "ADC #6");
        assert_eq!(disassemble(&[0x57], 3), "ADC #7");
        assert_eq!(disassemble(&[0x58], 3), "ADC #8");
        assert_eq!(disassemble(&[0x59], 3), "ADC #9");
        assert_eq!(disassemble(&[0x5a], 3), "ADC #10");
        assert_eq!(disassemble(&[0x5b], 3), "ADC #11");
        assert_eq!(disassemble(&[0x5c], 3), "ADC #12");
        assert_eq!(disassemble(&[0x5d], 3), "ADC #13");
        assert_eq!(disassemble(&[0x5e], 3), "ADC #14");
        assert_eq!(disassemble(&[0x5f], 3), "ADC #15");
    }

    #[test]
    fn disassemble_adc_register() {
        assert_eq!(disassemble(&[0x50], 1), "ADC R0");
        assert_eq!(disassemble(&[0x51], 1), "ADC R1");
        assert_eq!(disassemble(&[0x52], 1), "ADC R2");
        assert_eq!(disassemble(&[0x53], 1), "ADC R3");
        assert_eq!(disassemble(&[0x54], 1), "ADC R4");
        assert_eq!(disassemble(&[0x55], 1), "ADC R5");
        assert_eq!(disassemble(&[0x56], 1), "ADC R6");
        assert_eq!(disassemble(&[0x57], 1), "ADC R7");
        assert_eq!(disassemble(&[0x58], 1), "ADC R8");
        assert_eq!(disassemble(&[0x59], 1), "ADC R9");
        assert_eq!(disassemble(&[0x5a], 1), "ADC R10");
        assert_eq!(disassemble(&[0x5b], 1), "ADC R11");
        assert_eq!(disassemble(&[0x5c], 1), "ADC R12");
        assert_eq!(disassemble(&[0x5d], 1), "ADC R13");
        assert_eq!(disassemble(&[0x5e], 1), "ADC R14");
        assert_eq!(disassemble(&[0x5f], 1), "ADC R15");
    }

    #[test]
    fn disassemble_add_immediate() {
        assert_eq!(disassemble(&[0x50], 2), "ADD #0");
        assert_eq!(disassemble(&[0x51], 2), "ADD #1");
        assert_eq!(disassemble(&[0x52], 2), "ADD #2");
        assert_eq!(disassemble(&[0x53], 2), "ADD #3");
        assert_eq!(disassemble(&[0x54], 2), "ADD #4");
        assert_eq!(disassemble(&[0x55], 2), "ADD #5");
        assert_eq!(disassemble(&[0x56], 2), "ADD #6");
        assert_eq!(disassemble(&[0x57], 2), "ADD #7");
        assert_eq!(disassemble(&[0x58], 2), "ADD #8");
        assert_eq!(disassemble(&[0x59], 2), "ADD #9");
        assert_eq!(disassemble(&[0x5a], 2), "ADD #10");
        assert_eq!(disassemble(&[0x5b], 2), "ADD #11");
        assert_eq!(disassemble(&[0x5c], 2), "ADD #12");
        assert_eq!(disassemble(&[0x5d], 2), "ADD #13");
        assert_eq!(disassemble(&[0x5e], 2), "ADD #14");
        assert_eq!(disassemble(&[0x5f], 2), "ADD #15");
    }

    #[test]
    fn disassemble_add_register() {
        assert_eq!(disassemble(&[0x50], 0), "ADD R0");
        assert_eq!(disassemble(&[0x51], 0), "ADD R1");
        assert_eq!(disassemble(&[0x52], 0), "ADD R2");
        assert_eq!(disassemble(&[0x53], 0), "ADD R3");
        assert_eq!(disassemble(&[0x54], 0), "ADD R4");
        assert_eq!(disassemble(&[0x55], 0), "ADD R5");
        assert_eq!(disassemble(&[0x56], 0), "ADD R6");
        assert_eq!(disassemble(&[0x57], 0), "ADD R7");
        assert_eq!(disassemble(&[0x58], 0), "ADD R8");
        assert_eq!(disassemble(&[0x59], 0), "ADD R9");
        assert_eq!(disassemble(&[0x5a], 0), "ADD R10");
        assert_eq!(disassemble(&[0x5b], 0), "ADD R11");
        assert_eq!(disassemble(&[0x5c], 0), "ADD R12");
        assert_eq!(disassemble(&[0x5d], 0), "ADD R13");
        assert_eq!(disassemble(&[0x5e], 0), "ADD R14");
        assert_eq!(disassemble(&[0x5f], 0), "ADD R15");
    }

    #[test]
    fn disassemble_and_immediate() {
        assert_eq!(disassemble(&[0x71], 2), "AND #1");
        assert_eq!(disassemble(&[0x72], 2), "AND #2");
        assert_eq!(disassemble(&[0x73], 2), "AND #3");
        assert_eq!(disassemble(&[0x74], 2), "AND #4");
        assert_eq!(disassemble(&[0x75], 2), "AND #5");
        assert_eq!(disassemble(&[0x76], 2), "AND #6");
        assert_eq!(disassemble(&[0x77], 2), "AND #7");
        assert_eq!(disassemble(&[0x78], 2), "AND #8");
        assert_eq!(disassemble(&[0x79], 2), "AND #9");
        assert_eq!(disassemble(&[0x7a], 2), "AND #10");
        assert_eq!(disassemble(&[0x7b], 2), "AND #11");
        assert_eq!(disassemble(&[0x7c], 2), "AND #12");
        assert_eq!(disassemble(&[0x7d], 2), "AND #13");
        assert_eq!(disassemble(&[0x7e], 2), "AND #14");
        assert_eq!(disassemble(&[0x7f], 2), "AND #15");
    }

    #[test]
    fn disassemble_and_register() {
        assert_eq!(disassemble(&[0x71], 0), "AND R1");
        assert_eq!(disassemble(&[0x72], 0), "AND R2");
        assert_eq!(disassemble(&[0x73], 0), "AND R3");
        assert_eq!(disassemble(&[0x74], 0), "AND R4");
        assert_eq!(disassemble(&[0x75], 0), "AND R5");
        assert_eq!(disassemble(&[0x76], 0), "AND R6");
        assert_eq!(disassemble(&[0x77], 0), "AND R7");
        assert_eq!(disassemble(&[0x78], 0), "AND R8");
        assert_eq!(disassemble(&[0x79], 0), "AND R9");
        assert_eq!(disassemble(&[0x7a], 0), "AND R10");
        assert_eq!(disassemble(&[0x7b], 0), "AND R11");
        assert_eq!(disassemble(&[0x7c], 0), "AND R12");
        assert_eq!(disassemble(&[0x7d], 0), "AND R13");
        assert_eq!(disassemble(&[0x7e], 0), "AND R14");
        assert_eq!(disassemble(&[0x7f], 0), "AND R15");
    }

    #[test]
    fn disassemble_bic_immediate() {
        assert_eq!(disassemble(&[0x71], 3), "BIC #1");
        assert_eq!(disassemble(&[0x72], 3), "BIC #2");
        assert_eq!(disassemble(&[0x73], 3), "BIC #3");
        assert_eq!(disassemble(&[0x74], 3), "BIC #4");
        assert_eq!(disassemble(&[0x75], 3), "BIC #5");
        assert_eq!(disassemble(&[0x76], 3), "BIC #6");
        assert_eq!(disassemble(&[0x77], 3), "BIC #7");
        assert_eq!(disassemble(&[0x78], 3), "BIC #8");
        assert_eq!(disassemble(&[0x79], 3), "BIC #9");
        assert_eq!(disassemble(&[0x7a], 3), "BIC #10");
        assert_eq!(disassemble(&[0x7b], 3), "BIC #11");
        assert_eq!(disassemble(&[0x7c], 3), "BIC #12");
        assert_eq!(disassemble(&[0x7d], 3), "BIC #13");
        assert_eq!(disassemble(&[0x7e], 3), "BIC #14");
        assert_eq!(disassemble(&[0x7f], 3), "BIC #15");
    }

    #[test]
    fn disassemble_bic_register() {
        assert_eq!(disassemble(&[0x71], 1), "BIC R1");
        assert_eq!(disassemble(&[0x72], 1), "BIC R2");
        assert_eq!(disassemble(&[0x73], 1), "BIC R3");
        assert_eq!(disassemble(&[0x74], 1), "BIC R4");
        assert_eq!(disassemble(&[0x75], 1), "BIC R5");
        assert_eq!(disassemble(&[0x76], 1), "BIC R6");
        assert_eq!(disassemble(&[0x77], 1), "BIC R7");
        assert_eq!(disassemble(&[0x78], 1), "BIC R8");
        assert_eq!(disassemble(&[0x79], 1), "BIC R9");
        assert_eq!(disassemble(&[0x7a], 1), "BIC R10");
        assert_eq!(disassemble(&[0x7b], 1), "BIC R11");
        assert_eq!(disassemble(&[0x7c], 1), "BIC R12");
        assert_eq!(disassemble(&[0x7d], 1), "BIC R13");
        assert_eq!(disassemble(&[0x7e], 1), "BIC R14");
        assert_eq!(disassemble(&[0x7f], 1), "BIC R15");
    }

    #[test]
    fn disassemble_branch() {
        assert_eq!(disassemble(&[0x0c, 0x10], 0), "BCC $0012");
        assert_eq!(disassemble(&[0x0d, 0x0e], 0), "BCS $0010");
        assert_eq!(disassemble(&[0x09, 0x80], 0), "BEQ $ff82");
        assert_eq!(disassemble(&[0x06, 0x10], 0), "BGE $0012");
        assert_eq!(disassemble(&[0x07, 0x10], 0), "BLT $0012");
        assert_eq!(disassemble(&[0x0b, 0xf0], 0), "BMI $fff2");
        assert_eq!(disassemble(&[0x08, 0x81], 0), "BNE $ff83");
        assert_eq!(disassemble(&[0x0a, 0x7f], 0), "BPL $0081");
        assert_eq!(disassemble(&[0x05, 0x10], 0), "BRA $0012");
        assert_eq!(disassemble(&[0x0e, 0xfe], 0), "BVC $0000");
        assert_eq!(disassemble(&[0x0f, 0xfd], 0), "BVS $ffff");
        assert_eq!(
            disassemble_with_label(&[0x0c, 0x10], 0, 0x12, "foo"),
            "BCC foo"
        );
    }

    #[test]
    fn disassemble_cmp_register() {
        assert_eq!(disassemble(&[0x60], 3), "CMP R0");
        assert_eq!(disassemble(&[0x61], 3), "CMP R1");
        assert_eq!(disassemble(&[0x62], 3), "CMP R2");
        assert_eq!(disassemble(&[0x63], 3), "CMP R3");
        assert_eq!(disassemble(&[0x64], 3), "CMP R4");
        assert_eq!(disassemble(&[0x65], 3), "CMP R5");
        assert_eq!(disassemble(&[0x66], 3), "CMP R6");
        assert_eq!(disassemble(&[0x67], 3), "CMP R7");
        assert_eq!(disassemble(&[0x68], 3), "CMP R8");
        assert_eq!(disassemble(&[0x69], 3), "CMP R9");
        assert_eq!(disassemble(&[0x6a], 3), "CMP R10");
        assert_eq!(disassemble(&[0x6b], 3), "CMP R11");
        assert_eq!(disassemble(&[0x6c], 3), "CMP R12");
        assert_eq!(disassemble(&[0x6d], 3), "CMP R13");
        assert_eq!(disassemble(&[0x6e], 3), "CMP R14");
        assert_eq!(disassemble(&[0x6f], 3), "CMP R15");
    }

    #[test]
    fn disassemble_dec() {
        assert_eq!(disassemble(&[0xe0], 0), "DEC R0");
        assert_eq!(disassemble(&[0xe1], 0), "DEC R1");
        assert_eq!(disassemble(&[0xe2], 0), "DEC R2");
        assert_eq!(disassemble(&[0xe3], 0), "DEC R3");
        assert_eq!(disassemble(&[0xe4], 0), "DEC R4");
        assert_eq!(disassemble(&[0xe5], 0), "DEC R5");
        assert_eq!(disassemble(&[0xe6], 0), "DEC R6");
        assert_eq!(disassemble(&[0xe7], 0), "DEC R7");
        assert_eq!(disassemble(&[0xe8], 0), "DEC R8");
        assert_eq!(disassemble(&[0xe9], 0), "DEC R9");
        assert_eq!(disassemble(&[0xea], 0), "DEC R10");
        assert_eq!(disassemble(&[0xeb], 0), "DEC R11");
        assert_eq!(disassemble(&[0xec], 0), "DEC R12");
        assert_eq!(disassemble(&[0xed], 0), "DEC R13");
        assert_eq!(disassemble(&[0xee], 0), "DEC R14");
    }

    #[test]
    fn disassemble_from() {
        assert_eq!(disassemble(&[0xb0], 0), "FROM R0");
        assert_eq!(disassemble(&[0xb1], 0), "FROM R1");
        assert_eq!(disassemble(&[0xb2], 0), "FROM R2");
        assert_eq!(disassemble(&[0xb3], 0), "FROM R3");
        assert_eq!(disassemble(&[0xb4], 0), "FROM R4");
        assert_eq!(disassemble(&[0xb5], 0), "FROM R5");
        assert_eq!(disassemble(&[0xb6], 0), "FROM R6");
        assert_eq!(disassemble(&[0xb7], 0), "FROM R7");
        assert_eq!(disassemble(&[0xb8], 0), "FROM R8");
        assert_eq!(disassemble(&[0xb9], 0), "FROM R9");
        assert_eq!(disassemble(&[0xba], 0), "FROM R10");
        assert_eq!(disassemble(&[0xbb], 0), "FROM R11");
        assert_eq!(disassemble(&[0xbc], 0), "FROM R12");
        assert_eq!(disassemble(&[0xbd], 0), "FROM R13");
        assert_eq!(disassemble(&[0xbe], 0), "FROM R14");
        assert_eq!(disassemble(&[0xbf], 0), "FROM R15");
    }

    #[test]
    fn disassemble_ibt() {
        assert_eq!(disassemble(&[0xa0, 0x10], 0), "IBT R0, #$10");
        assert_eq!(disassemble(&[0xa1, 0x7f], 0), "IBT R1, #$7f");
        assert_eq!(disassemble(&[0xa2, 0x00], 0), "IBT R2, #$00");
        assert_eq!(disassemble(&[0xa3, 0x45], 0), "IBT R3, #$45");
        assert_eq!(disassemble(&[0xa4, 0x37], 0), "IBT R4, #$37");
        assert_eq!(disassemble(&[0xa5, 0x37], 0), "IBT R5, #$37");
        assert_eq!(disassemble(&[0xa6, 0x37], 0), "IBT R6, #$37");
        assert_eq!(disassemble(&[0xa7, 0x37], 0), "IBT R7, #$37");
        assert_eq!(disassemble(&[0xa8, 0x37], 0), "IBT R8, #$37");
        assert_eq!(disassemble(&[0xa9, 0x37], 0), "IBT R9, #$37");
        assert_eq!(disassemble(&[0xaa, 0x37], 0), "IBT R10, #$37");
        assert_eq!(disassemble(&[0xab, 0x37], 0), "IBT R11, #$37");
        assert_eq!(disassemble(&[0xac, 0x37], 0), "IBT R12, #$37");
        assert_eq!(disassemble(&[0xad, 0x37], 0), "IBT R13, #$37");
        assert_eq!(disassemble(&[0xae, 0x37], 0), "IBT R14, #$37");
        assert_eq!(disassemble(&[0xaf, 0x37], 0), "IBT R15, #$37");
    }

    #[test]
    fn disassemble_inc() {
        assert_eq!(disassemble(&[0xd0], 0), "INC R0");
        assert_eq!(disassemble(&[0xd1], 0), "INC R1");
        assert_eq!(disassemble(&[0xd2], 0), "INC R2");
        assert_eq!(disassemble(&[0xd3], 0), "INC R3");
        assert_eq!(disassemble(&[0xd4], 0), "INC R4");
        assert_eq!(disassemble(&[0xd5], 0), "INC R5");
        assert_eq!(disassemble(&[0xd6], 0), "INC R6");
        assert_eq!(disassemble(&[0xd7], 0), "INC R7");
        assert_eq!(disassemble(&[0xd8], 0), "INC R8");
        assert_eq!(disassemble(&[0xd9], 0), "INC R9");
        assert_eq!(disassemble(&[0xda], 0), "INC R10");
        assert_eq!(disassemble(&[0xdb], 0), "INC R11");
        assert_eq!(disassemble(&[0xdc], 0), "INC R12");
        assert_eq!(disassemble(&[0xdd], 0), "INC R13");
        assert_eq!(disassemble(&[0xde], 0), "INC R14");
    }

    #[test]
    fn disassemble_iwt() {
        assert_eq!(disassemble(&[0xf0, 0x00, 0x00], 0), "IWT R0, #$0000");
        assert_eq!(disassemble(&[0xf1, 0x34, 0x12], 0), "IWT R1, #$1234");
        assert_eq!(disassemble(&[0xf2, 0xfe, 0xff], 0), "IWT R2, #$fffe");
        assert_eq!(disassemble(&[0xf3, 0x34, 0x12], 0), "IWT R3, #$1234");
        assert_eq!(disassemble(&[0xf4, 0x34, 0x12], 0), "IWT R4, #$1234");
        assert_eq!(disassemble(&[0xf5, 0x34, 0x12], 0), "IWT R5, #$1234");
        assert_eq!(disassemble(&[0xf6, 0x34, 0x12], 0), "IWT R6, #$1234");
        assert_eq!(disassemble(&[0xf7, 0x34, 0x12], 0), "IWT R7, #$1234");
        assert_eq!(disassemble(&[0xf8, 0x34, 0x12], 0), "IWT R8, #$1234");
        assert_eq!(disassemble(&[0xf9, 0x34, 0x12], 0), "IWT R9, #$1234");
        assert_eq!(disassemble(&[0xfa, 0x34, 0x12], 0), "IWT R10, #$1234");
        assert_eq!(disassemble(&[0xfb, 0x34, 0x12], 0), "IWT R11, #$1234");
        assert_eq!(disassemble(&[0xfc, 0x34, 0x12], 0), "IWT R12, #$1234");
        assert_eq!(disassemble(&[0xfd, 0x34, 0x12], 0), "IWT R13, #$1234");
        assert_eq!(disassemble(&[0xfe, 0x34, 0x12], 0), "IWT R14, #$1234");
        assert_eq!(disassemble(&[0xff, 0x34, 0x12], 0), "IWT R15, #$1234");
    }

    #[test]
    fn disassemble_jmp() {
        assert_eq!(disassemble(&[0x98], 0), "JMP R8");
        assert_eq!(disassemble(&[0x99], 0), "JMP R9");
        assert_eq!(disassemble(&[0x9a], 0), "JMP R10");
        assert_eq!(disassemble(&[0x9b], 0), "JMP R11");
        assert_eq!(disassemble(&[0x9c], 0), "JMP R12");
        assert_eq!(disassemble(&[0x9d], 0), "JMP R13");
    }

    #[test]
    fn disassemble_ldb() {
        assert_eq!(disassemble(&[0x40], 1), "LDB (R0)");
        assert_eq!(disassemble(&[0x41], 1), "LDB (R1)");
        assert_eq!(disassemble(&[0x42], 1), "LDB (R2)");
        assert_eq!(disassemble(&[0x43], 1), "LDB (R3)");
        assert_eq!(disassemble(&[0x44], 1), "LDB (R4)");
        assert_eq!(disassemble(&[0x45], 1), "LDB (R5)");
        assert_eq!(disassemble(&[0x46], 1), "LDB (R6)");
        assert_eq!(disassemble(&[0x47], 1), "LDB (R7)");
        assert_eq!(disassemble(&[0x48], 1), "LDB (R8)");
        assert_eq!(disassemble(&[0x49], 1), "LDB (R9)");
        assert_eq!(disassemble(&[0x4a], 1), "LDB (R10)");
        assert_eq!(disassemble(&[0x4b], 1), "LDB (R11)");
    }

    #[test]
    fn disassemble_ldw() {
        assert_eq!(disassemble(&[0x40], 0), "LDW (R0)");
        assert_eq!(disassemble(&[0x41], 0), "LDW (R1)");
        assert_eq!(disassemble(&[0x42], 0), "LDW (R2)");
        assert_eq!(disassemble(&[0x43], 0), "LDW (R3)");
        assert_eq!(disassemble(&[0x44], 0), "LDW (R4)");
        assert_eq!(disassemble(&[0x45], 0), "LDW (R5)");
        assert_eq!(disassemble(&[0x46], 0), "LDW (R6)");
        assert_eq!(disassemble(&[0x47], 0), "LDW (R7)");
        assert_eq!(disassemble(&[0x48], 0), "LDW (R8)");
        assert_eq!(disassemble(&[0x49], 0), "LDW (R9)");
        assert_eq!(disassemble(&[0x4a], 0), "LDW (R10)");
        assert_eq!(disassemble(&[0x4b], 0), "LDW (R11)");
    }

    #[test]
    fn disassemble_link() {
        assert_eq!(disassemble(&[0x91], 0), "LINK #1");
        assert_eq!(disassemble(&[0x92], 0), "LINK #2");
        assert_eq!(disassemble(&[0x93], 0), "LINK #3");
        assert_eq!(disassemble(&[0x94], 0), "LINK #4");
    }

    #[test]
    fn disassemble_ljmp() {
        assert_eq!(disassemble(&[0x98], 1), "LJMP R8");
        assert_eq!(disassemble(&[0x99], 1), "LJMP R9");
        assert_eq!(disassemble(&[0x9a], 1), "LJMP R10");
        assert_eq!(disassemble(&[0x9b], 1), "LJMP R11");
        assert_eq!(disassemble(&[0x9c], 1), "LJMP R12");
        assert_eq!(disassemble(&[0x9d], 1), "LJMP R13");
    }

    #[test]
    fn disassemble_lm() {
        assert_eq!(disassemble(&[0xf0, 0x34, 0x12], 1), "LM R0, ($1234)");
        assert_eq!(disassemble(&[0xf1, 0x34, 0x12], 1), "LM R1, ($1234)");
        assert_eq!(disassemble(&[0xf2, 0x34, 0x12], 1), "LM R2, ($1234)");
        assert_eq!(disassemble(&[0xf3, 0x34, 0x12], 1), "LM R3, ($1234)");
        assert_eq!(disassemble(&[0xf4, 0x34, 0x12], 1), "LM R4, ($1234)");
        assert_eq!(disassemble(&[0xf5, 0x34, 0x12], 1), "LM R5, ($1234)");
        assert_eq!(disassemble(&[0xf6, 0x34, 0x12], 1), "LM R6, ($1234)");
        assert_eq!(disassemble(&[0xf7, 0x34, 0x12], 1), "LM R7, ($1234)");
        assert_eq!(disassemble(&[0xf8, 0x34, 0x12], 1), "LM R8, ($1234)");
        assert_eq!(disassemble(&[0xf9, 0x34, 0x12], 1), "LM R9, ($1234)");
        assert_eq!(disassemble(&[0xfa, 0x34, 0x12], 1), "LM R10, ($1234)");
        assert_eq!(disassemble(&[0xfb, 0x34, 0x12], 1), "LM R11, ($1234)");
        assert_eq!(disassemble(&[0xfc, 0x34, 0x12], 1), "LM R12, ($1234)");
        assert_eq!(disassemble(&[0xfd, 0x34, 0x12], 1), "LM R13, ($1234)");
        assert_eq!(disassemble(&[0xfe, 0x34, 0x12], 1), "LM R14, ($1234)");
        assert_eq!(disassemble(&[0xff, 0x34, 0x12], 1), "LM R15, ($1234)");
        assert_eq!(
            disassemble_with_label(&[0xf0, 0x34, 0x12], 1, 0x701234, "foo"),
            "LM R0, (foo)"
        );
    }

    #[test]
    fn disassemble_lms() {
        assert_eq!(disassemble(&[0xa0, 0x00], 1), "LMS R0, ($0000)");
        assert_eq!(disassemble(&[0xa1, 0x01], 1), "LMS R1, ($0002)");
        assert_eq!(disassemble(&[0xa2, 0xff], 1), "LMS R2, ($01fe)");
        assert_eq!(disassemble(&[0xa3, 0x80], 1), "LMS R3, ($0100)");
        assert_eq!(disassemble(&[0xa4, 0x12], 1), "LMS R4, ($0024)");
        assert_eq!(disassemble(&[0xa5, 0x12], 1), "LMS R5, ($0024)");
        assert_eq!(disassemble(&[0xa6, 0x12], 1), "LMS R6, ($0024)");
        assert_eq!(disassemble(&[0xa7, 0x12], 1), "LMS R7, ($0024)");
        assert_eq!(disassemble(&[0xa8, 0x12], 1), "LMS R8, ($0024)");
        assert_eq!(disassemble(&[0xa9, 0x12], 1), "LMS R9, ($0024)");
        assert_eq!(disassemble(&[0xaa, 0x12], 1), "LMS R10, ($0024)");
        assert_eq!(disassemble(&[0xab, 0x12], 1), "LMS R11, ($0024)");
        assert_eq!(disassemble(&[0xac, 0x12], 1), "LMS R12, ($0024)");
        assert_eq!(disassemble(&[0xad, 0x12], 1), "LMS R13, ($0024)");
        assert_eq!(disassemble(&[0xae, 0x12], 1), "LMS R14, ($0024)");
        assert_eq!(disassemble(&[0xaf, 0x12], 1), "LMS R15, ($0024)");
        assert_eq!(
            disassemble_with_label(&[0xa0, 0x12], 1, 0x700024, "foo"),
            "LMS R0, (foo)"
        );
    }

    #[test]
    fn disassemble_miscellaneous() {
        assert_eq!(disassemble(&[0x3d], 0), "ALT1");
        assert_eq!(disassemble(&[0x3e], 0), "ALT2");
        assert_eq!(disassemble(&[0x3f], 0), "ALT3");
        assert_eq!(disassemble(&[0x96], 0), "ASR");
        assert_eq!(disassemble(&[0x96], 2), "ASR");
        assert_eq!(disassemble(&[0x02], 0), "CACHE");
        assert_eq!(disassemble(&[0x4e], 1), "CMODE");
        assert_eq!(disassemble(&[0x4e], 3), "CMODE");
        assert_eq!(disassemble(&[0x4e], 0), "COLOR");
        assert_eq!(disassemble(&[0x4e], 2), "COLOR");
        assert_eq!(disassemble(&[0x96], 1), "DIV2");
        assert_eq!(disassemble(&[0x96], 3), "DIV2");
        assert_eq!(disassemble(&[0x9f], 0), "FMULT");
        assert_eq!(disassemble(&[0x9f], 2), "FMULT");
        assert_eq!(disassemble(&[0xef], 0), "GETB");
        assert_eq!(disassemble(&[0xef], 1), "GETBH");
        assert_eq!(disassemble(&[0xef], 2), "GETBL");
        assert_eq!(disassemble(&[0xef], 3), "GETBS");
        assert_eq!(disassemble(&[0xdf], 0), "GETC");
        assert_eq!(disassemble(&[0xdf], 1), "GETC");
        assert_eq!(disassemble(&[0xc0], 0), "HIB");
        assert_eq!(disassemble(&[0x9f], 1), "LMULT");
        assert_eq!(disassemble(&[0x9f], 3), "LMULT");
        assert_eq!(disassemble(&[0x9e], 0), "LOB");
        assert_eq!(disassemble(&[0x3c], 0), "LOOP");
        assert_eq!(disassemble(&[0x70], 0), "MERGE");
        assert_eq!(disassemble(&[0x01], 0), "NOP");
        assert_eq!(disassemble(&[0x4f], 0), "NOT");
        assert_eq!(disassemble(&[0x4c], 0), "PLOT");
        assert_eq!(disassemble(&[0x4c], 2), "PLOT");
        assert_eq!(disassemble(&[0xdf], 2), "RAMB");
        assert_eq!(disassemble(&[0xdf], 3), "ROMB");
        assert_eq!(disassemble(&[0x97], 0), "ROR");
        assert_eq!(disassemble(&[0x4c], 1), "RPIX");
        assert_eq!(disassemble(&[0x4c], 3), "RPIX");
        assert_eq!(disassemble(&[0x90], 0), "SBK");
        assert_eq!(disassemble(&[0x95], 0), "SEX");
        assert_eq!(disassemble(&[0x00], 0), "STOP");
        assert_eq!(disassemble(&[0x4d], 0), "SWAP");
    }

    #[test]
    fn disassemble_mult_immediate() {
        assert_eq!(disassemble(&[0x80], 2), "MULT #0");
        assert_eq!(disassemble(&[0x81], 2), "MULT #1");
        assert_eq!(disassemble(&[0x82], 2), "MULT #2");
        assert_eq!(disassemble(&[0x83], 2), "MULT #3");
        assert_eq!(disassemble(&[0x84], 2), "MULT #4");
        assert_eq!(disassemble(&[0x85], 2), "MULT #5");
        assert_eq!(disassemble(&[0x86], 2), "MULT #6");
        assert_eq!(disassemble(&[0x87], 2), "MULT #7");
        assert_eq!(disassemble(&[0x88], 2), "MULT #8");
        assert_eq!(disassemble(&[0x89], 2), "MULT #9");
        assert_eq!(disassemble(&[0x8a], 2), "MULT #10");
        assert_eq!(disassemble(&[0x8b], 2), "MULT #11");
        assert_eq!(disassemble(&[0x8c], 2), "MULT #12");
        assert_eq!(disassemble(&[0x8d], 2), "MULT #13");
        assert_eq!(disassemble(&[0x8e], 2), "MULT #14");
        assert_eq!(disassemble(&[0x8f], 2), "MULT #15");
    }

    #[test]
    fn disassemble_mult_register() {
        assert_eq!(disassemble(&[0x80], 0), "MULT R0");
        assert_eq!(disassemble(&[0x81], 0), "MULT R1");
        assert_eq!(disassemble(&[0x82], 0), "MULT R2");
        assert_eq!(disassemble(&[0x83], 0), "MULT R3");
        assert_eq!(disassemble(&[0x84], 0), "MULT R4");
        assert_eq!(disassemble(&[0x85], 0), "MULT R5");
        assert_eq!(disassemble(&[0x86], 0), "MULT R6");
        assert_eq!(disassemble(&[0x87], 0), "MULT R7");
        assert_eq!(disassemble(&[0x88], 0), "MULT R8");
        assert_eq!(disassemble(&[0x89], 0), "MULT R9");
        assert_eq!(disassemble(&[0x8a], 0), "MULT R10");
        assert_eq!(disassemble(&[0x8b], 0), "MULT R11");
        assert_eq!(disassemble(&[0x8c], 0), "MULT R12");
        assert_eq!(disassemble(&[0x8d], 0), "MULT R13");
        assert_eq!(disassemble(&[0x8e], 0), "MULT R14");
        assert_eq!(disassemble(&[0x8f], 0), "MULT R15");
    }

    #[test]
    fn disassemble_or_immediate() {
        assert_eq!(disassemble(&[0xc1], 2), "OR #1");
        assert_eq!(disassemble(&[0xc2], 2), "OR #2");
        assert_eq!(disassemble(&[0xc3], 2), "OR #3");
        assert_eq!(disassemble(&[0xc4], 2), "OR #4");
        assert_eq!(disassemble(&[0xc5], 2), "OR #5");
        assert_eq!(disassemble(&[0xc6], 2), "OR #6");
        assert_eq!(disassemble(&[0xc7], 2), "OR #7");
        assert_eq!(disassemble(&[0xc8], 2), "OR #8");
        assert_eq!(disassemble(&[0xc9], 2), "OR #9");
        assert_eq!(disassemble(&[0xca], 2), "OR #10");
        assert_eq!(disassemble(&[0xcb], 2), "OR #11");
        assert_eq!(disassemble(&[0xcc], 2), "OR #12");
        assert_eq!(disassemble(&[0xcd], 2), "OR #13");
        assert_eq!(disassemble(&[0xce], 2), "OR #14");
        assert_eq!(disassemble(&[0xcf], 2), "OR #15");
    }

    #[test]
    fn disassemble_or_register() {
        assert_eq!(disassemble(&[0xc1], 0), "OR R1");
        assert_eq!(disassemble(&[0xc2], 0), "OR R2");
        assert_eq!(disassemble(&[0xc3], 0), "OR R3");
        assert_eq!(disassemble(&[0xc4], 0), "OR R4");
        assert_eq!(disassemble(&[0xc5], 0), "OR R5");
        assert_eq!(disassemble(&[0xc6], 0), "OR R6");
        assert_eq!(disassemble(&[0xc7], 0), "OR R7");
        assert_eq!(disassemble(&[0xc8], 0), "OR R8");
        assert_eq!(disassemble(&[0xc9], 0), "OR R9");
        assert_eq!(disassemble(&[0xca], 0), "OR R10");
        assert_eq!(disassemble(&[0xcb], 0), "OR R11");
        assert_eq!(disassemble(&[0xcc], 0), "OR R12");
        assert_eq!(disassemble(&[0xcd], 0), "OR R13");
        assert_eq!(disassemble(&[0xce], 0), "OR R14");
        assert_eq!(disassemble(&[0xcf], 0), "OR R15");
    }

    #[test]
    fn disassemble_sbc_register() {
        assert_eq!(disassemble(&[0x60], 1), "SBC R0");
        assert_eq!(disassemble(&[0x61], 1), "SBC R1");
        assert_eq!(disassemble(&[0x62], 1), "SBC R2");
        assert_eq!(disassemble(&[0x63], 1), "SBC R3");
        assert_eq!(disassemble(&[0x64], 1), "SBC R4");
        assert_eq!(disassemble(&[0x65], 1), "SBC R5");
        assert_eq!(disassemble(&[0x66], 1), "SBC R6");
        assert_eq!(disassemble(&[0x67], 1), "SBC R7");
        assert_eq!(disassemble(&[0x68], 1), "SBC R8");
        assert_eq!(disassemble(&[0x69], 1), "SBC R9");
        assert_eq!(disassemble(&[0x6a], 1), "SBC R10");
        assert_eq!(disassemble(&[0x6b], 1), "SBC R11");
        assert_eq!(disassemble(&[0x6c], 1), "SBC R12");
        assert_eq!(disassemble(&[0x6d], 1), "SBC R13");
        assert_eq!(disassemble(&[0x6e], 1), "SBC R14");
        assert_eq!(disassemble(&[0x6f], 1), "SBC R15");
    }

    #[test]
    fn disassemble_shift() {
        assert_eq!(disassemble(&[0x03], 0), "LSR");
        assert_eq!(disassemble(&[0x04], 0), "ROL");
    }

    #[test]
    fn disassemble_sm() {
        assert_eq!(disassemble(&[0xf0, 0x34, 0x12], 2), "SM ($1234), R0");
        assert_eq!(disassemble(&[0xf1, 0x34, 0x12], 2), "SM ($1234), R1");
        assert_eq!(disassemble(&[0xf2, 0x34, 0x12], 2), "SM ($1234), R2");
        assert_eq!(disassemble(&[0xf3, 0x34, 0x12], 2), "SM ($1234), R3");
        assert_eq!(disassemble(&[0xf4, 0x34, 0x12], 2), "SM ($1234), R4");
        assert_eq!(disassemble(&[0xf5, 0x34, 0x12], 2), "SM ($1234), R5");
        assert_eq!(disassemble(&[0xf6, 0x34, 0x12], 2), "SM ($1234), R6");
        assert_eq!(disassemble(&[0xf7, 0x34, 0x12], 2), "SM ($1234), R7");
        assert_eq!(disassemble(&[0xf8, 0x34, 0x12], 2), "SM ($1234), R8");
        assert_eq!(disassemble(&[0xf9, 0x34, 0x12], 2), "SM ($1234), R9");
        assert_eq!(disassemble(&[0xfa, 0x34, 0x12], 2), "SM ($1234), R10");
        assert_eq!(disassemble(&[0xfb, 0x34, 0x12], 2), "SM ($1234), R11");
        assert_eq!(disassemble(&[0xfc, 0x34, 0x12], 2), "SM ($1234), R12");
        assert_eq!(disassemble(&[0xfd, 0x34, 0x12], 2), "SM ($1234), R13");
        assert_eq!(disassemble(&[0xfe, 0x34, 0x12], 2), "SM ($1234), R14");
        assert_eq!(disassemble(&[0xff, 0x34, 0x12], 2), "SM ($1234), R15");
        assert_eq!(
            disassemble_with_label(&[0xf0, 0x34, 0x12], 2, 0x701234, "foo"),
            "SM (foo), R0"
        );
    }

    #[test]
    fn disassemble_sms() {
        assert_eq!(disassemble(&[0xa0, 0x00], 2), "SMS ($0000), R0");
        assert_eq!(disassemble(&[0xa1, 0x01], 2), "SMS ($0002), R1");
        assert_eq!(disassemble(&[0xa2, 0xff], 2), "SMS ($01fe), R2");
        assert_eq!(disassemble(&[0xa3, 0x80], 2), "SMS ($0100), R3");
        assert_eq!(disassemble(&[0xa4, 0x12], 2), "SMS ($0024), R4");
        assert_eq!(disassemble(&[0xa5, 0x12], 2), "SMS ($0024), R5");
        assert_eq!(disassemble(&[0xa6, 0x12], 2), "SMS ($0024), R6");
        assert_eq!(disassemble(&[0xa7, 0x12], 2), "SMS ($0024), R7");
        assert_eq!(disassemble(&[0xa8, 0x12], 2), "SMS ($0024), R8");
        assert_eq!(disassemble(&[0xa9, 0x12], 2), "SMS ($0024), R9");
        assert_eq!(disassemble(&[0xaa, 0x12], 2), "SMS ($0024), R10");
        assert_eq!(disassemble(&[0xab, 0x12], 2), "SMS ($0024), R11");
        assert_eq!(disassemble(&[0xac, 0x12], 2), "SMS ($0024), R12");
        assert_eq!(disassemble(&[0xad, 0x12], 2), "SMS ($0024), R13");
        assert_eq!(disassemble(&[0xae, 0x12], 2), "SMS ($0024), R14");
        assert_eq!(disassemble(&[0xaf, 0x12], 2), "SMS ($0024), R15");
        assert_eq!(
            disassemble_with_label(&[0xa0, 0x12], 2, 0x700024, "foo"),
            "SMS (foo), R0"
        );
    }

    #[test]
    fn disassemble_stb() {
        assert_eq!(disassemble(&[0x30], 1), "STB (R0)");
        assert_eq!(disassemble(&[0x31], 1), "STB (R1)");
        assert_eq!(disassemble(&[0x32], 1), "STB (R2)");
        assert_eq!(disassemble(&[0x33], 1), "STB (R3)");
        assert_eq!(disassemble(&[0x34], 1), "STB (R4)");
        assert_eq!(disassemble(&[0x35], 1), "STB (R5)");
        assert_eq!(disassemble(&[0x36], 1), "STB (R6)");
        assert_eq!(disassemble(&[0x37], 1), "STB (R7)");
        assert_eq!(disassemble(&[0x38], 1), "STB (R8)");
        assert_eq!(disassemble(&[0x39], 1), "STB (R9)");
        assert_eq!(disassemble(&[0x3a], 1), "STB (R10)");
        assert_eq!(disassemble(&[0x3b], 1), "STB (R11)");
    }

    #[test]
    fn disassemble_stw() {
        assert_eq!(disassemble(&[0x30], 0), "STW (R0)");
        assert_eq!(disassemble(&[0x31], 0), "STW (R1)");
        assert_eq!(disassemble(&[0x32], 0), "STW (R2)");
        assert_eq!(disassemble(&[0x33], 0), "STW (R3)");
        assert_eq!(disassemble(&[0x34], 0), "STW (R4)");
        assert_eq!(disassemble(&[0x35], 0), "STW (R5)");
        assert_eq!(disassemble(&[0x36], 0), "STW (R6)");
        assert_eq!(disassemble(&[0x37], 0), "STW (R7)");
        assert_eq!(disassemble(&[0x38], 0), "STW (R8)");
        assert_eq!(disassemble(&[0x39], 0), "STW (R9)");
        assert_eq!(disassemble(&[0x3a], 0), "STW (R10)");
        assert_eq!(disassemble(&[0x3b], 0), "STW (R11)");
    }

    #[test]
    fn disassemble_sub_immediate() {
        assert_eq!(disassemble(&[0x60], 2), "SUB #0");
        assert_eq!(disassemble(&[0x61], 2), "SUB #1");
        assert_eq!(disassemble(&[0x62], 2), "SUB #2");
        assert_eq!(disassemble(&[0x63], 2), "SUB #3");
        assert_eq!(disassemble(&[0x64], 2), "SUB #4");
        assert_eq!(disassemble(&[0x65], 2), "SUB #5");
        assert_eq!(disassemble(&[0x66], 2), "SUB #6");
        assert_eq!(disassemble(&[0x67], 2), "SUB #7");
        assert_eq!(disassemble(&[0x68], 2), "SUB #8");
        assert_eq!(disassemble(&[0x69], 2), "SUB #9");
        assert_eq!(disassemble(&[0x6a], 2), "SUB #10");
        assert_eq!(disassemble(&[0x6b], 2), "SUB #11");
        assert_eq!(disassemble(&[0x6c], 2), "SUB #12");
        assert_eq!(disassemble(&[0x6d], 2), "SUB #13");
        assert_eq!(disassemble(&[0x6e], 2), "SUB #14");
        assert_eq!(disassemble(&[0x6f], 2), "SUB #15");
    }

    #[test]
    fn disassemble_sub_register() {
        assert_eq!(disassemble(&[0x60], 0), "SUB R0");
        assert_eq!(disassemble(&[0x61], 0), "SUB R1");
        assert_eq!(disassemble(&[0x62], 0), "SUB R2");
        assert_eq!(disassemble(&[0x63], 0), "SUB R3");
        assert_eq!(disassemble(&[0x64], 0), "SUB R4");
        assert_eq!(disassemble(&[0x65], 0), "SUB R5");
        assert_eq!(disassemble(&[0x66], 0), "SUB R6");
        assert_eq!(disassemble(&[0x67], 0), "SUB R7");
        assert_eq!(disassemble(&[0x68], 0), "SUB R8");
        assert_eq!(disassemble(&[0x69], 0), "SUB R9");
        assert_eq!(disassemble(&[0x6a], 0), "SUB R10");
        assert_eq!(disassemble(&[0x6b], 0), "SUB R11");
        assert_eq!(disassemble(&[0x6c], 0), "SUB R12");
        assert_eq!(disassemble(&[0x6d], 0), "SUB R13");
        assert_eq!(disassemble(&[0x6e], 0), "SUB R14");
        assert_eq!(disassemble(&[0x6f], 0), "SUB R15");
    }

    #[test]
    fn disassemble_to() {
        assert_eq!(disassemble(&[0x10], 0), "TO R0");
        assert_eq!(disassemble(&[0x11], 0), "TO R1");
        assert_eq!(disassemble(&[0x12], 0), "TO R2");
        assert_eq!(disassemble(&[0x13], 0), "TO R3");
        assert_eq!(disassemble(&[0x14], 0), "TO R4");
        assert_eq!(disassemble(&[0x15], 0), "TO R5");
        assert_eq!(disassemble(&[0x16], 0), "TO R6");
        assert_eq!(disassemble(&[0x17], 0), "TO R7");
        assert_eq!(disassemble(&[0x18], 0), "TO R8");
        assert_eq!(disassemble(&[0x19], 0), "TO R9");
        assert_eq!(disassemble(&[0x1a], 0), "TO R10");
        assert_eq!(disassemble(&[0x1b], 0), "TO R11");
        assert_eq!(disassemble(&[0x1c], 0), "TO R12");
        assert_eq!(disassemble(&[0x1d], 0), "TO R13");
        assert_eq!(disassemble(&[0x1e], 0), "TO R14");
        assert_eq!(disassemble(&[0x1f], 0), "TO R15");
    }

    #[test]
    fn disassemble_umult_immediate() {
        assert_eq!(disassemble(&[0x80], 3), "UMULT #0");
        assert_eq!(disassemble(&[0x81], 3), "UMULT #1");
        assert_eq!(disassemble(&[0x82], 3), "UMULT #2");
        assert_eq!(disassemble(&[0x83], 3), "UMULT #3");
        assert_eq!(disassemble(&[0x84], 3), "UMULT #4");
        assert_eq!(disassemble(&[0x85], 3), "UMULT #5");
        assert_eq!(disassemble(&[0x86], 3), "UMULT #6");
        assert_eq!(disassemble(&[0x87], 3), "UMULT #7");
        assert_eq!(disassemble(&[0x88], 3), "UMULT #8");
        assert_eq!(disassemble(&[0x89], 3), "UMULT #9");
        assert_eq!(disassemble(&[0x8a], 3), "UMULT #10");
        assert_eq!(disassemble(&[0x8b], 3), "UMULT #11");
        assert_eq!(disassemble(&[0x8c], 3), "UMULT #12");
        assert_eq!(disassemble(&[0x8d], 3), "UMULT #13");
        assert_eq!(disassemble(&[0x8e], 3), "UMULT #14");
        assert_eq!(disassemble(&[0x8f], 3), "UMULT #15");
    }

    #[test]
    fn disassemble_umult_register() {
        assert_eq!(disassemble(&[0x80], 1), "UMULT R0");
        assert_eq!(disassemble(&[0x81], 1), "UMULT R1");
        assert_eq!(disassemble(&[0x82], 1), "UMULT R2");
        assert_eq!(disassemble(&[0x83], 1), "UMULT R3");
        assert_eq!(disassemble(&[0x84], 1), "UMULT R4");
        assert_eq!(disassemble(&[0x85], 1), "UMULT R5");
        assert_eq!(disassemble(&[0x86], 1), "UMULT R6");
        assert_eq!(disassemble(&[0x87], 1), "UMULT R7");
        assert_eq!(disassemble(&[0x88], 1), "UMULT R8");
        assert_eq!(disassemble(&[0x89], 1), "UMULT R9");
        assert_eq!(disassemble(&[0x8a], 1), "UMULT R10");
        assert_eq!(disassemble(&[0x8b], 1), "UMULT R11");
        assert_eq!(disassemble(&[0x8c], 1), "UMULT R12");
        assert_eq!(disassemble(&[0x8d], 1), "UMULT R13");
        assert_eq!(disassemble(&[0x8e], 1), "UMULT R14");
        assert_eq!(disassemble(&[0x8f], 1), "UMULT R15");
    }

    #[test]
    fn disassemble_with() {
        assert_eq!(disassemble(&[0x20], 0), "WITH R0");
        assert_eq!(disassemble(&[0x21], 0), "WITH R1");
        assert_eq!(disassemble(&[0x22], 0), "WITH R2");
        assert_eq!(disassemble(&[0x23], 0), "WITH R3");
        assert_eq!(disassemble(&[0x24], 0), "WITH R4");
        assert_eq!(disassemble(&[0x25], 0), "WITH R5");
        assert_eq!(disassemble(&[0x26], 0), "WITH R6");
        assert_eq!(disassemble(&[0x27], 0), "WITH R7");
        assert_eq!(disassemble(&[0x28], 0), "WITH R8");
        assert_eq!(disassemble(&[0x29], 0), "WITH R9");
        assert_eq!(disassemble(&[0x2a], 0), "WITH R10");
        assert_eq!(disassemble(&[0x2b], 0), "WITH R11");
        assert_eq!(disassemble(&[0x2c], 0), "WITH R12");
        assert_eq!(disassemble(&[0x2d], 0), "WITH R13");
        assert_eq!(disassemble(&[0x2e], 0), "WITH R14");
        assert_eq!(disassemble(&[0x2f], 0), "WITH R15");
    }

    #[test]
    fn disassemble_xor_immediate() {
        assert_eq!(disassemble(&[0xc1], 3), "XOR #1");
        assert_eq!(disassemble(&[0xc2], 3), "XOR #2");
        assert_eq!(disassemble(&[0xc3], 3), "XOR #3");
        assert_eq!(disassemble(&[0xc4], 3), "XOR #4");
        assert_eq!(disassemble(&[0xc5], 3), "XOR #5");
        assert_eq!(disassemble(&[0xc6], 3), "XOR #6");
        assert_eq!(disassemble(&[0xc7], 3), "XOR #7");
        assert_eq!(disassemble(&[0xc8], 3), "XOR #8");
        assert_eq!(disassemble(&[0xc9], 3), "XOR #9");
        assert_eq!(disassemble(&[0xca], 3), "XOR #10");
        assert_eq!(disassemble(&[0xcb], 3), "XOR #11");
        assert_eq!(disassemble(&[0xcc], 3), "XOR #12");
        assert_eq!(disassemble(&[0xcd], 3), "XOR #13");
        assert_eq!(disassemble(&[0xce], 3), "XOR #14");
        assert_eq!(disassemble(&[0xcf], 3), "XOR #15");
    }

    #[test]
    fn disassemble_xor_register() {
        assert_eq!(disassemble(&[0xc1], 1), "XOR R1");
        assert_eq!(disassemble(&[0xc2], 1), "XOR R2");
        assert_eq!(disassemble(&[0xc3], 1), "XOR R3");
        assert_eq!(disassemble(&[0xc4], 1), "XOR R4");
        assert_eq!(disassemble(&[0xc5], 1), "XOR R5");
        assert_eq!(disassemble(&[0xc6], 1), "XOR R6");
        assert_eq!(disassemble(&[0xc7], 1), "XOR R7");
        assert_eq!(disassemble(&[0xc8], 1), "XOR R8");
        assert_eq!(disassemble(&[0xc9], 1), "XOR R9");
        assert_eq!(disassemble(&[0xca], 1), "XOR R10");
        assert_eq!(disassemble(&[0xcb], 1), "XOR R11");
        assert_eq!(disassemble(&[0xcc], 1), "XOR R12");
        assert_eq!(disassemble(&[0xcd], 1), "XOR R13");
        assert_eq!(disassemble(&[0xce], 1), "XOR R14");
        assert_eq!(disassemble(&[0xcf], 1), "XOR R15");
    }
}

//===========================================================================//
