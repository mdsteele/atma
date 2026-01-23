//! Facilities for disassembling SM83 machine code.

use crate::bus::{Addr, SimBus};
use std::fmt;

//===========================================================================//

/// A condition under which a conditional SM83 operation will be executed.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Condition {
    /// Always execute the operation.
    Always,
    /// Execute the operation if the carry flag is set.
    C,
    /// Execute the operation if the carry flag is not set.
    Nc,
    /// Execute the operation if the zero flag is set.
    Z,
    /// Execute the operation if the zero flag is not set.
    Nz,
}

impl Condition {
    fn format(self) -> &'static str {
        match self {
            Condition::Always => "",
            Condition::C => "C",
            Condition::Nc => "NC",
            Condition::Z => "Z",
            Condition::Nz => "NZ",
        }
    }
}

//===========================================================================//

/// An 8-bit register for an SM83 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Reg8 {
    /// The A register.
    A,
    /// The B register.
    B,
    /// The C register.
    C,
    /// The D register.
    D,
    /// The E register.
    E,
    /// The H register.
    H,
    /// The L register.
    L,
    /// The memory location pointed to by the combined BC register.
    Mbc,
    /// The memory location pointed to by the combined DE register.
    Mde,
    /// The memory location pointed to by the combined HL register.
    Mhl,
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Reg8::A => "A",
            Reg8::B => "B",
            Reg8::C => "C",
            Reg8::D => "D",
            Reg8::E => "E",
            Reg8::H => "H",
            Reg8::L => "L",
            Reg8::Mbc => "[BC]",
            Reg8::Mde => "[DE]",
            Reg8::Mhl => "[HL]",
        })
    }
}

//===========================================================================//

/// A 16-bit register for an SM83 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Reg16 {
    /// The combined AF register.
    Af,
    /// The combined BC register.
    Bc,
    /// The combined DE register.
    De,
    /// The combined HL register.
    Hl,
    /// The stack pointer.
    Sp,
}

impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Reg16::Af => "AF",
            Reg16::Bc => "BC",
            Reg16::De => "DE",
            Reg16::Hl => "HL",
            Reg16::Sp => "SP",
        })
    }
}

//===========================================================================//

/// An operation that can be executed by an SM83 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operation {
    /// Add an 8-bit immediate value to the A register with carry.
    AdcAI8,
    /// Add an 8-bit register to the A register with carry.
    AdcAR8(Reg8),
    /// Add a 16-bit register to the HL register.
    AddHlR16(Reg16),
    /// Add an 8-bit immediate value to the A register.
    AddAI8,
    /// Add an 8-bit register to the A register.
    AddAR8(Reg8),
    /// Add a signed 8-bit immediate value to the stack pointer.
    AddSpI8,
    /// Bitwise-AND an 8-bit immediate value into the A register.
    AndAI8,
    /// Bitwise-AND an 8-bit register into the A register.
    AndAR8(Reg8),
    /// Call subroutine at an immediate 16-bit address.
    CallM16(Condition),
    /// Complement the carry flag.
    Ccf,
    /// Compare the A register to an 8-bit immediate value.
    CpAI8,
    /// Compare the A register to an 8-bit register.
    CpAR8(Reg8),
    /// Complement the A register.
    Cpl,
    /// Decimal adjust accumulator.
    Daa,
    /// Decrement a 16-bit register.
    DecR16(Reg16),
    /// Decrement an 8-bit register.
    DecR8(Reg8),
    /// Disable interrupts.
    Di,
    /// Enable interrupts.
    Ei,
    /// Pause execution until the next interrupt.
    Halt,
    /// Increment a 16-bit register.
    IncR16(Reg16),
    /// Increment an 8-bit register.
    IncR8(Reg8),
    /// Invalid instruction.
    Invalid,
    /// Jump to the 16-bit immediate address.
    JpI16(Condition),
    /// Jump to the address held in the HL register.
    JpHl,
    /// Jump relative by a signed 8-bit immediate offset.
    JrI8(Condition),
    /// Load from memory at a 16-bit immediate address into the A register.
    LdAM16,
    /// Load from memory at the location pointed to by the HL register into the
    /// A register, then decrement the HL register.
    LdAMhld,
    /// Load from memory at the location pointed to by the HL register into the
    /// A register, then increment the HL register.
    LdAMhli,
    /// Load the stack pointer plus a signed 8-bit immediate offset into the HL
    /// register.
    LdHlSpI8,
    /// Load the A register into memory at a 16-bit immediate address.
    LdM16A,
    /// Load the stack pointer into memory at a 16-bit immediate address.
    LdM16Sp,
    /// Load the A register into the memory location pointed to by the HL
    /// register, then decrement the HL register.
    LdMhldA,
    /// Load the A register into the memory location pointed to by the HL
    /// register, then increment the HL register.
    LdMhliA,
    /// Load a 16-bit immediate value into a 16-bit register.
    LdR16I16(Reg16),
    /// Load an 8-bit immediate value into an 8-bit register.
    LdR8I8(Reg8),
    /// Load one 8-bit register into another.
    LdR8R8(Reg8, Reg8),
    /// Load the HL register into the stack pointer.
    LdSpHl,
    /// Load from the high page at an 8-bit immediate address into the A
    /// register.
    LdhAM8,
    /// Load from the high page at the location pointed to by the C register
    /// into the A register.
    LdhAMc,
    /// Load the A register into the high page at an 8-bit immediate address.
    LdhM8A,
    /// Load the A register into the high page at the location pointed to by
    /// the C register.
    LdhMcA,
    /// No-op.
    Nop,
    /// Bitwise-OR an 8-bit immediate value into the A register.
    OrAI8,
    /// Bitwise-OR an 8-bit register into the A register.
    OrAR8(Reg8),
    /// Pop a 16-bit register from the stack.
    Pop(Reg16),
    /// Prefix for a two-byte instruction.
    Prefix,
    /// Push a 16-bit register onto the stack.
    Push(Reg16),
    /// Return from subroutine.
    Ret(Condition),
    /// Return from interrupt.
    Reti,
    /// Rotate the A register left, through the carry flag (9-bit rotate).
    Rla,
    /// Rotate the A register left (8-bit rotate).
    Rlca,
    /// Rotate the A register right, through the carry flag (9-bit rotate).
    Rra,
    /// Rotate the A register right (8-bit rotate).
    Rrca,
    /// Call subroutine at the specified zero page address.
    Rst(u8),
    /// Subtract an 8-bit immediate value from the A register with carry.
    SbcAI8,
    /// Subtract an 8-bit register from the A register with carry.
    SbcAR8(Reg8),
    /// Set the carry flag.
    Scf,
    /// Stop execution and put the processor into very-low-power mode.
    Stop,
    /// Subtract an 8-bit immediate value from the A register.
    SubAI8,
    /// Subtract an 8-bit register from the A register.
    SubAR8(Reg8),
    /// Bitwise-XOR an 8-bit immediate value into the A register.
    XorAI8,
    /// Bitwise-XOR an 8-bit register into the A register.
    XorAR8(Reg8),
}

impl Operation {
    /// Decodes an SM83 opcode.
    pub fn from_opcode(opcode: u8) -> Operation {
        match opcode {
            0x00 => Operation::Nop,
            0x08 => Operation::LdM16Sp,
            0x10 => Operation::Stop,
            0x18 => Operation::JrI8(Condition::Always),
            0x20 => Operation::JrI8(Condition::Nz),
            0x28 => Operation::JrI8(Condition::Z),
            0x30 => Operation::JrI8(Condition::Nc),
            0x38 => Operation::JrI8(Condition::C),

            0x01 => Operation::LdR16I16(Reg16::Bc),
            0x09 => Operation::AddHlR16(Reg16::Bc),
            0x11 => Operation::LdR16I16(Reg16::De),
            0x19 => Operation::AddHlR16(Reg16::De),
            0x21 => Operation::LdR16I16(Reg16::Hl),
            0x29 => Operation::AddHlR16(Reg16::Hl),
            0x31 => Operation::LdR16I16(Reg16::Sp),
            0x39 => Operation::AddHlR16(Reg16::Sp),

            0x02 => Operation::LdR8R8(Reg8::Mbc, Reg8::A),
            0x0a => Operation::LdR8R8(Reg8::A, Reg8::Mbc),
            0x12 => Operation::LdR8R8(Reg8::Mde, Reg8::A),
            0x1a => Operation::LdR8R8(Reg8::A, Reg8::Mde),
            0x22 => Operation::LdMhliA,
            0x2a => Operation::LdAMhli,
            0x32 => Operation::LdMhldA,
            0x3a => Operation::LdAMhld,

            0x03 => Operation::IncR16(Reg16::Bc),
            0x0b => Operation::DecR16(Reg16::Bc),
            0x13 => Operation::IncR16(Reg16::De),
            0x1b => Operation::DecR16(Reg16::De),
            0x23 => Operation::IncR16(Reg16::Hl),
            0x2b => Operation::DecR16(Reg16::Hl),
            0x33 => Operation::IncR16(Reg16::Sp),
            0x3b => Operation::DecR16(Reg16::Sp),

            0x04 => Operation::IncR8(Reg8::B),
            0x0c => Operation::IncR8(Reg8::C),
            0x14 => Operation::IncR8(Reg8::D),
            0x1c => Operation::IncR8(Reg8::E),
            0x24 => Operation::IncR8(Reg8::H),
            0x2c => Operation::IncR8(Reg8::L),
            0x34 => Operation::IncR8(Reg8::Mhl),
            0x3c => Operation::IncR8(Reg8::A),

            0x05 => Operation::DecR8(Reg8::B),
            0x0d => Operation::DecR8(Reg8::C),
            0x15 => Operation::DecR8(Reg8::D),
            0x1d => Operation::DecR8(Reg8::E),
            0x25 => Operation::DecR8(Reg8::H),
            0x2d => Operation::DecR8(Reg8::L),
            0x35 => Operation::DecR8(Reg8::Mhl),
            0x3d => Operation::DecR8(Reg8::A),

            0x06 => Operation::LdR8I8(Reg8::B),
            0x0e => Operation::LdR8I8(Reg8::C),
            0x16 => Operation::LdR8I8(Reg8::D),
            0x1e => Operation::LdR8I8(Reg8::E),
            0x26 => Operation::LdR8I8(Reg8::H),
            0x2e => Operation::LdR8I8(Reg8::L),
            0x36 => Operation::LdR8I8(Reg8::Mhl),
            0x3e => Operation::LdR8I8(Reg8::A),

            0x07 => Operation::Rlca,
            0x0f => Operation::Rrca,
            0x17 => Operation::Rla,
            0x1f => Operation::Rra,
            0x27 => Operation::Daa,
            0x2f => Operation::Cpl,
            0x37 => Operation::Scf,
            0x3f => Operation::Ccf,

            0x40 => Operation::LdR8R8(Reg8::B, Reg8::B),
            0x48 => Operation::LdR8R8(Reg8::C, Reg8::B),
            0x50 => Operation::LdR8R8(Reg8::D, Reg8::B),
            0x58 => Operation::LdR8R8(Reg8::E, Reg8::B),
            0x60 => Operation::LdR8R8(Reg8::H, Reg8::B),
            0x68 => Operation::LdR8R8(Reg8::L, Reg8::B),
            0x70 => Operation::LdR8R8(Reg8::Mhl, Reg8::B),
            0x78 => Operation::LdR8R8(Reg8::A, Reg8::B),

            0x41 => Operation::LdR8R8(Reg8::B, Reg8::C),
            0x49 => Operation::LdR8R8(Reg8::C, Reg8::C),
            0x51 => Operation::LdR8R8(Reg8::D, Reg8::C),
            0x59 => Operation::LdR8R8(Reg8::E, Reg8::C),
            0x61 => Operation::LdR8R8(Reg8::H, Reg8::C),
            0x69 => Operation::LdR8R8(Reg8::L, Reg8::C),
            0x71 => Operation::LdR8R8(Reg8::Mhl, Reg8::C),
            0x79 => Operation::LdR8R8(Reg8::A, Reg8::C),

            0x42 => Operation::LdR8R8(Reg8::B, Reg8::D),
            0x4a => Operation::LdR8R8(Reg8::C, Reg8::D),
            0x52 => Operation::LdR8R8(Reg8::D, Reg8::D),
            0x5a => Operation::LdR8R8(Reg8::E, Reg8::D),
            0x62 => Operation::LdR8R8(Reg8::H, Reg8::D),
            0x6a => Operation::LdR8R8(Reg8::L, Reg8::D),
            0x72 => Operation::LdR8R8(Reg8::Mhl, Reg8::D),
            0x7a => Operation::LdR8R8(Reg8::A, Reg8::D),

            0x43 => Operation::LdR8R8(Reg8::B, Reg8::E),
            0x4b => Operation::LdR8R8(Reg8::C, Reg8::E),
            0x53 => Operation::LdR8R8(Reg8::D, Reg8::E),
            0x5b => Operation::LdR8R8(Reg8::E, Reg8::E),
            0x63 => Operation::LdR8R8(Reg8::H, Reg8::E),
            0x6b => Operation::LdR8R8(Reg8::L, Reg8::E),
            0x73 => Operation::LdR8R8(Reg8::Mhl, Reg8::E),
            0x7b => Operation::LdR8R8(Reg8::A, Reg8::E),

            0x44 => Operation::LdR8R8(Reg8::B, Reg8::H),
            0x4c => Operation::LdR8R8(Reg8::C, Reg8::H),
            0x54 => Operation::LdR8R8(Reg8::D, Reg8::H),
            0x5c => Operation::LdR8R8(Reg8::E, Reg8::H),
            0x64 => Operation::LdR8R8(Reg8::H, Reg8::H),
            0x6c => Operation::LdR8R8(Reg8::L, Reg8::H),
            0x74 => Operation::LdR8R8(Reg8::Mhl, Reg8::H),
            0x7c => Operation::LdR8R8(Reg8::A, Reg8::H),

            0x45 => Operation::LdR8R8(Reg8::B, Reg8::L),
            0x4d => Operation::LdR8R8(Reg8::C, Reg8::L),
            0x55 => Operation::LdR8R8(Reg8::D, Reg8::L),
            0x5d => Operation::LdR8R8(Reg8::E, Reg8::L),
            0x65 => Operation::LdR8R8(Reg8::H, Reg8::L),
            0x6d => Operation::LdR8R8(Reg8::L, Reg8::L),
            0x75 => Operation::LdR8R8(Reg8::Mhl, Reg8::L),
            0x7d => Operation::LdR8R8(Reg8::A, Reg8::L),

            0x46 => Operation::LdR8R8(Reg8::B, Reg8::Mhl),
            0x4e => Operation::LdR8R8(Reg8::C, Reg8::Mhl),
            0x56 => Operation::LdR8R8(Reg8::D, Reg8::Mhl),
            0x5e => Operation::LdR8R8(Reg8::E, Reg8::Mhl),
            0x66 => Operation::LdR8R8(Reg8::H, Reg8::Mhl),
            0x6e => Operation::LdR8R8(Reg8::L, Reg8::Mhl),
            0x76 => Operation::Halt,
            0x7e => Operation::LdR8R8(Reg8::A, Reg8::Mhl),

            0x47 => Operation::LdR8R8(Reg8::B, Reg8::A),
            0x4f => Operation::LdR8R8(Reg8::C, Reg8::A),
            0x57 => Operation::LdR8R8(Reg8::D, Reg8::A),
            0x5f => Operation::LdR8R8(Reg8::E, Reg8::A),
            0x67 => Operation::LdR8R8(Reg8::H, Reg8::A),
            0x6f => Operation::LdR8R8(Reg8::L, Reg8::A),
            0x77 => Operation::LdR8R8(Reg8::Mhl, Reg8::A),
            0x7f => Operation::LdR8R8(Reg8::A, Reg8::A),

            0x80 => Operation::AddAR8(Reg8::B),
            0x88 => Operation::AdcAR8(Reg8::B),
            0x90 => Operation::SubAR8(Reg8::B),
            0x98 => Operation::SbcAR8(Reg8::B),
            0xa0 => Operation::AndAR8(Reg8::B),
            0xa8 => Operation::XorAR8(Reg8::B),
            0xb0 => Operation::OrAR8(Reg8::B),
            0xb8 => Operation::CpAR8(Reg8::B),

            0x81 => Operation::AddAR8(Reg8::C),
            0x89 => Operation::AdcAR8(Reg8::C),
            0x91 => Operation::SubAR8(Reg8::C),
            0x99 => Operation::SbcAR8(Reg8::C),
            0xa1 => Operation::AndAR8(Reg8::C),
            0xa9 => Operation::XorAR8(Reg8::C),
            0xb1 => Operation::OrAR8(Reg8::C),
            0xb9 => Operation::CpAR8(Reg8::C),

            0x82 => Operation::AddAR8(Reg8::D),
            0x8a => Operation::AdcAR8(Reg8::D),
            0x92 => Operation::SubAR8(Reg8::D),
            0x9a => Operation::SbcAR8(Reg8::D),
            0xa2 => Operation::AndAR8(Reg8::D),
            0xaa => Operation::XorAR8(Reg8::D),
            0xb2 => Operation::OrAR8(Reg8::D),
            0xba => Operation::CpAR8(Reg8::D),

            0x83 => Operation::AddAR8(Reg8::E),
            0x8b => Operation::AdcAR8(Reg8::E),
            0x93 => Operation::SubAR8(Reg8::E),
            0x9b => Operation::SbcAR8(Reg8::E),
            0xa3 => Operation::AndAR8(Reg8::E),
            0xab => Operation::XorAR8(Reg8::E),
            0xb3 => Operation::OrAR8(Reg8::E),
            0xbb => Operation::CpAR8(Reg8::E),

            0x84 => Operation::AddAR8(Reg8::H),
            0x8c => Operation::AdcAR8(Reg8::H),
            0x94 => Operation::SubAR8(Reg8::H),
            0x9c => Operation::SbcAR8(Reg8::H),
            0xa4 => Operation::AndAR8(Reg8::H),
            0xac => Operation::XorAR8(Reg8::H),
            0xb4 => Operation::OrAR8(Reg8::H),
            0xbc => Operation::CpAR8(Reg8::H),

            0x85 => Operation::AddAR8(Reg8::L),
            0x8d => Operation::AdcAR8(Reg8::L),
            0x95 => Operation::SubAR8(Reg8::L),
            0x9d => Operation::SbcAR8(Reg8::L),
            0xa5 => Operation::AndAR8(Reg8::L),
            0xad => Operation::XorAR8(Reg8::L),
            0xb5 => Operation::OrAR8(Reg8::L),
            0xbd => Operation::CpAR8(Reg8::L),

            0x86 => Operation::AddAR8(Reg8::Mhl),
            0x8e => Operation::AdcAR8(Reg8::Mhl),
            0x96 => Operation::SubAR8(Reg8::Mhl),
            0x9e => Operation::SbcAR8(Reg8::Mhl),
            0xa6 => Operation::AndAR8(Reg8::Mhl),
            0xae => Operation::XorAR8(Reg8::Mhl),
            0xb6 => Operation::OrAR8(Reg8::Mhl),
            0xbe => Operation::CpAR8(Reg8::Mhl),

            0x87 => Operation::AddAR8(Reg8::A),
            0x8f => Operation::AdcAR8(Reg8::A),
            0x97 => Operation::SubAR8(Reg8::A),
            0x9f => Operation::SbcAR8(Reg8::A),
            0xa7 => Operation::AndAR8(Reg8::A),
            0xaf => Operation::XorAR8(Reg8::A),
            0xb7 => Operation::OrAR8(Reg8::A),
            0xbf => Operation::CpAR8(Reg8::A),

            0xc0 => Operation::Ret(Condition::Nz),
            0xc8 => Operation::Ret(Condition::Z),
            0xd0 => Operation::Ret(Condition::Nc),
            0xd8 => Operation::Ret(Condition::C),
            0xe0 => Operation::LdhM8A,
            0xe8 => Operation::AddSpI8,
            0xf0 => Operation::LdhAM8,
            0xf8 => Operation::LdHlSpI8,

            0xc1 => Operation::Pop(Reg16::Bc),
            0xc9 => Operation::Ret(Condition::Always),
            0xd1 => Operation::Pop(Reg16::De),
            0xd9 => Operation::Reti,
            0xe1 => Operation::Pop(Reg16::Hl),
            0xe9 => Operation::JpHl,
            0xf1 => Operation::Pop(Reg16::Af),
            0xf9 => Operation::LdSpHl,

            0xc2 => Operation::JpI16(Condition::Nz),
            0xca => Operation::JpI16(Condition::Z),
            0xd2 => Operation::JpI16(Condition::Nc),
            0xda => Operation::JpI16(Condition::C),
            0xe2 => Operation::LdhMcA,
            0xea => Operation::LdM16A,
            0xf2 => Operation::LdhAMc,
            0xfa => Operation::LdAM16,

            0xc3 => Operation::JpI16(Condition::Always),
            0xcb => Operation::Prefix,
            0xd3 => Operation::Invalid,
            0xdb => Operation::Invalid,
            0xe3 => Operation::Invalid,
            0xeb => Operation::Invalid,
            0xf3 => Operation::Di,
            0xfb => Operation::Ei,

            0xc4 => Operation::CallM16(Condition::Nz),
            0xcc => Operation::CallM16(Condition::Z),
            0xd4 => Operation::CallM16(Condition::Nc),
            0xdc => Operation::CallM16(Condition::C),
            0xe4 => Operation::Invalid,
            0xec => Operation::Invalid,
            0xf4 => Operation::Invalid,
            0xfc => Operation::Invalid,

            0xc5 => Operation::Push(Reg16::Bc),
            0xcd => Operation::CallM16(Condition::Always),
            0xd5 => Operation::Push(Reg16::De),
            0xdd => Operation::Invalid,
            0xe5 => Operation::Push(Reg16::Hl),
            0xed => Operation::Invalid,
            0xf5 => Operation::Push(Reg16::Af),
            0xfd => Operation::Invalid,

            0xc6 => Operation::AddAI8,
            0xce => Operation::AdcAI8,
            0xd6 => Operation::SubAI8,
            0xde => Operation::SbcAI8,
            0xe6 => Operation::AndAI8,
            0xee => Operation::XorAI8,
            0xf6 => Operation::OrAI8,
            0xfe => Operation::CpAI8,

            0xc7 => Operation::Rst(0x00),
            0xcf => Operation::Rst(0x08),
            0xd7 => Operation::Rst(0x10),
            0xdf => Operation::Rst(0x18),
            0xe7 => Operation::Rst(0x20),
            0xef => Operation::Rst(0x28),
            0xf7 => Operation::Rst(0x30),
            0xff => Operation::Rst(0x38),
        }
    }
}

//===========================================================================//

/// An argument value for an SM83 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    /// No operand.
    None,
    /// A signed 8-bit operand.
    I8(i8),
    /// An unsigned 8-bit operand.
    U8(u8),
    /// An unsigned 16-bit operand.
    U16(u16),
}

impl Operand {
    /// The size of this operand, in bytes.
    pub fn size(self) -> u32 {
        match self {
            Operand::None => 0,
            Operand::I8(_) | Operand::U8(_) => 1,
            Operand::U16(_) => 2,
        }
    }

    fn as_i32(self) -> i32 {
        match self {
            Operand::None => 0,
            Operand::I8(value) => i32::from(value),
            Operand::U8(value) => i32::from(value),
            Operand::U16(value) => i32::from(value),
        }
    }
}

//===========================================================================//

/// A prefixed operation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Prefixed {
    /// Rotate the register left, through the carry flag (9-bit rotate).
    Rl,
    /// Rotate the register left (8-bit rotate).
    Rlc,
    /// Rotate the register right, through the carry flag (9-bit rotate).
    Rr,
    /// Rotate the register right (8-bit rotate).
    Rrc,
    /// Shift the register left arithmetically.
    Sla,
    /// Shift the register right arithmetically.
    Sra,
    /// Shift the register right logically.
    Srl,
    /// Swap the hi and lo nibbles of the register.
    Swap,
    /// Test one bit of the register.
    Bit(u8),
    /// Reset one bit of the register.
    Res(u8),
    /// Set one bit of the register.
    Set(u8),
}

impl Prefixed {
    /// Decodes the second byte of an SM83 prefixed instruction.
    pub fn decode(byte: u8) -> (Prefixed, Reg8) {
        let (hi, mid, lo) = (byte >> 6, (byte >> 3) & 0x7, byte & 0x7);
        let prefixed = match hi {
            0 => match mid {
                0 => Prefixed::Rlc,
                1 => Prefixed::Rrc,
                2 => Prefixed::Rl,
                3 => Prefixed::Rr,
                4 => Prefixed::Sla,
                5 => Prefixed::Sra,
                6 => Prefixed::Swap,
                7 => Prefixed::Srl,
                _ => unreachable!(),
            },
            1 => Prefixed::Bit(mid),
            2 => Prefixed::Res(mid),
            3 => Prefixed::Set(mid),
            _ => unreachable!(),
        };
        let reg = match lo {
            0 => Reg8::B,
            1 => Reg8::C,
            2 => Reg8::D,
            3 => Reg8::E,
            4 => Reg8::H,
            5 => Reg8::L,
            6 => Reg8::Mhl,
            7 => Reg8::A,
            _ => unreachable!(),
        };
        (prefixed, reg)
    }

    /// Formats a prefixed SM83 instruction as a human-readable string.
    pub fn format(self, reg: Reg8) -> String {
        match self {
            Prefixed::Rl => format!("RL {reg}"),
            Prefixed::Rlc => format!("RLC {reg}"),
            Prefixed::Rr => format!("RR {reg}"),
            Prefixed::Rrc => format!("RRC {reg}"),
            Prefixed::Sla => format!("SLA {reg}"),
            Prefixed::Sra => format!("SRA {reg}"),
            Prefixed::Srl => format!("SRL {reg}"),
            Prefixed::Swap => format!("SWAP {reg}"),
            Prefixed::Bit(bit) => format!("BIT {bit}, {reg}"),
            Prefixed::Res(bit) => format!("RES {bit}, {reg}"),
            Prefixed::Set(bit) => format!("SET {bit}, {reg}"),
        }
    }
}

//===========================================================================//

/// A complete instruction, including parameter values, for an SM83 processor.
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

    /// Reads and decodes a single SM83 instruction.
    pub fn decode(bus: &dyn SimBus, pc: u16) -> Instruction {
        let opcode = bus.peek_byte(Addr::from(pc));
        let operation = Operation::from_opcode(opcode);
        let operand = match operation {
            Operation::AdcAR8(_)
            | Operation::AddHlR16(_)
            | Operation::AddAR8(_)
            | Operation::AndAR8(_)
            | Operation::Ccf
            | Operation::CpAR8(_)
            | Operation::Cpl
            | Operation::Daa
            | Operation::DecR16(_)
            | Operation::DecR8(_)
            | Operation::Di
            | Operation::Ei
            | Operation::Halt
            | Operation::IncR16(_)
            | Operation::IncR8(_)
            | Operation::Invalid
            | Operation::JpHl
            | Operation::LdAMhld
            | Operation::LdAMhli
            | Operation::LdMhldA
            | Operation::LdMhliA
            | Operation::LdR8R8(_, _)
            | Operation::LdSpHl
            | Operation::LdhAMc
            | Operation::LdhMcA
            | Operation::Nop
            | Operation::OrAR8(_)
            | Operation::Pop(_)
            | Operation::Push(_)
            | Operation::Ret(_)
            | Operation::Reti
            | Operation::Rla
            | Operation::Rlca
            | Operation::Rra
            | Operation::Rrca
            | Operation::Rst(_)
            | Operation::SbcAR8(_)
            | Operation::Scf
            | Operation::Stop
            | Operation::SubAR8(_)
            | Operation::XorAR8(_) => Operand::None,
            Operation::AddSpI8 | Operation::JrI8(_) | Operation::LdHlSpI8 => {
                Operand::I8(bus.peek_byte(Addr::from(pc.wrapping_add(1))) as i8)
            }
            Operation::AdcAI8
            | Operation::AddAI8
            | Operation::AndAI8
            | Operation::CpAI8
            | Operation::LdhAM8
            | Operation::LdhM8A
            | Operation::LdR8I8(_)
            | Operation::OrAI8
            | Operation::Prefix
            | Operation::SbcAI8
            | Operation::SubAI8
            | Operation::XorAI8 => {
                Operand::U8(bus.peek_byte(Addr::from(pc.wrapping_add(1))))
            }
            Operation::CallM16(_)
            | Operation::JpI16(_)
            | Operation::LdAM16
            | Operation::LdM16A
            | Operation::LdM16Sp
            | Operation::LdR16I16(_) => {
                let lo = bus.peek_byte(Addr::from(pc.wrapping_add(1)));
                let hi = bus.peek_byte(Addr::from(pc.wrapping_add(2)));
                Operand::U16((u16::from(hi) << 8) | u16::from(lo))
            }
        };
        Instruction { operation, operand }
    }

    /// Formats a disassembled SM83 instruction as a human-readable string.
    /// `addr` specifies the address of the start of the instruction.  `bus` is
    /// required for providing labels for addresses; if no labels are needed, a
    /// `new_open_bus` can be used.
    pub fn format(self, addr: u16, bus: &dyn SimBus) -> String {
        let operand = self.operand;
        match self.operation {
            Operation::AdcAI8 => format!("ADC A, ${:02x}", operand.as_i32()),
            Operation::AdcAR8(reg) => format!("ADC A, {reg}"),
            Operation::AddHlR16(reg) => format!("ADD HL, {reg}"),
            Operation::AddAI8 => format!("ADD A, ${:02x}", operand.as_i32()),
            Operation::AddAR8(reg) => format!("ADD A, {reg}"),
            Operation::AddSpI8 => format!("ADD SP, {}", operand.as_i32()),
            Operation::AndAI8 => format!("AND A, ${:02x}", operand.as_i32()),
            Operation::AndAR8(reg) => format!("AND A, {reg}"),
            Operation::CallM16(Condition::Always) => {
                format!("CALL {}", format_absolute(bus, operand))
            }
            Operation::CallM16(cond) => {
                format!(
                    "CALL {}, {}",
                    cond.format(),
                    format_absolute(bus, operand)
                )
            }
            Operation::Ccf => "CCF".to_string(),
            Operation::CpAI8 => format!("CP A, ${:02x}", operand.as_i32()),
            Operation::CpAR8(reg) => format!("CP A, {reg}"),
            Operation::Cpl => "CPL".to_string(),
            Operation::Daa => "DAA".to_string(),
            Operation::DecR16(reg) => format!("DEC {reg}"),
            Operation::DecR8(reg) => format!("DEC {reg}"),
            Operation::Di => "DI".to_string(),
            Operation::Ei => "EI".to_string(),
            Operation::Halt => "HALT".to_string(),
            Operation::IncR16(reg) => format!("INC {reg}"),
            Operation::IncR8(reg) => format!("INC {reg}"),
            Operation::Invalid => "invalid".to_string(),
            Operation::JpI16(Condition::Always) => {
                format!("JP {}", format_absolute(bus, operand))
            }
            Operation::JpI16(cond) => {
                format!(
                    "JP {}, {}",
                    cond.format(),
                    format_absolute(bus, operand)
                )
            }
            Operation::JpHl => "JP HL".to_string(),
            Operation::JrI8(Condition::Always) => {
                format!("JR {}", format_relative(bus, addr, operand))
            }
            Operation::JrI8(cond) => {
                format!(
                    "JR {}, {}",
                    cond.format(),
                    format_relative(bus, addr, operand)
                )
            }
            Operation::LdAM16 => {
                format!("LD A, [{}]", format_absolute(bus, operand))
            }
            Operation::LdAMhld => "LD A, [HL-]".to_string(),
            Operation::LdAMhli => "LD A, [HL+]".to_string(),
            Operation::LdHlSpI8 => {
                let offset = operand.as_i32();
                if offset < 0 {
                    format!("LD HL, SP - {}", -offset)
                } else {
                    format!("LD HL, SP + {}", offset)
                }
            }
            Operation::LdM16A => {
                format!("LD [{}], A", format_absolute(bus, operand))
            }
            Operation::LdM16Sp => {
                format!("LD [{}], SP", format_absolute(bus, operand))
            }
            Operation::LdMhldA => "LD [HL-], A".to_string(),
            Operation::LdMhliA => "LD [HL+], A".to_string(),
            Operation::LdR16I16(reg) => {
                format!("LD {}, ${:04x}", reg, operand.as_i32())
            }
            Operation::LdR8I8(reg) => {
                format!("LD {}, ${:02x}", reg, operand.as_i32())
            }
            Operation::LdR8R8(r1, r2) => format!("LD {r1}, {r2}"),
            Operation::LdSpHl => "LD SP, HL".to_string(),
            Operation::LdhAM8 => {
                format!("LDH A, [{}]", format_high_page(bus, operand))
            }
            Operation::LdhAMc => "LDH A, [C]".to_string(),
            Operation::LdhM8A => {
                format!("LDH [{}], A", format_high_page(bus, operand))
            }
            Operation::LdhMcA => "LDH [C], A".to_string(),
            Operation::Nop => "NOP".to_string(),
            Operation::OrAI8 => format!("OR A, ${:02x}", operand.as_i32()),
            Operation::OrAR8(reg) => format!("OR A, {reg}"),
            Operation::Pop(reg) => format!("POP {reg}"),
            Operation::Prefix => {
                let (prefixed, reg) = Prefixed::decode(operand.as_i32() as u8);
                prefixed.format(reg)
            }
            Operation::Push(reg) => format!("PUSH {reg}"),
            Operation::Ret(Condition::Always) => "RET".to_string(),
            Operation::Ret(cond) => format!("RET {}", cond.format()),
            Operation::Reti => "RETI".to_string(),
            Operation::Rla => "RLA".to_string(),
            Operation::Rlca => "RLCA".to_string(),
            Operation::Rra => "RRA".to_string(),
            Operation::Rrca => "RRCA".to_string(),
            Operation::Rst(zp) => match bus.label_at(Addr::from(zp)) {
                None => format!("RST ${zp:02x}"),
                Some(label) => format!("RST {label}"),
            },
            Operation::SbcAI8 => format!("SBC A, ${:02x}", operand.as_i32()),
            Operation::SbcAR8(reg) => format!("SBC A, {reg}"),
            Operation::Scf => "SCF".to_string(),
            Operation::Stop => "STOP".to_string(),
            Operation::SubAI8 => format!("SUB A, ${:02x}", operand.as_i32()),
            Operation::SubAR8(reg) => format!("SUB A, {reg}"),
            Operation::XorAI8 => format!("XOR A, ${:02x}", operand.as_i32()),
            Operation::XorAR8(reg) => format!("XOR A, {reg}"),
        }
    }
}

fn format_absolute(bus: &dyn SimBus, operand: Operand) -> String {
    format_address(bus, operand.as_i32() as u16)
}

fn format_high_page(bus: &dyn SimBus, operand: Operand) -> String {
    format_address(bus, 0xff00 | u16::from(operand.as_i32() as u8))
}

fn format_relative(
    bus: &dyn SimBus,
    instruction_addr: u16,
    operand: Operand,
) -> String {
    let base_addr = instruction_addr.wrapping_add(operand.size() as u16 + 1);
    let destination_addr = base_addr.wrapping_add(operand.as_i32() as u16);
    format_address(bus, destination_addr)
}

fn format_address(bus: &dyn SimBus, addr: u16) -> String {
    match bus.label_at(Addr::from(addr)) {
        None => format!("${addr:04x}"),
        Some(label) => label.to_string(),
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::Instruction;
    use crate::bus::{Addr, LabeledBus, SimBus, new_rom_bus};
    use std::collections::HashMap;

    fn make_test_bus(code: &[u8]) -> Box<dyn SimBus> {
        let mut rom = vec![0u8; 1 << 4];
        rom[..code.len()].copy_from_slice(code);
        new_rom_bus(rom.into_boxed_slice())
    }

    fn disassemble(code: &[u8]) -> String {
        disassemble_with_bus(&*make_test_bus(code))
    }

    fn disassemble_with_label(code: &[u8], addr: u16, label: &str) -> String {
        let labels = HashMap::from([(label.to_string(), Addr::from(addr))]);
        let bus = LabeledBus::new(make_test_bus(code), labels);
        disassemble_with_bus(&bus)
    }

    fn disassemble_with_bus(bus: &dyn SimBus) -> String {
        Instruction::decode(bus, 0).format(0, bus)
    }

    #[test]
    fn disassemble_miscellaneous() {
        assert_eq!(disassemble(&[0x3f]), "CCF");
        assert_eq!(disassemble(&[0x2f]), "CPL");
        assert_eq!(disassemble(&[0x27]), "DAA");
        assert_eq!(disassemble(&[0x76]), "HALT");
        assert_eq!(disassemble(&[0x00]), "NOP");
        assert_eq!(disassemble(&[0x17]), "RLA");
        assert_eq!(disassemble(&[0x07]), "RLCA");
        assert_eq!(disassemble(&[0x1f]), "RRA");
        assert_eq!(disassemble(&[0x0f]), "RRCA");
        assert_eq!(disassemble(&[0x37]), "SCF");
        assert_eq!(disassemble(&[0x10]), "STOP");
    }

    #[test]
    fn disassemble_arithmetic_immediate() {
        assert_eq!(disassemble(&[0xce, 0x12]), "ADC A, $12");
        assert_eq!(disassemble(&[0xc6, 0x34]), "ADD A, $34");
        assert_eq!(disassemble(&[0xe6, 0x56]), "AND A, $56");
        assert_eq!(disassemble(&[0xfe, 0x78]), "CP A, $78");
        assert_eq!(disassemble(&[0xf6, 0x9a]), "OR A, $9a");
        assert_eq!(disassemble(&[0xde, 0xbc]), "SBC A, $bc");
        assert_eq!(disassemble(&[0xd6, 0xde]), "SUB A, $de");
        assert_eq!(disassemble(&[0xee, 0xef]), "XOR A, $ef");
    }

    #[test]
    fn disassemble_arithmetic_register() {
        assert_eq!(disassemble(&[0x8f]), "ADC A, A");
        assert_eq!(disassemble(&[0x80]), "ADD A, B");
        assert_eq!(disassemble(&[0xa1]), "AND A, C");
        assert_eq!(disassemble(&[0xba]), "CP A, D");
        assert_eq!(disassemble(&[0xb3]), "OR A, E");
        assert_eq!(disassemble(&[0x9c]), "SBC A, H");
        assert_eq!(disassemble(&[0x95]), "SUB A, L");
        assert_eq!(disassemble(&[0xae]), "XOR A, [HL]");
    }

    #[test]
    fn disassemble_add_hl() {
        assert_eq!(disassemble(&[0x09]), "ADD HL, BC");
        assert_eq!(disassemble(&[0x19]), "ADD HL, DE");
        assert_eq!(disassemble(&[0x29]), "ADD HL, HL");
        assert_eq!(disassemble(&[0x39]), "ADD HL, SP");
    }

    #[test]
    fn disassemble_call() {
        assert_eq!(disassemble(&[0xcd, 0x22, 0x00]), "CALL $0022");
        assert_eq!(disassemble(&[0xdc, 0x34, 0x12]), "CALL C, $1234");
        assert_eq!(disassemble(&[0xd4, 0x02, 0x00]), "CALL NC, $0002");
        assert_eq!(disassemble(&[0xcc, 0xfe, 0xff]), "CALL Z, $fffe");
        assert_eq!(disassemble(&[0xc4, 0x82, 0xff]), "CALL NZ, $ff82");
        assert_eq!(
            disassemble_with_label(&[0xcd, 0x34, 0x12], 0x1234, "foo"),
            "CALL foo"
        );
        assert_eq!(
            disassemble_with_label(&[0xcc, 0x00, 0x00], 0x0000, "foo"),
            "CALL Z, foo"
        );
    }

    #[test]
    fn disassemble_dec() {
        assert_eq!(disassemble(&[0x3d]), "DEC A");
        assert_eq!(disassemble(&[0x05]), "DEC B");
        assert_eq!(disassemble(&[0x0d]), "DEC C");
        assert_eq!(disassemble(&[0x15]), "DEC D");
        assert_eq!(disassemble(&[0x1d]), "DEC E");
        assert_eq!(disassemble(&[0x25]), "DEC H");
        assert_eq!(disassemble(&[0x2d]), "DEC L");
        assert_eq!(disassemble(&[0x35]), "DEC [HL]");
        assert_eq!(disassemble(&[0x0b]), "DEC BC");
        assert_eq!(disassemble(&[0x1b]), "DEC DE");
        assert_eq!(disassemble(&[0x2b]), "DEC HL");
        assert_eq!(disassemble(&[0x3b]), "DEC SP");
    }

    #[test]
    fn disassemble_inc() {
        assert_eq!(disassemble(&[0x3c]), "INC A");
        assert_eq!(disassemble(&[0x04]), "INC B");
        assert_eq!(disassemble(&[0x0c]), "INC C");
        assert_eq!(disassemble(&[0x14]), "INC D");
        assert_eq!(disassemble(&[0x1c]), "INC E");
        assert_eq!(disassemble(&[0x24]), "INC H");
        assert_eq!(disassemble(&[0x2c]), "INC L");
        assert_eq!(disassemble(&[0x34]), "INC [HL]");
        assert_eq!(disassemble(&[0x03]), "INC BC");
        assert_eq!(disassemble(&[0x13]), "INC DE");
        assert_eq!(disassemble(&[0x23]), "INC HL");
        assert_eq!(disassemble(&[0x33]), "INC SP");
    }

    #[test]
    fn disassemble_invalid() {
        assert_eq!(disassemble(&[0xd3]), "invalid");
        assert_eq!(disassemble(&[0xdb]), "invalid");
        assert_eq!(disassemble(&[0xdd]), "invalid");
        assert_eq!(disassemble(&[0xe3]), "invalid");
        assert_eq!(disassemble(&[0xe4]), "invalid");
        assert_eq!(disassemble(&[0xeb]), "invalid");
        assert_eq!(disassemble(&[0xec]), "invalid");
        assert_eq!(disassemble(&[0xed]), "invalid");
        assert_eq!(disassemble(&[0xf4]), "invalid");
        assert_eq!(disassemble(&[0xfc]), "invalid");
        assert_eq!(disassemble(&[0xfd]), "invalid");
    }

    #[test]
    fn disassemble_jp() {
        assert_eq!(disassemble(&[0xe9]), "JP HL");
        assert_eq!(disassemble(&[0xc3, 0x22, 0x00]), "JP $0022");
        assert_eq!(disassemble(&[0xda, 0x34, 0x12]), "JP C, $1234");
        assert_eq!(disassemble(&[0xd2, 0x02, 0x00]), "JP NC, $0002");
        assert_eq!(disassemble(&[0xca, 0xfe, 0xff]), "JP Z, $fffe");
        assert_eq!(disassemble(&[0xc2, 0x82, 0xff]), "JP NZ, $ff82");
        assert_eq!(
            disassemble_with_label(&[0xc3, 0x34, 0x12], 0x1234, "foo"),
            "JP foo"
        );
        assert_eq!(
            disassemble_with_label(&[0xda, 0x00, 0x00], 0x0000, "foo"),
            "JP C, foo"
        );
    }

    #[test]
    fn disassemble_jr() {
        assert_eq!(disassemble(&[0x18, 0x20]), "JR $0022");
        assert_eq!(disassemble(&[0x38, 0x7f]), "JR C, $0081");
        assert_eq!(disassemble(&[0x30, 0x00]), "JR NC, $0002");
        assert_eq!(disassemble(&[0x28, 0xfc]), "JR Z, $fffe");
        assert_eq!(disassemble(&[0x20, 0x80]), "JR NZ, $ff82");
        assert_eq!(
            disassemble_with_label(&[0x18, 0x4e], 0x0050, "foo"),
            "JR foo"
        );
        assert_eq!(
            disassemble_with_label(&[0x20, 0xfe], 0x0000, "foo"),
            "JR NZ, foo"
        );
    }

    #[test]
    fn disassemble_ld_r8_immediate() {
        assert_eq!(disassemble(&[0x3e, 0x12]), "LD A, $12");
        assert_eq!(disassemble(&[0x06, 0x34]), "LD B, $34");
        assert_eq!(disassemble(&[0x0e, 0x56]), "LD C, $56");
        assert_eq!(disassemble(&[0x16, 0x78]), "LD D, $78");
        assert_eq!(disassemble(&[0x1e, 0x9a]), "LD E, $9a");
        assert_eq!(disassemble(&[0x26, 0xbc]), "LD H, $bc");
        assert_eq!(disassemble(&[0x2e, 0xde]), "LD L, $de");
        assert_eq!(disassemble(&[0x36, 0xf0]), "LD [HL], $f0");
    }

    #[test]
    fn disassemble_ld_a_memory() {
        assert_eq!(disassemble(&[0x0a]), "LD A, [BC]");
        assert_eq!(disassemble(&[0x02]), "LD [BC], A");
        assert_eq!(disassemble(&[0x1a]), "LD A, [DE]");
        assert_eq!(disassemble(&[0x12]), "LD [DE], A");
        assert_eq!(disassemble(&[0x7e]), "LD A, [HL]");
        assert_eq!(disassemble(&[0x77]), "LD [HL], A");
        assert_eq!(disassemble(&[0x2a]), "LD A, [HL+]");
        assert_eq!(disassemble(&[0x22]), "LD [HL+], A");
        assert_eq!(disassemble(&[0x3a]), "LD A, [HL-]");
        assert_eq!(disassemble(&[0x32]), "LD [HL-], A");
        assert_eq!(disassemble(&[0xfa, 0x34, 0x12]), "LD A, [$1234]");
        assert_eq!(disassemble(&[0xea, 0x78, 0x56]), "LD [$5678], A");
        assert_eq!(
            disassemble_with_label(&[0xfa, 0x34, 0x12], 0x1234, "foo"),
            "LD A, [foo]"
        );
        assert_eq!(
            disassemble_with_label(&[0xea, 0x78, 0x56], 0x5678, "foo"),
            "LD [foo], A"
        );
    }

    #[test]
    fn disassemble_ld_r16() {
        assert_eq!(disassemble(&[0x01, 0x34, 0x12]), "LD BC, $1234");
        assert_eq!(disassemble(&[0x11, 0x78, 0x56]), "LD DE, $5678");
        assert_eq!(disassemble(&[0x21, 0xbc, 0x9a]), "LD HL, $9abc");
        assert_eq!(disassemble(&[0x31, 0xf0, 0xde]), "LD SP, $def0");
        assert_eq!(disassemble(&[0xf9]), "LD SP, HL");
        assert_eq!(disassemble(&[0xf8, 0x00]), "LD HL, SP + 0");
        assert_eq!(disassemble(&[0xf8, 0x7f]), "LD HL, SP + 127");
        assert_eq!(disassemble(&[0xf8, 0x80]), "LD HL, SP - 128");
        assert_eq!(disassemble(&[0x08, 0x34, 0x12]), "LD [$1234], SP");
        assert_eq!(
            disassemble_with_label(&[0x08, 0xcd, 0xab], 0xabcd, "foo"),
            "LD [foo], SP"
        );
    }

    #[test]
    fn disassemble_ldh() {
        assert_eq!(disassemble(&[0xf2]), "LDH A, [C]");
        assert_eq!(disassemble(&[0xe2]), "LDH [C], A");
        assert_eq!(disassemble(&[0xf0, 0x42]), "LDH A, [$ff42]");
        assert_eq!(disassemble(&[0xe0, 0x42]), "LDH [$ff42], A");
        assert_eq!(
            disassemble_with_label(&[0xf0, 0x12], 0xff12, "foo"),
            "LDH A, [foo]"
        );
        assert_eq!(
            disassemble_with_label(&[0xe0, 0xcd], 0xffcd, "foo"),
            "LDH [foo], A"
        );
    }

    #[test]
    fn disassemble_pop() {
        assert_eq!(disassemble(&[0xf1]), "POP AF");
        assert_eq!(disassemble(&[0xc1]), "POP BC");
        assert_eq!(disassemble(&[0xd1]), "POP DE");
        assert_eq!(disassemble(&[0xe1]), "POP HL");
    }

    #[test]
    fn disassemble_push() {
        assert_eq!(disassemble(&[0xf5]), "PUSH AF");
        assert_eq!(disassemble(&[0xc5]), "PUSH BC");
        assert_eq!(disassemble(&[0xd5]), "PUSH DE");
        assert_eq!(disassemble(&[0xe5]), "PUSH HL");
    }

    #[test]
    fn disassemble_ret() {
        assert_eq!(disassemble(&[0xc9]), "RET");
        assert_eq!(disassemble(&[0xd8]), "RET C");
        assert_eq!(disassemble(&[0xd0]), "RET NC");
        assert_eq!(disassemble(&[0xc8]), "RET Z");
        assert_eq!(disassemble(&[0xc0]), "RET NZ");
        assert_eq!(disassemble(&[0xd9]), "RETI");
    }

    #[test]
    fn disassemble_rst() {
        assert_eq!(disassemble(&[0xc7]), "RST $00");
        assert_eq!(disassemble(&[0xcf]), "RST $08");
        assert_eq!(disassemble(&[0xd7]), "RST $10");
        assert_eq!(disassemble(&[0xdf]), "RST $18");
        assert_eq!(disassemble(&[0xe7]), "RST $20");
        assert_eq!(disassemble(&[0xef]), "RST $28");
        assert_eq!(disassemble(&[0xf7]), "RST $30");
        assert_eq!(disassemble(&[0xff]), "RST $38");
        assert_eq!(disassemble_with_label(&[0xef], 0x0028, "foo"), "RST foo");
    }

    #[test]
    fn disassemble_prefixed_miscellaneous() {
        assert_eq!(disassemble(&[0xcb, 0x17]), "RL A");
        assert_eq!(disassemble(&[0xcb, 0x00]), "RLC B");
        assert_eq!(disassemble(&[0xcb, 0x19]), "RR C");
        assert_eq!(disassemble(&[0xcb, 0x0a]), "RRC D");
        assert_eq!(disassemble(&[0xcb, 0x23]), "SLA E");
        assert_eq!(disassemble(&[0xcb, 0x2c]), "SRA H");
        assert_eq!(disassemble(&[0xcb, 0x3d]), "SRL L");
        assert_eq!(disassemble(&[0xcb, 0x36]), "SWAP [HL]");
    }

    #[test]
    fn disassemble_prefixed_bit() {
        assert_eq!(disassemble(&[0xcb, 0x47]), "BIT 0, A");
        assert_eq!(disassemble(&[0xcb, 0x48]), "BIT 1, B");
        assert_eq!(disassemble(&[0xcb, 0x51]), "BIT 2, C");
        assert_eq!(disassemble(&[0xcb, 0x5a]), "BIT 3, D");
        assert_eq!(disassemble(&[0xcb, 0x63]), "BIT 4, E");
        assert_eq!(disassemble(&[0xcb, 0x6c]), "BIT 5, H");
        assert_eq!(disassemble(&[0xcb, 0x75]), "BIT 6, L");
        assert_eq!(disassemble(&[0xcb, 0x7e]), "BIT 7, [HL]");
    }

    #[test]
    fn disassemble_prefixed_res() {
        assert_eq!(disassemble(&[0xcb, 0x8f]), "RES 1, A");
        assert_eq!(disassemble(&[0xcb, 0x98]), "RES 3, B");
        assert_eq!(disassemble(&[0xcb, 0xa9]), "RES 5, C");
        assert_eq!(disassemble(&[0xcb, 0xba]), "RES 7, D");
        assert_eq!(disassemble(&[0xcb, 0x83]), "RES 0, E");
        assert_eq!(disassemble(&[0xcb, 0x94]), "RES 2, H");
        assert_eq!(disassemble(&[0xcb, 0xa5]), "RES 4, L");
        assert_eq!(disassemble(&[0xcb, 0xb6]), "RES 6, [HL]");
    }

    #[test]
    fn disassemble_prefixed_set() {
        assert_eq!(disassemble(&[0xcb, 0xff]), "SET 7, A");
        assert_eq!(disassemble(&[0xcb, 0xf0]), "SET 6, B");
        assert_eq!(disassemble(&[0xcb, 0xe9]), "SET 5, C");
        assert_eq!(disassemble(&[0xcb, 0xe2]), "SET 4, D");
        assert_eq!(disassemble(&[0xcb, 0xdb]), "SET 3, E");
        assert_eq!(disassemble(&[0xcb, 0xd4]), "SET 2, H");
        assert_eq!(disassemble(&[0xcb, 0xcd]), "SET 1, L");
        assert_eq!(disassemble(&[0xcb, 0xc6]), "SET 0, [HL]");
    }
}

//===========================================================================//
