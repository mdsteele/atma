//! Facilities for disassembling SPC-700 machine code.

use crate::bus::SimBus;
use std::fmt;

//===========================================================================//

/// An 8-bit register for an SPC-700 processor.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Reg {
    /// The A (accumulator) register.
    A,
    /// The X index register.
    X,
    /// The Y index register.
    Y,
    /// The stack pointer.
    Sp,
    /// The program status word.
    Psw,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Reg::A => f.write_str("A"),
            Reg::X => f.write_str("X"),
            Reg::Y => f.write_str("Y"),
            Reg::Sp => f.write_str("SP"),
            Reg::Psw => f.write_str("PSW"),
        }
    }
}

//===========================================================================//

/// An addressing mode for an SPC-700 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AddrMode {
    /// Operate on a constant byte immediately following the opcode.
    Immediate,
    /// Operate on an address that is offset (by the signed byte following the
    /// opcode) from the address of this instruction.
    Relative,
    /// Operate on the absolute 16-bit address following the opcode.
    Absolute,
    /// Operate on a the absolute 16-bit address following the opcode, offset
    /// by index X.
    XIndexedAbsolute,
    /// Operate on a the absolute 16-bit address following the opcode, offset
    /// by index Y.
    YIndexedAbsolute,
    /// Operate on the 16-bit address that is stored in memory, at the absolute
    /// 16-bit address following the opcode offset by index X.
    XIndexedAbsoluteIndirect,
    /// Operate on an address in the high page, using the byte immediately
    /// following the opcode as the high page address.
    HighPage,
    /// Operate on a byte in the current direct page, using the byte
    /// immediately following the opcode as the direct page offset.
    DirectPage,
    /// Operate on a byte in the current direct page, using the X register as
    /// the direct page offset.
    DirectPageX,
    /// Operate on a byte in the current direct page, using the X register as
    /// the direct page offset, and incrementing X after the operation.
    DirectPageXInc,
    /// Operate on the 8-bit direct page address following the opcode, offset
    /// by index X.
    XIndexedDirectPage,
    /// Operate on the 8-bit direct page address following the opcode, offset
    /// by index Y.
    YIndexedDirectPage,
    /// Operate on the 16-bit address that is stored in memory, at the 8-bit
    /// direct page address following the opcode offset by index X.
    XIndexedDirectPageIndirect,
    /// Operate on a address equal to the 16-bit address stored at the 8-bit
    /// direct page address following the opcode, offset by index Y.
    DirectPageIndirectYIndexed,
}

impl AddrMode {
    fn decode(self, bus: &dyn SimBus, pc: u16) -> Operand {
        match self {
            AddrMode::Immediate => Operand::Immediate(next_byte(bus, pc)),
            AddrMode::Relative => Operand::Relative(next_byte(bus, pc) as i8),
            AddrMode::Absolute => Operand::Absolute(next_word(bus, pc)),
            AddrMode::XIndexedAbsolute => {
                Operand::XIndexedAbsolute(next_word(bus, pc))
            }
            AddrMode::YIndexedAbsolute => {
                Operand::YIndexedAbsolute(next_word(bus, pc))
            }
            AddrMode::XIndexedAbsoluteIndirect => {
                Operand::XIndexedAbsoluteIndirect(next_word(bus, pc))
            }
            AddrMode::HighPage => Operand::HighPage(next_byte(bus, pc)),
            AddrMode::DirectPage => Operand::DirectPage(next_byte(bus, pc)),
            AddrMode::DirectPageX => Operand::DirectPageX,
            AddrMode::DirectPageXInc => Operand::DirectPageXInc,
            AddrMode::XIndexedDirectPage => {
                Operand::XIndexedDirectPage(next_byte(bus, pc))
            }
            AddrMode::YIndexedDirectPage => {
                Operand::YIndexedDirectPage(next_byte(bus, pc))
            }
            AddrMode::XIndexedDirectPageIndirect => {
                Operand::XIndexedDirectPageIndirect(next_byte(bus, pc))
            }
            AddrMode::DirectPageIndirectYIndexed => {
                Operand::DirectPageIndirectYIndexed(next_byte(bus, pc))
            }
        }
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

/// An addressing mode and argument value for an SPC-700 processor instruction.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Operand {
    /// Operate on the given constant byte.
    Immediate(u8),
    /// Operate on an address that is offset (by the given signed byte) from
    /// the address of this instruction.
    Relative(i8),
    /// Operate on the given absolute address.
    Absolute(u16),
    /// Operate on the given absolute address, offset by index X.
    XIndexedAbsolute(u16),
    /// Operate on the given absolute address, offset by index Y.
    YIndexedAbsolute(u16),
    /// Operate on the address that is stored in memory, offset from the given
    /// absolute address by index X.
    XIndexedAbsoluteIndirect(u16),
    /// Operate on the given high page address.
    HighPage(u8),
    /// Operate on the given direct page address.
    DirectPage(u8),
    /// Operate on the direct page address stored in the X register.
    DirectPageX,
    /// Operate on the direct page address stored in the X register,
    /// incrementing X after the operation.
    DirectPageXInc,
    /// Operate on the given direct page address, offset by index X.
    XIndexedDirectPage(u8),
    /// Operate on the given direct page address, offset by index Y.
    YIndexedDirectPage(u8),
    /// Operate on the address in memory that is offset from the given direct
    /// page address by index X.
    XIndexedDirectPageIndirect(u8),
    /// Operate on an address in memory equal to the address stored at the
    /// given direct page address, offset by index Y.
    DirectPageIndirectYIndexed(u8),
}

impl Operand {
    fn size(self) -> u32 {
        match self {
            Operand::Immediate(_) => 1,
            Operand::Relative(_) => 1,
            Operand::Absolute(_) => 2,
            Operand::XIndexedAbsolute(_) => 2,
            Operand::YIndexedAbsolute(_) => 2,
            Operand::XIndexedAbsoluteIndirect(_) => 2,
            Operand::HighPage(_) => 1,
            Operand::DirectPage(_) => 1,
            Operand::DirectPageX => 0,
            Operand::DirectPageXInc => 0,
            Operand::XIndexedDirectPage(_) => 1,
            Operand::YIndexedDirectPage(_) => 1,
            Operand::XIndexedDirectPageIndirect(_) => 1,
            Operand::DirectPageIndirectYIndexed(_) => 1,
        }
    }

    /// Format this operand as a string.  `next` gives the address of the next
    /// instruction after this one.  `bus` is used for supplying address labels
    /// as needed.
    fn format(self, bus: &dyn SimBus, next: u16) -> String {
        match self {
            Operand::Immediate(byte) => format!("#${byte:02x}"),
            Operand::Relative(offset) => {
                format_address(bus, next.wrapping_add(offset as u16))
            }
            Operand::Absolute(addr) => {
                format!("!{}", format_address(bus, addr))
            }
            Operand::XIndexedAbsolute(addr) => {
                format!("!{} + X", format_address(bus, addr))
            }
            Operand::YIndexedAbsolute(addr) => {
                format!("!{} + Y", format_address(bus, addr))
            }
            Operand::XIndexedAbsoluteIndirect(addr) => {
                format!("[!{} + X]", format_address(bus, addr))
            }
            Operand::HighPage(hp) => format_high_page(bus, hp),
            Operand::DirectPage(dp) => format_direct_page(bus, dp),
            Operand::DirectPageX => "(X)".to_string(),
            Operand::DirectPageXInc => "(X)+".to_string(),
            Operand::XIndexedDirectPage(dp) => {
                format!("{} + X", format_direct_page(bus, dp))
            }
            Operand::YIndexedDirectPage(dp) => {
                format!("{} + Y", format_direct_page(bus, dp))
            }
            Operand::XIndexedDirectPageIndirect(dp) => {
                format!("[{} + X]", format_direct_page(bus, dp))
            }
            Operand::DirectPageIndirectYIndexed(dp) => {
                format!("[{}] + Y", format_direct_page(bus, dp))
            }
        }
    }
}

fn format_address(bus: &dyn SimBus, addr: u16) -> String {
    match bus.label_at(u32::from(addr)) {
        None => format!("${addr:04x}"),
        Some(label) => label.to_string(),
    }
}

fn format_direct_page(bus: &dyn SimBus, dp: u8) -> String {
    let addr = u32::from(dp);
    match bus.label_at(addr).or_else(|| bus.label_at(0x100 | addr)) {
        None => format!("${dp:02x}"),
        Some(label) => label.to_string(),
    }
}

fn format_high_page(bus: &dyn SimBus, hp: u8) -> String {
    match bus.label_at(0xff00 | u32::from(hp)) {
        None => format!("${hp:02x}"),
        Some(label) => label.to_string(),
    }
}

//===========================================================================//

/// Represents an SPC-700 instruction type, abstracted over how addresses are
/// specified.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Mnemonic<ADDR> {
    /// Add the byte at the specified address to the A register with carry.
    AdcAAddr(ADDR),
    /// Add the byte at the second address to the first address with carry.
    AdcAddrAddr(ADDR, ADDR),
    /// Bitwise-AND the byte at the specified address into the A register.
    AndAAddr(ADDR),
    /// Bitwise-AND the byte at the second address into the first address.
    AndAddrAddr(ADDR, ADDR),
    /// Arithmetically shift the A register left by one bit.
    AslA,
    /// Arithmetically shift the byte at the specified address left by one bit.
    AslAddr(ADDR),
    /// Branch to the specified address if the carry flag is clear.
    Bcc(ADDR),
    /// Branch to the specified address if the carry flag is set.
    Bcs(ADDR),
    /// Branch to the specified address if the zero flag is set.
    Beq(ADDR),
    /// Branch to the specified address if the negative flag is set.
    Bmi(ADDR),
    /// Branch to the specified address if the zero flag is clear.
    Bne(ADDR),
    /// Branch to the specified address if the negative flag is clear.
    Bpl(ADDR),
    /// Branch to the specified address unconditionally.
    Bra(ADDR),
    /// Initiate software interrupt.
    Brk,
    /// Branch to the specified address if the overflow flag is clear.
    Bvc(ADDR),
    /// Branch to the specified address if the overflow flag is set.
    Bvs(ADDR),
    /// Call a subroutine located at the specified address.
    Call(ADDR),
    /// Clear the carry flag.
    Clrc,
    /// Clear the direct page flag (making page 0 the direct page).
    Clrp,
    /// Clear the overflow flag.
    Clrv,
    /// Compare the byte at the first address to the byte at the second
    /// address.
    CmpAddrAddr(ADDR, ADDR),
    /// Compare the contents of the specified register to the byte at the
    /// specified address.
    CmpRegAddr(Reg, ADDR),
    /// Decimal adjust for addition.
    Daa,
    /// Decimal adjust for subtraction.
    Das,
    /// Decrement the byte at the specified address.
    DecAddr(ADDR),
    /// Decrement the specified register.
    DecReg(Reg),
    /// Decrement the word at the specified address.
    Decw(ADDR),
    /// Disable interrupts.
    Di,
    /// Divide YA by X, storing quotient in A and remainder in Y.
    Div,
    /// Enable interrupts.
    Ei,
    /// Bitwise-XOR the byte at the specified address into the A register.
    EorAAddr(ADDR),
    /// Bitwise-XOR the byte at the second address into the first address.
    EorAddrAddr(ADDR, ADDR),
    /// Increment the byte at the specified address.
    IncAddr(ADDR),
    /// Increment the specified register.
    IncReg(Reg),
    /// Increment the word at the specified address.
    Incw(ADDR),
    /// Jump to the specified address.
    Jmp(ADDR),
    /// Logically shift the A register right by one bit.
    LsrA,
    /// Logically shift the byte at the specified address right by one bit.
    LsrAddr(ADDR),
    /// Copy the byte at the second address to the first address.
    MovAddrAddr(ADDR, ADDR),
    /// Copy the contents of the specified register to the specified address.
    MovAddrReg(ADDR, Reg),
    /// Copy the byte at the specified address into the specified register.
    MovRegAddr(Reg, ADDR),
    /// Copy the contents of the second register into the first register.
    MovRegReg(Reg, Reg),
    /// Multiply Y by A into YA.
    Mul,
    /// No-op.
    Nop,
    /// Invert the carry flag.
    Notc,
    /// Bitwise-OR the byte at the specified address into the A register.
    OrAAddr(ADDR),
    /// Bitwise-OR the byte at the second address into the first address.
    OrAddrAddr(ADDR, ADDR),
    /// Call a subroutine located at the specified high page address.
    Pcall(ADDR),
    /// Pop a register value from the stack.
    Pop(Reg),
    /// Push a register value onto the stack.
    Push(Reg),
    /// Return from subroutine.
    Ret,
    /// Return from interrupt.
    Reti,
    /// Rotate the A register left by one bit.
    RolA,
    /// Rotate the byte at the specified address left by one bit.
    RolAddr(ADDR),
    /// Rotate the A register right by one bit.
    RorA,
    /// Rotate the byte at the specified address right by one bit.
    RorAddr(ADDR),
    /// Subtract the byte at the specified address from the A register with
    /// borrow.
    SbcAAddr(ADDR),
    /// Subtract the byte at the second address from the first address with
    /// borrow.
    SbcAddrAddr(ADDR, ADDR),
    /// Set the carry flag.
    Setc,
    /// Set the direct page flag (making page 1 the direct page).
    Setp,
    /// Wait for interrupt.
    Sleep,
    /// Stop the processor.
    Stop,
    /// Call the subroutine pointed to by the address stored in memory starting
    /// at the specified high page address.
    Tcall(u8),
    /// Exchange the upper and lower nibbles of the A register.
    Xcn,
}

//===========================================================================//

/// An operation that can be executed by an SPC-700 processor.
pub type Operation = Mnemonic<AddrMode>;

impl Operation {
    /// Decodes an SPC-700 opcode.
    pub fn from_opcode(opcode: u8) -> Operation {
        match opcode {
            0x00 => Operation::Nop,
            0x10 => Operation::Bpl(AddrMode::Relative),
            0x20 => Operation::Clrp,
            0x30 => Operation::Bmi(AddrMode::Relative),
            0x40 => Operation::Setp,
            0x50 => Operation::Bvc(AddrMode::Relative),
            0x60 => Operation::Clrc,
            0x70 => Operation::Bvs(AddrMode::Relative),
            0x80 => Operation::Setc,
            0x90 => Operation::Bcc(AddrMode::Relative),
            0xa0 => Operation::Ei,
            0xb0 => Operation::Bcs(AddrMode::Relative),
            0xc0 => Operation::Di,
            0xd0 => Operation::Bne(AddrMode::Relative),
            0xe0 => Operation::Clrv,
            0xf0 => Operation::Beq(AddrMode::Relative),

            0x01 => Operation::Tcall(0xde),
            0x11 => Operation::Tcall(0xdc),
            0x21 => Operation::Tcall(0xda),
            0x31 => Operation::Tcall(0xd8),
            0x41 => Operation::Tcall(0xd6),
            0x51 => Operation::Tcall(0xd4),
            0x61 => Operation::Tcall(0xd2),
            0x71 => Operation::Tcall(0xd0),
            0x81 => Operation::Tcall(0xce),
            0x91 => Operation::Tcall(0xcc),
            0xa1 => Operation::Tcall(0xca),
            0xb1 => Operation::Tcall(0xc8),
            0xc1 => Operation::Tcall(0xc6),
            0xd1 => Operation::Tcall(0xc4),
            0xe1 => Operation::Tcall(0xc2),
            0xf1 => Operation::Tcall(0xc0),

            0x04 => Operation::OrAAddr(AddrMode::DirectPage),
            0x14 => Operation::OrAAddr(AddrMode::XIndexedDirectPage),
            0x24 => Operation::AndAAddr(AddrMode::DirectPage),
            0x34 => Operation::AndAAddr(AddrMode::XIndexedDirectPage),
            0x44 => Operation::EorAAddr(AddrMode::DirectPage),
            0x54 => Operation::EorAAddr(AddrMode::XIndexedDirectPage),
            0x64 => Operation::CmpRegAddr(Reg::A, AddrMode::DirectPage),
            0x74 => Operation::CmpRegAddr(Reg::A, AddrMode::XIndexedDirectPage),
            0x84 => Operation::AdcAAddr(AddrMode::DirectPage),
            0x94 => Operation::AdcAAddr(AddrMode::XIndexedDirectPage),
            0xa4 => Operation::SbcAAddr(AddrMode::DirectPage),
            0xb4 => Operation::SbcAAddr(AddrMode::XIndexedDirectPage),
            0xc4 => Operation::MovAddrReg(AddrMode::DirectPage, Reg::A),
            0xd4 => {
                Operation::MovAddrReg(AddrMode::XIndexedDirectPage, Reg::A)
            }
            0xe4 => Operation::MovRegAddr(Reg::A, AddrMode::DirectPage),
            0xf4 => {
                Operation::MovRegAddr(Reg::A, AddrMode::XIndexedDirectPage)
            }

            0x05 => Operation::OrAAddr(AddrMode::Absolute),
            0x15 => Operation::OrAAddr(AddrMode::XIndexedAbsolute),
            0x25 => Operation::AndAAddr(AddrMode::Absolute),
            0x35 => Operation::AndAAddr(AddrMode::XIndexedAbsolute),
            0x45 => Operation::EorAAddr(AddrMode::Absolute),
            0x55 => Operation::EorAAddr(AddrMode::XIndexedAbsolute),
            0x65 => Operation::CmpRegAddr(Reg::A, AddrMode::Absolute),
            0x75 => Operation::CmpRegAddr(Reg::A, AddrMode::XIndexedAbsolute),
            0x85 => Operation::AdcAAddr(AddrMode::Absolute),
            0x95 => Operation::AdcAAddr(AddrMode::XIndexedAbsolute),
            0xa5 => Operation::SbcAAddr(AddrMode::Absolute),
            0xb5 => Operation::SbcAAddr(AddrMode::XIndexedAbsolute),
            0xc5 => Operation::MovAddrReg(AddrMode::Absolute, Reg::A),
            0xd5 => Operation::MovAddrReg(AddrMode::XIndexedAbsolute, Reg::A),
            0xe5 => Operation::MovRegAddr(Reg::A, AddrMode::Absolute),
            0xf5 => Operation::MovRegAddr(Reg::A, AddrMode::XIndexedAbsolute),

            0x06 => Operation::OrAAddr(AddrMode::DirectPageX),
            0x16 => Operation::OrAAddr(AddrMode::YIndexedAbsolute),
            0x26 => Operation::AndAAddr(AddrMode::DirectPageX),
            0x36 => Operation::AndAAddr(AddrMode::YIndexedAbsolute),
            0x46 => Operation::EorAAddr(AddrMode::DirectPageX),
            0x56 => Operation::EorAAddr(AddrMode::YIndexedAbsolute),
            0x66 => Operation::CmpRegAddr(Reg::A, AddrMode::DirectPageX),
            0x76 => Operation::CmpRegAddr(Reg::A, AddrMode::YIndexedAbsolute),
            0x86 => Operation::AdcAAddr(AddrMode::DirectPageX),
            0x96 => Operation::AdcAAddr(AddrMode::YIndexedAbsolute),
            0xa6 => Operation::SbcAAddr(AddrMode::DirectPageX),
            0xb6 => Operation::SbcAAddr(AddrMode::YIndexedAbsolute),
            0xc6 => Operation::MovAddrReg(AddrMode::DirectPageX, Reg::A),
            0xd6 => Operation::MovAddrReg(AddrMode::YIndexedAbsolute, Reg::A),
            0xe6 => Operation::MovRegAddr(Reg::A, AddrMode::DirectPageX),
            0xf6 => Operation::MovRegAddr(Reg::A, AddrMode::YIndexedAbsolute),

            0x07 => Operation::OrAAddr(AddrMode::XIndexedDirectPageIndirect),
            0x17 => Operation::OrAAddr(AddrMode::DirectPageIndirectYIndexed),
            0x27 => Operation::AndAAddr(AddrMode::XIndexedDirectPageIndirect),
            0x37 => Operation::AndAAddr(AddrMode::DirectPageIndirectYIndexed),
            0x47 => Operation::EorAAddr(AddrMode::XIndexedDirectPageIndirect),
            0x57 => Operation::EorAAddr(AddrMode::DirectPageIndirectYIndexed),
            0x67 => Operation::CmpRegAddr(Reg::A, AddrMode::XIndexedDirectPageIndirect),
            0x77 => Operation::CmpRegAddr(Reg::A, AddrMode::DirectPageIndirectYIndexed),
            0x87 => Operation::AdcAAddr(AddrMode::XIndexedDirectPageIndirect),
            0x97 => Operation::AdcAAddr(AddrMode::DirectPageIndirectYIndexed),
            0xa7 => Operation::SbcAAddr(AddrMode::XIndexedDirectPageIndirect),
            0xb7 => Operation::SbcAAddr(AddrMode::DirectPageIndirectYIndexed),
            0xc7 => Operation::MovAddrReg(
                AddrMode::XIndexedDirectPageIndirect,
                Reg::A,
            ),
            0xd7 => Operation::MovAddrReg(
                AddrMode::DirectPageIndirectYIndexed,
                Reg::A,
            ),
            0xe7 => Operation::MovRegAddr(
                Reg::A,
                AddrMode::XIndexedDirectPageIndirect,
            ),
            0xf7 => Operation::MovRegAddr(
                Reg::A,
                AddrMode::DirectPageIndirectYIndexed,
            ),

            0xc8 => Operation::CmpRegAddr(Reg::X, AddrMode::Immediate),
            0xd8 => Operation::MovAddrReg(AddrMode::DirectPage, Reg::X),
            0xe8 => Operation::MovRegAddr(Reg::A, AddrMode::Immediate),
            0xf8 => Operation::MovRegAddr(Reg::X, AddrMode::DirectPage),

            0xc9 => Operation::MovAddrReg(AddrMode::Absolute, Reg::X),
            0xd9 => {
                Operation::MovAddrReg(AddrMode::YIndexedDirectPage, Reg::X)
            }
            0xe9 => Operation::MovRegAddr(Reg::X, AddrMode::Absolute),
            0xf9 => {
                Operation::MovRegAddr(Reg::X, AddrMode::YIndexedDirectPage)
            }

            0x1a => Operation::Decw(AddrMode::DirectPage),
            0x3a => Operation::Incw(AddrMode::DirectPage),
            0xfa => Operation::MovAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),

            0x0b => Operation::AslAddr(AddrMode::DirectPage),
            0x1b => Operation::AslAddr(AddrMode::XIndexedDirectPage),
            0x2b => Operation::RolAddr(AddrMode::DirectPage),
            0x3b => Operation::RolAddr(AddrMode::XIndexedDirectPage),
            0x4b => Operation::LsrAddr(AddrMode::DirectPage),
            0x5b => Operation::LsrAddr(AddrMode::XIndexedDirectPage),
            0x6b => Operation::RorAddr(AddrMode::DirectPage),
            0x7b => Operation::RorAddr(AddrMode::XIndexedDirectPage),
            0x8b => Operation::DecAddr(AddrMode::DirectPage),
            0x9b => Operation::DecAddr(AddrMode::XIndexedDirectPage),
            0xab => Operation::IncAddr(AddrMode::DirectPage),
            0xbb => Operation::IncAddr(AddrMode::XIndexedDirectPage),
            0xcb => Operation::MovAddrReg(AddrMode::DirectPage, Reg::Y),
            0xdb => {
                Operation::MovAddrReg(AddrMode::XIndexedDirectPage, Reg::Y)
            }
            0xeb => Operation::MovRegAddr(Reg::Y, AddrMode::DirectPage),
            0xfb => {
                Operation::MovRegAddr(Reg::Y, AddrMode::XIndexedDirectPage)
            }

            0x0c => Operation::AslAddr(AddrMode::Absolute),
            0x1c => Operation::AslA,
            0x2c => Operation::RolAddr(AddrMode::Absolute),
            0x3c => Operation::RolA,
            0x4c => Operation::LsrAddr(AddrMode::Absolute),
            0x5c => Operation::LsrA,
            0x6c => Operation::RorAddr(AddrMode::Absolute),
            0x7c => Operation::RorA,
            0x8c => Operation::DecAddr(AddrMode::Absolute),
            0x9c => Operation::DecReg(Reg::A),
            0xac => Operation::IncAddr(AddrMode::Absolute),
            0xbc => Operation::IncReg(Reg::A),
            0xcc => Operation::MovAddrReg(AddrMode::Absolute, Reg::Y),
            0xdc => Operation::DecReg(Reg::Y),
            0xec => Operation::MovRegAddr(Reg::Y, AddrMode::Absolute),
            0xfc => Operation::IncReg(Reg::Y),

            0x0d => Operation::Push(Reg::Psw),
            0x1d => Operation::DecReg(Reg::X),
            0x2d => Operation::Push(Reg::A),
            0x3d => Operation::IncReg(Reg::X),
            0x4d => Operation::Push(Reg::X),
            0x5d => Operation::MovRegReg(Reg::X, Reg::A),
            0x6d => Operation::Push(Reg::Y),
            0x7d => Operation::MovRegReg(Reg::A, Reg::X),
            0x8d => Operation::MovRegAddr(Reg::Y, AddrMode::Immediate),
            0x9d => Operation::MovRegReg(Reg::X, Reg::Sp),
            0xbd => Operation::MovRegReg(Reg::Sp, Reg::X),
            0xcd => Operation::MovRegAddr(Reg::X, AddrMode::Immediate),
            0xdd => Operation::MovRegReg(Reg::A, Reg::Y),
            0xed => Operation::Notc,
            0xfd => Operation::MovRegReg(Reg::Y, Reg::A),

            0x8e => Operation::Pop(Reg::Psw),
            0x9e => Operation::Div,
            0xae => Operation::Pop(Reg::A),
            0xbe => Operation::Das,
            0xce => Operation::Pop(Reg::X),
            0xee => Operation::Pop(Reg::Y),

            0x0f => Operation::Brk,
            0x1f => Operation::Jmp(AddrMode::XIndexedAbsoluteIndirect),
            0x2f => Operation::Bra(AddrMode::Relative),
            0x3f => Operation::Call(AddrMode::Absolute),
            0x4f => Operation::Pcall(AddrMode::HighPage),
            0x5f => Operation::Jmp(AddrMode::Absolute),
            0x6f => Operation::Ret,
            0x7f => Operation::Reti,
            0x8f => Operation::MovAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0x9f => Operation::Xcn,
            0xaf => Operation::MovAddrReg(AddrMode::DirectPageXInc, Reg::A),
            0xbf => Operation::MovRegAddr(Reg::A, AddrMode::DirectPageXInc),
            0xcf => Operation::Mul,
            0xdf => Operation::Daa,
            0xef => Operation::Sleep,
            0xff => Operation::Stop,

            _ => todo!("opcode=0x{opcode:02x}"),
        }
    }
}

//===========================================================================//

/// A complete instruction, including parameter values, for an SPC-700
/// processor.
pub type Instruction = Mnemonic<Operand>;

impl Instruction {
    /// Returns the size of this instruction, in bytes.
    pub fn size(self) -> u32 {
        match self {
            Instruction::AslA
            | Instruction::Brk
            | Instruction::Clrc
            | Instruction::Clrp
            | Instruction::Clrv
            | Instruction::Daa
            | Instruction::Das
            | Instruction::DecReg(_)
            | Instruction::Di
            | Instruction::Div
            | Instruction::Ei
            | Instruction::IncReg(_)
            | Instruction::LsrA
            | Instruction::MovRegReg(_, _)
            | Instruction::Mul
            | Instruction::Nop
            | Instruction::Notc
            | Instruction::Pop(_)
            | Instruction::Push(_)
            | Instruction::Ret
            | Instruction::Reti
            | Instruction::RolA
            | Instruction::RorA
            | Instruction::Setc
            | Instruction::Setp
            | Instruction::Sleep
            | Instruction::Stop
            | Instruction::Tcall(_)
            | Instruction::Xcn => 1,
            Instruction::AdcAAddr(operand)
            | Instruction::AndAAddr(operand)
            | Instruction::AslAddr(operand)
            | Instruction::Bcc(operand)
            | Instruction::Bcs(operand)
            | Instruction::Beq(operand)
            | Instruction::Bmi(operand)
            | Instruction::Bne(operand)
            | Instruction::Bpl(operand)
            | Instruction::Bra(operand)
            | Instruction::Bvc(operand)
            | Instruction::Bvs(operand)
            | Instruction::Call(operand)
            | Instruction::CmpRegAddr(_, operand)
            | Instruction::DecAddr(operand)
            | Instruction::Decw(operand)
            | Instruction::EorAAddr(operand)
            | Instruction::IncAddr(operand)
            | Instruction::Incw(operand)
            | Instruction::Jmp(operand)
            | Instruction::LsrAddr(operand)
            | Instruction::MovAddrReg(operand, _)
            | Instruction::OrAAddr(operand)
            | Instruction::Pcall(operand)
            | Instruction::MovRegAddr(_, operand)
            | Instruction::RolAddr(operand)
            | Instruction::RorAddr(operand)
            | Instruction::SbcAAddr(operand) => 1 + operand.size(),
            Instruction::AdcAddrAddr(op1, op2)
            | Instruction::AndAddrAddr(op1, op2)
            | Instruction::CmpAddrAddr(op1, op2)
            | Instruction::EorAddrAddr(op1, op2)
            | Instruction::MovAddrAddr(op1, op2)
            | Instruction::OrAddrAddr(op1, op2)
            | Instruction::SbcAddrAddr(op1, op2) => {
                1 + op1.size() + op2.size()
            }
        }
    }

    /// Reads and decodes a single SPC-700 instruction.
    pub fn decode(bus: &dyn SimBus, pc: u16) -> Instruction {
        match Operation::from_opcode(bus.peek_byte(u32::from(pc))) {
            Operation::AdcAAddr(mode) => {
                Instruction::AdcAAddr(mode.decode(bus, pc))
            }
            Operation::AdcAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::AdcAddrAddr(op1, op2)
            }
            Operation::AndAAddr(mode) => {
                Instruction::AndAAddr(mode.decode(bus, pc))
            }
            Operation::AndAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::AndAddrAddr(op1, op2)
            }
            Operation::AslA => Instruction::AslA,
            Operation::AslAddr(mode) => {
                Instruction::AslAddr(mode.decode(bus, pc))
            }
            Operation::Bcc(mode) => Instruction::Bcc(mode.decode(bus, pc)),
            Operation::Bcs(mode) => Instruction::Bcs(mode.decode(bus, pc)),
            Operation::Beq(mode) => Instruction::Beq(mode.decode(bus, pc)),
            Operation::Bmi(mode) => Instruction::Bmi(mode.decode(bus, pc)),
            Operation::Bne(mode) => Instruction::Bne(mode.decode(bus, pc)),
            Operation::Bpl(mode) => Instruction::Bpl(mode.decode(bus, pc)),
            Operation::Bra(mode) => Instruction::Bra(mode.decode(bus, pc)),
            Operation::Brk => Instruction::Brk,
            Operation::Bvc(mode) => Instruction::Bvc(mode.decode(bus, pc)),
            Operation::Bvs(mode) => Instruction::Bvs(mode.decode(bus, pc)),
            Operation::Call(mode) => Instruction::Call(mode.decode(bus, pc)),
            Operation::Clrc => Instruction::Clrc,
            Operation::Clrp => Instruction::Clrp,
            Operation::Clrv => Instruction::Clrv,
            Operation::CmpAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::CmpAddrAddr(op1, op2)
            }
            Operation::CmpRegAddr(reg, mode) => {
                Instruction::CmpRegAddr(reg, mode.decode(bus, pc))
            }
            Operation::Daa => Instruction::Daa,
            Operation::Das => Instruction::Das,
            Operation::DecAddr(mode) => {
                Instruction::DecAddr(mode.decode(bus, pc))
            }
            Operation::DecReg(reg) => Instruction::DecReg(reg),
            Operation::Decw(mode) => Instruction::Decw(mode.decode(bus, pc)),
            Operation::Di => Instruction::Di,
            Operation::Div => Instruction::Div,
            Operation::Ei => Instruction::Ei,
            Operation::EorAAddr(mode) => {
                Instruction::EorAAddr(mode.decode(bus, pc))
            }
            Operation::EorAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::EorAddrAddr(op1, op2)
            }
            Operation::IncAddr(mode) => {
                Instruction::IncAddr(mode.decode(bus, pc))
            }
            Operation::IncReg(reg) => Instruction::IncReg(reg),
            Operation::Incw(mode) => Instruction::Incw(mode.decode(bus, pc)),
            Operation::Jmp(mode) => Instruction::Jmp(mode.decode(bus, pc)),
            Operation::LsrA => Instruction::LsrA,
            Operation::LsrAddr(mode) => {
                Instruction::LsrAddr(mode.decode(bus, pc))
            }
            Operation::MovAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::MovAddrAddr(op1, op2)
            }
            Operation::MovAddrReg(mode, reg) => {
                Instruction::MovAddrReg(mode.decode(bus, pc), reg)
            }
            Operation::MovRegAddr(reg, mode) => {
                Instruction::MovRegAddr(reg, mode.decode(bus, pc))
            }
            Operation::MovRegReg(r1, r2) => Instruction::MovRegReg(r1, r2),
            Operation::Mul => Instruction::Mul,
            Operation::Nop => Instruction::Nop,
            Operation::Notc => Instruction::Notc,
            Operation::OrAAddr(mode) => {
                Instruction::OrAAddr(mode.decode(bus, pc))
            }
            Operation::OrAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::OrAddrAddr(op1, op2)
            }
            Operation::Pcall(mode) => Instruction::Pcall(mode.decode(bus, pc)),
            Operation::Pop(reg) => Instruction::Pop(reg),
            Operation::Push(reg) => Instruction::Push(reg),
            Operation::Ret => Instruction::Ret,
            Operation::Reti => Instruction::Reti,
            Operation::RolA => Instruction::RolA,
            Operation::RolAddr(mode) => {
                Instruction::RolAddr(mode.decode(bus, pc))
            }
            Operation::RorA => Instruction::RorA,
            Operation::RorAddr(mode) => {
                Instruction::RorAddr(mode.decode(bus, pc))
            }
            Operation::SbcAAddr(mode) => {
                Instruction::SbcAAddr(mode.decode(bus, pc))
            }
            Operation::SbcAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::SbcAddrAddr(op1, op2)
            }
            Operation::Setc => Instruction::Setc,
            Operation::Setp => Instruction::Setp,
            Operation::Sleep => Instruction::Sleep,
            Operation::Stop => Instruction::Stop,
            Operation::Tcall(hp) => Instruction::Tcall(hp),
            Operation::Xcn => Instruction::Xcn,
        }
    }

    /// Formats a disassembled SM83 instruction as a human-readable string.
    /// `pc` specifies the address of the start of the instruction.  `bus` is
    /// required for providing labels for addresses; if no labels are needed, a
    /// `new_open_bus` can be used.
    pub fn format(self, bus: &dyn SimBus, pc: u16) -> String {
        let next = pc.wrapping_add(self.size() as u16);
        match self {
            Instruction::AdcAAddr(op) => {
                format!("ADC A, {}", op.format(bus, next))
            }
            Instruction::AdcAddrAddr(op1, op2) => format!(
                "ADC {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::AndAAddr(op) => {
                format!("AND A, {}", op.format(bus, next))
            }
            Instruction::AndAddrAddr(op1, op2) => format!(
                "AND {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::AslA => "ASL A".to_string(),
            Instruction::AslAddr(op) => {
                format!("ASL {}", op.format(bus, next))
            }
            Instruction::Bcc(op) => format!("BCC {}", op.format(bus, next)),
            Instruction::Bcs(op) => format!("BCS {}", op.format(bus, next)),
            Instruction::Beq(op) => format!("BEQ {}", op.format(bus, next)),
            Instruction::Bmi(op) => format!("BMI {}", op.format(bus, next)),
            Instruction::Bne(op) => format!("BNE {}", op.format(bus, next)),
            Instruction::Bpl(op) => format!("BPL {}", op.format(bus, next)),
            Instruction::Bra(op) => format!("BRA {}", op.format(bus, next)),
            Instruction::Brk => "BRK".to_string(),
            Instruction::Bvc(op) => format!("BVC {}", op.format(bus, next)),
            Instruction::Bvs(op) => format!("BVS {}", op.format(bus, next)),
            Instruction::Call(op) => format!("CALL {}", op.format(bus, next)),
            Instruction::Clrc => "CLRC".to_string(),
            Instruction::Clrp => "CLRP".to_string(),
            Instruction::Clrv => "CLRV".to_string(),
            Instruction::CmpRegAddr(reg, op) => {
                format!("CMP {reg}, {}", op.format(bus, next))
            }
            Instruction::CmpAddrAddr(op1, op2) => format!(
                "CMP {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::Daa => "DAA A".to_string(),
            Instruction::Das => "DAS A".to_string(),
            Instruction::DecAddr(op) => {
                format!("DEC {}", op.format(bus, next))
            }
            Instruction::DecReg(reg) => format!("DEC {reg}"),
            Instruction::Decw(op) => format!("DECW {}", op.format(bus, next)),
            Instruction::Di => "DI".to_string(),
            Instruction::Div => "DIV YA, X".to_string(),
            Instruction::Ei => "EI".to_string(),
            Instruction::EorAAddr(op) => {
                format!("EOR A, {}", op.format(bus, next))
            }
            Instruction::EorAddrAddr(op1, op2) => format!(
                "EOR {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::IncAddr(op) => {
                format!("INC {}", op.format(bus, next))
            }
            Instruction::IncReg(reg) => format!("INC {reg}"),
            Instruction::Incw(op) => format!("INCW {}", op.format(bus, next)),
            Instruction::Jmp(op) => format!("JMP {}", op.format(bus, next)),
            Instruction::LsrA => "LSR A".to_string(),
            Instruction::LsrAddr(op) => {
                format!("LSR {}", op.format(bus, next))
            }
            Instruction::MovAddrAddr(op1, op2) => format!(
                "MOV {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::MovAddrReg(op, reg) => {
                format!("MOV {}, {reg}", op.format(bus, next))
            }
            Instruction::MovRegAddr(reg, op) => {
                format!("MOV {reg}, {}", op.format(bus, next))
            }
            Instruction::MovRegReg(r1, r2) => format!("MOV {r1}, {r2}"),
            Instruction::Mul => "MUL YA".to_string(),
            Instruction::Nop => "NOP".to_string(),
            Instruction::Notc => "NOTC".to_string(),
            Instruction::OrAAddr(op) => {
                format!("OR A, {}", op.format(bus, next))
            }
            Instruction::OrAddrAddr(op1, op2) => format!(
                "OR {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::Pcall(op) => {
                format!("PCALL {}", op.format(bus, next))
            }
            Instruction::Pop(reg) => format!("POP {reg}"),
            Instruction::Push(reg) => format!("PUSH {reg}"),
            Instruction::Ret => "RET".to_string(),
            Instruction::Reti => "RETI".to_string(),
            Instruction::RolA => "ROL A".to_string(),
            Instruction::RolAddr(op) => {
                format!("ROL {}", op.format(bus, next))
            }
            Instruction::RorA => "ROR A".to_string(),
            Instruction::RorAddr(op) => {
                format!("ROR {}", op.format(bus, next))
            }
            Instruction::SbcAAddr(op) => {
                format!("SBC A, {}", op.format(bus, next))
            }
            Instruction::SbcAddrAddr(op1, op2) => format!(
                "SBC {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::Setc => "SETC".to_string(),
            Instruction::Setp => "SETP".to_string(),
            Instruction::Sleep => "SLEEP".to_string(),
            Instruction::Stop => "STOP".to_string(),
            Instruction::Tcall(hp) => {
                format!(
                    "TCALL [{}]",
                    Operand::Absolute(0xff00 | u16::from(hp))
                        .format(bus, next)
                )
            }
            Instruction::Xcn => "XCN A".to_string(),
        }
    }
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
        let (size, string) = disassemble_with_bus(&*make_test_bus(code));
        assert_eq!(size, code.len() as u32);
        string
    }

    fn disassemble_with_label(code: &[u8], addr: u32, label: &str) -> String {
        let labels = HashMap::from([(label.to_string(), addr)]);
        let bus = LabeledBus::new(make_test_bus(code), labels);
        let (size, string) = disassemble_with_bus(&bus);
        assert_eq!(size, code.len() as u32);
        string
    }

    fn disassemble_with_bus(bus: &dyn SimBus) -> (u32, String) {
        let instruction = Instruction::decode(bus, 0);
        (instruction.size(), instruction.format(bus, 0))
    }

    #[test]
    fn disassemble_branch() {
        assert_eq!(disassemble(&[0x90, 0x40]), "BCC $0042");
        assert_eq!(disassemble(&[0xb0, 0x7f]), "BCS $0081");
        assert_eq!(disassemble(&[0xd0, 0x80]), "BNE $ff82");
        assert_eq!(disassemble(&[0xf0, 0x00]), "BEQ $0002");
        assert_eq!(disassemble(&[0x10, 0xfe]), "BPL $0000");
        assert_eq!(disassemble(&[0x30, 0xfd]), "BMI $ffff");
        assert_eq!(disassemble(&[0x2f, 0x01]), "BRA $0003");
        assert_eq!(disassemble(&[0x50, 0x02]), "BVC $0004");
        assert_eq!(disassemble(&[0x70, 0xc0]), "BVS $ffc2");
        assert_eq!(
            disassemble_with_label(&[0x90, 0x60], 0x0062, "foo"),
            "BCC foo"
        );
    }

    #[test]
    fn disassemble_call() {
        assert_eq!(disassemble(&[0x3f, 0x34, 0x12]), "CALL !$1234");
        assert_eq!(
            disassemble_with_label(&[0x3f, 0x34, 0x12], 0x1234, "foo"),
            "CALL !foo"
        );
    }

    #[test]
    fn disassemble_dec() {
        assert_eq!(disassemble(&[0x9c]), "DEC A");
        assert_eq!(disassemble(&[0x1d]), "DEC X");
        assert_eq!(disassemble(&[0xdc]), "DEC Y");
        assert_eq!(disassemble(&[0x8b, 0x12]), "DEC $12");
        assert_eq!(disassemble(&[0x9b, 0x34]), "DEC $34 + X");
        assert_eq!(disassemble(&[0x8c, 0x34, 0x12]), "DEC !$1234");
        assert_eq!(disassemble(&[0x1a, 0x56]), "DECW $56");
    }

    #[test]
    fn disassemble_flag() {
        assert_eq!(disassemble(&[0x60]), "CLRC");
        assert_eq!(disassemble(&[0x20]), "CLRP");
        assert_eq!(disassemble(&[0xe0]), "CLRV");
        assert_eq!(disassemble(&[0xc0]), "DI");
        assert_eq!(disassemble(&[0xa0]), "EI");
        assert_eq!(disassemble(&[0xed]), "NOTC");
        assert_eq!(disassemble(&[0x80]), "SETC");
        assert_eq!(disassemble(&[0x40]), "SETP");
    }

    #[test]
    fn disassemble_inc() {
        assert_eq!(disassemble(&[0xbc]), "INC A");
        assert_eq!(disassemble(&[0x3d]), "INC X");
        assert_eq!(disassemble(&[0xfc]), "INC Y");
        assert_eq!(disassemble(&[0xab, 0x12]), "INC $12");
        assert_eq!(disassemble(&[0xbb, 0x34]), "INC $34 + X");
        assert_eq!(disassemble(&[0xac, 0x34, 0x12]), "INC !$1234");
        assert_eq!(disassemble(&[0x3a, 0x56]), "INCW $56");
    }

    #[test]
    fn disassemble_jmp() {
        assert_eq!(disassemble(&[0x5f, 0x34, 0x12]), "JMP !$1234");
        assert_eq!(disassemble(&[0x1f, 0x34, 0x12]), "JMP [!$1234 + X]");
        assert_eq!(
            disassemble_with_label(&[0x1f, 0x78, 0x56], 0x5678, "foo"),
            "JMP [!foo + X]"
        );
    }

    #[test]
    fn disassemble_miscellaneous() {
        assert_eq!(disassemble(&[0x0f]), "BRK");
        assert_eq!(disassemble(&[0xdf]), "DAA A");
        assert_eq!(disassemble(&[0xbe]), "DAS A");
        assert_eq!(disassemble(&[0x9e]), "DIV YA, X");
        assert_eq!(disassemble(&[0xcf]), "MUL YA");
        assert_eq!(disassemble(&[0x00]), "NOP");
        assert_eq!(disassemble(&[0x6f]), "RET");
        assert_eq!(disassemble(&[0x7f]), "RETI");
        assert_eq!(disassemble(&[0xef]), "SLEEP");
        assert_eq!(disassemble(&[0xff]), "STOP");
        assert_eq!(disassemble(&[0x9f]), "XCN A");
    }

    #[test]
    fn disassemble_mov() {
        assert_eq!(disassemble(&[0xcd, 0xef]), "MOV X, #$ef");
        assert_eq!(disassemble(&[0xbd]), "MOV SP, X");
        assert_eq!(disassemble(&[0xe8, 0x00]), "MOV A, #$00");
        assert_eq!(disassemble(&[0xc6]), "MOV (X), A");
        assert_eq!(disassemble(&[0x8f, 0xaa, 0xf4]), "MOV $f4, #$aa");
        assert_eq!(disassemble(&[0xeb, 0xf4]), "MOV Y, $f4");
        assert_eq!(disassemble(&[0xe4, 0xf5]), "MOV A, $f5");
        assert_eq!(disassemble(&[0xcb, 0xf4]), "MOV $f4, Y");
        assert_eq!(disassemble(&[0xd7, 0x00]), "MOV [$00] + Y, A");
        assert_eq!(disassemble(&[0xdd]), "MOV A, Y");
        assert_eq!(disassemble(&[0x5d]), "MOV X, A");
    }

    #[test]
    fn disassemble_pcall() {
        assert_eq!(disassemble(&[0x4f, 0x37]), "PCALL $37");
        assert_eq!(
            disassemble_with_label(&[0x4f, 0x80], 0xff80, "foo"),
            "PCALL foo"
        );
    }

    #[test]
    fn disassemble_pop() {
        assert_eq!(disassemble(&[0xae]), "POP A");
        assert_eq!(disassemble(&[0xce]), "POP X");
        assert_eq!(disassemble(&[0xee]), "POP Y");
        assert_eq!(disassemble(&[0x8e]), "POP PSW");
    }

    #[test]
    fn disassemble_push() {
        assert_eq!(disassemble(&[0x2d]), "PUSH A");
        assert_eq!(disassemble(&[0x4d]), "PUSH X");
        assert_eq!(disassemble(&[0x6d]), "PUSH Y");
        assert_eq!(disassemble(&[0x0d]), "PUSH PSW");
    }

    #[test]
    fn disassemble_shift() {
        assert_eq!(disassemble(&[0x1c]), "ASL A");
        assert_eq!(disassemble(&[0x0b, 0x12]), "ASL $12");
        assert_eq!(disassemble(&[0x1b, 0x34]), "ASL $34 + X");
        assert_eq!(disassemble(&[0x0c, 0x34, 0x12]), "ASL !$1234");
        assert_eq!(disassemble(&[0x5c]), "LSR A");
        assert_eq!(disassemble(&[0x4b, 0x12]), "LSR $12");
        assert_eq!(disassemble(&[0x5b, 0x34]), "LSR $34 + X");
        assert_eq!(disassemble(&[0x4c, 0x34, 0x12]), "LSR !$1234");
    }

    #[test]
    fn disassemble_rotate() {
        assert_eq!(disassemble(&[0x3c]), "ROL A");
        assert_eq!(disassemble(&[0x2b, 0x12]), "ROL $12");
        assert_eq!(disassemble(&[0x3b, 0x34]), "ROL $34 + X");
        assert_eq!(disassemble(&[0x2c, 0x34, 0x12]), "ROL !$1234");
        assert_eq!(disassemble(&[0x7c]), "ROR A");
        assert_eq!(disassemble(&[0x6b, 0x12]), "ROR $12");
        assert_eq!(disassemble(&[0x7b, 0x34]), "ROR $34 + X");
        assert_eq!(disassemble(&[0x6c, 0x34, 0x12]), "ROR !$1234");
    }

    #[test]
    fn disassemble_tcall() {
        assert_eq!(disassemble(&[0x01]), "TCALL [!$ffde]");
        assert_eq!(disassemble(&[0x11]), "TCALL [!$ffdc]");
        assert_eq!(disassemble(&[0x21]), "TCALL [!$ffda]");
        assert_eq!(disassemble(&[0x31]), "TCALL [!$ffd8]");
        assert_eq!(disassemble(&[0x41]), "TCALL [!$ffd6]");
        assert_eq!(disassemble(&[0x51]), "TCALL [!$ffd4]");
        assert_eq!(disassemble(&[0x61]), "TCALL [!$ffd2]");
        assert_eq!(disassemble(&[0x71]), "TCALL [!$ffd0]");
        assert_eq!(disassemble(&[0x81]), "TCALL [!$ffce]");
        assert_eq!(disassemble(&[0x91]), "TCALL [!$ffcc]");
        assert_eq!(disassemble(&[0xa1]), "TCALL [!$ffca]");
        assert_eq!(disassemble(&[0xb1]), "TCALL [!$ffc8]");
        assert_eq!(disassemble(&[0xc1]), "TCALL [!$ffc6]");
        assert_eq!(disassemble(&[0xd1]), "TCALL [!$ffc4]");
        assert_eq!(disassemble(&[0xe1]), "TCALL [!$ffc2]");
        assert_eq!(disassemble(&[0xf1]), "TCALL [!$ffc0]");
        assert_eq!(
            disassemble_with_label(&[0x51], 0xffd4, "foo"),
            "TCALL [!foo]"
        );
    }
}

//===========================================================================//
