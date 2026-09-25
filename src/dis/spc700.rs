//! Facilities for disassembling SPC-700 machine code.

use crate::addr::Addr;
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
    /// Operate on the Nth bit of the byte stored at address M, where M is the
    /// lower 13 bits of the 16-bit argument following the opcode, and N is the
    /// upper 3 bits.
    AbsoluteBit,
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
    /// Operate on a byte in the current direct page, using the Y register as
    /// the direct page offset.
    DirectPageY,
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
            AddrMode::AbsoluteBit => {
                let word = next_word(bus, pc);
                Operand::AbsoluteBit(word & 0x1fff, (word >> 13) as u8)
            }
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
            AddrMode::DirectPageY => Operand::DirectPageY,
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
    bus.peek_byte(Addr::from(pc.wrapping_add(1)))
}

fn next_word(bus: &dyn SimBus, pc: u16) -> u16 {
    let lo = bus.peek_byte(Addr::from(pc.wrapping_add(1)));
    let hi = bus.peek_byte(Addr::from(pc.wrapping_add(2)));
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
    /// Operate on the specified bit (0-7) of the byte stored at the given
    /// 13-bit absolute address.
    AbsoluteBit(u16, u8),
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
    /// Operate on the direct page address stored in the Y register.
    DirectPageY,
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
            Operand::AbsoluteBit(_, _) => 2,
            Operand::XIndexedAbsolute(_) => 2,
            Operand::YIndexedAbsolute(_) => 2,
            Operand::XIndexedAbsoluteIndirect(_) => 2,
            Operand::HighPage(_) => 1,
            Operand::DirectPage(_) => 1,
            Operand::DirectPageX => 0,
            Operand::DirectPageY => 0,
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
            Operand::AbsoluteBit(addr, bit) => {
                format!("{}, {bit}", format_address(bus, addr))
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
            Operand::DirectPageY => "(Y)".to_string(),
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
    match bus.label_at(Addr::from(addr)) {
        None => format!("${addr:04x}"),
        Some(label) => label.to_string(),
    }
}

fn format_direct_page(bus: &dyn SimBus, dp: u8) -> String {
    let addr = Addr::from(dp);
    match bus
        .label_at(addr)
        .or_else(|| bus.label_at(Addr::from(0x0100u16) | addr))
    {
        None => format!("${dp:02x}"),
        Some(label) => label.to_string(),
    }
}

fn format_high_page(bus: &dyn SimBus, hp: u8) -> String {
    match bus.label_at(Addr::from(0xff00u16) | Addr::from(hp)) {
        None => format!("$ff{hp:02x}"),
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
    /// Add the word at the specified address to the YA register.
    Addw(ADDR),
    /// AND the specified bit in memory into the C flag.
    And1(ADDR),
    /// AND the inverse of the the specified bit in memory into the C flag.
    And1Inv(ADDR),
    /// Bitwise-AND the byte at the specified address into the A register.
    AndAAddr(ADDR),
    /// Bitwise-AND the byte at the second address into the first address.
    AndAddrAddr(ADDR, ADDR),
    /// Arithmetically shift the A register left by one bit.
    AslA,
    /// Arithmetically shift the byte at the specified address left by one bit.
    AslAddr(ADDR),
    /// Branch to the second address if the specified bit (0-7) is cleared in
    /// the byte at the first address.
    Bbc(ADDR, u8, ADDR),
    /// Branch to the second address if the specified bit (0-7) is set in the
    /// byte at the first address.
    Bbs(ADDR, u8, ADDR),
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
    /// Compare the A register to the byte at the first address, and branch to
    /// the second address if they're not equal.
    Cbne(ADDR, ADDR),
    /// Clear the specified bit (0-7) in the byte at the specified address.
    Clr1(u8, ADDR),
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
    /// Compare the YA register to the word at the specified address.
    Cmpw(ADDR),
    /// Decimal adjust for addition.
    Daa,
    /// Decimal adjust for subtraction.
    Das,
    /// Decrement the byte at the first address, then branch to the second
    /// address unless that byte is now equal to zero.
    DbnzAddr(ADDR, ADDR),
    /// Decrement the Y register, then branch to the specified address unless
    /// the Y register is now equal to zero.
    DbnzY(ADDR),
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
    /// XOR the specified bit in memory with the carry flag, and store the
    /// result in the carry flag.
    Eor1(ADDR),
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
    /// Copy the carry flag into the specified bit in memory.
    Mov1AddrC(ADDR),
    /// Copy the specified bit in memory into the carry flag.
    Mov1CAddr(ADDR),
    /// Copy the byte at the second address to the first address.
    MovAddrAddr(ADDR, ADDR),
    /// Copy the contents of the specified register to the specified address.
    MovAddrReg(ADDR, Reg),
    /// Copy the byte at the specified address into the specified register.
    MovRegAddr(Reg, ADDR),
    /// Copy the contents of the second register into the first register.
    MovRegReg(Reg, Reg),
    /// Copy the contents of the YA register to the specified address.
    MovwAddrYa(ADDR),
    /// Copy the word at the specified address into the YA register.
    MovwYaAddr(ADDR),
    /// Multiply Y by A into YA.
    Mul,
    /// No-op.
    Nop,
    /// Invert the specified bit in memory.
    Not1(ADDR),
    /// Invert the carry flag.
    Notc,
    /// OR the specified bit in memory into the C flag.
    Or1(ADDR),
    /// OR the inverse of the specified bit in memory into the C flag.
    Or1Inv(ADDR),
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
    /// Set the specified bit (0-7) in the byte at the specified address.
    Set1(u8, ADDR),
    /// Set the carry flag.
    Setc,
    /// Set the direct page flag (making page 1 the direct page).
    Setp,
    /// Wait for interrupt.
    Sleep,
    /// Stop the processor.
    Stop,
    /// Subtract the word at the specified address from the YA register.
    Subw(ADDR),
    /// Call the subroutine pointed to by the address stored in memory starting
    /// at the specified high page address.
    Tcall(u8),
    /// Set the N and Z flags as if comparing the accumulator to the byte at
    /// the specified address, then set that byte to the bitwise AND of itself
    /// and the complement of the accumulator.
    Tclr1(ADDR),
    /// Set the N and Z flags as if comparing the accumulator to the byte at
    /// the specified address, then set that byte to the bitwise OR of itself
    /// and the accumulator.
    Tset1(ADDR),
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

            0x02 => Operation::Set1(0, AddrMode::DirectPage),
            0x12 => Operation::Clr1(0, AddrMode::DirectPage),
            0x22 => Operation::Set1(1, AddrMode::DirectPage),
            0x32 => Operation::Clr1(1, AddrMode::DirectPage),
            0x42 => Operation::Set1(2, AddrMode::DirectPage),
            0x52 => Operation::Clr1(2, AddrMode::DirectPage),
            0x62 => Operation::Set1(3, AddrMode::DirectPage),
            0x72 => Operation::Clr1(3, AddrMode::DirectPage),
            0x82 => Operation::Set1(4, AddrMode::DirectPage),
            0x92 => Operation::Clr1(4, AddrMode::DirectPage),
            0xa2 => Operation::Set1(5, AddrMode::DirectPage),
            0xb2 => Operation::Clr1(5, AddrMode::DirectPage),
            0xc2 => Operation::Set1(6, AddrMode::DirectPage),
            0xd2 => Operation::Clr1(6, AddrMode::DirectPage),
            0xe2 => Operation::Set1(7, AddrMode::DirectPage),
            0xf2 => Operation::Clr1(7, AddrMode::DirectPage),

            0x03 => {
                Operation::Bbs(AddrMode::DirectPage, 0, AddrMode::Relative)
            }
            0x13 => {
                Operation::Bbc(AddrMode::DirectPage, 0, AddrMode::Relative)
            }
            0x23 => {
                Operation::Bbs(AddrMode::DirectPage, 1, AddrMode::Relative)
            }
            0x33 => {
                Operation::Bbc(AddrMode::DirectPage, 1, AddrMode::Relative)
            }
            0x43 => {
                Operation::Bbs(AddrMode::DirectPage, 2, AddrMode::Relative)
            }
            0x53 => {
                Operation::Bbc(AddrMode::DirectPage, 2, AddrMode::Relative)
            }
            0x63 => {
                Operation::Bbs(AddrMode::DirectPage, 3, AddrMode::Relative)
            }
            0x73 => {
                Operation::Bbc(AddrMode::DirectPage, 3, AddrMode::Relative)
            }
            0x83 => {
                Operation::Bbs(AddrMode::DirectPage, 4, AddrMode::Relative)
            }
            0x93 => {
                Operation::Bbc(AddrMode::DirectPage, 4, AddrMode::Relative)
            }
            0xa3 => {
                Operation::Bbs(AddrMode::DirectPage, 5, AddrMode::Relative)
            }
            0xb3 => {
                Operation::Bbc(AddrMode::DirectPage, 5, AddrMode::Relative)
            }
            0xc3 => {
                Operation::Bbs(AddrMode::DirectPage, 6, AddrMode::Relative)
            }
            0xd3 => {
                Operation::Bbc(AddrMode::DirectPage, 6, AddrMode::Relative)
            }
            0xe3 => {
                Operation::Bbs(AddrMode::DirectPage, 7, AddrMode::Relative)
            }
            0xf3 => {
                Operation::Bbc(AddrMode::DirectPage, 7, AddrMode::Relative)
            }

            0x04 => Operation::OrAAddr(AddrMode::DirectPage),
            0x14 => Operation::OrAAddr(AddrMode::XIndexedDirectPage),
            0x24 => Operation::AndAAddr(AddrMode::DirectPage),
            0x34 => Operation::AndAAddr(AddrMode::XIndexedDirectPage),
            0x44 => Operation::EorAAddr(AddrMode::DirectPage),
            0x54 => Operation::EorAAddr(AddrMode::XIndexedDirectPage),
            0x64 => Operation::CmpRegAddr(Reg::A, AddrMode::DirectPage),
            0x74 => {
                Operation::CmpRegAddr(Reg::A, AddrMode::XIndexedDirectPage)
            }
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
            0x67 => Operation::CmpRegAddr(
                Reg::A,
                AddrMode::XIndexedDirectPageIndirect,
            ),
            0x77 => Operation::CmpRegAddr(
                Reg::A,
                AddrMode::DirectPageIndirectYIndexed,
            ),
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

            0x08 => Operation::OrAAddr(AddrMode::Immediate),
            0x18 => Operation::OrAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0x28 => Operation::AndAAddr(AddrMode::Immediate),
            0x38 => Operation::AndAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0x48 => Operation::EorAAddr(AddrMode::Immediate),
            0x58 => Operation::EorAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0x68 => Operation::CmpRegAddr(Reg::A, AddrMode::Immediate),
            0x78 => Operation::CmpAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0x88 => Operation::AdcAAddr(AddrMode::Immediate),
            0x98 => Operation::AdcAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0xa8 => Operation::SbcAAddr(AddrMode::Immediate),
            0xb8 => Operation::SbcAddrAddr(
                AddrMode::DirectPage,
                AddrMode::Immediate,
            ),
            0xc8 => Operation::CmpRegAddr(Reg::X, AddrMode::Immediate),
            0xd8 => Operation::MovAddrReg(AddrMode::DirectPage, Reg::X),
            0xe8 => Operation::MovRegAddr(Reg::A, AddrMode::Immediate),
            0xf8 => Operation::MovRegAddr(Reg::X, AddrMode::DirectPage),

            0x09 => Operation::OrAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),
            0x19 => Operation::OrAddrAddr(
                AddrMode::DirectPageX,
                AddrMode::DirectPageY,
            ),
            0x29 => Operation::AndAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),
            0x39 => Operation::AndAddrAddr(
                AddrMode::DirectPageX,
                AddrMode::DirectPageY,
            ),
            0x49 => Operation::EorAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),
            0x59 => Operation::EorAddrAddr(
                AddrMode::DirectPageX,
                AddrMode::DirectPageY,
            ),
            0x69 => Operation::CmpAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),
            0x79 => Operation::CmpAddrAddr(
                AddrMode::DirectPageX,
                AddrMode::DirectPageY,
            ),
            0x89 => Operation::AdcAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),
            0x99 => Operation::AdcAddrAddr(
                AddrMode::DirectPageX,
                AddrMode::DirectPageY,
            ),
            0xa9 => Operation::SbcAddrAddr(
                AddrMode::DirectPage,
                AddrMode::DirectPage,
            ),
            0xb9 => Operation::SbcAddrAddr(
                AddrMode::DirectPageX,
                AddrMode::DirectPageY,
            ),
            0xc9 => Operation::MovAddrReg(AddrMode::Absolute, Reg::X),
            0xd9 => {
                Operation::MovAddrReg(AddrMode::YIndexedDirectPage, Reg::X)
            }
            0xe9 => Operation::MovRegAddr(Reg::X, AddrMode::Absolute),
            0xf9 => {
                Operation::MovRegAddr(Reg::X, AddrMode::YIndexedDirectPage)
            }

            0x0a => Operation::Or1(AddrMode::AbsoluteBit),
            0x1a => Operation::Decw(AddrMode::DirectPage),
            0x2a => Operation::Or1Inv(AddrMode::AbsoluteBit),
            0x3a => Operation::Incw(AddrMode::DirectPage),
            0x4a => Operation::And1(AddrMode::AbsoluteBit),
            0x5a => Operation::Cmpw(AddrMode::DirectPage),
            0x6a => Operation::And1Inv(AddrMode::AbsoluteBit),
            0x7a => Operation::Addw(AddrMode::DirectPage),
            0x8a => Operation::Eor1(AddrMode::AbsoluteBit),
            0x9a => Operation::Subw(AddrMode::DirectPage),
            0xaa => Operation::Mov1CAddr(AddrMode::AbsoluteBit),
            0xba => Operation::MovwYaAddr(AddrMode::DirectPage),
            0xca => Operation::Mov1AddrC(AddrMode::AbsoluteBit),
            0xda => Operation::MovwAddrYa(AddrMode::DirectPage),
            0xea => Operation::Not1(AddrMode::AbsoluteBit),
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
            0xad => Operation::CmpRegAddr(Reg::Y, AddrMode::Immediate),
            0xbd => Operation::MovRegReg(Reg::Sp, Reg::X),
            0xcd => Operation::MovRegAddr(Reg::X, AddrMode::Immediate),
            0xdd => Operation::MovRegReg(Reg::A, Reg::Y),
            0xed => Operation::Notc,
            0xfd => Operation::MovRegReg(Reg::Y, Reg::A),

            0x0e => Operation::Tset1(AddrMode::Absolute),
            0x1e => Operation::CmpRegAddr(Reg::X, AddrMode::Absolute),
            0x2e => Operation::Cbne(AddrMode::DirectPage, AddrMode::Relative),
            0x3e => Operation::CmpRegAddr(Reg::X, AddrMode::DirectPage),
            0x4e => Operation::Tclr1(AddrMode::Absolute),
            0x5e => Operation::CmpRegAddr(Reg::Y, AddrMode::Absolute),
            0x6e => {
                Operation::DbnzAddr(AddrMode::DirectPage, AddrMode::Relative)
            }
            0x7e => Operation::CmpRegAddr(Reg::Y, AddrMode::DirectPage),
            0x8e => Operation::Pop(Reg::Psw),
            0x9e => Operation::Div,
            0xae => Operation::Pop(Reg::A),
            0xbe => Operation::Das,
            0xce => Operation::Pop(Reg::X),
            0xde => Operation::Cbne(
                AddrMode::XIndexedDirectPage,
                AddrMode::Relative,
            ),
            0xee => Operation::Pop(Reg::Y),
            0xfe => Operation::DbnzY(AddrMode::Relative),

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
            | Instruction::Addw(operand)
            | Instruction::And1(operand)
            | Instruction::And1Inv(operand)
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
            | Instruction::Clr1(_, operand)
            | Instruction::CmpRegAddr(_, operand)
            | Instruction::Cmpw(operand)
            | Instruction::DbnzY(operand)
            | Instruction::DecAddr(operand)
            | Instruction::Decw(operand)
            | Instruction::Eor1(operand)
            | Instruction::EorAAddr(operand)
            | Instruction::IncAddr(operand)
            | Instruction::Incw(operand)
            | Instruction::Jmp(operand)
            | Instruction::LsrAddr(operand)
            | Instruction::Mov1AddrC(operand)
            | Instruction::MovAddrReg(operand, _)
            | Instruction::Mov1CAddr(operand)
            | Instruction::MovRegAddr(_, operand)
            | Instruction::MovwAddrYa(operand)
            | Instruction::MovwYaAddr(operand)
            | Instruction::Not1(operand)
            | Instruction::Or1(operand)
            | Instruction::Or1Inv(operand)
            | Instruction::OrAAddr(operand)
            | Instruction::Pcall(operand)
            | Instruction::RolAddr(operand)
            | Instruction::RorAddr(operand)
            | Instruction::SbcAAddr(operand)
            | Instruction::Set1(_, operand)
            | Instruction::Subw(operand)
            | Instruction::Tclr1(operand)
            | Instruction::Tset1(operand) => 1 + operand.size(),
            Instruction::AdcAddrAddr(op1, op2)
            | Instruction::AndAddrAddr(op1, op2)
            | Instruction::Bbc(op1, _, op2)
            | Instruction::Bbs(op1, _, op2)
            | Instruction::Cbne(op1, op2)
            | Instruction::CmpAddrAddr(op1, op2)
            | Instruction::DbnzAddr(op1, op2)
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
        match Operation::from_opcode(bus.peek_byte(Addr::from(pc))) {
            Operation::AdcAAddr(mode) => {
                Instruction::AdcAAddr(mode.decode(bus, pc))
            }
            Operation::AdcAddrAddr(mode1, mode2) => {
                let op2 = mode2.decode(bus, pc);
                let op1 =
                    mode1.decode(bus, pc.wrapping_add(op2.size() as u16));
                Instruction::AdcAddrAddr(op1, op2)
            }
            Operation::Addw(mode) => Instruction::Addw(mode.decode(bus, pc)),
            Operation::And1(mode) => Instruction::And1(mode.decode(bus, pc)),
            Operation::And1Inv(mode) => {
                Instruction::And1Inv(mode.decode(bus, pc))
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
            Operation::Bbc(mode1, bit, mode2) => {
                let op1 = mode1.decode(bus, pc);
                let op2 =
                    mode2.decode(bus, pc.wrapping_add(op1.size() as u16));
                Instruction::Bbc(op1, bit, op2)
            }
            Operation::Bbs(mode1, bit, mode2) => {
                let op1 = mode1.decode(bus, pc);
                let op2 =
                    mode2.decode(bus, pc.wrapping_add(op1.size() as u16));
                Instruction::Bbs(op1, bit, op2)
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
            Operation::Cbne(mode1, mode2) => {
                let op1 = mode1.decode(bus, pc);
                let op2 =
                    mode2.decode(bus, pc.wrapping_add(op1.size() as u16));
                Instruction::Cbne(op1, op2)
            }
            Operation::Clr1(bit, mode) => {
                Instruction::Clr1(bit, mode.decode(bus, pc))
            }
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
            Operation::Cmpw(mode) => Instruction::Cmpw(mode.decode(bus, pc)),
            Operation::Daa => Instruction::Daa,
            Operation::Das => Instruction::Das,
            Operation::DbnzAddr(mode1, mode2) => {
                let op1 = mode1.decode(bus, pc);
                let op2 =
                    mode2.decode(bus, pc.wrapping_add(op1.size() as u16));
                Instruction::DbnzAddr(op1, op2)
            }
            Operation::DecAddr(mode) => {
                Instruction::DecAddr(mode.decode(bus, pc))
            }
            Operation::DbnzY(mode) => Instruction::DbnzY(mode.decode(bus, pc)),
            Operation::DecReg(reg) => Instruction::DecReg(reg),
            Operation::Decw(mode) => Instruction::Decw(mode.decode(bus, pc)),
            Operation::Di => Instruction::Di,
            Operation::Div => Instruction::Div,
            Operation::Ei => Instruction::Ei,
            Operation::Eor1(mode) => Instruction::Eor1(mode.decode(bus, pc)),
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
            Operation::Mov1AddrC(mode) => {
                Instruction::Mov1AddrC(mode.decode(bus, pc))
            }
            Operation::Mov1CAddr(mode) => {
                Instruction::Mov1CAddr(mode.decode(bus, pc))
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
            Operation::MovwAddrYa(mode) => {
                Instruction::MovwAddrYa(mode.decode(bus, pc))
            }
            Operation::MovwYaAddr(mode) => {
                Instruction::MovwYaAddr(mode.decode(bus, pc))
            }
            Operation::Mul => Instruction::Mul,
            Operation::Nop => Instruction::Nop,
            Operation::Not1(mode) => Instruction::Not1(mode.decode(bus, pc)),
            Operation::Notc => Instruction::Notc,
            Operation::Or1(mode) => Instruction::Or1(mode.decode(bus, pc)),
            Operation::Or1Inv(mode) => {
                Instruction::Or1Inv(mode.decode(bus, pc))
            }
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
            Operation::Set1(bit, mode) => {
                Instruction::Set1(bit, mode.decode(bus, pc))
            }
            Operation::Setc => Instruction::Setc,
            Operation::Setp => Instruction::Setp,
            Operation::Sleep => Instruction::Sleep,
            Operation::Stop => Instruction::Stop,
            Operation::Subw(mode) => Instruction::Subw(mode.decode(bus, pc)),
            Operation::Tcall(hp) => Instruction::Tcall(hp),
            Operation::Tclr1(mode) => Instruction::Tclr1(mode.decode(bus, pc)),
            Operation::Tset1(mode) => Instruction::Tset1(mode.decode(bus, pc)),
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
            Instruction::Addw(op) => {
                format!("ADDW YA, {}", op.format(bus, next))
            }
            Instruction::And1(op) => {
                format!("AND1 C, {}", op.format(bus, next))
            }
            Instruction::And1Inv(op) => {
                format!("AND1 C, /{}", op.format(bus, next))
            }
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
            Instruction::Bbc(op1, bit, op2) => format!(
                "BBC {}, {bit}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::Bbs(op1, bit, op2) => format!(
                "BBS {}, {bit}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
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
            Instruction::Cbne(op1, op2) => format!(
                "CBNE {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::Clr1(bit, op) => {
                format!("CLR1 {}, {bit}", op.format(bus, next))
            }
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
            Instruction::Cmpw(op) => {
                format!("CMPW YA, {}", op.format(bus, next))
            }
            Instruction::Daa => "DAA A".to_string(),
            Instruction::Das => "DAS A".to_string(),
            Instruction::DbnzAddr(op1, op2) => format!(
                "DBNZ {}, {}",
                op1.format(bus, next),
                op2.format(bus, next)
            ),
            Instruction::DecAddr(op) => {
                format!("DEC {}", op.format(bus, next))
            }
            Instruction::DbnzY(op) => {
                format!("DBNZ Y, {}", op.format(bus, next))
            }
            Instruction::DecReg(reg) => format!("DEC {reg}"),
            Instruction::Decw(op) => format!("DECW {}", op.format(bus, next)),
            Instruction::Di => "DI".to_string(),
            Instruction::Div => "DIV YA, X".to_string(),
            Instruction::Ei => "EI".to_string(),
            Instruction::Eor1(op) => {
                format!("EOR1 C, {}", op.format(bus, next))
            }
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
            Instruction::Mov1AddrC(op) => {
                format!("MOV1 {}, C", op.format(bus, next))
            }
            Instruction::Mov1CAddr(op) => {
                format!("MOV1 C, {}", op.format(bus, next))
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
            Instruction::MovwAddrYa(op) => {
                format!("MOVW {}, YA", op.format(bus, next))
            }
            Instruction::MovwYaAddr(op) => {
                format!("MOVW YA, {}", op.format(bus, next))
            }
            Instruction::Mul => "MUL YA".to_string(),
            Instruction::Nop => "NOP".to_string(),
            Instruction::Not1(op) => format!("NOT1 {}", op.format(bus, next)),
            Instruction::Notc => "NOTC".to_string(),
            Instruction::Or1(op) => format!("OR1 C, {}", op.format(bus, next)),
            Instruction::Or1Inv(op) => {
                format!("OR1 C, /{}", op.format(bus, next))
            }
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
            Instruction::Set1(bit, op) => {
                format!("SET1 {}, {bit}", op.format(bus, next))
            }
            Instruction::Setc => "SETC".to_string(),
            Instruction::Setp => "SETP".to_string(),
            Instruction::Sleep => "SLEEP".to_string(),
            Instruction::Stop => "STOP".to_string(),
            Instruction::Subw(op) => {
                format!("SUBW YA, {}", op.format(bus, next))
            }
            Instruction::Tcall(hp) => {
                format!("TCALL {}", 0xdeu8.wrapping_sub(hp) >> 1)
            }
            Instruction::Tclr1(op) => {
                format!("TCLR1 {}", op.format(bus, next))
            }
            Instruction::Tset1(op) => {
                format!("TSET1 {}", op.format(bus, next))
            }
            Instruction::Xcn => "XCN A".to_string(),
        }
    }
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
        let (size, string) = disassemble_with_bus(&*make_test_bus(code));
        assert_eq!(size, code.len() as u32);
        string
    }

    fn disassemble_with_label(code: &[u8], addr: u16, label: &str) -> String {
        let labels = HashMap::from([(label.to_string(), Addr::from(addr))]);
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
    fn disassemble_adc() {
        assert_eq!(disassemble(&[0x88, 0x12]), "ADC A, #$12");
        assert_eq!(disassemble(&[0x86]), "ADC A, (X)");
        assert_eq!(disassemble(&[0x84, 0x34]), "ADC A, $34");
        assert_eq!(disassemble(&[0x94, 0x56]), "ADC A, $56 + X");
        assert_eq!(disassemble(&[0x85, 0x34, 0x12]), "ADC A, !$1234");
        assert_eq!(disassemble(&[0x95, 0x34, 0x12]), "ADC A, !$1234 + X");
        assert_eq!(disassemble(&[0x96, 0x34, 0x12]), "ADC A, !$1234 + Y");
        assert_eq!(disassemble(&[0x87, 0xcd]), "ADC A, [$cd + X]");
        assert_eq!(disassemble(&[0x97, 0xab]), "ADC A, [$ab] + Y");
        assert_eq!(disassemble(&[0x99]), "ADC (X), (Y)");
        assert_eq!(disassemble(&[0x89, 0x34, 0x12]), "ADC $12, $34");
        assert_eq!(disassemble(&[0x98, 0x78, 0x56]), "ADC $56, #$78");
    }

    #[test]
    fn disassemble_and() {
        assert_eq!(disassemble(&[0x28, 0x12]), "AND A, #$12");
        assert_eq!(disassemble(&[0x26]), "AND A, (X)");
        assert_eq!(disassemble(&[0x24, 0x34]), "AND A, $34");
        assert_eq!(disassemble(&[0x34, 0x56]), "AND A, $56 + X");
        assert_eq!(disassemble(&[0x25, 0x34, 0x12]), "AND A, !$1234");
        assert_eq!(disassemble(&[0x35, 0x34, 0x12]), "AND A, !$1234 + X");
        assert_eq!(disassemble(&[0x36, 0x34, 0x12]), "AND A, !$1234 + Y");
        assert_eq!(disassemble(&[0x27, 0xcd]), "AND A, [$cd + X]");
        assert_eq!(disassemble(&[0x37, 0xab]), "AND A, [$ab] + Y");
        assert_eq!(disassemble(&[0x39]), "AND (X), (Y)");
        assert_eq!(disassemble(&[0x29, 0x34, 0x12]), "AND $12, $34");
        assert_eq!(disassemble(&[0x38, 0x78, 0x56]), "AND $56, #$78");
    }

    #[test]
    fn disassemble_bbc() {
        assert_eq!(disassemble(&[0x13, 0x12, 0x31]), "BBC $12, 0, $0034");
        assert_eq!(disassemble(&[0x33, 0x56, 0x75]), "BBC $56, 1, $0078");
        assert_eq!(disassemble(&[0x53, 0x9a, 0xfc]), "BBC $9a, 2, $ffff");
        assert_eq!(disassemble(&[0x73, 0xbc, 0xfd]), "BBC $bc, 3, $0000");
        assert_eq!(disassemble(&[0x93, 0x12, 0x31]), "BBC $12, 4, $0034");
        assert_eq!(disassemble(&[0xb3, 0x56, 0x75]), "BBC $56, 5, $0078");
        assert_eq!(disassemble(&[0xd3, 0x9a, 0xfc]), "BBC $9a, 6, $ffff");
        assert_eq!(disassemble(&[0xf3, 0xbc, 0xfd]), "BBC $bc, 7, $0000");
    }

    #[test]
    fn disassemble_bbs() {
        assert_eq!(disassemble(&[0x03, 0x12, 0x31]), "BBS $12, 0, $0034");
        assert_eq!(disassemble(&[0x23, 0x56, 0x75]), "BBS $56, 1, $0078");
        assert_eq!(disassemble(&[0x43, 0x9a, 0xfc]), "BBS $9a, 2, $ffff");
        assert_eq!(disassemble(&[0x63, 0xbc, 0xfd]), "BBS $bc, 3, $0000");
        assert_eq!(disassemble(&[0x83, 0x12, 0x31]), "BBS $12, 4, $0034");
        assert_eq!(disassemble(&[0xa3, 0x56, 0x75]), "BBS $56, 5, $0078");
        assert_eq!(disassemble(&[0xc3, 0x9a, 0xfc]), "BBS $9a, 6, $ffff");
        assert_eq!(disassemble(&[0xe3, 0xbc, 0xfd]), "BBS $bc, 7, $0000");
    }

    #[test]
    fn disassemble_bit() {
        assert_eq!(disassemble(&[0x4a, 0x00, 0x10]), "AND1 C, $1000, 0");
        assert_eq!(disassemble(&[0x6a, 0x00, 0xa0]), "AND1 C, /$0000, 5");
        assert_eq!(disassemble(&[0x8a, 0x23, 0x41]), "EOR1 C, $0123, 2");
        assert_eq!(disassemble(&[0xaa, 0xff, 0x3f]), "MOV1 C, $1fff, 1");
        assert_eq!(disassemble(&[0xca, 0x12, 0xe0]), "MOV1 $0012, 7, C");
        assert_eq!(disassemble(&[0xea, 0x34, 0xd2]), "NOT1 $1234, 6");
        assert_eq!(disassemble(&[0x0a, 0x01, 0x90]), "OR1 C, $1001, 4");
        assert_eq!(disassemble(&[0x2a, 0x02, 0x60]), "OR1 C, /$0002, 3");
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
    fn disassemble_cbne() {
        assert_eq!(disassemble(&[0x2e, 0x12, 0x34]), "CBNE $12, $0037");
        assert_eq!(disassemble(&[0xde, 0x56, 0x78]), "CBNE $56 + X, $007b");
        assert_eq!(
            disassemble_with_label(&[0x2e, 0x12, 0x34], 0x0012, "foo"),
            "CBNE foo, $0037"
        );
        assert_eq!(
            disassemble_with_label(&[0xde, 0x56, 0x78], 0x007b, "foo"),
            "CBNE $56 + X, foo"
        );
    }

    #[test]
    fn disassemble_clr1() {
        assert_eq!(disassemble(&[0x12, 0x12]), "CLR1 $12, 0");
        assert_eq!(disassemble(&[0x32, 0x56]), "CLR1 $56, 1");
        assert_eq!(disassemble(&[0x52, 0x9a]), "CLR1 $9a, 2");
        assert_eq!(disassemble(&[0x72, 0xbc]), "CLR1 $bc, 3");
        assert_eq!(disassemble(&[0x92, 0x12]), "CLR1 $12, 4");
        assert_eq!(disassemble(&[0xb2, 0x56]), "CLR1 $56, 5");
        assert_eq!(disassemble(&[0xd2, 0x9a]), "CLR1 $9a, 6");
        assert_eq!(disassemble(&[0xf2, 0xbc]), "CLR1 $bc, 7");
    }

    #[test]
    fn disassemble_cmp() {
        assert_eq!(disassemble(&[0x68, 0x12]), "CMP A, #$12");
        assert_eq!(disassemble(&[0x66]), "CMP A, (X)");
        assert_eq!(disassemble(&[0x64, 0x34]), "CMP A, $34");
        assert_eq!(disassemble(&[0x74, 0x56]), "CMP A, $56 + X");
        assert_eq!(disassemble(&[0x65, 0x34, 0x12]), "CMP A, !$1234");
        assert_eq!(disassemble(&[0x75, 0x34, 0x12]), "CMP A, !$1234 + X");
        assert_eq!(disassemble(&[0x76, 0x34, 0x12]), "CMP A, !$1234 + Y");
        assert_eq!(disassemble(&[0x67, 0xcd]), "CMP A, [$cd + X]");
        assert_eq!(disassemble(&[0x77, 0xab]), "CMP A, [$ab] + Y");
        assert_eq!(disassemble(&[0x79]), "CMP (X), (Y)");
        assert_eq!(disassemble(&[0x69, 0x34, 0x12]), "CMP $12, $34");
        assert_eq!(disassemble(&[0x78, 0x78, 0x56]), "CMP $56, #$78");
        assert_eq!(disassemble(&[0xc8, 0x12]), "CMP X, #$12");
        assert_eq!(disassemble(&[0x3e, 0x34]), "CMP X, $34");
        assert_eq!(disassemble(&[0x1e, 0x34, 0x12]), "CMP X, !$1234");
        assert_eq!(disassemble(&[0xad, 0x12]), "CMP Y, #$12");
        assert_eq!(disassemble(&[0x7e, 0x34]), "CMP Y, $34");
        assert_eq!(disassemble(&[0x5e, 0x34, 0x12]), "CMP Y, !$1234");
    }

    #[test]
    fn disassemble_dbnz() {
        assert_eq!(disassemble(&[0xfe, 0x12]), "DBNZ Y, $0014");
        assert_eq!(disassemble(&[0x6e, 0x34, 0x12]), "DBNZ $34, $0015");
        assert_eq!(
            disassemble_with_label(&[0xfe, 0x12], 0x0014, "foo"),
            "DBNZ Y, foo"
        );
        assert_eq!(
            disassemble_with_label(&[0x6e, 0x34, 0x12], 0x0034, "foo"),
            "DBNZ foo, $0015"
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
    }

    #[test]
    fn disassemble_eor() {
        assert_eq!(disassemble(&[0x48, 0x12]), "EOR A, #$12");
        assert_eq!(disassemble(&[0x46]), "EOR A, (X)");
        assert_eq!(disassemble(&[0x44, 0x34]), "EOR A, $34");
        assert_eq!(disassemble(&[0x54, 0x56]), "EOR A, $56 + X");
        assert_eq!(disassemble(&[0x45, 0x34, 0x12]), "EOR A, !$1234");
        assert_eq!(disassemble(&[0x55, 0x34, 0x12]), "EOR A, !$1234 + X");
        assert_eq!(disassemble(&[0x56, 0x34, 0x12]), "EOR A, !$1234 + Y");
        assert_eq!(disassemble(&[0x47, 0xcd]), "EOR A, [$cd + X]");
        assert_eq!(disassemble(&[0x57, 0xab]), "EOR A, [$ab] + Y");
        assert_eq!(disassemble(&[0x59]), "EOR (X), (Y)");
        assert_eq!(disassemble(&[0x49, 0x34, 0x12]), "EOR $12, $34");
        assert_eq!(disassemble(&[0x58, 0x78, 0x56]), "EOR $56, #$78");
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
        assert_eq!(disassemble(&[0xe8, 0x00]), "MOV A, #$00");
        assert_eq!(disassemble(&[0xe6]), "MOV A, (X)");
        assert_eq!(disassemble(&[0xbf]), "MOV A, (X)+");
        assert_eq!(disassemble(&[0xe4, 0xf5]), "MOV A, $f5");
        assert_eq!(disassemble(&[0xf4, 0xf5]), "MOV A, $f5 + X");
        assert_eq!(disassemble(&[0xe5, 0x34, 0x12]), "MOV A, !$1234");
        assert_eq!(disassemble(&[0xf5, 0x34, 0x12]), "MOV A, !$1234 + X");
        assert_eq!(disassemble(&[0xf6, 0x34, 0x12]), "MOV A, !$1234 + Y");
        assert_eq!(disassemble(&[0xe7, 0xcd]), "MOV A, [$cd + X]");
        assert_eq!(disassemble(&[0xf7, 0xcd]), "MOV A, [$cd] + Y");
        assert_eq!(disassemble(&[0xcd, 0xef]), "MOV X, #$ef");
        assert_eq!(disassemble(&[0xf8, 0xf4]), "MOV X, $f4");
        assert_eq!(disassemble(&[0xf9, 0xf4]), "MOV X, $f4 + Y");
        assert_eq!(disassemble(&[0xe9, 0x34, 0x12]), "MOV X, !$1234");
        assert_eq!(disassemble(&[0x8d, 0xef]), "MOV Y, #$ef");
        assert_eq!(disassemble(&[0xeb, 0xf4]), "MOV Y, $f4");
        assert_eq!(disassemble(&[0xfb, 0xf4]), "MOV Y, $f4 + X");
        assert_eq!(disassemble(&[0xec, 0x34, 0x12]), "MOV Y, !$1234");
        assert_eq!(disassemble(&[0xc6]), "MOV (X), A");
        assert_eq!(disassemble(&[0xaf]), "MOV (X)+, A");
        assert_eq!(disassemble(&[0xc4, 0xfe]), "MOV $fe, A");
        assert_eq!(disassemble(&[0xd4, 0xfe]), "MOV $fe + X, A");
        assert_eq!(disassemble(&[0xc5, 0x34, 0x12]), "MOV !$1234, A");
        assert_eq!(disassemble(&[0xd5, 0x34, 0x12]), "MOV !$1234 + X, A");
        assert_eq!(disassemble(&[0xd6, 0x34, 0x12]), "MOV !$1234 + Y, A");
        assert_eq!(disassemble(&[0xc7, 0xfe]), "MOV [$fe + X], A");
        assert_eq!(disassemble(&[0xd7, 0xfe]), "MOV [$fe] + Y, A");
        assert_eq!(disassemble(&[0xd8, 0xf4]), "MOV $f4, X");
        assert_eq!(disassemble(&[0xd9, 0xf4]), "MOV $f4 + Y, X");
        assert_eq!(disassemble(&[0xc9, 0x34, 0x12]), "MOV !$1234, X");
        assert_eq!(disassemble(&[0xcb, 0xf4]), "MOV $f4, Y");
        assert_eq!(disassemble(&[0xdb, 0xf4]), "MOV $f4 + X, Y");
        assert_eq!(disassemble(&[0xcc, 0x34, 0x12]), "MOV !$1234, Y");
        assert_eq!(disassemble(&[0x7d]), "MOV A, X");
        assert_eq!(disassemble(&[0xdd]), "MOV A, Y");
        assert_eq!(disassemble(&[0x5d]), "MOV X, A");
        assert_eq!(disassemble(&[0xfd]), "MOV Y, A");
        assert_eq!(disassemble(&[0x9d]), "MOV X, SP");
        assert_eq!(disassemble(&[0xbd]), "MOV SP, X");
        assert_eq!(disassemble(&[0xfa, 0xbb, 0xf4]), "MOV $f4, $bb");
        assert_eq!(disassemble(&[0x8f, 0xaa, 0xf4]), "MOV $f4, #$aa");
    }

    #[test]
    fn disassemble_or() {
        assert_eq!(disassemble(&[0x08, 0x12]), "OR A, #$12");
        assert_eq!(disassemble(&[0x06]), "OR A, (X)");
        assert_eq!(disassemble(&[0x04, 0x34]), "OR A, $34");
        assert_eq!(disassemble(&[0x14, 0x56]), "OR A, $56 + X");
        assert_eq!(disassemble(&[0x05, 0x34, 0x12]), "OR A, !$1234");
        assert_eq!(disassemble(&[0x15, 0x34, 0x12]), "OR A, !$1234 + X");
        assert_eq!(disassemble(&[0x16, 0x34, 0x12]), "OR A, !$1234 + Y");
        assert_eq!(disassemble(&[0x07, 0xcd]), "OR A, [$cd + X]");
        assert_eq!(disassemble(&[0x17, 0xab]), "OR A, [$ab] + Y");
        assert_eq!(disassemble(&[0x19]), "OR (X), (Y)");
        assert_eq!(disassemble(&[0x09, 0x34, 0x12]), "OR $12, $34");
        assert_eq!(disassemble(&[0x18, 0x78, 0x56]), "OR $56, #$78");
    }

    #[test]
    fn disassemble_pcall() {
        assert_eq!(disassemble(&[0x4f, 0x37]), "PCALL $ff37");
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
    fn disassemble_sbc() {
        assert_eq!(disassemble(&[0xa8, 0x12]), "SBC A, #$12");
        assert_eq!(disassemble(&[0xa6]), "SBC A, (X)");
        assert_eq!(disassemble(&[0xa4, 0x34]), "SBC A, $34");
        assert_eq!(disassemble(&[0xb4, 0x56]), "SBC A, $56 + X");
        assert_eq!(disassemble(&[0xa5, 0x34, 0x12]), "SBC A, !$1234");
        assert_eq!(disassemble(&[0xb5, 0x34, 0x12]), "SBC A, !$1234 + X");
        assert_eq!(disassemble(&[0xb6, 0x34, 0x12]), "SBC A, !$1234 + Y");
        assert_eq!(disassemble(&[0xa7, 0xcd]), "SBC A, [$cd + X]");
        assert_eq!(disassemble(&[0xb7, 0xab]), "SBC A, [$ab] + Y");
        assert_eq!(disassemble(&[0xb9]), "SBC (X), (Y)");
        assert_eq!(disassemble(&[0xa9, 0x34, 0x12]), "SBC $12, $34");
        assert_eq!(disassemble(&[0xb8, 0x78, 0x56]), "SBC $56, #$78");
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
    fn disassemble_rol() {
        assert_eq!(disassemble(&[0x3c]), "ROL A");
        assert_eq!(disassemble(&[0x2b, 0x12]), "ROL $12");
        assert_eq!(disassemble(&[0x3b, 0x34]), "ROL $34 + X");
        assert_eq!(disassemble(&[0x2c, 0x34, 0x12]), "ROL !$1234");
    }

    #[test]
    fn disassemble_ror() {
        assert_eq!(disassemble(&[0x7c]), "ROR A");
        assert_eq!(disassemble(&[0x6b, 0x12]), "ROR $12");
        assert_eq!(disassemble(&[0x7b, 0x34]), "ROR $34 + X");
        assert_eq!(disassemble(&[0x6c, 0x34, 0x12]), "ROR !$1234");
    }

    #[test]
    fn disassemble_set1() {
        assert_eq!(disassemble(&[0x02, 0x12]), "SET1 $12, 0");
        assert_eq!(disassemble(&[0x22, 0x56]), "SET1 $56, 1");
        assert_eq!(disassemble(&[0x42, 0x9a]), "SET1 $9a, 2");
        assert_eq!(disassemble(&[0x62, 0xbc]), "SET1 $bc, 3");
        assert_eq!(disassemble(&[0x82, 0x12]), "SET1 $12, 4");
        assert_eq!(disassemble(&[0xa2, 0x56]), "SET1 $56, 5");
        assert_eq!(disassemble(&[0xc2, 0x9a]), "SET1 $9a, 6");
        assert_eq!(disassemble(&[0xe2, 0xbc]), "SET1 $bc, 7");
    }

    #[test]
    fn disassemble_tcall() {
        assert_eq!(disassemble(&[0x01]), "TCALL 0");
        assert_eq!(disassemble(&[0x11]), "TCALL 1");
        assert_eq!(disassemble(&[0x21]), "TCALL 2");
        assert_eq!(disassemble(&[0x31]), "TCALL 3");
        assert_eq!(disassemble(&[0x41]), "TCALL 4");
        assert_eq!(disassemble(&[0x51]), "TCALL 5");
        assert_eq!(disassemble(&[0x61]), "TCALL 6");
        assert_eq!(disassemble(&[0x71]), "TCALL 7");
        assert_eq!(disassemble(&[0x81]), "TCALL 8");
        assert_eq!(disassemble(&[0x91]), "TCALL 9");
        assert_eq!(disassemble(&[0xa1]), "TCALL 10");
        assert_eq!(disassemble(&[0xb1]), "TCALL 11");
        assert_eq!(disassemble(&[0xc1]), "TCALL 12");
        assert_eq!(disassemble(&[0xd1]), "TCALL 13");
        assert_eq!(disassemble(&[0xe1]), "TCALL 14");
        assert_eq!(disassemble(&[0xf1]), "TCALL 15");
    }

    #[test]
    fn disassemble_tclr1_tset1() {
        assert_eq!(disassemble(&[0x4e, 0x34, 0x12]), "TCLR1 !$1234");
        assert_eq!(disassemble(&[0x0e, 0x78, 0x56]), "TSET1 !$5678");
    }

    #[test]
    fn disassemble_word() {
        assert_eq!(disassemble(&[0x7a, 0x12]), "ADDW YA, $12");
        assert_eq!(disassemble(&[0x5a, 0x34]), "CMPW YA, $34");
        assert_eq!(disassemble(&[0x1a, 0x56]), "DECW $56");
        assert_eq!(disassemble(&[0x3a, 0x78]), "INCW $78");
        assert_eq!(disassemble(&[0xba, 0x9a]), "MOVW YA, $9a");
        assert_eq!(disassemble(&[0xda, 0xbc]), "MOVW $bc, YA");
        assert_eq!(disassemble(&[0x9a, 0xde]), "SUBW YA, $de");
    }
}

//===========================================================================//
