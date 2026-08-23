use super::build::AddrMode;

//===========================================================================//

pub(super) const ARCH_SM83: &str = "SM83";
pub(super) const RES_SM83: &[&str] = &[
    "A", "AF", "B", "BC", "C", "D", "DE", "E", "H", "HL", "L", "NC", "NZ",
    "SP", "Z",
];
// TODO: fill in the rest of these
pub(super) const MACROS_SM83: &[(&str, u8, AddrMode)] = &[
    ("CALL", 0xcd, AddrMode::Addr16),
    ("CCF", 0x3d, AddrMode::Implied),
    ("CPL", 0x2f, AddrMode::Implied),
    ("DAA", 0x27, AddrMode::Implied),
    ("DEC", 0x05, AddrMode::Reg("B")),
    ("DEC", 0x0d, AddrMode::Reg("C")),
    ("DEC", 0x15, AddrMode::Reg("D")),
    ("DEC", 0x1d, AddrMode::Reg("E")),
    ("DEC", 0x25, AddrMode::Reg("H")),
    ("DEC", 0x2d, AddrMode::Reg("L")),
    ("DEC", 0x3d, AddrMode::Reg("A")),
    ("DI", 0xf3, AddrMode::Implied),
    ("EI", 0xfb, AddrMode::Implied),
    ("HALT", 0x76, AddrMode::Implied),
    ("JP", 0xc3, AddrMode::Addr16),
    ("JR", 0x18, AddrMode::Relative8),
    ("LD", 0x40, AddrMode::RegCommaReg("B", "B")),
    ("LD", 0x41, AddrMode::RegCommaReg("B", "C")),
    ("LD", 0x42, AddrMode::RegCommaReg("B", "D")),
    ("LD", 0x43, AddrMode::RegCommaReg("B", "E")),
    ("LD", 0x44, AddrMode::RegCommaReg("B", "H")),
    ("LD", 0x45, AddrMode::RegCommaReg("B", "L")),
    ("LD", 0x47, AddrMode::RegCommaReg("B", "A")),
    ("LD", 0x48, AddrMode::RegCommaReg("C", "B")),
    ("LD", 0x49, AddrMode::RegCommaReg("C", "C")),
    ("LD", 0x4a, AddrMode::RegCommaReg("C", "D")),
    ("LD", 0x4b, AddrMode::RegCommaReg("C", "E")),
    ("LD", 0x4c, AddrMode::RegCommaReg("C", "H")),
    ("LD", 0x4d, AddrMode::RegCommaReg("C", "L")),
    ("LD", 0x4f, AddrMode::RegCommaReg("C", "A")),
    ("NOP", 0x00, AddrMode::Implied),
    ("RET", 0xc9, AddrMode::Implied),
    ("RETI", 0xd9, AddrMode::Implied),
    ("RLA", 0x17, AddrMode::Implied),
    ("RLCA", 0x07, AddrMode::Implied),
    ("RRA", 0x1f, AddrMode::Implied),
    ("RRCA", 0x0f, AddrMode::Implied),
    ("SCF", 0x37, AddrMode::Implied),
    ("STOP", 0x10, AddrMode::Implied),
];

//===========================================================================//
