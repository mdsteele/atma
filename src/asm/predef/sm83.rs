use super::build::AddrMode;

//===========================================================================//

pub(super) const ARCH_SM83: &str = "SM83";
pub(super) const RES_SM83: &[&str] = &[
    "A", "AF", "B", "BC", "C", "D", "DE", "E", "H", "HL", "L", "NC", "NZ",
    "SP", "Z",
];
// TODO: fill in the rest of these
pub(super) const MACROS_SM83: &[(&str, &[u8], AddrMode)] = &[
    // TODO: ADC opcodes
    // TODO: ADD opcodes
    // TODO: AND opcodes
    // TODO: BIT opcodes
    ("CALL", &[0xcd], AddrMode::Addr16),
    ("CCF", &[0x3f], AddrMode::Implied),
    // TODO: CP opcodes
    ("CPL", &[0x2f], AddrMode::Implied),
    ("DAA", &[0x27], AddrMode::Implied),
    ("DEC", &[0x05], AddrMode::Reg("B")),
    ("DEC", &[0x0b], AddrMode::Reg("BC")),
    ("DEC", &[0x0d], AddrMode::Reg("C")),
    ("DEC", &[0x15], AddrMode::Reg("D")),
    ("DEC", &[0x1b], AddrMode::Reg("DE")),
    ("DEC", &[0x1d], AddrMode::Reg("E")),
    ("DEC", &[0x25], AddrMode::Reg("H")),
    ("DEC", &[0x2b], AddrMode::Reg("HL")),
    ("DEC", &[0x2d], AddrMode::Reg("L")),
    ("DEC", &[0x35], AddrMode::BracRegKets("HL")),
    ("DEC", &[0x3b], AddrMode::Reg("SP")),
    ("DEC", &[0x3d], AddrMode::Reg("A")),
    ("DI", &[0xf3], AddrMode::Implied),
    ("EI", &[0xfb], AddrMode::Implied),
    ("HALT", &[0x76], AddrMode::Implied),
    ("INC", &[0x03], AddrMode::Reg("BC")),
    ("INC", &[0x04], AddrMode::Reg("B")),
    ("INC", &[0x0c], AddrMode::Reg("C")),
    ("INC", &[0x13], AddrMode::Reg("DE")),
    ("INC", &[0x14], AddrMode::Reg("D")),
    ("INC", &[0x1c], AddrMode::Reg("E")),
    ("INC", &[0x23], AddrMode::Reg("HL")),
    ("INC", &[0x24], AddrMode::Reg("H")),
    ("INC", &[0x2c], AddrMode::Reg("L")),
    ("INC", &[0x33], AddrMode::Reg("SP")),
    ("INC", &[0x34], AddrMode::BracRegKets("HL")),
    ("INC", &[0x3c], AddrMode::Reg("A")),
    ("JP", &[0xc3], AddrMode::Addr16),
    // TODO: other JP opcodes
    ("JR", &[0x18], AddrMode::Relative8),
    // TODO: other JR opcodes
    ("LD", &[0x40], AddrMode::RegCommaReg("B", "B")),
    ("LD", &[0x41], AddrMode::RegCommaReg("B", "C")),
    ("LD", &[0x42], AddrMode::RegCommaReg("B", "D")),
    ("LD", &[0x43], AddrMode::RegCommaReg("B", "E")),
    ("LD", &[0x44], AddrMode::RegCommaReg("B", "H")),
    ("LD", &[0x45], AddrMode::RegCommaReg("B", "L")),
    ("LD", &[0x47], AddrMode::RegCommaReg("B", "A")),
    ("LD", &[0x48], AddrMode::RegCommaReg("C", "B")),
    ("LD", &[0x49], AddrMode::RegCommaReg("C", "C")),
    ("LD", &[0x4a], AddrMode::RegCommaReg("C", "D")),
    ("LD", &[0x4b], AddrMode::RegCommaReg("C", "E")),
    ("LD", &[0x4c], AddrMode::RegCommaReg("C", "H")),
    ("LD", &[0x4d], AddrMode::RegCommaReg("C", "L")),
    ("LD", &[0x4f], AddrMode::RegCommaReg("C", "A")),
    // TODO: other LD opcodes
    // TODO: LDH opcodes
    ("NOP", &[0x00], AddrMode::Implied),
    // TODO: OR opcodes
    // TODO: POP opcodes
    // TODO: PUSH opcodes
    // TODO: RES opcodes
    ("RET", &[0xc9], AddrMode::Implied),
    ("RET", &[0xc0], AddrMode::Reg("NZ")),
    ("RET", &[0xc8], AddrMode::Reg("Z")),
    ("RET", &[0xd0], AddrMode::Reg("NC")),
    ("RET", &[0xd8], AddrMode::Reg("C")),
    ("RETI", &[0xd9], AddrMode::Implied),
    ("RL", &[0xcb, 0x10], AddrMode::Reg("B")),
    ("RL", &[0xcb, 0x11], AddrMode::Reg("C")),
    ("RL", &[0xcb, 0x12], AddrMode::Reg("D")),
    ("RL", &[0xcb, 0x13], AddrMode::Reg("E")),
    ("RL", &[0xcb, 0x14], AddrMode::Reg("H")),
    ("RL", &[0xcb, 0x15], AddrMode::Reg("L")),
    ("RL", &[0xcb, 0x16], AddrMode::BracRegKets("HL")),
    ("RL", &[0xcb, 0x17], AddrMode::Reg("A")),
    ("RLA", &[0x17], AddrMode::Implied),
    ("RLC", &[0xcb, 0x00], AddrMode::Reg("B")),
    ("RLC", &[0xcb, 0x01], AddrMode::Reg("C")),
    ("RLC", &[0xcb, 0x02], AddrMode::Reg("D")),
    ("RLC", &[0xcb, 0x03], AddrMode::Reg("E")),
    ("RLC", &[0xcb, 0x04], AddrMode::Reg("H")),
    ("RLC", &[0xcb, 0x05], AddrMode::Reg("L")),
    ("RLC", &[0xcb, 0x06], AddrMode::BracRegKets("HL")),
    ("RLC", &[0xcb, 0x07], AddrMode::Reg("A")),
    ("RLCA", &[0x07], AddrMode::Implied),
    ("RR", &[0xcb, 0x18], AddrMode::Reg("B")),
    ("RR", &[0xcb, 0x19], AddrMode::Reg("C")),
    ("RR", &[0xcb, 0x1a], AddrMode::Reg("D")),
    ("RR", &[0xcb, 0x1b], AddrMode::Reg("E")),
    ("RR", &[0xcb, 0x1c], AddrMode::Reg("H")),
    ("RR", &[0xcb, 0x1d], AddrMode::Reg("L")),
    ("RR", &[0xcb, 0x1e], AddrMode::BracRegKets("HL")),
    ("RR", &[0xcb, 0x1f], AddrMode::Reg("A")),
    ("RRA", &[0x1f], AddrMode::Implied),
    ("RRC", &[0xcb, 0x08], AddrMode::Reg("B")),
    ("RRC", &[0xcb, 0x09], AddrMode::Reg("C")),
    ("RRC", &[0xcb, 0x0a], AddrMode::Reg("D")),
    ("RRC", &[0xcb, 0x0b], AddrMode::Reg("E")),
    ("RRC", &[0xcb, 0x0c], AddrMode::Reg("H")),
    ("RRC", &[0xcb, 0x0d], AddrMode::Reg("L")),
    ("RRC", &[0xcb, 0x0e], AddrMode::BracRegKets("HL")),
    ("RRC", &[0xcb, 0x0f], AddrMode::Reg("A")),
    ("RRCA", &[0x0f], AddrMode::Implied),
    // TODO: RST opcodes
    // TODO: SBC opcodes
    ("SCF", &[0x37], AddrMode::Implied),
    // TODO: SET opcodes
    // TODO: SLA opcodes
    // TODO: SRA opcodes
    // TODO: SRL opcodes
    ("STOP", &[0x10], AddrMode::Implied),
    // TODO: SUB opcodes
    // TODO: SWAP opcodes
    // TODO: XOR opcodes
];

//===========================================================================//
