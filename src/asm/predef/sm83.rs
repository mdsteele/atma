use super::addrmode::{
    AddrMode, PLACEHOLDER_ADDR, Reg, addr_arg, macro_arg, reg_arg, token,
};
use super::pool::RcPool;
use crate::lex::TokenValue;
use crate::parse::{AsmMacroArgAst, AsmStmtAst};

//===========================================================================//

pub(super) const ARCH_SM83: &str = "SM83";
pub(super) const RES_SM83: &[&str] = &[
    "A", "AF", "B", "BC", "C", "D", "DE", "E", "H", "HL", "L", "NC", "NZ",
    "SP", "Z",
];
// TODO: fill in the rest of these
pub(super) const MACROS_SM83: &[(&str, &[u8], Sm83)] = &[
    // TODO: ADC opcodes
    // TODO: ADD opcodes
    // TODO: AND opcodes
    // TODO: BIT opcodes
    ("CALL", &[0xcd], Sm83::Addr16),
    ("CCF", &[0x3f], Sm83::Implied),
    // TODO: CP opcodes
    ("CPL", &[0x2f], Sm83::Implied),
    ("DAA", &[0x27], Sm83::Implied),
    ("DEC", &[0x05], Sm83::Reg("B")),
    ("DEC", &[0x0b], Sm83::Reg("BC")),
    ("DEC", &[0x0d], Sm83::Reg("C")),
    ("DEC", &[0x15], Sm83::Reg("D")),
    ("DEC", &[0x1b], Sm83::Reg("DE")),
    ("DEC", &[0x1d], Sm83::Reg("E")),
    ("DEC", &[0x25], Sm83::Reg("H")),
    ("DEC", &[0x2b], Sm83::Reg("HL")),
    ("DEC", &[0x2d], Sm83::Reg("L")),
    ("DEC", &[0x35], Sm83::BracRegKets("HL")),
    ("DEC", &[0x3b], Sm83::Reg("SP")),
    ("DEC", &[0x3d], Sm83::Reg("A")),
    ("DI", &[0xf3], Sm83::Implied),
    ("EI", &[0xfb], Sm83::Implied),
    ("HALT", &[0x76], Sm83::Implied),
    ("INC", &[0x03], Sm83::Reg("BC")),
    ("INC", &[0x04], Sm83::Reg("B")),
    ("INC", &[0x0c], Sm83::Reg("C")),
    ("INC", &[0x13], Sm83::Reg("DE")),
    ("INC", &[0x14], Sm83::Reg("D")),
    ("INC", &[0x1c], Sm83::Reg("E")),
    ("INC", &[0x23], Sm83::Reg("HL")),
    ("INC", &[0x24], Sm83::Reg("H")),
    ("INC", &[0x2c], Sm83::Reg("L")),
    ("INC", &[0x33], Sm83::Reg("SP")),
    ("INC", &[0x34], Sm83::BracRegKets("HL")),
    ("INC", &[0x3c], Sm83::Reg("A")),
    ("JP", &[0xc3], Sm83::Addr16),
    // TODO: other JP opcodes
    ("JR", &[0x18], Sm83::Relative8),
    // TODO: other JR opcodes
    ("LD", &[0x40], Sm83::RegCommaReg("B", "B")),
    ("LD", &[0x41], Sm83::RegCommaReg("B", "C")),
    ("LD", &[0x42], Sm83::RegCommaReg("B", "D")),
    ("LD", &[0x43], Sm83::RegCommaReg("B", "E")),
    ("LD", &[0x44], Sm83::RegCommaReg("B", "H")),
    ("LD", &[0x45], Sm83::RegCommaReg("B", "L")),
    ("LD", &[0x47], Sm83::RegCommaReg("B", "A")),
    ("LD", &[0x48], Sm83::RegCommaReg("C", "B")),
    ("LD", &[0x49], Sm83::RegCommaReg("C", "C")),
    ("LD", &[0x4a], Sm83::RegCommaReg("C", "D")),
    ("LD", &[0x4b], Sm83::RegCommaReg("C", "E")),
    ("LD", &[0x4c], Sm83::RegCommaReg("C", "H")),
    ("LD", &[0x4d], Sm83::RegCommaReg("C", "L")),
    ("LD", &[0x4f], Sm83::RegCommaReg("C", "A")),
    // TODO: other LD opcodes
    // TODO: LDH opcodes
    ("NOP", &[0x00], Sm83::Implied),
    // TODO: OR opcodes
    // TODO: POP opcodes
    // TODO: PUSH opcodes
    // TODO: RES opcodes
    ("RET", &[0xc9], Sm83::Implied),
    ("RET", &[0xc0], Sm83::Reg("NZ")),
    ("RET", &[0xc8], Sm83::Reg("Z")),
    ("RET", &[0xd0], Sm83::Reg("NC")),
    ("RET", &[0xd8], Sm83::Reg("C")),
    ("RETI", &[0xd9], Sm83::Implied),
    ("RL", &[0xcb, 0x10], Sm83::Reg("B")),
    ("RL", &[0xcb, 0x11], Sm83::Reg("C")),
    ("RL", &[0xcb, 0x12], Sm83::Reg("D")),
    ("RL", &[0xcb, 0x13], Sm83::Reg("E")),
    ("RL", &[0xcb, 0x14], Sm83::Reg("H")),
    ("RL", &[0xcb, 0x15], Sm83::Reg("L")),
    ("RL", &[0xcb, 0x16], Sm83::BracRegKets("HL")),
    ("RL", &[0xcb, 0x17], Sm83::Reg("A")),
    ("RLA", &[0x17], Sm83::Implied),
    ("RLC", &[0xcb, 0x00], Sm83::Reg("B")),
    ("RLC", &[0xcb, 0x01], Sm83::Reg("C")),
    ("RLC", &[0xcb, 0x02], Sm83::Reg("D")),
    ("RLC", &[0xcb, 0x03], Sm83::Reg("E")),
    ("RLC", &[0xcb, 0x04], Sm83::Reg("H")),
    ("RLC", &[0xcb, 0x05], Sm83::Reg("L")),
    ("RLC", &[0xcb, 0x06], Sm83::BracRegKets("HL")),
    ("RLC", &[0xcb, 0x07], Sm83::Reg("A")),
    ("RLCA", &[0x07], Sm83::Implied),
    ("RR", &[0xcb, 0x18], Sm83::Reg("B")),
    ("RR", &[0xcb, 0x19], Sm83::Reg("C")),
    ("RR", &[0xcb, 0x1a], Sm83::Reg("D")),
    ("RR", &[0xcb, 0x1b], Sm83::Reg("E")),
    ("RR", &[0xcb, 0x1c], Sm83::Reg("H")),
    ("RR", &[0xcb, 0x1d], Sm83::Reg("L")),
    ("RR", &[0xcb, 0x1e], Sm83::BracRegKets("HL")),
    ("RR", &[0xcb, 0x1f], Sm83::Reg("A")),
    ("RRA", &[0x1f], Sm83::Implied),
    ("RRC", &[0xcb, 0x08], Sm83::Reg("B")),
    ("RRC", &[0xcb, 0x09], Sm83::Reg("C")),
    ("RRC", &[0xcb, 0x0a], Sm83::Reg("D")),
    ("RRC", &[0xcb, 0x0b], Sm83::Reg("E")),
    ("RRC", &[0xcb, 0x0c], Sm83::Reg("H")),
    ("RRC", &[0xcb, 0x0d], Sm83::Reg("L")),
    ("RRC", &[0xcb, 0x0e], Sm83::BracRegKets("HL")),
    ("RRC", &[0xcb, 0x0f], Sm83::Reg("A")),
    ("RRCA", &[0x0f], Sm83::Implied),
    // TODO: RST opcodes
    // TODO: SBC opcodes
    ("SCF", &[0x37], Sm83::Implied),
    // TODO: SET opcodes
    // TODO: SLA opcodes
    // TODO: SRA opcodes
    // TODO: SRL opcodes
    ("STOP", &[0x10], Sm83::Implied),
    // TODO: SUB opcodes
    // TODO: SWAP opcodes
    // TODO: XOR opcodes
];

//===========================================================================//

#[derive(Clone, Copy)]
pub(super) enum Sm83 {
    /// FOO addr
    Addr16,
    /// FOO [reg]
    BracRegKets(Reg),
    /// FOO
    Implied,
    /// FOO reg
    Reg(Reg),
    /// FOO R1, R2
    RegCommaReg(Reg, Reg),
    /// FOO addr
    Relative8,
}

impl AddrMode for Sm83 {
    fn macro_args(&self, pool: &mut RcPool) -> Vec<AsmMacroArgAst> {
        match *self {
            Self::Addr16 | Self::Relative8 => {
                vec![addr_arg(pool)]
            }
            Self::BracRegKets(r1) => vec![brac_reg_kets_arg(pool, r1)],
            Self::Implied => vec![],
            Self::Reg(reg) => vec![reg_arg(pool, reg)],
            Self::RegCommaReg(r1, r2) => {
                vec![reg_arg(pool, r1), reg_arg(pool, r2)]
            }
        }
    }

    fn macro_body(
        &self,
        pool: &mut RcPool,
        prefix_bytes: &[u8],
    ) -> Vec<AsmStmtAst> {
        match *self {
            Self::Addr16 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u16le(PLACEHOLDER_ADDR),
            ],
            Self::BracRegKets(_)
            | Self::Implied
            | Self::Reg(_)
            | Self::RegCommaReg(_, _) => {
                vec![pool.constant_bytes_stmt(prefix_bytes)]
            }
            Self::Relative8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR),
            ],
        }
    }
}

//===========================================================================//

fn brac_reg_kets_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::BracketOpen),
        pool.identifier_token(reg),
        token(TokenValue::BracketClose),
    ])
}

//===========================================================================//
