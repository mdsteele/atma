use super::addrmode::{
    AddrMode, PLACEHOLDER_ADDR, PLACEHOLDER_IMM, Reg, addr_arg, bang_addr_arg,
    macro_arg, pound_imm_arg, reg_arg, token,
};
use super::pool::RcPool;
use crate::lex::TokenValue;
use crate::parse::{AsmMacroArgAst, AsmStmtAst};

//===========================================================================//

pub(super) const ARCH_65XX: &str = "65xx";
pub(super) const RES_65XX: &[&str] = &["A", "X", "Y"];
pub(super) const MACROS_65XX: &[(&str, &[u8], Mos6502)] = &[
    ("ADC", &[0x61], Mos6502::ParAddr8CommaRegEns("X")),
    ("ADC", &[0x65], Mos6502::Addr8),
    ("ADC", &[0x69], Mos6502::PoundImm8),
    ("ADC", &[0x6d], Mos6502::BangAddr16),
    ("ADC", &[0x71], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("ADC", &[0x75], Mos6502::Addr8CommaReg("X")),
    ("ADC", &[0x79], Mos6502::BangAddr16CommaReg("Y")),
    ("ADC", &[0x7d], Mos6502::BangAddr16CommaReg("X")),
    ("AND", &[0x21], Mos6502::ParAddr8CommaRegEns("X")),
    ("AND", &[0x25], Mos6502::Addr8),
    ("AND", &[0x29], Mos6502::PoundImm8),
    ("AND", &[0x2d], Mos6502::BangAddr16),
    ("AND", &[0x31], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("AND", &[0x35], Mos6502::Addr8CommaReg("X")),
    ("AND", &[0x39], Mos6502::BangAddr16CommaReg("Y")),
    ("AND", &[0x3d], Mos6502::BangAddr16CommaReg("X")),
    ("ASL", &[0x06], Mos6502::Addr8),
    ("ASL", &[0x0a], Mos6502::Reg("A")),
    ("ASL", &[0x0e], Mos6502::BangAddr16),
    ("ASL", &[0x16], Mos6502::Addr8CommaReg("X")),
    ("ASL", &[0x1e], Mos6502::BangAddr16CommaReg("X")),
    ("BCC", &[0x90], Mos6502::Relative8),
    ("BCS", &[0xb0], Mos6502::Relative8),
    ("BEQ", &[0xf0], Mos6502::Relative8),
    ("BIT", &[0x24], Mos6502::Addr8),
    ("BIT", &[0x2c], Mos6502::BangAddr16),
    ("BMI", &[0x30], Mos6502::Relative8),
    ("BNE", &[0xd0], Mos6502::Relative8),
    ("BPL", &[0x10], Mos6502::Relative8),
    ("BRK", &[0x00], Mos6502::PoundImm8),
    ("BVC", &[0x50], Mos6502::Relative8),
    ("BVS", &[0x70], Mos6502::Relative8),
    ("CLC", &[0x18], Mos6502::Implied),
    ("CLD", &[0xd8], Mos6502::Implied),
    ("CLI", &[0x58], Mos6502::Implied),
    ("CLV", &[0xb8], Mos6502::Implied),
    ("CMP", &[0xc1], Mos6502::ParAddr8CommaRegEns("X")),
    ("CMP", &[0xc5], Mos6502::Addr8),
    ("CMP", &[0xc9], Mos6502::PoundImm8),
    ("CMP", &[0xcd], Mos6502::BangAddr16),
    ("CMP", &[0xd1], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("CMP", &[0xd5], Mos6502::Addr8CommaReg("X")),
    ("CMP", &[0xd9], Mos6502::BangAddr16CommaReg("Y")),
    ("CMP", &[0xdd], Mos6502::BangAddr16CommaReg("X")),
    ("CPX", &[0xe0], Mos6502::PoundImm8),
    ("CPX", &[0xe4], Mos6502::Addr8),
    ("CPX", &[0xec], Mos6502::BangAddr16),
    ("CPY", &[0xc0], Mos6502::PoundImm8),
    ("CPY", &[0xc4], Mos6502::Addr8),
    ("CPY", &[0xcc], Mos6502::BangAddr16),
    ("DEC", &[0xc6], Mos6502::Addr8),
    ("DEC", &[0xce], Mos6502::BangAddr16),
    ("DEC", &[0xd6], Mos6502::Addr8CommaReg("X")),
    ("DEC", &[0xde], Mos6502::BangAddr16CommaReg("X")),
    ("DEX", &[0xca], Mos6502::Implied),
    ("DEY", &[0x88], Mos6502::Implied),
    ("EOR", &[0x41], Mos6502::ParAddr8CommaRegEns("X")),
    ("EOR", &[0x45], Mos6502::Addr8),
    ("EOR", &[0x49], Mos6502::PoundImm8),
    ("EOR", &[0x4d], Mos6502::BangAddr16),
    ("EOR", &[0x51], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("EOR", &[0x55], Mos6502::Addr8CommaReg("X")),
    ("EOR", &[0x59], Mos6502::BangAddr16CommaReg("Y")),
    ("EOR", &[0x5d], Mos6502::BangAddr16CommaReg("X")),
    ("INC", &[0xe6], Mos6502::Addr8),
    ("INC", &[0xee], Mos6502::BangAddr16),
    ("INC", &[0xf6], Mos6502::Addr8CommaReg("X")),
    ("INC", &[0xfe], Mos6502::BangAddr16CommaReg("X")),
    ("INX", &[0xe8], Mos6502::Implied),
    ("INY", &[0xc8], Mos6502::Implied),
    ("JMP", &[0x4c], Mos6502::BangAddr16),
    ("JMP", &[0x6c], Mos6502::ParBangAddr16Ens),
    ("JSR", &[0x20], Mos6502::BangAddr16),
    ("LDA", &[0xa1], Mos6502::ParAddr8CommaRegEns("X")),
    ("LDA", &[0xa5], Mos6502::Addr8),
    ("LDA", &[0xa9], Mos6502::PoundImm8),
    ("LDA", &[0xad], Mos6502::BangAddr16),
    ("LDA", &[0xb1], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("LDA", &[0xb5], Mos6502::Addr8CommaReg("X")),
    ("LDA", &[0xb9], Mos6502::BangAddr16CommaReg("Y")),
    ("LDA", &[0xbd], Mos6502::BangAddr16CommaReg("X")),
    ("LDX", &[0xa2], Mos6502::PoundImm8),
    ("LDX", &[0xa6], Mos6502::Addr8),
    ("LDX", &[0xae], Mos6502::BangAddr16),
    ("LDX", &[0xb6], Mos6502::Addr8CommaReg("Y")),
    ("LDX", &[0xbe], Mos6502::BangAddr16CommaReg("Y")),
    ("LDY", &[0xa0], Mos6502::PoundImm8),
    ("LDY", &[0xa4], Mos6502::Addr8),
    ("LDY", &[0xac], Mos6502::BangAddr16),
    ("LDY", &[0xb4], Mos6502::Addr8CommaReg("X")),
    ("LDY", &[0xbc], Mos6502::BangAddr16CommaReg("X")),
    ("LSR", &[0x46], Mos6502::Addr8),
    ("LSR", &[0x4a], Mos6502::Reg("A")),
    ("LSR", &[0x4e], Mos6502::BangAddr16),
    ("LSR", &[0x56], Mos6502::Addr8CommaReg("X")),
    ("LSR", &[0x5e], Mos6502::BangAddr16CommaReg("X")),
    ("NOP", &[0xea], Mos6502::Implied),
    ("ORA", &[0x01], Mos6502::ParAddr8CommaRegEns("X")),
    ("ORA", &[0x05], Mos6502::Addr8),
    ("ORA", &[0x09], Mos6502::PoundImm8),
    ("ORA", &[0x0d], Mos6502::BangAddr16),
    ("ORA", &[0x11], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("ORA", &[0x15], Mos6502::Addr8CommaReg("X")),
    ("ORA", &[0x19], Mos6502::BangAddr16CommaReg("Y")),
    ("ORA", &[0x1d], Mos6502::BangAddr16CommaReg("X")),
    ("PHA", &[0x48], Mos6502::Implied),
    ("PHP", &[0x08], Mos6502::Implied),
    ("PLA", &[0x68], Mos6502::Implied),
    ("PLP", &[0x28], Mos6502::Implied),
    ("ROL", &[0x26], Mos6502::Addr8),
    ("ROL", &[0x2a], Mos6502::Reg("A")),
    ("ROL", &[0x2e], Mos6502::BangAddr16),
    ("ROL", &[0x36], Mos6502::Addr8CommaReg("X")),
    ("ROL", &[0x3e], Mos6502::BangAddr16CommaReg("X")),
    ("ROR", &[0x66], Mos6502::Addr8),
    ("ROR", &[0x6a], Mos6502::Reg("A")),
    ("ROR", &[0x6e], Mos6502::BangAddr16),
    ("ROR", &[0x76], Mos6502::Addr8CommaReg("X")),
    ("ROR", &[0x7e], Mos6502::BangAddr16CommaReg("X")),
    ("RTI", &[0x40], Mos6502::Implied),
    ("RTS", &[0x60], Mos6502::Implied),
    ("SBC", &[0xe1], Mos6502::ParAddr8CommaRegEns("X")),
    ("SBC", &[0xe5], Mos6502::Addr8),
    ("SBC", &[0xe9], Mos6502::PoundImm8),
    ("SBC", &[0xed], Mos6502::BangAddr16),
    ("SBC", &[0xf1], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("SBC", &[0xf5], Mos6502::Addr8CommaReg("X")),
    ("SBC", &[0xf9], Mos6502::BangAddr16CommaReg("Y")),
    ("SBC", &[0xfd], Mos6502::BangAddr16CommaReg("X")),
    ("SEC", &[0x38], Mos6502::Implied),
    ("SED", &[0xf8], Mos6502::Implied),
    ("SEI", &[0x78], Mos6502::Implied),
    ("STA", &[0x81], Mos6502::ParAddr8CommaRegEns("X")),
    ("STA", &[0x85], Mos6502::Addr8),
    ("STA", &[0x8d], Mos6502::BangAddr16),
    ("STA", &[0x91], Mos6502::ParAddr8EnsCommaReg("Y")),
    ("STA", &[0x95], Mos6502::Addr8CommaReg("X")),
    ("STA", &[0x99], Mos6502::BangAddr16CommaReg("Y")),
    ("STA", &[0x9d], Mos6502::BangAddr16CommaReg("X")),
    ("STX", &[0x86], Mos6502::Addr8),
    ("STX", &[0x8e], Mos6502::BangAddr16),
    ("STX", &[0x96], Mos6502::Addr8CommaReg("Y")),
    ("STY", &[0x84], Mos6502::Addr8),
    ("STY", &[0x8c], Mos6502::BangAddr16),
    ("STY", &[0x94], Mos6502::Addr8CommaReg("X")),
    ("TAX", &[0xaa], Mos6502::Implied),
    ("TAY", &[0xa8], Mos6502::Implied),
    ("TSX", &[0xba], Mos6502::Implied),
    ("TXA", &[0x8a], Mos6502::Implied),
    ("TXS", &[0x9a], Mos6502::Implied),
    ("TYA", &[0x98], Mos6502::Implied),
];

//===========================================================================//

pub(super) const ARCH_6502: &str = "6502";
pub(super) const RES_6502: &[&str] = &[];
pub(super) const MACROS_6502: &[(&str, &[u8], Mos6502)] =
    &[("JAM", &[0x02], Mos6502::Implied)];

//===========================================================================//

#[derive(Clone, Copy)]
pub(super) enum Mos6502 {
    /// FOO addr
    Addr8,
    /// FOO addr, R
    Addr8CommaReg(Reg),
    /// FOO !addr
    BangAddr16,
    /// FOO !addr, R
    BangAddr16CommaReg(Reg),
    /// FOO (addr, R)
    ParAddr8CommaRegEns(Reg),
    /// FOO (addr), R
    ParAddr8EnsCommaReg(Reg),
    /// FOO (!addr)
    ParBangAddr16Ens,
    /// FOO #imm
    PoundImm8,
    /// FOO
    Implied,
    /// FOO R
    Reg(Reg),
    /// FOO addr
    Relative8,
}

impl AddrMode for Mos6502 {
    fn macro_args(&self, pool: &mut RcPool) -> Vec<AsmMacroArgAst> {
        match *self {
            Self::Addr8 | Self::Relative8 => {
                vec![addr_arg(pool)]
            }
            Self::Addr8CommaReg(r1) => {
                vec![addr_arg(pool), reg_arg(pool, r1)]
            }
            Self::BangAddr16 => vec![bang_addr_arg(pool)],
            Self::BangAddr16CommaReg(r1) => {
                vec![bang_addr_arg(pool), reg_arg(pool, r1)]
            }
            Self::Implied => vec![],
            Self::ParAddr8CommaRegEns(r1) => {
                vec![par_addr_comma_reg_ens_arg(pool, r1)]
            }
            Self::ParAddr8EnsCommaReg(r1) => {
                vec![par_addr_ens_arg(pool), reg_arg(pool, r1)]
            }
            Self::ParBangAddr16Ens => vec![par_bang_addr_ens_arg(pool)],
            Self::PoundImm8 => {
                vec![pound_imm_arg(pool)]
            }
            Self::Reg(r1) => vec![reg_arg(pool, r1)],
        }
    }

    fn macro_body(
        &self,
        pool: &mut RcPool,
        prefix_bytes: &[u8],
    ) -> Vec<AsmStmtAst> {
        match *self {
            Self::Addr8
            | Self::Addr8CommaReg(_)
            | Self::ParAddr8CommaRegEns(_)
            | Self::ParAddr8EnsCommaReg(_) => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            Self::BangAddr16
            | Self::BangAddr16CommaReg(_)
            | Self::ParBangAddr16Ens => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u16le(PLACEHOLDER_ADDR),
            ],
            Self::Implied | Self::Reg(_) => {
                vec![pool.constant_bytes_stmt(prefix_bytes)]
            }
            Self::PoundImm8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_IMM),
            ],
            Self::Relative8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR),
            ],
        }
    }
}

//===========================================================================//

pub(super) fn par_addr_comma_reg_ens_arg(
    pool: &mut RcPool,
    reg: Reg,
) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::ParenOpen),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::Comma),
        pool.identifier_token(reg),
        token(TokenValue::ParenClose),
    ])
}

pub(super) fn par_addr_ens_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::ParenOpen),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::ParenClose),
    ])
}

fn par_bang_addr_ens_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::ParenOpen),
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::ParenClose),
    ])
}

//===========================================================================//
