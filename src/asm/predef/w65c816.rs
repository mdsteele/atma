use super::addrmode::{
    AddrMode, PLACEHOLDER_ADDR, PLACEHOLDER_IMM, PLACEHOLDER_IMM2, Reg,
    addr_arg, macro_arg, pound_imm_arg, reg_arg, token,
};
use super::mos6502::{Mos6502, par_addr_comma_reg_ens_arg, par_addr_ens_arg};
use super::pool::RcPool;
use crate::lex::TokenValue;
use crate::parse::{AsmMacroArgAst, AsmStmtAst};

//===========================================================================//

pub(super) const ARCH_65C816: &str = "65C816";
pub(super) const RES_65C816: &[&str] = &["S"];
pub(super) const MACROS_65C816: &[(&str, &[u8], W65c816)] = &[
    ("ADC", &[0x69], W65c816::PoundPoundImm16),
    ("BIT", &[0x34], W65c816::Mos(Mos6502::Addr8CommaReg("X"))),
    ("BIT", &[0x3c], W65c816::Mos(Mos6502::BangAddr16CommaReg("X"))),
    ("BIT", &[0x89], W65c816::Mos(Mos6502::PoundImm8)),
    ("BIT", &[0x89], W65c816::PoundPoundImm16),
    ("BRA", &[0x80], W65c816::Mos(Mos6502::Relative8)),
    ("BRL", &[0x82], W65c816::Relative16),
    ("COP", &[0x02], W65c816::Mos(Mos6502::PoundImm8)),
    ("DEC", &[0x3a], W65c816::Mos(Mos6502::Reg("A"))),
    ("INC", &[0x1a], W65c816::Mos(Mos6502::Reg("A"))),
    ("JML", &[0x5c], W65c816::BangBangAddr24),
    ("JML", &[0xdc], W65c816::BracBangAddr16Kets),
    ("JMP", &[0x7c], W65c816::ParBangAddr16CommaRegEns("X")),
    ("JSL", &[0x22], W65c816::BangBangAddr24),
    ("JSR", &[0xfc], W65c816::ParBangAddr16CommaRegEns("X")),
    ("LDA", &[0xa3], W65c816::Mos(Mos6502::Addr8CommaReg("S"))),
    ("LDA", &[0xa7], W65c816::BracAddr8Kets),
    ("LDA", &[0xa9], W65c816::PoundPoundImm16),
    ("LDA", &[0xaf], W65c816::BangBangAddr24),
    ("LDA", &[0xb2], W65c816::ParAddr8Ens),
    ("LDA", &[0xb3], W65c816::ParAddr8CommaRegEnsCommaReg("S", "Y")),
    ("LDA", &[0xb7], W65c816::BracAddr8KetsCommaReg("Y")),
    ("LDA", &[0xbf], W65c816::BangBangAddr24CommaReg("X")),
    ("LDX", &[0xa2], W65c816::PoundPoundImm16),
    ("LDY", &[0xa0], W65c816::PoundPoundImm16),
    ("MVN", &[0x54], W65c816::PoundImm8CommaPoundImm8),
    ("MVP", &[0x44], W65c816::PoundImm8CommaPoundImm8),
    ("PEA", &[0xf4], W65c816::Mos(Mos6502::BangAddr16)),
    ("PEI", &[0xd4], W65c816::ParAddr8Ens),
    ("PER", &[0x62], W65c816::Relative16),
    ("PHB", &[0x8b], W65c816::Mos(Mos6502::Implied)),
    ("PHD", &[0x0b], W65c816::Mos(Mos6502::Implied)),
    ("PHK", &[0x4b], W65c816::Mos(Mos6502::Implied)),
    ("PHX", &[0xda], W65c816::Mos(Mos6502::Implied)),
    ("PHY", &[0x5a], W65c816::Mos(Mos6502::Implied)),
    ("PLB", &[0xab], W65c816::Mos(Mos6502::Implied)),
    ("PLD", &[0x2b], W65c816::Mos(Mos6502::Implied)),
    ("PLX", &[0xfa], W65c816::Mos(Mos6502::Implied)),
    ("PLY", &[0x7a], W65c816::Mos(Mos6502::Implied)),
    ("REP", &[0xc2], W65c816::Mos(Mos6502::PoundImm8)),
    ("RTL", &[0x6b], W65c816::Mos(Mos6502::Implied)),
    ("SEP", &[0xe2], W65c816::Mos(Mos6502::PoundImm8)),
    ("STA", &[0x83], W65c816::Mos(Mos6502::Addr8CommaReg("S"))),
    ("STA", &[0x87], W65c816::BracAddr8Kets),
    ("STA", &[0x8f], W65c816::BangBangAddr24),
    ("STA", &[0x92], W65c816::ParAddr8Ens),
    ("STA", &[0x93], W65c816::ParAddr8CommaRegEnsCommaReg("S", "Y")),
    ("STA", &[0x97], W65c816::BracAddr8KetsCommaReg("Y")),
    ("STA", &[0x9f], W65c816::BangBangAddr24CommaReg("X")),
    ("STP", &[0xdb], W65c816::Mos(Mos6502::Implied)),
    ("STZ", &[0x64], W65c816::Mos(Mos6502::Addr8)),
    ("STZ", &[0x74], W65c816::Mos(Mos6502::Addr8CommaReg("X"))),
    ("STZ", &[0x9c], W65c816::Mos(Mos6502::BangAddr16)),
    ("STZ", &[0x9e], W65c816::Mos(Mos6502::BangAddr16CommaReg("X"))),
    ("TCD", &[0x5b], W65c816::Mos(Mos6502::Implied)),
    ("TCS", &[0x1b], W65c816::Mos(Mos6502::Implied)),
    ("TDC", &[0x7b], W65c816::Mos(Mos6502::Implied)),
    ("TRB", &[0x14], W65c816::Mos(Mos6502::Addr8)),
    ("TRB", &[0x1c], W65c816::Mos(Mos6502::BangAddr16)),
    ("TSB", &[0x04], W65c816::Mos(Mos6502::Addr8)),
    ("TSB", &[0x0c], W65c816::Mos(Mos6502::BangAddr16)),
    ("TSC", &[0x3b], W65c816::Mos(Mos6502::Implied)),
    ("TXY", &[0x9b], W65c816::Mos(Mos6502::Implied)),
    ("TYX", &[0xbb], W65c816::Mos(Mos6502::Implied)),
    ("WAI", &[0xcb], W65c816::Mos(Mos6502::Implied)),
    ("WDM", &[0x42], W65c816::Mos(Mos6502::PoundImm8)),
    ("XBA", &[0xeb], W65c816::Mos(Mos6502::Implied)),
    ("XCE", &[0xfb], W65c816::Mos(Mos6502::Implied)),
    // TODO: Remove these redundances with 65xx macros, once the tests can pass
    // without them:
    ("LDA", &[0xa1], W65c816::Mos(Mos6502::ParAddr8CommaRegEns("X"))),
    ("STA", &[0x81], W65c816::Mos(Mos6502::ParAddr8CommaRegEns("X"))),
];

//===========================================================================//

#[derive(Clone, Copy)]
pub(super) enum W65c816 {
    Mos(Mos6502),
    BangBangAddr24,
    BangBangAddr24CommaReg(Reg),
    BracAddr8Kets,
    BracAddr8KetsCommaReg(Reg),
    BracBangAddr16Kets,
    ParAddr8Ens,
    ParAddr8CommaRegEnsCommaReg(Reg, Reg),
    ParBangAddr16CommaRegEns(Reg),
    PoundImm8CommaPoundImm8,
    PoundPoundImm16,
    Relative16,
}

impl AddrMode for W65c816 {
    fn macro_args(&self, pool: &mut RcPool) -> Vec<AsmMacroArgAst> {
        match *self {
            Self::Mos(mos) => mos.macro_args(pool),
            Self::Relative16 => {
                vec![addr_arg(pool)]
            }
            Self::BangBangAddr24 => vec![bang_bang_addr_arg(pool)],
            Self::BangBangAddr24CommaReg(reg) => {
                vec![bang_bang_addr_arg(pool), reg_arg(pool, reg)]
            }
            Self::BracAddr8Kets => vec![brac_addr_kets_arg(pool)],
            Self::BracAddr8KetsCommaReg(reg) => {
                vec![brac_addr_kets_arg(pool), reg_arg(pool, reg)]
            }
            Self::BracBangAddr16Kets => vec![brac_bang_addr_kets_arg(pool)],
            Self::ParAddr8CommaRegEnsCommaReg(r1, r2) => {
                vec![par_addr_comma_reg_ens_arg(pool, r1), reg_arg(pool, r2)]
            }
            Self::ParAddr8Ens => vec![par_addr_ens_arg(pool)],
            Self::ParBangAddr16CommaRegEns(reg) => {
                vec![par_bang_addr_comma_reg_ens_arg(pool, reg)]
            }
            Self::PoundImm8CommaPoundImm8 => {
                vec![pound_imm_arg(pool), pound_imm2_arg(pool)]
            }
            Self::PoundPoundImm16 => vec![pound_pound_imm_arg(pool)],
        }
    }

    fn macro_body(
        &self,
        pool: &mut RcPool,
        prefix_bytes: &[u8],
    ) -> Vec<AsmStmtAst> {
        match *self {
            Self::Mos(mos) => mos.macro_body(pool, prefix_bytes),
            Self::BracAddr8Kets
            | Self::BracAddr8KetsCommaReg(_)
            | Self::ParAddr8CommaRegEnsCommaReg(_, _)
            | Self::ParAddr8Ens => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            Self::BracBangAddr16Kets | Self::ParBangAddr16CommaRegEns(_) => {
                vec![
                    pool.constant_bytes_stmt(prefix_bytes),
                    pool.placeholder_u16le(PLACEHOLDER_ADDR),
                ]
            }
            Self::BangBangAddr24 | Self::BangBangAddr24CommaReg(_) => {
                vec![
                    pool.constant_bytes_stmt(prefix_bytes),
                    pool.placeholder_u24le(PLACEHOLDER_ADDR),
                ]
            }
            Self::PoundImm8CommaPoundImm8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_u8(PLACEHOLDER_IMM2),
                pool.placeholder_u8(PLACEHOLDER_IMM),
            ],
            Self::PoundPoundImm16 => {
                vec![
                    pool.constant_bytes_stmt(prefix_bytes),
                    pool.placeholder_u16le(PLACEHOLDER_IMM),
                ]
            }
            Self::Relative16 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_addr16_rel16le(PLACEHOLDER_ADDR),
            ],
        }
    }
}

//===========================================================================//

fn bang_bang_addr_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Bang),
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
    ])
}

fn brac_addr_kets_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::BracketOpen),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::BracketClose),
    ])
}

fn brac_bang_addr_kets_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::BracketOpen),
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::BracketClose),
    ])
}

fn par_bang_addr_comma_reg_ens_arg(
    pool: &mut RcPool,
    reg: Reg,
) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::ParenOpen),
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
        token(TokenValue::Comma),
        pool.identifier_token(reg),
        token(TokenValue::ParenClose),
    ])
}

fn pound_imm2_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Pound),
        pool.placeholder_token(PLACEHOLDER_IMM2),
    ])
}

fn pound_pound_imm_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Pound),
        token(TokenValue::Pound),
        pool.placeholder_token(PLACEHOLDER_IMM),
    ])
}

//===========================================================================//
