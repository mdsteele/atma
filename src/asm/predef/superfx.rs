use super::addrmode::{
    AddrMode, PLACEHOLDER_ADDR, PLACEHOLDER_IMM, Reg, addr_arg,
    par_reg_ens_arg, pound_imm_arg, reg_arg,
};
use super::pool::RcPool;
use crate::parse::{
    AsmIntTypeAst, AsmMacroArgAst, AsmRelTypeAst, AsmStmtAst, BinOpAst,
};

//===========================================================================//

pub(super) const ARCH_SUPERFX: &str = "SuperFX";
pub(super) const RES_SUPERFX: &[&str] = &[
    "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11",
    "R12", "R13", "R14", "R15",
];
pub(super) const MACROS_SUPERFX: &[(&str, &[u8], SuperFx)] = &[
    ("ADD", &[0x50], SuperFx::Reg("R0")),
    ("ADD", &[0x51], SuperFx::Reg("R1")),
    ("ADD", &[0x52], SuperFx::Reg("R2")),
    ("ADD", &[0x53], SuperFx::Reg("R3")),
    ("ADD", &[0x54], SuperFx::Reg("R4")),
    ("ADD", &[0x55], SuperFx::Reg("R5")),
    ("ADD", &[0x56], SuperFx::Reg("R6")),
    ("ADD", &[0x57], SuperFx::Reg("R7")),
    ("ADD", &[0x58], SuperFx::Reg("R8")),
    ("ADD", &[0x59], SuperFx::Reg("R9")),
    ("ADD", &[0x5a], SuperFx::Reg("R10")),
    ("ADD", &[0x5b], SuperFx::Reg("R11")),
    ("ADD", &[0x5c], SuperFx::Reg("R12")),
    ("ADD", &[0x5d], SuperFx::Reg("R13")),
    ("ADD", &[0x5e], SuperFx::Reg("R14")),
    ("ADD", &[0x5f], SuperFx::Reg("R15")),
    ("ALT1", &[0x3d], SuperFx::Implied),
    ("ALT2", &[0x3e], SuperFx::Implied),
    ("ALT3", &[0x3f], SuperFx::Implied),
    ("AND", &[0x71], SuperFx::Reg("R1")),
    ("AND", &[0x72], SuperFx::Reg("R2")),
    ("AND", &[0x73], SuperFx::Reg("R3")),
    ("AND", &[0x74], SuperFx::Reg("R4")),
    ("AND", &[0x75], SuperFx::Reg("R5")),
    ("AND", &[0x76], SuperFx::Reg("R6")),
    ("AND", &[0x77], SuperFx::Reg("R7")),
    ("AND", &[0x78], SuperFx::Reg("R8")),
    ("AND", &[0x79], SuperFx::Reg("R9")),
    ("AND", &[0x7a], SuperFx::Reg("R10")),
    ("AND", &[0x7b], SuperFx::Reg("R11")),
    ("AND", &[0x7c], SuperFx::Reg("R12")),
    ("AND", &[0x7d], SuperFx::Reg("R13")),
    ("AND", &[0x7e], SuperFx::Reg("R14")),
    ("AND", &[0x7f], SuperFx::Reg("R15")),
    ("ASR", &[0x96], SuperFx::Implied),
    ("BCC", &[0x0c], SuperFx::Relative8),
    ("BCS", &[0x0d], SuperFx::Relative8),
    ("BEQ", &[0x09], SuperFx::Relative8),
    ("BGE", &[0x06], SuperFx::Relative8),
    ("BLT", &[0x07], SuperFx::Relative8),
    ("BMI", &[0x0b], SuperFx::Relative8),
    ("BNE", &[0x08], SuperFx::Relative8),
    ("BPL", &[0x0a], SuperFx::Relative8),
    ("BRA", &[0x05], SuperFx::Relative8),
    ("BVC", &[0x0e], SuperFx::Relative8),
    ("BVS", &[0x0f], SuperFx::Relative8),
    ("CACHE", &[0x02], SuperFx::Implied),
    ("COLOR", &[0x4e], SuperFx::Implied),
    ("DEC", &[0xe0], SuperFx::Reg("R0")),
    ("DEC", &[0xe1], SuperFx::Reg("R1")),
    ("DEC", &[0xe2], SuperFx::Reg("R2")),
    ("DEC", &[0xe3], SuperFx::Reg("R3")),
    ("DEC", &[0xe4], SuperFx::Reg("R4")),
    ("DEC", &[0xe5], SuperFx::Reg("R5")),
    ("DEC", &[0xe6], SuperFx::Reg("R6")),
    ("DEC", &[0xe7], SuperFx::Reg("R7")),
    ("DEC", &[0xe8], SuperFx::Reg("R8")),
    ("DEC", &[0xe9], SuperFx::Reg("R9")),
    ("DEC", &[0xea], SuperFx::Reg("R10")),
    ("DEC", &[0xeb], SuperFx::Reg("R11")),
    ("DEC", &[0xec], SuperFx::Reg("R12")),
    ("DEC", &[0xed], SuperFx::Reg("R13")),
    ("DEC", &[0xee], SuperFx::Reg("R14")),
    ("FMULT", &[0x9f], SuperFx::Implied),
    ("FROM", &[0xb0], SuperFx::Reg("R0")),
    ("FROM", &[0xb1], SuperFx::Reg("R1")),
    ("FROM", &[0xb2], SuperFx::Reg("R2")),
    ("FROM", &[0xb3], SuperFx::Reg("R3")),
    ("FROM", &[0xb4], SuperFx::Reg("R4")),
    ("FROM", &[0xb5], SuperFx::Reg("R5")),
    ("FROM", &[0xb6], SuperFx::Reg("R6")),
    ("FROM", &[0xb7], SuperFx::Reg("R7")),
    ("FROM", &[0xb8], SuperFx::Reg("R8")),
    ("FROM", &[0xb9], SuperFx::Reg("R9")),
    ("FROM", &[0xba], SuperFx::Reg("R10")),
    ("FROM", &[0xbb], SuperFx::Reg("R11")),
    ("FROM", &[0xbc], SuperFx::Reg("R12")),
    ("FROM", &[0xbd], SuperFx::Reg("R13")),
    ("FROM", &[0xbe], SuperFx::Reg("R14")),
    ("FROM", &[0xbf], SuperFx::Reg("R15")),
    ("GETB", &[0xef], SuperFx::Implied),
    ("GETC", &[0xdf], SuperFx::Implied),
    ("HIB", &[0xc0], SuperFx::Implied),
    ("IBT", &[0xa0], SuperFx::RegCommaPoundImm8("R0")),
    ("IBT", &[0xa1], SuperFx::RegCommaPoundImm8("R1")),
    ("IBT", &[0xa2], SuperFx::RegCommaPoundImm8("R2")),
    ("IBT", &[0xa3], SuperFx::RegCommaPoundImm8("R3")),
    ("IBT", &[0xa4], SuperFx::RegCommaPoundImm8("R4")),
    ("IBT", &[0xa5], SuperFx::RegCommaPoundImm8("R5")),
    ("IBT", &[0xa6], SuperFx::RegCommaPoundImm8("R6")),
    ("IBT", &[0xa7], SuperFx::RegCommaPoundImm8("R7")),
    ("IBT", &[0xa8], SuperFx::RegCommaPoundImm8("R8")),
    ("IBT", &[0xa9], SuperFx::RegCommaPoundImm8("R9")),
    ("IBT", &[0xaa], SuperFx::RegCommaPoundImm8("R10")),
    ("IBT", &[0xab], SuperFx::RegCommaPoundImm8("R11")),
    ("IBT", &[0xac], SuperFx::RegCommaPoundImm8("R12")),
    ("IBT", &[0xad], SuperFx::RegCommaPoundImm8("R13")),
    ("IBT", &[0xae], SuperFx::RegCommaPoundImm8("R14")),
    ("IBT", &[0xaf], SuperFx::RegCommaPoundImm8("R15")),
    ("INC", &[0xd0], SuperFx::Reg("R0")),
    ("INC", &[0xd1], SuperFx::Reg("R1")),
    ("INC", &[0xd2], SuperFx::Reg("R2")),
    ("INC", &[0xd3], SuperFx::Reg("R3")),
    ("INC", &[0xd4], SuperFx::Reg("R4")),
    ("INC", &[0xd5], SuperFx::Reg("R5")),
    ("INC", &[0xd6], SuperFx::Reg("R6")),
    ("INC", &[0xd7], SuperFx::Reg("R7")),
    ("INC", &[0xd8], SuperFx::Reg("R8")),
    ("INC", &[0xd9], SuperFx::Reg("R9")),
    ("INC", &[0xda], SuperFx::Reg("R10")),
    ("INC", &[0xdb], SuperFx::Reg("R11")),
    ("INC", &[0xdc], SuperFx::Reg("R12")),
    ("INC", &[0xdd], SuperFx::Reg("R13")),
    ("INC", &[0xde], SuperFx::Reg("R14")),
    ("IWT", &[0xf0], SuperFx::RegCommaPoundImm16("R0")),
    ("IWT", &[0xf1], SuperFx::RegCommaPoundImm16("R1")),
    ("IWT", &[0xf2], SuperFx::RegCommaPoundImm16("R2")),
    ("IWT", &[0xf3], SuperFx::RegCommaPoundImm16("R3")),
    ("IWT", &[0xf4], SuperFx::RegCommaPoundImm16("R4")),
    ("IWT", &[0xf5], SuperFx::RegCommaPoundImm16("R5")),
    ("IWT", &[0xf6], SuperFx::RegCommaPoundImm16("R6")),
    ("IWT", &[0xf7], SuperFx::RegCommaPoundImm16("R7")),
    ("IWT", &[0xf8], SuperFx::RegCommaPoundImm16("R8")),
    ("IWT", &[0xf9], SuperFx::RegCommaPoundImm16("R9")),
    ("IWT", &[0xfa], SuperFx::RegCommaPoundImm16("R10")),
    ("IWT", &[0xfb], SuperFx::RegCommaPoundImm16("R11")),
    ("IWT", &[0xfc], SuperFx::RegCommaPoundImm16("R12")),
    ("IWT", &[0xfd], SuperFx::RegCommaPoundImm16("R13")),
    ("IWT", &[0xfe], SuperFx::RegCommaPoundImm16("R14")),
    ("IWT", &[0xff], SuperFx::RegCommaPoundImm16("R15")),
    ("JMP", &[0x98], SuperFx::Reg("R8")),
    ("JMP", &[0x99], SuperFx::Reg("R9")),
    ("JMP", &[0x9a], SuperFx::Reg("R10")),
    ("JMP", &[0x9b], SuperFx::Reg("R11")),
    ("JMP", &[0x9c], SuperFx::Reg("R12")),
    ("JMP", &[0x9d], SuperFx::Reg("R13")),
    ("LDW", &[0x40], SuperFx::ParRegEns("R0")),
    ("LDW", &[0x41], SuperFx::ParRegEns("R1")),
    ("LDW", &[0x42], SuperFx::ParRegEns("R2")),
    ("LDW", &[0x43], SuperFx::ParRegEns("R3")),
    ("LDW", &[0x44], SuperFx::ParRegEns("R4")),
    ("LDW", &[0x45], SuperFx::ParRegEns("R5")),
    ("LDW", &[0x46], SuperFx::ParRegEns("R6")),
    ("LDW", &[0x47], SuperFx::ParRegEns("R7")),
    ("LDW", &[0x48], SuperFx::ParRegEns("R8")),
    ("LDW", &[0x49], SuperFx::ParRegEns("R9")),
    ("LDW", &[0x4a], SuperFx::ParRegEns("R10")),
    ("LDW", &[0x4b], SuperFx::ParRegEns("R11")),
    ("LINK", &[], SuperFx::LinkPoundImm),
    ("LINK", &[], SuperFx::LinkRelative),
    ("LOB", &[0x9e], SuperFx::Implied),
    ("LOOP", &[0x3c], SuperFx::Implied),
    ("LSR", &[0x03], SuperFx::Implied),
    ("MERGE", &[0x70], SuperFx::Implied),
    ("MULT", &[0x80], SuperFx::Reg("R0")),
    ("MULT", &[0x81], SuperFx::Reg("R1")),
    ("MULT", &[0x82], SuperFx::Reg("R2")),
    ("MULT", &[0x83], SuperFx::Reg("R3")),
    ("MULT", &[0x84], SuperFx::Reg("R4")),
    ("MULT", &[0x85], SuperFx::Reg("R5")),
    ("MULT", &[0x86], SuperFx::Reg("R6")),
    ("MULT", &[0x87], SuperFx::Reg("R7")),
    ("MULT", &[0x88], SuperFx::Reg("R8")),
    ("MULT", &[0x89], SuperFx::Reg("R9")),
    ("MULT", &[0x8a], SuperFx::Reg("R10")),
    ("MULT", &[0x8b], SuperFx::Reg("R11")),
    ("MULT", &[0x8c], SuperFx::Reg("R12")),
    ("MULT", &[0x8d], SuperFx::Reg("R13")),
    ("MULT", &[0x8e], SuperFx::Reg("R14")),
    ("MULT", &[0x8f], SuperFx::Reg("R15")),
    ("NOP", &[0x01], SuperFx::Implied),
    ("NOT", &[0x4f], SuperFx::Implied),
    ("OR", &[0xc1], SuperFx::Reg("R1")),
    ("OR", &[0xc2], SuperFx::Reg("R2")),
    ("OR", &[0xc3], SuperFx::Reg("R3")),
    ("OR", &[0xc4], SuperFx::Reg("R4")),
    ("OR", &[0xc5], SuperFx::Reg("R5")),
    ("OR", &[0xc6], SuperFx::Reg("R6")),
    ("OR", &[0xc7], SuperFx::Reg("R7")),
    ("OR", &[0xc8], SuperFx::Reg("R8")),
    ("OR", &[0xc9], SuperFx::Reg("R9")),
    ("OR", &[0xca], SuperFx::Reg("R10")),
    ("OR", &[0xcb], SuperFx::Reg("R11")),
    ("OR", &[0xcc], SuperFx::Reg("R12")),
    ("OR", &[0xcd], SuperFx::Reg("R13")),
    ("OR", &[0xce], SuperFx::Reg("R14")),
    ("OR", &[0xcf], SuperFx::Reg("R15")),
    ("PLOT", &[0x4c], SuperFx::Implied),
    ("ROL", &[0x04], SuperFx::Implied),
    ("ROR", &[0x97], SuperFx::Implied),
    ("SBK", &[0x90], SuperFx::Implied),
    ("SEX", &[0x95], SuperFx::Implied),
    ("STOP", &[0x00], SuperFx::Implied),
    ("STW", &[0x30], SuperFx::ParRegEns("R0")),
    ("STW", &[0x31], SuperFx::ParRegEns("R1")),
    ("STW", &[0x32], SuperFx::ParRegEns("R2")),
    ("STW", &[0x33], SuperFx::ParRegEns("R3")),
    ("STW", &[0x34], SuperFx::ParRegEns("R4")),
    ("STW", &[0x35], SuperFx::ParRegEns("R5")),
    ("STW", &[0x36], SuperFx::ParRegEns("R6")),
    ("STW", &[0x37], SuperFx::ParRegEns("R7")),
    ("STW", &[0x38], SuperFx::ParRegEns("R8")),
    ("STW", &[0x39], SuperFx::ParRegEns("R9")),
    ("STW", &[0x3a], SuperFx::ParRegEns("R10")),
    ("STW", &[0x3b], SuperFx::ParRegEns("R11")),
    ("SUB", &[0x60], SuperFx::Reg("R0")),
    ("SUB", &[0x61], SuperFx::Reg("R1")),
    ("SUB", &[0x62], SuperFx::Reg("R2")),
    ("SUB", &[0x63], SuperFx::Reg("R3")),
    ("SUB", &[0x64], SuperFx::Reg("R4")),
    ("SUB", &[0x65], SuperFx::Reg("R5")),
    ("SUB", &[0x66], SuperFx::Reg("R6")),
    ("SUB", &[0x67], SuperFx::Reg("R7")),
    ("SUB", &[0x68], SuperFx::Reg("R8")),
    ("SUB", &[0x69], SuperFx::Reg("R9")),
    ("SUB", &[0x6a], SuperFx::Reg("R10")),
    ("SUB", &[0x6b], SuperFx::Reg("R11")),
    ("SUB", &[0x6c], SuperFx::Reg("R12")),
    ("SUB", &[0x6d], SuperFx::Reg("R13")),
    ("SUB", &[0x6e], SuperFx::Reg("R14")),
    ("SUB", &[0x6f], SuperFx::Reg("R15")),
    ("SWAP", &[0x4d], SuperFx::Implied),
    ("TO", &[0x10], SuperFx::Reg("R0")),
    ("TO", &[0x11], SuperFx::Reg("R1")),
    ("TO", &[0x12], SuperFx::Reg("R2")),
    ("TO", &[0x13], SuperFx::Reg("R3")),
    ("TO", &[0x14], SuperFx::Reg("R4")),
    ("TO", &[0x15], SuperFx::Reg("R5")),
    ("TO", &[0x16], SuperFx::Reg("R6")),
    ("TO", &[0x17], SuperFx::Reg("R7")),
    ("TO", &[0x18], SuperFx::Reg("R8")),
    ("TO", &[0x19], SuperFx::Reg("R9")),
    ("TO", &[0x1a], SuperFx::Reg("R10")),
    ("TO", &[0x1b], SuperFx::Reg("R11")),
    ("TO", &[0x1c], SuperFx::Reg("R12")),
    ("TO", &[0x1d], SuperFx::Reg("R13")),
    ("TO", &[0x1e], SuperFx::Reg("R14")),
    ("TO", &[0x1f], SuperFx::Reg("R15")),
    ("WITH", &[0x20], SuperFx::Reg("R0")),
    ("WITH", &[0x21], SuperFx::Reg("R1")),
    ("WITH", &[0x22], SuperFx::Reg("R2")),
    ("WITH", &[0x23], SuperFx::Reg("R3")),
    ("WITH", &[0x24], SuperFx::Reg("R4")),
    ("WITH", &[0x25], SuperFx::Reg("R5")),
    ("WITH", &[0x26], SuperFx::Reg("R6")),
    ("WITH", &[0x27], SuperFx::Reg("R7")),
    ("WITH", &[0x28], SuperFx::Reg("R8")),
    ("WITH", &[0x29], SuperFx::Reg("R9")),
    ("WITH", &[0x2a], SuperFx::Reg("R10")),
    ("WITH", &[0x2b], SuperFx::Reg("R11")),
    ("WITH", &[0x2c], SuperFx::Reg("R12")),
    ("WITH", &[0x2d], SuperFx::Reg("R13")),
    ("WITH", &[0x2e], SuperFx::Reg("R14")),
    ("WITH", &[0x2f], SuperFx::Reg("R15")),
];

//===========================================================================//

#[derive(Clone, Copy)]
pub(super) enum SuperFx {
    /// FOO
    Implied,
    /// LINK #imm
    LinkPoundImm,
    /// LINK addr
    LinkRelative,
    /// FOO (reg)
    ParRegEns(Reg),
    /// FOO reg
    Reg(Reg),
    /// FOO R1, #imm
    RegCommaPoundImm8(Reg),
    /// FOO R1, #imm
    RegCommaPoundImm16(Reg),
    /// FOO addr
    Relative8,
}

impl AddrMode for SuperFx {
    fn macro_args(&self, pool: &mut RcPool) -> Vec<AsmMacroArgAst> {
        match *self {
            Self::Implied => vec![],
            Self::LinkPoundImm => vec![pound_imm_arg(pool)],
            Self::LinkRelative | Self::Relative8 => {
                vec![addr_arg(pool)]
            }
            Self::ParRegEns(reg) => vec![par_reg_ens_arg(pool, reg)],
            Self::Reg(reg) => vec![reg_arg(pool, reg)],
            Self::RegCommaPoundImm8(reg) | Self::RegCommaPoundImm16(reg) => {
                vec![reg_arg(pool, reg), pound_imm_arg(pool)]
            }
        }
    }

    fn macro_body(
        &self,
        pool: &mut RcPool,
        prefix_bytes: &[u8],
    ) -> Vec<AsmStmtAst> {
        match *self {
            Self::Implied | Self::ParRegEns(_) | Self::Reg(_) => {
                vec![pool.constant_bytes_stmt(prefix_bytes)]
            }
            Self::LinkPoundImm => link_pound_imm_body(pool, PLACEHOLDER_IMM),
            Self::LinkRelative => link_relative_body(pool, PLACEHOLDER_ADDR),
            Self::RegCommaPoundImm8(_) => {
                vec![
                    pool.constant_bytes_stmt(prefix_bytes),
                    pool.placeholder_u8(PLACEHOLDER_IMM),
                ]
            }
            Self::RegCommaPoundImm16(_) => {
                vec![
                    pool.constant_bytes_stmt(prefix_bytes),
                    pool.placeholder_u16le(PLACEHOLDER_IMM),
                ]
            }
            Self::Relative8 => vec![
                pool.constant_bytes_stmt(prefix_bytes),
                pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR),
            ],
        }
    }
}

//===========================================================================//

fn link_pound_imm_body(
    pool: &mut RcPool,
    placeholder: &'static str,
) -> Vec<AsmStmtAst> {
    // TODO: use a let statement to only eval the placeholder expression once
    let opcode_expr = {
        let lhs = pool.int_literal_expr(0x90);
        let rhs = {
            let pred = {
                let lhs = {
                    let lhs = pool.placeholder_expr(placeholder);
                    let rhs = pool.int_literal_expr(1);
                    pool.binop_expr(BinOpAst::CmpGe, lhs, rhs)
                };
                let rhs = {
                    let lhs = pool.placeholder_expr(placeholder);
                    let rhs = pool.int_literal_expr(4);
                    pool.binop_expr(BinOpAst::CmpLe, lhs, rhs)
                };
                pool.binop_expr(BinOpAst::LogAnd, lhs, rhs)
            };
            let ok = pool.placeholder_expr(placeholder);
            let err = pool.error_expr(
                "LINK immediate value must be in the range [1, 4]",
            );
            pool.ternary_expr(pred, ok, err)
        };
        pool.binop_expr(BinOpAst::Add, lhs, rhs)
    };
    vec![pool.int_data_stmt(AsmIntTypeAst::U8, opcode_expr)]
}

fn link_relative_body(
    pool: &mut RcPool,
    placeholder: &'static str,
) -> Vec<AsmStmtAst> {
    let dest_expr = pool.placeholder_expr(placeholder);
    let base_expr = {
        // TODO: Use $> here instead of ($< + 1)
        let lhs = pool.here_label_expr();
        let rhs = pool.int_literal_expr(1);
        pool.binop_expr(BinOpAst::Add, lhs, rhs)
    };
    vec![pool.rel_addr_stmt(
        AsmRelTypeAst::Addr16RelLink,
        dest_expr,
        base_expr,
    )]
}

//===========================================================================//
