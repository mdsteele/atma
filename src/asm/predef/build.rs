use super::super::arch::ArchTree;
use super::super::macros::MacroTable;
use super::pool::RcPool;
use crate::error::SrcSpan;
use crate::obj::ObjSrcContext;
use crate::parse::{
    AsmDefMacroAst, AsmIntTypeAst, AsmMacroArgAst, AsmRelTypeAst, AsmStmtAst,
    BinOpAst, Token, TokenValue,
};
use std::rc::Rc;

//===========================================================================//

const PLACEHOLDER_ADDR: &str = "%ADDR";
const PLACEHOLDER_ADDR2: &str = "%ADDR2";
const PLACEHOLDER_IMM: &str = "%IMM";
const PLACEHOLDER_IMM2: &str = "%IMM2";

//===========================================================================//

pub(super) type Reg = &'static str;

#[derive(Clone, Copy)]
pub(super) enum AddrMode {
    /// FOO addr
    Addr8,
    /// FOO addr1, addr2
    Addr8CommaAddr8,
    /// FOO addr, #imm
    Addr8CommaPoundImm8,
    /// FOO addr + R
    Addr8PlusReg(Reg),
    /// FOO addr + R1, R2
    Addr8PlusRegCommaReg(Reg, Reg),
    /// FOO addr, R
    Addr8CommaReg(Reg),
    /// FOO addr
    Addr16,
    /// FOO addr
    AddrHi,
    /// FOO !addr
    BangAddr16,
    /// FOO !addr, R
    BangAddr16CommaReg(Reg),
    /// FOO !addr + R1, R2
    BangAddr16PlusRegCommaReg(Reg, Reg),
    /// FOO !!addr
    BangBangAddr24,
    /// FOO !!addr, R
    BangBangAddr24CommaReg(Reg),
    /// FOO [addr]
    BracAddr8Kets,
    /// FOO [addr], R
    BracAddr8KetsCommaReg(Reg),
    /// FOO [addr] + R1, R2
    BracAddr8KetsPlusRegCommaReg(Reg, Reg),
    /// FOO [addr + R1], R2
    BracAddr8PlusRegKetsCommaReg(Reg, Reg),
    /// FOO [!addr]
    BracBangAddr16Kets,
    /// FOO [!addr + R]
    BracBangAddr16PlusRegKets(Reg),
    /// FOO (addr, R)
    ParAddr8CommaRegEns(Reg),
    /// FOO (addr, R1), R2
    ParAddr8CommaRegEnsCommaReg(Reg, Reg),
    /// FOO (addr)
    ParAddr8Ens,
    /// FOO (addr), R
    ParAddr8EnsCommaReg(Reg),
    /// FOO (!addr, R)
    ParBangAddr16CommaRegEns(Reg),
    /// FOO (!addr)
    ParBangAddr16Ens,
    /// FOO (R)
    ParRegEns(Reg),
    /// FOO (R1), (R2)
    ParRegEnsCommaParRegEns(Reg, Reg),
    /// FOO (R1), R2
    ParRegEnsCommaReg(Reg, Reg),
    /// FOO (R1)+, R2
    ParRegEnsPlusCommaReg(Reg, Reg),
    /// FOO #imm
    PoundImm8,
    /// FOO #imm, #imm2
    PoundImm8CommaPoundImm8,
    /// FOO ##imm
    PoundPoundImm16,
    /// FOO
    Implied,
    /// FOO R
    Reg(Reg),
    /// FOO R, addr
    RegCommaAddr8(Reg),
    /// FOO R1, addr + R2
    RegCommaAddr8PlusReg(Reg, Reg),
    /// FOO R, !addr
    RegCommaBangAddr16(Reg),
    /// FOO R1, !addr + R2
    RegCommaBangAddr16PlusReg(Reg, Reg),
    /// FOO R1, [addr] + R2
    RegCommaBracAddr8KetsPlusReg(Reg, Reg),
    /// FOO R1, [addr + R2]
    RegCommaBracAddr8PlusRegKets(Reg, Reg),
    /// FOO R1, (R2)
    RegCommaParRegEns(Reg, Reg),
    /// FOO R1, (R2)+
    RegCommaParRegEnsPlus(Reg, Reg),
    /// FOO R1, #imm
    RegCommaPoundImm8(Reg),
    /// FOO R1, #imm
    RegCommaPoundImm16(Reg),
    /// FOO R1, R2
    RegCommaReg(Reg, Reg),
    /// FOO addr
    Relative8,
    /// FOO addr
    Relative16,
    /// FOO #imm
    SuperFxLinkImm,
    /// FOO addr
    SuperFxLinkRel,
}

//===========================================================================//

pub(super) struct BuiltinBuilder {
    pub(super) arch_tree: ArchTree,
    pub(super) macros: MacroTable,
    pub(super) context: Rc<ObjSrcContext>,
    pub(super) pool: RcPool,
}

impl BuiltinBuilder {
    pub fn add_macros(
        &mut self,
        arch: &'static str,
        macros: &[(&'static str, u8, AddrMode)],
    ) {
        let arch = self.pool.string(arch);
        for &(name, opcode, addr_mode) in macros {
            self.add_macro(&arch, name, opcode, addr_mode);
        }
    }

    fn add_macro(
        &mut self,
        arch: &Rc<str>,
        name: &'static str,
        opcode_byte: u8,
        addr_mode: AddrMode,
    ) {
        let params = match addr_mode {
            AddrMode::Addr8
            | AddrMode::Addr16
            | AddrMode::AddrHi
            | AddrMode::Relative8
            | AddrMode::Relative16
            | AddrMode::SuperFxLinkRel => {
                vec![self.addr_arg()]
            }
            AddrMode::Addr8CommaAddr8 => {
                vec![self.addr_arg(), self.addr2_arg()]
            }
            AddrMode::Addr8PlusReg(r1) => {
                vec![self.addr_plus_reg_arg(r1)]
            }
            AddrMode::Addr8PlusRegCommaReg(r1, r2) => {
                vec![self.addr_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::Addr8CommaPoundImm8 => {
                vec![self.addr_arg(), self.pound_imm_arg()]
            }
            AddrMode::Addr8CommaReg(r1) => {
                vec![self.addr_arg(), self.reg_arg(r1)]
            }
            AddrMode::BangAddr16 => vec![self.bang_addr_arg()],
            AddrMode::BangAddr16PlusRegCommaReg(r1, r2) => {
                vec![self.bang_addr_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BangAddr16CommaReg(r1) => {
                vec![self.bang_addr_arg(), self.reg_arg(r1)]
            }
            AddrMode::BangBangAddr24 => vec![self.bang_bang_addr_arg()],
            AddrMode::BangBangAddr24CommaReg(r1) => {
                vec![self.bang_bang_addr_arg(), self.reg_arg(r1)]
            }
            AddrMode::BracAddr8Kets => vec![self.brac_addr_kets_arg()],
            AddrMode::BracAddr8KetsCommaReg(r1) => {
                vec![self.brac_addr_kets_arg(), self.reg_arg(r1)]
            }
            AddrMode::BracAddr8KetsPlusRegCommaReg(r1, r2) => {
                vec![self.brac_addr_kets_plus_reg_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BracAddr8PlusRegKetsCommaReg(r1, r2) => {
                vec![self.brac_addr_plus_reg_kets_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::BracBangAddr16Kets => {
                vec![self.brac_bang_addr_kets_arg()]
            }
            AddrMode::BracBangAddr16PlusRegKets(r1) => {
                vec![self.brac_bang_addr_plus_reg_kets_arg(r1)]
            }
            AddrMode::Implied => vec![],
            AddrMode::ParAddr8CommaRegEns(r1) => {
                vec![self.par_addr_comma_reg_ens_arg(r1)]
            }
            AddrMode::ParAddr8CommaRegEnsCommaReg(r1, r2) => {
                vec![self.par_addr_comma_reg_ens_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::ParAddr8Ens => vec![self.par_addr_ens_arg()],
            AddrMode::ParAddr8EnsCommaReg(r1) => {
                vec![self.par_addr_ens_arg(), self.reg_arg(r1)]
            }
            AddrMode::ParBangAddr16CommaRegEns(r1) => {
                vec![self.par_bang_addr_comma_reg_ens_arg(r1)]
            }
            AddrMode::ParBangAddr16Ens => vec![self.par_bang_addr_ens_arg()],
            AddrMode::ParRegEns(r1) => vec![self.par_reg_ens_arg(r1)],
            AddrMode::ParRegEnsCommaParRegEns(r1, r2) => {
                vec![self.par_reg_ens_arg(r1), self.par_reg_ens_arg(r2)]
            }
            AddrMode::ParRegEnsPlusCommaReg(r1, r2) => {
                vec![self.par_reg_ens_plus_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::ParRegEnsCommaReg(r1, r2) => {
                vec![self.par_reg_ens_arg(r1), self.reg_arg(r2)]
            }
            AddrMode::PoundImm8 | AddrMode::SuperFxLinkImm => {
                vec![self.pound_imm_arg()]
            }
            AddrMode::PoundImm8CommaPoundImm8 => {
                vec![self.pound_imm_arg(), self.pound_imm2_arg()]
            }
            AddrMode::PoundPoundImm16 => vec![self.pound_pound_imm_arg()],
            AddrMode::Reg(r1) => vec![self.reg_arg(r1)],
            AddrMode::RegCommaAddr8(r1) => {
                vec![self.reg_arg(r1), self.addr_arg()]
            }
            AddrMode::RegCommaAddr8PlusReg(r1, r2) => {
                vec![self.reg_arg(r1), self.addr_plus_reg_arg(r2)]
            }
            AddrMode::RegCommaBangAddr16(r1) => {
                vec![self.reg_arg(r1), self.bang_addr_arg()]
            }
            AddrMode::RegCommaBangAddr16PlusReg(r1, r2) => {
                vec![self.reg_arg(r1), self.bang_addr_plus_reg_arg(r2)]
            }
            AddrMode::RegCommaBracAddr8KetsPlusReg(r1, r2) => {
                vec![self.reg_arg(r1), self.brac_addr_kets_plus_reg_arg(r2)]
            }
            AddrMode::RegCommaBracAddr8PlusRegKets(r1, r2) => {
                vec![self.reg_arg(r1), self.brac_addr_plus_reg_kets_arg(r2)]
            }
            AddrMode::RegCommaParRegEns(r1, r2) => {
                vec![self.reg_arg(r1), self.par_reg_ens_arg(r2)]
            }
            AddrMode::RegCommaParRegEnsPlus(r1, r2) => {
                vec![self.reg_arg(r1), self.par_reg_ens_plus_arg(r2)]
            }
            AddrMode::RegCommaPoundImm8(r1)
            | AddrMode::RegCommaPoundImm16(r1) => {
                vec![self.reg_arg(r1), self.pound_imm_arg()]
            }
            AddrMode::RegCommaReg(r1, r2) => {
                vec![self.reg_arg(r1), self.reg_arg(r2)]
            }
        };
        let body = match addr_mode {
            AddrMode::Addr8
            | AddrMode::Addr8PlusReg(_)
            | AddrMode::Addr8PlusRegCommaReg(_, _)
            | AddrMode::Addr8CommaReg(_)
            | AddrMode::BracAddr8Kets
            | AddrMode::BracAddr8KetsCommaReg(_)
            | AddrMode::BracAddr8KetsPlusRegCommaReg(_, _)
            | AddrMode::BracAddr8PlusRegKetsCommaReg(_, _)
            | AddrMode::ParAddr8CommaRegEns(_)
            | AddrMode::ParAddr8CommaRegEnsCommaReg(_, _)
            | AddrMode::ParAddr8Ens
            | AddrMode::ParAddr8EnsCommaReg(_)
            | AddrMode::RegCommaAddr8(_)
            | AddrMode::RegCommaAddr8PlusReg(_, _)
            | AddrMode::RegCommaBracAddr8PlusRegKets(_, _)
            | AddrMode::RegCommaBracAddr8KetsPlusReg(_, _) => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            AddrMode::Addr8CommaAddr8 => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_u8(PLACEHOLDER_ADDR2),
                self.pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            AddrMode::Addr8CommaPoundImm8 => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_u8(PLACEHOLDER_IMM),
                self.pool.placeholder_u8(PLACEHOLDER_ADDR),
            ],
            AddrMode::Addr16
            | AddrMode::BangAddr16
            | AddrMode::BangAddr16PlusRegCommaReg(_, _)
            | AddrMode::BangAddr16CommaReg(_)
            | AddrMode::BracBangAddr16Kets
            | AddrMode::BracBangAddr16PlusRegKets(_)
            | AddrMode::ParBangAddr16CommaRegEns(_)
            | AddrMode::ParBangAddr16Ens
            | AddrMode::RegCommaBangAddr16(_)
            | AddrMode::RegCommaBangAddr16PlusReg(_, _) => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_u16le(PLACEHOLDER_ADDR),
            ],
            AddrMode::AddrHi => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.high_page_addr(PLACEHOLDER_ADDR),
            ],
            AddrMode::BangBangAddr24 | AddrMode::BangBangAddr24CommaReg(_) => {
                vec![
                    self.pool.constant_u8(opcode_byte),
                    self.pool.placeholder_u24le(PLACEHOLDER_ADDR),
                ]
            }
            AddrMode::Implied
            | AddrMode::ParRegEns(_)
            | AddrMode::ParRegEnsCommaParRegEns(_, _)
            | AddrMode::ParRegEnsPlusCommaReg(_, _)
            | AddrMode::ParRegEnsCommaReg(_, _)
            | AddrMode::Reg(_)
            | AddrMode::RegCommaReg(_, _)
            | AddrMode::RegCommaParRegEns(_, _)
            | AddrMode::RegCommaParRegEnsPlus(_, _) => {
                vec![self.pool.constant_u8(opcode_byte)]
            }
            AddrMode::PoundImm8 | AddrMode::RegCommaPoundImm8(_) => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_u8(PLACEHOLDER_IMM),
            ],
            AddrMode::PoundImm8CommaPoundImm8 => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_u8(PLACEHOLDER_IMM2),
                self.pool.placeholder_u8(PLACEHOLDER_IMM),
            ],
            AddrMode::PoundPoundImm16 | AddrMode::RegCommaPoundImm16(_) => {
                vec![
                    self.pool.constant_u8(opcode_byte),
                    self.pool.placeholder_u16le(PLACEHOLDER_IMM),
                ]
            }
            AddrMode::Relative8 => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_addr16_rel8(PLACEHOLDER_ADDR),
            ],
            AddrMode::Relative16 => vec![
                self.pool.constant_u8(opcode_byte),
                self.pool.placeholder_addr16_rel16le(PLACEHOLDER_ADDR),
            ],
            AddrMode::SuperFxLinkImm => {
                super_fx_link_imm(&mut self.pool, PLACEHOLDER_IMM)
            }
            AddrMode::SuperFxLinkRel => {
                super_fx_link_rel(&mut self.pool, PLACEHOLDER_ADDR)
            }
        };
        let definition =
            AsmDefMacroAst { id: self.pool.standard_id(name), params, body };
        let reserved = self.arch_tree.reserved_names(arch);
        self.macros
            .define(self.context.clone(), arch, reserved, definition)
            .unwrap();
    }

    fn addr_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![self.pool.placeholder_token(PLACEHOLDER_ADDR)],
        }
    }

    fn addr2_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![self.pool.placeholder_token(PLACEHOLDER_ADDR2)],
        }
    }

    fn addr_plus_reg_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::Plus),
                self.pool.identifier_token(reg),
            ],
        }
    }

    fn bang_addr_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
            ],
        }
    }

    fn bang_bang_addr_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::Bang),
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
            ],
        }
    }

    fn bang_addr_plus_reg_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::Plus),
                self.pool.identifier_token(reg),
            ],
        }
    }

    fn brac_addr_kets_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::BracketOpen),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn brac_addr_kets_plus_reg_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::BracketOpen),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::BracketClose),
                token(TokenValue::Plus),
                self.pool.identifier_token(reg),
            ],
        }
    }

    fn brac_addr_plus_reg_kets_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::BracketOpen),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::Plus),
                self.pool.identifier_token(reg),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn brac_bang_addr_kets_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn brac_bang_addr_plus_reg_kets_arg(
        &mut self,
        reg: Reg,
    ) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::BracketOpen),
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::Plus),
                self.pool.identifier_token(reg),
                token(TokenValue::BracketClose),
            ],
        }
    }

    fn par_addr_comma_reg_ens_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::ParenOpen),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::Comma),
                self.pool.identifier_token(reg),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_addr_ens_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::ParenOpen),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_bang_addr_ens_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::ParenOpen),
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_bang_addr_comma_reg_ens_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::ParenOpen),
                token(TokenValue::Bang),
                self.pool.placeholder_token(PLACEHOLDER_ADDR),
                token(TokenValue::Comma),
                self.pool.identifier_token(reg),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_reg_ens_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::ParenOpen),
                self.pool.identifier_token(reg),
                token(TokenValue::ParenClose),
            ],
        }
    }

    fn par_reg_ens_plus_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::ParenOpen),
                self.pool.identifier_token(reg),
                token(TokenValue::ParenClose),
                token(TokenValue::Plus),
            ],
        }
    }

    fn pound_imm_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::Pound),
                self.pool.placeholder_token(PLACEHOLDER_IMM),
            ],
        }
    }

    fn pound_imm2_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::Pound),
                self.pool.placeholder_token(PLACEHOLDER_IMM2),
            ],
        }
    }

    fn pound_pound_imm_arg(&mut self) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![
                token(TokenValue::Pound),
                token(TokenValue::Pound),
                self.pool.placeholder_token(PLACEHOLDER_IMM),
            ],
        }
    }

    fn reg_arg(&mut self, reg: Reg) -> AsmMacroArgAst {
        AsmMacroArgAst {
            span: SrcSpan::INTERNAL,
            tokens: vec![self.pool.identifier_token(reg)],
        }
    }
}

//===========================================================================//

fn super_fx_link_imm(
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

fn super_fx_link_rel(
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

fn token(value: TokenValue) -> Token {
    Token { span: SrcSpan::INTERNAL, value }
}

//===========================================================================//
