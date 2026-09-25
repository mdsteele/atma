use super::pool::RcPool;
use crate::error::SrcSpan;
use crate::lex::{Token, TokenValue};
use crate::parse::{AsmMacroArgAst, AsmStmtAst};

//===========================================================================//

pub(super) const PLACEHOLDER_ADDR: &str = "%ADDR";
pub(super) const PLACEHOLDER_ADDR2: &str = "%ADDR2";
pub(super) const PLACEHOLDER_IMM: &str = "%IMM";
pub(super) const PLACEHOLDER_IMM2: &str = "%IMM2";

//===========================================================================//

pub(super) type Reg = &'static str;

//===========================================================================//

pub(super) trait AddrMode {
    fn macro_args(&self, pool: &mut RcPool) -> Vec<AsmMacroArgAst>;

    fn macro_body(
        &self,
        pool: &mut RcPool,
        prefix_bytes: &[u8],
    ) -> Vec<AsmStmtAst>;
}

//===========================================================================//

pub(super) fn addr_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![pool.placeholder_token(PLACEHOLDER_ADDR)])
}

pub(super) fn bang_addr_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Bang),
        pool.placeholder_token(PLACEHOLDER_ADDR),
    ])
}

pub(super) fn par_reg_ens_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::ParenOpen),
        pool.identifier_token(reg),
        token(TokenValue::ParenClose),
    ])
}

pub(super) fn pound_imm_arg(pool: &mut RcPool) -> AsmMacroArgAst {
    macro_arg(vec![
        token(TokenValue::Pound),
        pool.placeholder_token(PLACEHOLDER_IMM),
    ])
}

pub(super) fn reg_arg(pool: &mut RcPool, reg: Reg) -> AsmMacroArgAst {
    macro_arg(vec![pool.identifier_token(reg)])
}

//===========================================================================//

pub(super) fn macro_arg(tokens: Vec<Token>) -> AsmMacroArgAst {
    AsmMacroArgAst { span: SrcSpan::INTERNAL, tokens }
}

pub(super) fn token(value: TokenValue) -> Token {
    Token { span: SrcSpan::INTERNAL, value }
}

//===========================================================================//
