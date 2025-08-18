use super::expr::{AdsExpr, AdsTypeEnv};
use super::value::AdsType;
use crate::parse::{BreakpointAst, ParseError, SrcSpan};

//===========================================================================//

pub enum AdsBreakpoint {
    Pc(AdsExpr),
}

impl AdsBreakpoint {
    pub fn typecheck(
        breakpoint_ast: &BreakpointAst,
        env: &AdsTypeEnv,
    ) -> Result<AdsBreakpoint, Vec<ParseError>> {
        match breakpoint_ast {
            BreakpointAst::Pc(expr_ast) => {
                let (expr, ty) = AdsExpr::typecheck(expr_ast, env)?;
                AdsBreakpoint::check_addr_type(expr_ast.span, ty)?;
                Ok(AdsBreakpoint::Pc(expr))
            }
        }
    }

    fn check_addr_type(
        span: SrcSpan,
        ty: AdsType,
    ) -> Result<(), Vec<ParseError>> {
        if let AdsType::Integer = ty {
            Ok(())
        } else {
            let message =
                format!("breakpoint address must be of type int, not {ty}");
            let label = format!("this expression has type {ty}");
            let error = ParseError::new(span, message).with_label(span, label);
            Err(vec![error])
        }
    }
}

//===========================================================================//
