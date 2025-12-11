use crate::expr::{ExprCompiler, ExprEnv, ExprType, ExprValue};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, ParseError, ParseResult, SrcSpan};
use std::rc::Rc;

//===========================================================================//

pub(crate) struct AsmTypeEnv {}

impl AsmTypeEnv {
    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> ParseResult<(ObjExpr, ExprType)> {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, expr_type, _static_value)) => {
                debug_assert!(!ops.is_empty());
                Ok((ObjExpr { ops: Rc::from(ops) }, expr_type))
            }
            Err(errors) => Err(errors),
        }
    }
}

impl ExprEnv for AsmTypeEnv {
    type Op = ObjExprOp;

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        _id: &str,
    ) -> ParseResult<(Self::Op, ExprType, Option<ExprValue>)> {
        let message = "TODO: support identifiers".to_string();
        Err(vec![ParseError::new(span, message)])
    }
}

//===========================================================================//
