use crate::expr::{ExprCompiler, ExprEnv, ExprType, ExprValue};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, ParseError, SrcSpan};

//===========================================================================//

pub(crate) struct AsmTypeEnv {}

impl AsmTypeEnv {
    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> Result<(ObjExpr, ExprType), Vec<ParseError>> {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, ty)) => {
                debug_assert!(!ops.is_empty());
                Ok((ObjExpr { ops }, ty))
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
    ) -> Result<(Self::Op, ExprType, Option<ExprValue>), Vec<ParseError>> {
        let message = "TODO: support identifiers".to_string();
        Err(vec![ParseError::new(span, message)])
    }
}

//===========================================================================//
