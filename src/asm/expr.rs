use crate::expr::{ExprCompiler, ExprEnv, ExprType, ExprValue};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, IdentifierAst, ParseError, ParseResult, SrcSpan};
use num_bigint::BigInt;
use std::collections::HashSet;
use std::rc::Rc;

//===========================================================================//

pub(crate) struct AsmTypeEnv {
    labels: HashSet<Rc<str>>,
}

impl AsmTypeEnv {
    pub fn new() -> AsmTypeEnv {
        AsmTypeEnv { labels: HashSet::new() }
    }

    pub fn declare_label(&mut self, id_ast: &IdentifierAst) {
        self.labels.insert(id_ast.name.clone());
    }

    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> ParseResult<(ObjExpr, ExprType)> {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, expr_type, _static_value)) => {
                debug_assert!(!ops.is_empty());
                Ok((ObjExpr { ops }, expr_type))
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
        id: &str,
    ) -> ParseResult<(Self::Op, ExprType, Option<ExprValue>)> {
        if self.labels.contains(id) {
            let value = ExprValue::Label(Rc::from(id), Rc::from(BigInt::ZERO));
            let op = ObjExprOp::Push(value.clone());
            Ok((op, ExprType::Label, Some(value)))
        } else {
            let message = format!("unknown identifier: {id}");
            Err(vec![ParseError::new(span, message)])
        }
    }
}

//===========================================================================//
