use crate::expr::{ExprCompiler, ExprEnv, ExprType, ExprValue};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, IdentifierAst, ParseError, ParseResult, SrcSpan};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(crate) struct AsmTypeEnv {
    labels: HashMap<Rc<str>, SrcSpan>,
}

impl AsmTypeEnv {
    pub fn new() -> AsmTypeEnv {
        AsmTypeEnv { labels: HashMap::new() }
    }

    pub fn declare_import(
        &mut self,
        id_ast: &IdentifierAst,
    ) -> ParseResult<()> {
        self.declare_label(id_ast)
    }

    pub fn declare_label(
        &mut self,
        id_ast: &IdentifierAst,
    ) -> ParseResult<()> {
        if let Some(&prev_span) = self.labels.get(&id_ast.name) {
            let message =
                format!("symbol was already declared: {}", id_ast.name);
            let label1 = "previously declared here".to_string();
            let label2 = "redeclared here".to_string();
            Err(vec![
                ParseError::new(id_ast.span, message)
                    .with_label(prev_span, label1)
                    .with_label(id_ast.span, label2),
            ])
        } else {
            self.labels.insert(id_ast.name.clone(), id_ast.span);
            Ok(())
        }
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
        if self.labels.contains_key(id) {
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
