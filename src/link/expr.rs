use crate::error::{SourceError, SourceResult, SrcSpan};
use crate::expr::{
    ExprCompiler, ExprEnv, ExprType, ExprTypeError, ExprTypeResult, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, IdentifierAst};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(crate) struct LinkDecl {
    pub id_span: SrcSpan,
    pub var_type: ExprType,
    pub static_value: Option<ExprValue>,
    pub stack_index: usize,
}

//===========================================================================//

pub(crate) struct LinkTypeEnv {
    variables: HashMap<Rc<str>, LinkDecl>,
}

impl LinkTypeEnv {
    pub fn new() -> LinkTypeEnv {
        LinkTypeEnv { variables: HashMap::new() }
    }

    pub fn get_declaration(&self, id: &str) -> Option<&LinkDecl> {
        self.variables.get(id)
    }

    pub fn add_declaration(
        &mut self,
        id: IdentifierAst,
        var_type: ExprType,
        static_value: Option<ExprValue>,
    ) {
        let id_span = id.span;
        let stack_index = self.variables.len();
        let decl = LinkDecl { id_span, var_type, static_value, stack_index };
        self.variables.insert(id.name, decl);
    }

    pub fn typecheck_expression(
        &self,
        expr: &ExprAst,
    ) -> SourceResult<(ObjExpr, ExprType, Option<ExprValue>)> {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, expr_type, static_value)) => {
                debug_assert!(!ops.is_empty());
                Ok((ObjExpr { ops }, expr_type, static_value))
            }
            Err(errors) => Err(SourceError::from_errors(errors)),
        }
    }
}

impl ExprEnv for LinkTypeEnv {
    type Op = ObjExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, Option<ExprValue>)> {
        Err(vec![ExprTypeError::RelativeLabelInLinkerConfig { span }])
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, Option<ExprValue>)> {
        match self.variables.get(name) {
            Some(decl) => {
                let var_type = decl.var_type.clone();
                let static_value = decl.static_value.clone();
                let op = ObjExprOp::GetValue(decl.stack_index);
                Ok((op, var_type, static_value))
            }
            None => Err(vec![ExprTypeError::UnknownIdentifier {
                span,
                name: name.clone(),
            }]),
        }
    }
}

//===========================================================================//
