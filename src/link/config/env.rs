use super::ConfigVariableOr;
use crate::error::{SourceError, SourceResult, SrcSpan};
use crate::expr::{
    ExprCompiler, ExprEnv, ExprType, ExprTypeError, ExprTypeResult, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, IdentifierAst};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct LinkDecl {
    pub id_span: SrcSpan,
    pub var_type: ExprType,
    pub value: ConfigVariableOr<ExprValue>,
}

//===========================================================================//

pub(super) struct LinkTypeEnv {
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
        value: ConfigVariableOr<ExprValue>,
    ) {
        let id_span = id.span;
        let decl = LinkDecl { id_span, var_type, value };
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
                let (op, static_value) = match &decl.value {
                    &ConfigVariableOr::Variable(index) => {
                        (ObjExprOp::GetValue(index), None)
                    }
                    ConfigVariableOr::Static(value) => {
                        (ObjExprOp::Push(value.clone()), Some(value.clone()))
                    }
                };
                Ok((op, decl.var_type.clone(), static_value))
            }
            None => Err(vec![ExprTypeError::UnknownIdentifier {
                span,
                name: name.clone(),
            }]),
        }
    }
}

//===========================================================================//
