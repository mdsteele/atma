use super::ConfigVariableOr;
use super::error::ConfigResult;
use crate::addr::Addr;
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprLabel, ExprNotStaticReason,
    ExprStatic, ExprType, ExprTypeError, ExprTypeResult, ExprUnOp, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp};
use crate::parse::{ExprAst, IdentifierAst};
use num_bigint::BigInt;
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
    exports: HashMap<Rc<str>, SrcSpan>,
    imports: HashMap<Rc<str>, SrcSpan>,
    variables: HashMap<Rc<str>, LinkDecl>,
}

impl LinkTypeEnv {
    pub fn new() -> LinkTypeEnv {
        LinkTypeEnv {
            exports: HashMap::new(),
            imports: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn get_declaration(&self, name: &str) -> Option<&LinkDecl> {
        self.variables.get(name)
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

    pub fn get_export(&self, name: &str) -> Option<SrcSpan> {
        self.exports.get(name).copied()
    }

    pub fn add_export(
        &mut self,
        id: IdentifierAst,
        space: Rc<str>,
        address: ConfigVariableOr<Addr>,
    ) {
        self.exports.insert(id.name.clone(), id.span);
        let address_value = address.map_static(|addr| {
            ExprValue::Label(ExprLabel::AddrAbsolute {
                space,
                address: BigInt::from(addr),
            })
        });
        self.add_declaration(id, ExprType::Label, address_value);
    }

    pub fn get_import(&self, name: &str) -> Option<SrcSpan> {
        self.imports.get(name).copied()
    }

    pub fn add_import(&mut self, id: IdentifierAst) {
        self.imports.insert(id.name.clone(), id.span);
        let label = ExprLabel::SymbolRelative {
            name: id.name.clone(),
            offset: BigInt::ZERO,
        };
        let value = ConfigVariableOr::Static(ExprValue::Label(label));
        self.add_declaration(id, ExprType::Label, value);
    }

    pub fn typecheck_expression(
        &self,
        expr: ExprAst,
    ) -> ConfigResult<(ObjExpr, ExprType, ExprStatic)> {
        let (ops, expr_type, expr_static) =
            ExprCompiler::new(self).typecheck(expr).map_err(Errs::coerce)?;
        debug_assert!(!ops.is_empty());
        Ok((ObjExpr { ops }, expr_type, expr_static))
    }
}

impl ExprEnv for LinkTypeEnv {
    type Op = ObjExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)> {
        Err(Errs::one(ExprTypeError::RelativeLabelInLinkerConfig { span }))
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, ExprStatic)> {
        match self.variables.get(name) {
            Some(decl) => {
                let (op, expr_static) = match &decl.value {
                    &ConfigVariableOr::Variable(index) => {
                        let reason = ExprNotStaticReason::Variable {
                            span,
                            name: name.clone(),
                        };
                        (ObjExprOp::GetValue(index), Err(reason))
                    }
                    ConfigVariableOr::Static(value) => {
                        (ObjExprOp::Push(value.clone()), Ok(value.clone()))
                    }
                };
                Ok((op, decl.var_type.clone(), expr_static))
            }
            None => Err(Errs::one(ExprTypeError::UnknownIdentifier {
                span,
                name: name.clone(),
            })),
        }
    }

    fn apply_function_op(
        &self,
        _func_span: SrcSpan,
        _arg_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::Apply
    }

    fn binary_operation_op(
        &self,
        binop: ExprBinOp,
        _op_span: SrcSpan,
        _lhs_span: SrcSpan,
        _rhs_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::BinOp(binop)
    }

    fn list_index_op(
        &self,
        _list_span: SrcSpan,
        _index_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::ListIndex
    }

    fn unary_operation_op(
        &self,
        unop: ExprUnOp,
        _op_span: SrcSpan,
        _arg_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::UnOp(unop)
    }
}

//===========================================================================//
