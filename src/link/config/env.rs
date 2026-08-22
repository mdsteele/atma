use super::ConfigVariableOr;
use super::error::ConfigResult;
use crate::addr::Addr;
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprLabel, ExprNotStaticReason,
    ExprStatic, ExprType, ExprTypeError, ExprTypeResult, ExprUnOp, ExprValue,
    make_global_builtin_values,
};
use crate::obj::{ObjExpr, ObjExprOp, ObjSrcContext, ObjSrcLoc, ObjSrcParent};
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
    context: Rc<ObjSrcContext>,
    builtins: HashMap<Rc<str>, (ExprValue, ExprType)>,
    exports: HashMap<Rc<str>, SrcSpan>,
    imports: HashMap<Rc<str>, SrcSpan>,
    variables: HashMap<Rc<str>, LinkDecl>,
}

impl LinkTypeEnv {
    pub fn new(src_path: Rc<str>) -> LinkTypeEnv {
        LinkTypeEnv {
            context: Rc::new(ObjSrcContext {
                path: src_path,
                parent: ObjSrcParent::Root,
            }),
            builtins: make_global_builtin_values(),
            exports: HashMap::new(),
            imports: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn current_src_context(&self) -> Rc<ObjSrcContext> {
        self.context.clone()
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

    pub fn make_loc(&self, span: SrcSpan) -> ObjSrcLoc {
        ObjSrcLoc { span, context: self.current_src_context() }
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
        if let Some(decl) = self.variables.get(name) {
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
            return Ok((op, decl.var_type.clone(), expr_static));
        }
        if let Some((value, expr_type)) = self.builtins.get(name) {
            let op = ObjExprOp::Push(value.clone());
            return Ok((op, expr_type.clone(), Ok(value.clone())));
        }
        Err(Errs::one(ExprTypeError::UnknownIdentifier {
            span,
            name: name.clone(),
        }))
    }

    fn apply_function_op(
        &self,
        _func_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::Apply { context: self.current_src_context(), arg_span }
    }

    fn binary_operation_op(
        &self,
        binop: ExprBinOp,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::BinOp {
            context: self.current_src_context(),
            binop,
            op_span,
            lhs_span,
            rhs_span,
        }
    }

    fn list_index_op(
        &self,
        list_span: SrcSpan,
        index_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::ListIndex {
            context: self.current_src_context(),
            list_span,
            index_span,
        }
    }

    fn unary_operation_op(
        &self,
        unop: ExprUnOp,
        op_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> Self::Op {
        ObjExprOp::UnOp {
            context: self.current_src_context(),
            unop,
            op_span,
            arg_span,
        }
    }
}

//===========================================================================//
