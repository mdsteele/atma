use super::ConfigVariableOr;
use super::builtin::make_config_builtin_values;
use super::error::{ConfigError, ConfigResult};
use crate::addr::Addr;
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprLabel, ExprNotStaticReason,
    ExprStatic, ExprType, ExprTypeError, ExprTypeResult, ExprUnOp, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp, ObjSrcContext, ObjSrcLoc};
use crate::parse::{ExprAst, IdentifierAst, LinkConfigAst};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct ConfigDecl {
    pub id_loc: ObjSrcLoc,
    pub var_type: ExprType,
    pub value: ConfigVariableOr<ExprValue>,
}

//===========================================================================//

pub(super) struct ConfigTypeEnv {
    context_stack: Vec<Rc<ObjSrcContext>>,
    builtins: HashMap<Rc<str>, (ExprValue, ExprType)>,
    exports: HashMap<Rc<str>, ObjSrcLoc>,
    imports: HashMap<Rc<str>, ObjSrcLoc>,
    variables: HashMap<Rc<str>, ConfigDecl>,
}

impl ConfigTypeEnv {
    pub fn new(root_path: Rc<str>) -> ConfigTypeEnv {
        let root_context = Rc::new(ObjSrcContext::root(root_path));
        ConfigTypeEnv {
            context_stack: vec![root_context],
            builtins: make_config_builtin_values(),
            exports: HashMap::new(),
            imports: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn current_src_context(&self) -> Rc<ObjSrcContext> {
        self.context_stack.last().unwrap().clone()
    }

    pub fn push_src_context(&mut self, context: Rc<ObjSrcContext>) {
        self.context_stack.push(context);
    }

    pub fn pop_src_context(&mut self) {
        debug_assert!(self.context_stack.len() >= 2);
        self.context_stack.pop();
    }

    pub fn parse_source(
        &self,
        source_code: &str,
    ) -> ConfigResult<LinkConfigAst> {
        LinkConfigAst::parse_source(source_code).map_err(|errs| {
            let context = self.current_src_context();
            errs.map(|error| ConfigError::ParseError {
                context: context.clone(),
                error,
            })
        })
    }

    pub fn get_declaration(&self, name: &str) -> Option<&ConfigDecl> {
        self.variables.get(name)
    }

    pub fn add_declaration(
        &mut self,
        id: IdentifierAst,
        var_type: ExprType,
        value: ConfigVariableOr<ExprValue>,
    ) {
        let id_loc = self.make_loc(id.span);
        let decl = ConfigDecl { id_loc, var_type, value };
        self.variables.insert(id.name, decl);
    }

    pub fn get_export(&self, name: &str) -> Option<ObjSrcLoc> {
        self.exports.get(name).cloned()
    }

    pub fn add_export(
        &mut self,
        id: IdentifierAst,
        space: Rc<str>,
        address: ConfigVariableOr<Addr>,
    ) {
        self.exports.insert(id.name.clone(), self.make_loc(id.span));
        let address_value = address.map_static(|addr| {
            ExprValue::Label(ExprLabel::AddrAbsolute {
                space,
                address: BigInt::from(addr),
            })
        });
        self.add_declaration(id, ExprType::Label, address_value);
    }

    pub fn get_import(&self, name: &str) -> Option<ObjSrcLoc> {
        self.imports.get(name).cloned()
    }

    pub fn add_import(&mut self, id: IdentifierAst) {
        self.imports.insert(id.name.clone(), self.make_loc(id.span));
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
            ExprCompiler::new(self).typecheck(expr).map_err(|errs| {
                errs.map(|error| ConfigError::ExprTypeError {
                    context: self.current_src_context(),
                    error,
                })
            })?;
        Ok((ObjExpr { ops }, expr_type, expr_static))
    }

    pub fn make_loc(&self, span: SrcSpan) -> ObjSrcLoc {
        ObjSrcLoc { span, context: self.current_src_context() }
    }
}

impl ExprEnv for ConfigTypeEnv {
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
