use super::arch::ArchTree;
use super::error::{AsmError, AsmResult, AsmSrcContext, AsmSrcLoc};
use crate::addr::Offset;
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprFunc, ExprLabel,
    ExprNotStaticReason, ExprStatic, ExprType, ExprTypeError, ExprTypeResult,
    ExprUnOp, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp, ObjPatch, ObjPatchData, ObjSymbol};
use crate::parse::{
    AsmLabelAst, AsmModuleAst, DeclarationKind, ExprAst, IdentifierAst,
    IdentifierKind,
};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct AsmTypeEnv {
    arch_tree: ArchTree,
    builtins: HashMap<Rc<str>, (ExprValue, ExprType)>,
    arch_stack: Vec<Rc<str>>,
    chunk_stack: Vec<ChunkEnv>,
    context_stack: Vec<Rc<AsmSrcContext>>,
    scope_stack: Vec<AsmScopeEnv>,
}

impl AsmTypeEnv {
    pub fn new(root_path: Rc<str>, arch_tree: ArchTree) -> AsmTypeEnv {
        let mut builtins = HashMap::<Rc<str>, (ExprValue, ExprType)>::new();
        let expr_type = ExprType::Function(Rc::new((
            ExprType::Integer,
            ExprType::Integer,
        )));
        for func in [ExprFunc::Cbrtz, ExprFunc::Sqrtz] {
            let value = ExprValue::Function(func);
            builtins.insert(Rc::from(func.name()), (value, expr_type.clone()));
        }
        let root_context = Rc::new(AsmSrcContext::root(root_path));
        AsmTypeEnv {
            arch_tree,
            builtins,
            arch_stack: vec![Rc::from(ArchTree::ROOT_ARCH_NAME)],
            chunk_stack: Vec::new(),
            context_stack: vec![root_context],
            scope_stack: vec![AsmScopeEnv::root()],
        }
    }

    pub fn arch_tree(&self) -> &ArchTree {
        &self.arch_tree
    }

    pub fn current_src_context(&self) -> Rc<AsmSrcContext> {
        debug_assert!(!self.context_stack.is_empty());
        self.context_stack.last().unwrap().clone()
    }

    pub fn push_src_context(&mut self, context: Rc<AsmSrcContext>) {
        self.context_stack.push(context);
    }

    pub fn pop_src_context(&mut self) {
        debug_assert!(self.context_stack.len() >= 2);
        self.context_stack.pop();
    }

    pub fn parse_source(&self, source_code: &str) -> AsmResult<AsmModuleAst> {
        AsmModuleAst::parse_source(source_code).map_err(|errs| {
            let context = self.current_src_context();
            errs.map(|error| AsmError::ParseError {
                context: context.clone(),
                error,
            })
        })
    }

    pub fn declare_variable(
        &mut self,
        kind: DeclarationKind,
        id: IdentifierAst,
        expr_type: ExprType,
        value: AsmDeclValue,
    ) -> AsmResult<()> {
        self.verify_not_builtin_or_reserved(&id)?;
        debug_assert!(!self.scope_stack.is_empty());
        let scope = self.scope_stack.last().unwrap();
        if let Some(decl) = scope.decls.get(&id.name)
            && let AsmDeclKind::Label = decl.kind
        {
            let full_name = scope.prefixed(&id.name);
            return Err(Errs::one(AsmError::SymbolAlreadyDeclared {
                full_name,
                name_loc: self.make_loc(id.span),
                prev_loc: decl.id_loc.clone(),
            }));
        }
        let decl = AsmDecl {
            kind: match kind {
                DeclarationKind::Let => AsmDeclKind::Constant,
                DeclarationKind::Var => AsmDeclKind::Variable,
            },
            id_loc: self.make_loc(id.span),
            expr_type,
            value,
        };
        debug_assert!(!self.scope_stack.is_empty());
        self.scope_stack.last_mut().unwrap().decls.insert(id.name, decl);
        Ok(())
    }

    pub fn reassign_variable(&mut self, name: Rc<str>, value: AsmDeclValue) {
        debug_assert!(!self.scope_stack.is_empty());
        let scope = self.scope_stack.last_mut().unwrap();
        debug_assert!(scope.decls.contains_key(&name));
        let decl = scope.decls.get_mut(&name).unwrap();
        decl.value = value;
    }

    pub fn declare_import(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        self.declare_symbol(id_ast)
    }

    pub fn declare_label(&mut self, label_ast: &AsmLabelAst) -> AsmResult<()> {
        self.declare_symbol(&label_ast.identifier)
    }

    fn declare_symbol(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        self.verify_not_builtin_or_reserved(id_ast)?;
        let full_name = self.current_scope().prefixed(&id_ast.name);
        let id_loc = self.make_loc(id_ast.span);
        let mut qualified_name: Rc<str> = id_ast.name.clone();
        for scope in self.scope_stack.iter_mut().rev() {
            if let Some(prev_decl) = scope.decls.get(&qualified_name) {
                errs.push(AsmError::SymbolAlreadyDeclared {
                    full_name: full_name.clone(),
                    name_loc: id_loc,
                    prev_loc: prev_decl.id_loc.clone(),
                });
                break;
            }
            let label_value = ExprLabel::SymbolRelative {
                name: full_name.clone(),
                offset: BigInt::ZERO,
            };
            let decl = AsmDecl {
                kind: AsmDeclKind::Label,
                id_loc: id_loc.clone(),
                expr_type: ExprType::Label,
                value: AsmDeclValue::Static(ExprValue::Label(label_value)),
            };
            scope.decls.insert(qualified_name.clone(), decl);
            if let Some(name) = &scope.name {
                qualified_name = Rc::from(format!("{name}::{qualified_name}"));
            } else {
                break;
            }
        }
        errs.result()
    }

    pub fn begin_chunk(&mut self, chunk_index: usize) {
        self.chunk_stack.push(ChunkEnv::with_chunk_index(chunk_index));
        debug_assert!(!self.arch_stack.is_empty());
        self.arch_stack.push(self.arch_stack.last().unwrap().clone());
    }

    pub fn current_chunk(&mut self) -> Option<&mut ChunkEnv> {
        self.chunk_stack.last_mut()
    }

    pub fn end_chunk(&mut self) -> ChunkEnv {
        debug_assert!(self.arch_stack.len() >= 2);
        self.arch_stack.pop();
        debug_assert!(!self.chunk_stack.is_empty());
        self.chunk_stack.pop().unwrap()
    }

    pub fn current_arch(&self) -> &Rc<str> {
        debug_assert!(!self.arch_stack.is_empty());
        self.arch_stack.last().unwrap()
    }

    pub fn set_current_arch(&mut self, arch: Rc<str>) {
        debug_assert!(self.arch_tree.contains_arch(&arch));
        debug_assert!(!self.arch_stack.is_empty());
        *self.arch_stack.last_mut().unwrap() = arch;
    }

    pub fn begin_scope(&mut self, name: Rc<str>, anonymous: bool) {
        let current_scope = self.current_scope();
        let mut decls = HashMap::<Rc<str>, AsmDecl>::new();
        if !anonymous {
            let prefix = format!("{name}::");
            for (label_name, decl) in current_scope.decls.iter() {
                if let AsmDeclKind::Label = decl.kind
                    && label_name.starts_with(&prefix)
                {
                    let stripped_name = Rc::from(&label_name[prefix.len()..]);
                    decls.insert(stripped_name, decl.clone());
                }
            }
        }
        let full_prefix =
            Rc::from(format!("{}{name}::", current_scope.full_prefix));
        let name = if anonymous { None } else { Some(name) };
        self.scope_stack.push(AsmScopeEnv { name, full_prefix, decls });
    }

    pub fn is_at_top_level(&self) -> bool {
        debug_assert!(!self.scope_stack.is_empty());
        self.scope_stack.len() == 1 && self.chunk_stack.is_empty()
    }

    pub fn current_scope(&self) -> &AsmScopeEnv {
        debug_assert!(!self.scope_stack.is_empty());
        self.scope_stack.last().unwrap()
    }

    pub fn end_scope(&mut self) {
        debug_assert!(self.scope_stack.len() >= 2);
        self.scope_stack.pop();
    }

    fn look_up_decl(&self, name: &str) -> Option<&AsmDecl> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(decl) = scope.decls.get(name) {
                return Some(decl);
            }
        }
        None
    }

    pub fn typecheck_expression(
        &self,
        expr: ExprAst,
    ) -> AsmResult<(ObjExpr, ExprType, ExprStatic)> {
        let (ops, expr_type, expr_static) = ExprCompiler::new(self)
            .typecheck(expr)
            .map_err(|errs| self.map_type_errors(errs))?;
        debug_assert!(!ops.is_empty());
        Ok((ObjExpr { ops }, expr_type, expr_static))
    }

    pub fn typecheck_lvalue(
        &self,
        lvalue: IdentifierAst,
    ) -> AsmResult<ExprType> {
        self.verify_not_builtin_or_reserved(&lvalue)?;
        if let Some(decl) = self.look_up_decl(&lvalue.name) {
            match decl.kind {
                AsmDeclKind::Variable => Ok(decl.expr_type.clone()),
                AsmDeclKind::Constant | AsmDeclKind::Label => {
                    Err(Errs::one(AsmError::CannotModifyConstant {
                        name: lvalue.name,
                        lvalue_loc: self.make_loc(lvalue.span),
                        decl_loc: decl.id_loc.clone(),
                    }))
                }
            }
        } else {
            Err(Errs::one(AsmError::UnknownVariable {
                name: lvalue.name,
                loc: self.make_loc(lvalue.span),
            }))
        }
    }

    fn verify_not_builtin_or_reserved(
        &self,
        id_ast: &IdentifierAst,
    ) -> AsmResult<()> {
        self.verify_not_reserved(id_ast.span, &id_ast.name)
            .map_err(|errs| self.map_type_errors(errs))?;
        match id_ast.kind {
            IdentifierKind::Standard => Ok(()),
            IdentifierKind::Builtin => {
                Err(Errs::one(AsmError::AssignmentToBuiltin {
                    loc: self.make_loc(id_ast.span),
                    name: id_ast.name.clone(),
                }))
            }
            IdentifierKind::Placeholder => unreachable!(),
        }
    }

    fn verify_not_reserved(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<()> {
        let arch = self.current_arch();
        if self.arch_tree.reserved_names(arch).contains(name) {
            return Err(Errs::one(ExprTypeError::ReservedIdentifier {
                span,
                name: name.clone(),
                arch: arch.clone(),
            }));
        }
        Ok(())
    }

    fn map_type_errors(&self, errs: Errs<ExprTypeError>) -> Errs<AsmError> {
        errs.map(|error| AsmError::ExprTypeError {
            context: self.current_src_context(),
            error,
        })
    }

    pub fn make_loc(&self, span: SrcSpan) -> AsmSrcLoc {
        AsmSrcLoc { span, context: self.current_src_context() }
    }
}

impl ExprEnv for AsmTypeEnv {
    type Op = ObjExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)> {
        if let Some(chunk_env) = self.chunk_stack.last() {
            let chunk_index = chunk_env.chunk_index;
            let offset = BigInt::from(chunk_env.data.len());
            let value = ExprValue::Label(ExprLabel::ChunkRelative {
                chunk_index,
                offset,
            });
            let op = ObjExprOp::Push(value.clone());
            Ok((op, Ok(value)))
        } else {
            Err(Errs::one(ExprTypeError::RelativeLabelOutsideOfAnySection {
                span,
            }))
        }
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, ExprStatic)> {
        self.verify_not_reserved(span, name)?;
        if let Some(decl) = self.look_up_decl(name) {
            let (op, expr_static) = match &decl.value {
                AsmDeclValue::Static(value) => {
                    (ObjExprOp::Push(value.clone()), Ok(value.clone()))
                }
                AsmDeclValue::Variable(index, reason) => {
                    (ObjExprOp::GetValue(*index), Err(reason.clone()))
                }
            };
            return Ok((op, decl.expr_type.clone(), expr_static));
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

pub(super) struct ChunkEnv {
    chunk_index: usize,
    data: Vec<u8>,
    padding: usize,
    patches: Vec<ObjPatch>,
    symbols: Vec<ObjSymbol>,
}

impl ChunkEnv {
    fn with_chunk_index(chunk_index: usize) -> ChunkEnv {
        ChunkEnv {
            chunk_index,
            data: Vec::new(),
            padding: 0,
            patches: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn total_size(&self) -> usize {
        // TODO: check for overflow
        self.data.len() + self.padding
    }

    pub fn data_mut(&mut self) -> &mut Vec<u8> {
        if self.padding > 0 {
            // TODO: If chunk has explicit fill byte, then no need for patch.
            self.add_patch(ObjPatch {
                // TODO: check for overflow
                offset: Offset::try_from(self.data.len()).unwrap(),
                data: ObjPatchData::Fill(self.padding),
            });
            self.data.resize(self.data.len() + self.padding, 0u8);
            self.padding = 0;
        }
        &mut self.data
    }

    pub fn add_padding(&mut self, padding: usize) {
        // TODO: check for overflow
        self.padding += padding;
    }

    pub fn add_patch(&mut self, patch: ObjPatch) {
        self.patches.push(patch);
    }

    pub fn add_symbol(&mut self, symbol: ObjSymbol) {
        self.symbols.push(symbol);
    }

    pub fn finish(self) -> FinishedChunk {
        FinishedChunk {
            data: Box::from(self.data),
            patches: Box::from(self.patches),
            symbols: Box::from(self.symbols),
        }
    }
}

//===========================================================================//

pub(super) struct FinishedChunk {
    pub data: Box<[u8]>,
    pub patches: Box<[ObjPatch]>,
    pub symbols: Box<[ObjSymbol]>,
}

//===========================================================================//

pub(super) struct AsmScopeEnv {
    /// The name of the scope, or `None` if the scope is anonymous (or is the
    /// root scope).
    name: Option<Rc<str>>,
    /// The full prefix for symbols declared in this scope.
    full_prefix: Rc<str>,
    /// The symbols and variables/constants currently visible in this scope.
    decls: HashMap<Rc<str>, AsmDecl>,
}

impl AsmScopeEnv {
    /// Creates a root scope.
    pub fn root() -> Self {
        Self { name: None, full_prefix: Rc::from(""), decls: HashMap::new() }
    }

    pub fn prefixed(&self, name: &Rc<str>) -> Rc<str> {
        if self.full_prefix.is_empty() {
            name.clone()
        } else {
            Rc::from(format!("{}{name}", self.full_prefix))
        }
    }
}

//===========================================================================//

#[derive(Clone)]
struct AsmDecl {
    pub kind: AsmDeclKind,
    pub id_loc: AsmSrcLoc,
    pub expr_type: ExprType,
    pub value: AsmDeclValue,
}

//===========================================================================//

#[derive(Clone, Copy)]
enum AsmDeclKind {
    Constant,
    Variable,
    Label,
}

//===========================================================================//

#[derive(Clone)]
pub(super) enum AsmDeclValue {
    Static(ExprValue),
    Variable(usize, ExprNotStaticReason),
}

//===========================================================================//
