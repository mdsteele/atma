use super::arch::ArchTree;
use super::error::{AsmError, AsmResult};
use crate::addr::{Addr, Offset, Size};
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprLabel, ExprNotStaticReason,
    ExprStatic, ExprType, ExprTypeError, ExprTypeResult, ExprUnOp, ExprValue,
    make_global_builtin_values,
};
use crate::obj::{
    ObjExpr, ObjExprOp, ObjPatch, ObjPatchData, ObjSrcContext, ObjSrcLoc,
    ObjSymbol,
};
use crate::parse::{
    AsmDataTypeAst, AsmLabelAst, AsmModuleAst, DeclarationKind, ExprAst,
    IdentifierAst, IdentifierKind,
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
    context_stack: Vec<Rc<ObjSrcContext>>,
    scope_stack: Vec<AsmScopeEnv>,
    next_anonymous_scope_number: u32,
}

impl AsmTypeEnv {
    pub fn new(root_path: Rc<str>, arch_tree: ArchTree) -> AsmTypeEnv {
        let root_context = Rc::new(ObjSrcContext::root(root_path));
        AsmTypeEnv {
            arch_tree,
            builtins: make_global_builtin_values(),
            arch_stack: vec![Rc::from(ArchTree::ROOT_ARCH_NAME)],
            chunk_stack: Vec::new(),
            context_stack: vec![root_context],
            scope_stack: vec![AsmScopeEnv::root()],
            next_anonymous_scope_number: 0,
        }
    }

    pub fn arch_tree(&self) -> &ArchTree {
        &self.arch_tree
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

    pub fn parse_source(&self, source_code: &str) -> AsmResult<AsmModuleAst> {
        AsmModuleAst::parse_source(source_code).map_err(|errs| {
            let context = self.current_src_context();
            errs.map(|error| AsmError::ParseError {
                context: context.clone(),
                error,
            })
        })
    }

    pub fn check_for_inevitable_eval_error(
        &self,
        reason: &ExprNotStaticReason,
    ) -> AsmResult<()> {
        if let Some(error) = reason.inevitable_eval_error() {
            Err(Errs::one(AsmError::StaticEvalError {
                context: self.current_src_context(),
                error,
            }))
        } else {
            Ok(())
        }
    }

    fn add_declaration(
        &mut self,
        kind: AsmDeclKind,
        id: &IdentifierAst,
        expr_type: ExprType,
        value: AsmDeclValue,
    ) -> AsmResult<()> {
        let scope = self.scope_stack.last().unwrap();
        if let Some(decl) = scope.decls.get(&id.name)
            && (decl.kind == AsmDeclKind::Fixed || kind == AsmDeclKind::Fixed)
        {
            let full_name = scope.prefixed(&id.name);
            return Err(Errs::one(AsmError::NameAlreadyDeclared {
                full_name,
                name_loc: self.make_loc(id.span),
                prev_loc: decl.id_loc.clone(),
            }));
        }
        let id_loc = self.make_loc(id.span);
        let decl = AsmDecl { kind, id_loc, expr_type, value };
        let scope = self.scope_stack.last_mut().unwrap();
        scope.decls.insert(id.name.clone(), decl);
        Ok(())
    }

    pub fn declare_fixed_value(
        &mut self,
        id: &IdentifierAst,
        expr_type: ExprType,
        value: AsmDeclValue,
    ) -> AsmResult<()> {
        self.verify_not_builtin_or_reserved(id)?;
        self.add_declaration(AsmDeclKind::Fixed, id, expr_type, value)
    }

    pub fn declare_enum_values(
        &mut self,
        enum_id_span: SrcSpan,
        values: Vec<ExprValue>,
    ) -> AsmResult<()> {
        let id = IdentifierAst {
            span: enum_id_span,
            name: Rc::from("%values"),
            kind: IdentifierKind::Builtin,
        };
        let expr_type = ExprType::List(Rc::from(ExprType::Integer));
        let value = AsmDeclValue::Static(ExprValue::List(Rc::from(values)));
        self.add_declaration(AsmDeclKind::Fixed, &id, expr_type, value)
    }

    pub fn declare_variable(
        &mut self,
        kind: DeclarationKind,
        id: &IdentifierAst,
        expr_type: ExprType,
        value: AsmDeclValue,
    ) -> AsmResult<()> {
        self.verify_not_builtin_or_reserved(id)?;
        let asm_decl_kind = match kind {
            DeclarationKind::Let => AsmDeclKind::Rebindable,
            DeclarationKind::Var => AsmDeclKind::Settable,
        };
        self.add_declaration(asm_decl_kind, id, expr_type, value)
    }

    pub fn reassign_variable(&mut self, name: Rc<str>, value: AsmDeclValue) {
        for scope in self.scope_stack.iter_mut().rev() {
            if let Some(decl) = scope.decls.get_mut(&name) {
                decl.value = value;
                break;
            }
        }
    }

    pub fn declare_import(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        self.declare_symbol(id_ast)
    }

    pub fn declare_label(&mut self, label_ast: &AsmLabelAst) -> AsmResult<()> {
        self.declare_symbol(&label_ast.identifier)
    }

    fn declare_symbol(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        let label = ExprLabel::SymbolRelative {
            name: self.current_scope().prefixed(&id_ast.name),
            offset: BigInt::ZERO,
        };
        let value = AsmDeclValue::Static(ExprValue::Label(label));
        self.declare_fixed_value(id_ast, ExprType::Label, value)
    }

    pub fn begin_chunk(
        &mut self,
        chunk_index: usize,
        start_addr: Option<Addr>,
        fill_byte: Option<u8>,
    ) {
        self.chunk_stack.push(ChunkEnv::new(
            chunk_index,
            start_addr,
            fill_byte,
        ));
        self.arch_stack.push(self.arch_stack.last().unwrap().clone());
    }

    pub fn current_chunk(&self) -> Option<&ChunkEnv> {
        self.chunk_stack.last()
    }

    pub fn current_chunk_mut(&mut self) -> Option<&mut ChunkEnv> {
        self.chunk_stack.last_mut()
    }

    pub fn end_chunk(&mut self) -> ChunkEnv {
        debug_assert!(self.arch_stack.len() >= 2);
        self.arch_stack.pop();
        self.chunk_stack.pop().unwrap()
    }

    pub fn current_arch(&self) -> &Rc<str> {
        self.arch_stack.last().unwrap()
    }

    pub fn set_current_arch(&mut self, arch: Rc<str>) {
        debug_assert!(self.arch_tree.contains_arch(&arch));
        *self.arch_stack.last_mut().unwrap() = arch;
    }

    pub fn begin_anonymous_scope(&mut self) {
        let name = format!("${:x}", self.next_anonymous_scope_number);
        self.next_anonymous_scope_number += 1;
        self.begin_scope(Rc::from(name), true);
    }

    pub fn begin_named_scope(&mut self, name: Rc<str>) {
        self.begin_scope(name, false);
    }

    fn begin_scope(&mut self, name: Rc<str>, anonymous: bool) {
        let current_scope = self.current_scope();
        let mut decls = HashMap::<Rc<str>, AsmDecl>::new();
        if !anonymous {
            let prefix = format!("{name}::");
            for (decl_name, decl) in current_scope.decls.iter() {
                if decl_name.starts_with(&prefix) {
                    let stripped_name = Rc::from(&decl_name[prefix.len()..]);
                    decls.insert(stripped_name, decl.clone());
                }
            }
        }
        let full_prefix =
            Rc::from(format!("{}{name}::", current_scope.full_prefix));
        let name = if anonymous { None } else { Some(name) };
        self.scope_stack.push(AsmScopeEnv {
            name,
            full_prefix,
            decls,
            structs: HashMap::new(),
        });
    }

    pub fn is_at_top_level(&self) -> bool {
        debug_assert!(!self.scope_stack.is_empty());
        self.scope_stack.len() == 1 && self.chunk_stack.is_empty()
    }

    pub fn current_scope(&self) -> &AsmScopeEnv {
        self.scope_stack.last().unwrap()
    }

    pub fn end_scope(&mut self) {
        let inner = self.scope_stack.pop().unwrap();
        let outer = self.scope_stack.last_mut().unwrap();
        if let Some(inner_name) = inner.name {
            for (decl_name, decl) in inner.decls {
                let prefixed_name = format!("{inner_name}::{decl_name}");
                outer.decls.insert(Rc::from(prefixed_name), decl);
            }
        }
    }

    fn look_up_decl(&self, name: &str) -> Option<&AsmDecl> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(decl) = scope.decls.get(name) {
                return Some(decl);
            }
        }
        None
    }

    fn look_up_struct(&self, name: &str) -> Option<&AsmStructDef> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(decl) = scope.structs.get(name) {
                return Some(decl);
            }
        }
        None
    }

    pub fn declare_struct(
        &mut self,
        struct_id: IdentifierAst,
        fields: Vec<(IdentifierAst, Offset)>,
        size: Size,
    ) -> AsmResult<()> {
        let scope = self.scope_stack.last().unwrap();
        let full_name = scope.prefixed(&struct_id.name);
        if let Some(prev) = self.look_up_decl(&full_name) {
            Err(Errs::one(AsmError::NameAlreadyDeclared {
                full_name,
                name_loc: self.make_loc(struct_id.span),
                prev_loc: prev.id_loc.clone(),
            }))
        } else {
            let context = self.current_src_context();
            let scope = self.scope_stack.last_mut().unwrap();
            scope.define_struct(context, struct_id, fields, size);
            Ok(())
        }
    }

    pub fn data_type_size(
        &self,
        data_type: AsmDataTypeAst,
    ) -> AsmResult<Size> {
        match data_type {
            AsmDataTypeAst::Int(_, int_type) => Ok(int_type.size()),
            AsmDataTypeAst::Struct(id) => {
                match self.look_up_struct(&id.name) {
                    Some(def) => Ok(def.size),
                    None => Err(Errs::one(AsmError::UnknownStruct {
                        name: id.name,
                        loc: self.make_loc(id.span),
                    })),
                }
            }
        }
    }

    pub fn typecheck_expression(
        &self,
        expr: ExprAst,
    ) -> ((ObjExpr, ExprType, ExprStatic), Errs<AsmError>) {
        match ExprCompiler::new(self).typecheck(expr) {
            Ok((ops, expr_type, expr_static)) => {
                debug_assert!(!ops.is_empty());
                ((ObjExpr { ops }, expr_type, expr_static), Errs::new())
            }
            Err(errs) => {
                let expr = ObjExpr::from(false);
                let expr_type = ExprType::Undefined;
                let expr_static = Err(ExprNotStaticReason::TypeError);
                ((expr, expr_type, expr_static), self.map_type_errors(errs))
            }
        }
    }

    pub fn typecheck_lvalue(
        &self,
        lvalue: IdentifierAst,
    ) -> AsmResult<ExprType> {
        self.verify_not_builtin_or_reserved(&lvalue)?;
        if let Some(decl) = self.look_up_decl(&lvalue.name) {
            match decl.kind {
                AsmDeclKind::Settable => Ok(decl.expr_type.clone()),
                AsmDeclKind::Rebindable | AsmDeclKind::Fixed => {
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

    pub fn verify_not_builtin_or_reserved(
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

    pub fn make_loc(&self, span: SrcSpan) -> ObjSrcLoc {
        ObjSrcLoc { span, context: self.current_src_context() }
    }
}

impl ExprEnv for AsmTypeEnv {
    type Op = ObjExprOp;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)> {
        if let Some(chunk_env) = self.chunk_stack.last() {
            let chunk_index = chunk_env.chunk_index();
            let offset = BigInt::from(chunk_env.total_size());
            let label = if let Some(start) = chunk_env.start_addr {
                ExprLabel::ChunkAbsolute {
                    chunk_index,
                    address: BigInt::from(start) + offset,
                }
            } else {
                ExprLabel::ChunkRelative { chunk_index, offset }
            };
            let value = ExprValue::Label(label);
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

pub(super) struct ChunkEnv {
    chunk_index: usize,
    start_addr: Option<Addr>,
    fill_byte: Option<u8>,
    data: Vec<u8>,
    padding: usize,
    patches: Vec<ObjPatch>,
    symbols: Vec<ObjSymbol>,
}

impl ChunkEnv {
    fn new(
        chunk_index: usize,
        start_addr: Option<Addr>,
        fill_byte: Option<u8>,
    ) -> ChunkEnv {
        ChunkEnv {
            chunk_index,
            start_addr,
            fill_byte,
            data: Vec::new(),
            padding: 0,
            patches: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn chunk_index(&self) -> usize {
        self.chunk_index
    }

    pub fn total_size(&self) -> usize {
        self.data.len() + self.padding
    }

    pub fn data_mut(&mut self) -> &mut Vec<u8> {
        if self.padding > 0 {
            let fill_byte = self.fill_byte.unwrap_or_else(|| {
                self.add_patch(ObjPatch {
                    // TODO: check for overflow
                    offset: Offset::try_from(self.data.len()).unwrap(),
                    data: ObjPatchData::Fill(self.padding),
                });
                0u8
            });
            self.data.resize(self.data.len() + self.padding, fill_byte);
            self.padding = 0;
        }
        &mut self.data
    }

    pub fn append_padding(&mut self, padding: usize) -> AsmResult<()> {
        // TODO: check for overflow (counting both padding and data.len())
        self.padding += padding;
        Ok(())
    }

    pub fn append_patch(&mut self, data: ObjPatchData) -> AsmResult<()> {
        let old_size = self.total_size();
        // TODO: Error instead of crash if offset is too large.
        let offset = Offset::try_from(old_size).unwrap();
        self.data_mut().resize(old_size + data.num_bytes(), 0u8);
        self.add_patch(ObjPatch { offset, data });
        Ok(())
    }

    fn add_patch(&mut self, patch: ObjPatch) {
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
    /// The struct types defined in this scope.
    structs: HashMap<Rc<str>, AsmStructDef>,
}

impl AsmScopeEnv {
    /// Creates a root scope.
    pub fn root() -> Self {
        Self {
            name: None,
            full_prefix: Rc::from(""),
            decls: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn prefixed(&self, name: &Rc<str>) -> Rc<str> {
        if self.full_prefix.is_empty() {
            name.clone()
        } else {
            Rc::from(format!("{}{name}", self.full_prefix))
        }
    }

    fn define_struct(
        &mut self,
        context: Rc<ObjSrcContext>,
        struct_id: IdentifierAst,
        fields: Vec<(IdentifierAst, Offset)>,
        size: Size,
    ) {
        for (field_id, offset) in fields {
            let field_name =
                Rc::from(format!("{}::{}", struct_id.name, field_id.name));
            let span = field_id.span;
            let offset_value = ExprValue::Integer(BigInt::from(offset));
            let field_decl = AsmDecl {
                kind: AsmDeclKind::Fixed,
                id_loc: ObjSrcLoc { span, context: context.clone() },
                expr_type: ExprType::Integer,
                value: AsmDeclValue::Static(offset_value),
            };
            debug_assert!(!self.decls.contains_key(&field_name));
            self.decls.insert(field_name, field_decl);
        }

        let size_name = Rc::from(format!("{}::%size", struct_id.name));
        let size_value = ExprValue::Integer(BigInt::from(size));
        let size_decl = AsmDecl {
            kind: AsmDeclKind::Fixed,
            id_loc: ObjSrcLoc {
                span: struct_id.span,
                context: context.clone(),
            },
            expr_type: ExprType::Integer,
            value: AsmDeclValue::Static(size_value),
        };
        debug_assert!(!self.decls.contains_key(&size_name));
        self.decls.insert(size_name, size_decl);

        let struct_value = ExprValue::Entity(struct_id.name.clone());
        let struct_decl = AsmDecl {
            kind: AsmDeclKind::Fixed,
            id_loc: ObjSrcLoc {
                span: struct_id.span,
                context: context.clone(),
            },
            expr_type: ExprType::Entity(Rc::from("struct")),
            value: AsmDeclValue::Static(struct_value),
        };
        debug_assert!(!self.decls.contains_key(&struct_id.name));
        self.decls.insert(struct_id.name.clone(), struct_decl);
        debug_assert!(!self.structs.contains_key(&struct_id.name));
        self.structs.insert(struct_id.name, AsmStructDef { size });
    }
}

//===========================================================================//

#[derive(Clone)]
struct AsmDecl {
    pub kind: AsmDeclKind,
    pub id_loc: ObjSrcLoc,
    pub expr_type: ExprType,
    pub value: AsmDeclValue,
}

//===========================================================================//

#[derive(Clone, Copy, Eq, PartialEq)]
enum AsmDeclKind {
    /// A declaration that cannot be rebound or mutated.
    Fixed,
    /// A declaration that can be rebound (to a new `Rebindable` or
    /// `Settable`), but that cannot be mutated.
    Rebindable,
    /// A declaration that can be rebound (to a new `Rebindable` or
    /// `Settable`) or mutated in place.
    Settable,
}

//===========================================================================//

#[derive(Clone)]
pub(super) enum AsmDeclValue {
    Static(ExprValue),
    Variable(usize, ExprNotStaticReason),
}

//===========================================================================//

struct AsmStructDef {
    size: Size,
}

//===========================================================================//
