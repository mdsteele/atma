use super::arch::ArchTree;
use super::charmap::Charmap;
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
    AsmChunkKind, AsmDataTypeAst, AsmLabelAst, AsmModuleAst, DeclarationKind,
    ExprAst, HereLabelKind, IdentifierAst, IdentifierKind,
};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) struct AsmTypeEnv {
    arch_tree: ArchTree,
    builtins: HashMap<Rc<str>, (ExprValue, ExprType)>,
    charmaps: HashMap<Rc<str>, (SrcSpan, Charmap)>,
    /// Settings for zones outside of any chunk.  Never empty, as it always
    /// contains the root, top-level zone.
    outer_zone_stack: Vec<ZoneSettings>,
    /// Nested chunks.  Empty when outside of any chunk.
    chunk_stack: Vec<ChunkEnv>,
    /// Nested source contexts.  Never empty, as it always contains the root
    /// context.
    context_stack: Vec<Rc<ObjSrcContext>>,
    /// Nested scopes.  Never empty, as it always contains the root, top-level
    /// scope.
    scope_stack: Vec<AsmScopeEnv>,
    next_anonymous_scope_number: u32,
}

impl AsmTypeEnv {
    pub fn new(root_path: Rc<str>, arch_tree: ArchTree) -> AsmTypeEnv {
        let root_context = Rc::new(ObjSrcContext::root(root_path));
        AsmTypeEnv {
            arch_tree,
            builtins: make_global_builtin_values(),
            charmaps: HashMap::new(),
            outer_zone_stack: vec![ZoneSettings::root()],
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

    pub fn begin_with(
        &mut self,
        arch: Option<Rc<str>>,
        charmap: Option<Rc<str>>,
        fill: Option<u8>,
    ) {
        let settings = self.current_settings().with(arch, charmap, fill);
        self.begin_zone(settings);
    }

    pub fn end_with(&mut self) {
        self.end_zone();
    }

    pub fn begin_chunk(
        &mut self,
        chunk_index: usize,
        kind: AsmChunkKind,
        start_addr: Option<Addr>,
        arch: Option<Rc<str>>,
        charmap: Option<Rc<str>>,
        fill: Option<u8>,
    ) {
        let settings = self.current_settings().with(arch, charmap, fill);
        self.chunk_stack.push(ChunkEnv::new(
            chunk_index,
            kind,
            start_addr,
            settings,
        ));
    }

    pub fn current_chunk(&self) -> Option<&ChunkEnv> {
        self.chunk_stack.last()
    }

    pub fn end_chunk(&mut self) -> ChunkEnv {
        let chunk_env = self.chunk_stack.pop().unwrap();
        debug_assert_eq!(chunk_env.zone_stack.len(), 1);
        chunk_env
    }

    fn with_mutable_chunk<F>(&mut self, func: F) -> AsmResult<()>
    where
        F: FnOnce(&mut ChunkEnv) -> AsmResult<()>,
    {
        let mut errs = Errs::<AsmError>::new();
        let mut i = self.chunk_stack.len();
        while i > 0 {
            i -= 1;
            let chunk_env = &mut self.chunk_stack[i];
            match chunk_env.kind {
                AsmChunkKind::Loadable => {}
                AsmChunkKind::Section | AsmChunkKind::Elsewhere => {
                    let old_size = chunk_env.total_size();
                    errs.also(func(chunk_env));
                    let new_size = chunk_env.total_size();
                    debug_assert!(new_size >= old_size);
                    let added = new_size - old_size;
                    if added > 0 {
                        for j in (i + 1)..self.chunk_stack.len() {
                            let loadable = &mut self.chunk_stack[j];
                            errs.also(loadable.append_padding(added));
                        }
                    }
                    break;
                }
            }
        }
        errs.result()
    }

    /// Appends the given data to the current chunk, if any.  If the current
    /// chunk is a `Loadable` chunk, then this instead appends padding to that
    /// chunk and tries again on the enclosing chunk.  May return one or more
    /// errors if this process causes any chunks to exceed their maximum size.
    ///
    /// If there is no current chunk, this does nothing and emits no error; the
    /// caller is expected to have already emitted a single error for whichever
    /// directive is appending data.
    pub fn append_chunk_data(&mut self, data: &[u8]) -> AsmResult<()> {
        self.with_mutable_chunk(|chunk_env| chunk_env.append_data(data))
    }

    pub fn with_chunk_data<F>(&mut self, func: F) -> AsmResult<()>
    where
        F: FnOnce(&mut Vec<u8>) -> AsmResult<()>,
    {
        self.with_mutable_chunk(|chunk_env| func(chunk_env.data_mut()))
    }

    /// Appends the specified number of padding bytes to the current chunk, if
    /// any.  If the current chunk is a `Loadable` chunk, then this appends
    /// padding to that chunk and then repeats with the enclosing chunk.  May
    /// return one or more errors if this process causes any chunks to exceed
    /// their maximum size.
    ///
    /// If there is no current chunk, this does nothing and emits no error; the
    /// caller is expected to have already emitted a single error for whichever
    /// directive is appending padding.
    pub fn append_chunk_padding(&mut self, padding: usize) -> AsmResult<()> {
        self.with_mutable_chunk(|chunk_env| chunk_env.append_padding(padding))
    }

    /// Appends the given patch to the current chunk, if any.  If the current
    /// chunk is a `Loadable` chunk, then this instead appends padding to that
    /// chunk and tries again on the enclosing chunk.  May return one or more
    /// errors if this process causes any chunks to exceed their maximum size.
    ///
    /// If there is no current chunk, this does nothing and emits no error; the
    /// caller is expected to have already emitted a single error for whichever
    /// directive is appending data.
    pub fn append_chunk_patch(&mut self, data: ObjPatchData) -> AsmResult<()> {
        self.with_mutable_chunk(|chunk_env| chunk_env.append_patch(data))
    }

    pub fn append_chunk_symbol(
        &mut self,
        name: Rc<str>,
        loc: ObjSrcLoc,
        exported: bool,
    ) -> AsmResult<()> {
        if let Some(chunk_env) = self.chunk_stack.last_mut() {
            // TODO: handle overflow
            let offset = Offset::try_from(chunk_env.total_size()).unwrap();
            chunk_env.symbols.push(ObjSymbol { name, loc, exported, offset });
        }
        Ok(())
    }

    fn begin_zone(&mut self, settings: ZoneSettings) {
        debug_assert!(self.arch_tree.contains_arch(&settings.arch));
        debug_assert!(
            settings
                .charmap
                .as_ref()
                .map(|charmap| self.charmaps.contains_key(charmap))
                .unwrap_or(true)
        );
        if let Some(chunk_env) = self.chunk_stack.last_mut() {
            chunk_env.begin_zone(settings);
        } else {
            self.outer_zone_stack.push(settings);
        }
    }

    fn end_zone(&mut self) {
        if let Some(chunk_env) = self.chunk_stack.last_mut() {
            chunk_env.end_zone();
        } else {
            self.outer_zone_stack.pop().unwrap();
            debug_assert!(!self.outer_zone_stack.is_empty());
        }
    }

    fn current_settings(&self) -> &ZoneSettings {
        if let Some(chunk_env) = self.chunk_stack.last() {
            chunk_env.current_settings()
        } else {
            self.outer_zone_stack.last().unwrap()
        }
    }

    pub fn current_arch(&self) -> &Rc<str> {
        &self.current_settings().arch
    }

    /// Returns true if `name` is a defined charmap in this environment.
    pub fn contains_charmap(&self, name: &str) -> bool {
        self.charmaps.contains_key(name)
    }

    pub fn get_charmap(&self, name: &str) -> Option<(SrcSpan, &Charmap)> {
        self.charmaps.get(name).map(|(span, charmap)| (*span, charmap))
    }

    pub fn current_charmap(&self) -> Option<(&Rc<str>, &Charmap)> {
        self.current_settings()
            .charmap
            .as_ref()
            .map(|name| (name, &self.charmaps.get(name).unwrap().1))
    }

    pub fn add_charmap(
        &mut self,
        name: Rc<str>,
        name_span: SrcSpan,
        charmap: Charmap,
    ) {
        debug_assert!(!self.contains_charmap(&name));
        self.charmaps.insert(name, (name_span, charmap));
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
        self.begin_zone(self.current_settings().clone());
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
        self.end_zone();
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
        kind: HereLabelKind,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)> {
        if let Some(chunk_env) = self.chunk_stack.last() {
            let chunk_index = chunk_env.chunk_index();
            let offset = match kind {
                HereLabelKind::StmtStart => {
                    BigInt::from(chunk_env.total_size())
                }
                HereLabelKind::ZoneStart => {
                    BigInt::from(chunk_env.current_zone().start_offset)
                }
            };
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
            Err(Errs::one(ExprTypeError::HereLabelOutsideOfAnySection {
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

    fn with_addr_op(&self, op_span: SrcSpan) -> ExprTypeResult<Self::Op> {
        if let Some(chunk_env) = self.current_chunk() {
            let chunk_index = chunk_env.chunk_index();
            Ok(ObjExprOp::WithAddr { chunk_index })
        } else {
            Err(Errs::one(ExprTypeError::WithAddrOutsideOfAnyAddrSpace {
                op_span,
            }))
        }
    }

    fn with_addr_static(
        &self,
        op_span: SrcSpan,
        address: BigInt,
    ) -> ExprTypeResult<ExprLabel> {
        if let Some(chunk_env) = self.current_chunk() {
            let chunk_index = chunk_env.chunk_index();
            Ok(ExprLabel::ChunkAbsolute { chunk_index, address })
        } else {
            Err(Errs::one(ExprTypeError::WithAddrOutsideOfAnyAddrSpace {
                op_span,
            }))
        }
    }
}

//===========================================================================//

pub(super) struct ChunkEnv {
    chunk_index: usize,
    kind: AsmChunkKind,
    start_addr: Option<Addr>,
    data: Vec<u8>,
    padding: usize,
    patches: Vec<ObjPatch>,
    symbols: Vec<ObjSymbol>,
    zone_stack: Vec<InnerZone>,
}

impl ChunkEnv {
    fn new(
        chunk_index: usize,
        kind: AsmChunkKind,
        start_addr: Option<Addr>,
        settings: ZoneSettings,
    ) -> ChunkEnv {
        ChunkEnv {
            chunk_index,
            kind,
            start_addr,
            data: Vec::new(),
            padding: 0,
            patches: Vec::new(),
            symbols: Vec::new(),
            zone_stack: vec![InnerZone::new(settings, Offset::ZERO)],
        }
    }

    pub fn chunk_index(&self) -> usize {
        self.chunk_index
    }

    pub fn total_size(&self) -> usize {
        self.data.len() + self.padding
    }

    fn current_settings(&self) -> &ZoneSettings {
        &self.zone_stack.last().unwrap().settings
    }

    fn data_mut(&mut self) -> &mut Vec<u8> {
        if self.padding > 0 {
            let fill = self.current_settings().fill.unwrap_or_else(|| {
                self.patches.push(ObjPatch {
                    // TODO: check for overflow
                    offset: Offset::try_from(self.data.len()).unwrap(),
                    data: ObjPatchData::Fill(self.padding),
                });
                0u8
            });
            self.data.resize(self.data.len() + self.padding, fill);
            self.padding = 0;
        }
        &mut self.data
    }

    fn append_data(&mut self, data: &[u8]) -> AsmResult<()> {
        // TODO: check for overflow (counting both padding and data.len())
        self.data_mut().extend_from_slice(data);
        Ok(())
    }

    fn append_padding(&mut self, padding: usize) -> AsmResult<()> {
        // TODO: check for overflow (counting both padding and data.len())
        self.padding += padding;
        Ok(())
    }

    fn append_patch(&mut self, data: ObjPatchData) -> AsmResult<()> {
        let old_size = self.total_size();
        // TODO: Error instead of crash if offset is too large.
        let offset = Offset::try_from(old_size).unwrap();
        self.data_mut().resize(old_size + data.num_bytes(), 0u8);
        self.patches.push(ObjPatch { offset, data });
        Ok(())
    }

    fn current_zone(&self) -> &InnerZone {
        self.zone_stack.last().unwrap()
    }

    fn begin_zone(&mut self, settings: ZoneSettings) {
        // If this new zone is going to override the fill byte of the enclosing
        // zone, then we need to explicitly fill in any existing padding.
        if settings.fill != self.current_settings().fill {
            debug_assert!(settings.fill.is_some());
            self.data_mut(); // force existing padding to be filled in
        }
        // TODO: handle overflow
        let offset = Offset::try_from(self.total_size()).unwrap();
        self.zone_stack.push(InnerZone::new(settings, offset));
    }

    fn end_zone(&mut self) {
        let zone = self.zone_stack.pop().unwrap();
        debug_assert!(!self.zone_stack.is_empty());
        // If this zone overrode the fill byte of the enclosing zone, then we
        // need to explicitly fill in any padding at the end of this zone.
        if zone.settings.fill != self.current_settings().fill {
            let fill = zone.settings.fill.unwrap();
            self.data.resize(self.data.len() + self.padding, fill);
            self.padding = 0;
        }
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

/// Represents settings for a zone.
///
/// A "zone" refers to any of (1) a chunk, (2) a "with" block, or (3) a scope
/// (in other words, any statement block that's either terminated by an `.END`
/// directive or enclosed by curly braces).  The root top-level scope also
/// counts as a zone.  A zone that is inside of a chunk or is itself a chunk is
/// called an "inner zone"; anything else (e.g. a top-level scope or "with"
/// block that is outside of any chunk) is called an "outer zone".
#[derive(Clone)]
struct ZoneSettings {
    pub arch: Rc<str>,
    pub charmap: Option<Rc<str>>,
    pub fill: Option<u8>,
}

impl ZoneSettings {
    fn root() -> Self {
        Self {
            arch: Rc::from(ArchTree::ROOT_ARCH_NAME),
            charmap: None,
            fill: None,
        }
    }

    fn with(
        &self,
        arch: Option<Rc<str>>,
        charmap: Option<Rc<str>>,
        fill: Option<u8>,
    ) -> Self {
        Self {
            arch: arch.unwrap_or_else(|| self.arch.clone()),
            charmap: charmap.or_else(|| self.charmap.clone()),
            fill: fill.or(self.fill),
        }
    }
}

//===========================================================================//

/// Represents an inner zone (i.e. a zone that is inside of a chunk or is
/// itself a chunk).  Unlike outer zones, inner zones have start addresses and
/// sizes, and can thus be referred to with "here" labels (e.g. `$^`).
struct InnerZone {
    /// Settings for this zone.
    settings: ZoneSettings,
    /// The offset, relative to the start of the current chunk, for the start
    /// of this zone.
    start_offset: Offset,
}

impl InnerZone {
    /// Returns a new `InnerZone` with the given start offset relative to the
    /// start of the current chunk.
    fn new(settings: ZoneSettings, start_offset: Offset) -> Self {
        Self { settings, start_offset }
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
    /// Creates a new root scope.
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
