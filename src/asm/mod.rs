//! Facilities for assembling source files into object files.

mod arch;
mod builtins;
mod error;
mod expr;
mod macros;

use crate::addr::{Addr, Align, Endianness, Offset, Size};
use crate::error::SrcSpan;
use crate::expr::{ExprType, ExprValue};
use crate::obj::{
    ObjAssert, ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjPatch, ObjPatchData,
    ObjPatchIntType, ObjSymbol,
};
use crate::parse::{
    AsmAssertAst, AsmDataTypeAst, AsmDefMacroAst, AsmIntDataAst,
    AsmIntTypeAst, AsmInvokeAst, AsmModuleAst, AsmReserveAst, AsmSectionAst,
    AsmStmtAst, AsmUtf8DataAst, ExprAst, IdentifierAst,
};
use arch::ArchTree;
pub use error::{AsmError, AsmResult};
use expr::AsmTypeEnv;
use macros::MacroTable;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::range::RangeInclusive;
use std::rc::Rc;

//===========================================================================//

/// Assembles an object file from source code.
pub fn assemble_source(source: &str) -> AsmResult<ObjFile> {
    assemble_ast(AsmModuleAst::parse_source(source).map_err(|errors| {
        errors
            .into_iter()
            .map(|error| AsmError::ParseError { error })
            .collect::<Vec<_>>()
    })?)
}

fn assemble_ast(module: AsmModuleAst) -> AsmResult<ObjFile> {
    let mut assembler = Assembler::new();
    assembler.predeclare_module(&module);
    assembler.expand_module(module);
    assembler.finish()
}

//===========================================================================//

struct Assembler {
    arch_tree: ArchTree,
    macros: MacroTable,
    next_anonymous_scope_number: u32,
    env: AsmTypeEnv,
    next_chunk_index: usize,
    chunks: BTreeMap<usize, ObjChunk>,
    imports: Vec<Rc<str>>,
    asserts: Vec<ObjAssert>,
    errors: Vec<AsmError>,
}

impl Assembler {
    fn new() -> Assembler {
        let (arch_tree, macros) = builtins::make_builtins();
        Assembler {
            arch_tree,
            macros,
            next_anonymous_scope_number: 0,
            env: AsmTypeEnv::new(),
            next_chunk_index: 0,
            chunks: BTreeMap::new(),
            imports: Vec::new(),
            asserts: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Scans over a module AST (without expanding macros) and collects
    /// existing labels and scopes into the environment.
    fn predeclare_module(&mut self, module: &AsmModuleAst) {
        self.predeclare_statements(&module.statements);
    }

    /// Scans over a list of statement ASTs (without expanding macros) and
    /// collects existing labels and scopes into the environment.
    fn predeclare_statements(&mut self, statements: &[AsmStmtAst]) {
        for statement in statements {
            self.predeclare_statement(statement);
        }
    }

    /// Scans over a statement AST (without expanding macros) and collects
    /// existing labels and scopes into the environment.
    fn predeclare_statement(&mut self, statement: &AsmStmtAst) {
        match statement {
            AsmStmtAst::AnonymousScope(_) => {}
            AsmStmtAst::Assert(_) => {}
            AsmStmtAst::DefMacro(_) => {}
            AsmStmtAst::Import(id) => self.predeclare_import(id),
            AsmStmtAst::IntData(_) => {}
            AsmStmtAst::Invoke(_) => {}
            AsmStmtAst::Label(id) => self.predeclare_label(id),
            AsmStmtAst::NamedScope(id, body) => {
                self.predeclare_named_scope(id, body)
            }
            AsmStmtAst::Reserve(_) => {}
            AsmStmtAst::Section(section) => self.predeclare_section(section),
            AsmStmtAst::Utf8Data(_) => {}
        }
    }

    fn predeclare_import(&mut self, id_ast: &IdentifierAst) {
        // TODO: error if name is reserved in current arch
        let result = self.env.declare_import(id_ast);
        self.merge_errors(result);
    }

    fn predeclare_label(&mut self, id_ast: &IdentifierAst) {
        // TODO: error if name is reserved in current arch
        let result = self.env.declare_label(id_ast);
        self.merge_errors(result);
    }

    fn predeclare_named_scope(
        &mut self,
        id: &IdentifierAst,
        body: &[AsmStmtAst],
    ) {
        self.predeclare_label(id);
        self.env.begin_scope(&id.name);
        self.predeclare_statements(body);
        self.env.end_scope();
    }

    fn predeclare_section(&mut self, section_ast: &AsmSectionAst) {
        self.predeclare_statements(&section_ast.body);
    }

    /// Consumes a module AST, expanding macros and directives into chunk data.
    fn expand_module(&mut self, module: AsmModuleAst) {
        self.expand_statements(module.statements);
    }

    /// Consumes a list of statement ASTs, expanding macros and directives into
    /// chunk data.
    fn expand_statements(&mut self, statements: Vec<AsmStmtAst>) {
        for statement in statements {
            self.expand_statement(statement);
        }
    }

    /// Consumes a statement AST, expanding macros and directives into chunk
    /// data.
    fn expand_statement(&mut self, statement: AsmStmtAst) {
        match statement {
            AsmStmtAst::AnonymousScope(body) => {
                self.expand_anonymous_scope(body)
            }
            AsmStmtAst::Assert(assert) => self.expand_assert(assert),
            AsmStmtAst::DefMacro(def) => self.expand_macro_definition(def),
            AsmStmtAst::Import(id) => self.expand_import(id),
            AsmStmtAst::IntData(data) => self.expand_int_data(data),
            AsmStmtAst::Invoke(invoke) => self.expand_macro_invocation(invoke),
            AsmStmtAst::Label(id) => self.expand_label(id),
            AsmStmtAst::NamedScope(id, body) => {
                self.expand_named_scope(id, body)
            }
            AsmStmtAst::Reserve(reserve) => self.expand_reserve(reserve),
            AsmStmtAst::Section(section) => self.expand_section(section),
            AsmStmtAst::Utf8Data(data) => self.expand_utf8_data(data),
        }
    }

    fn expand_anonymous_scope(&mut self, body: Vec<AsmStmtAst>) {
        let name = format!("${:x}", self.next_anonymous_scope_number);
        self.next_anonymous_scope_number += 1;
        self.env.begin_scope(&Rc::<str>::from(name));
        self.predeclare_statements(&body);
        self.expand_statements(body);
        self.env.end_scope();
    }

    fn expand_assert(&mut self, assert_ast: AsmAssertAst) {
        let opt_message_expr: Option<ObjExpr> =
            assert_ast.message.and_then(|expr_ast| {
                self.typecheck_dir_expr_as(
                    (".ASSERT", "message"),
                    &expr_ast,
                    ExprType::String,
                )
            });
        let condition_span = assert_ast.condition.span;
        let condition_expr: ObjExpr = 'condition: {
            match self.typecheck_dir_expr_as(
                (".ASSERT", "condition"),
                &assert_ast.condition,
                ExprType::Boolean,
            ) {
                None => return,
                Some(condition_expr) => match condition_expr.static_value() {
                    None => condition_expr,
                    Some(ExprValue::Boolean(true)) => return,
                    Some(_) => {
                        let additional_message = match &opt_message_expr {
                            None => None,
                            Some(message_expr) => {
                                match message_expr.static_value() {
                                    Some(value) => {
                                        Some(value.unwrap_str_ref().clone())
                                    }
                                    None => break 'condition condition_expr,
                                }
                            }
                        };
                        self.errors.push(
                            AsmError::AssertionStaticallyFailed {
                                condition_span,
                                additional_message,
                            },
                        );
                        return;
                    }
                },
            }
        };
        self.asserts.push(ObjAssert {
            condition: condition_expr,
            message: opt_message_expr,
        });
    }

    fn expand_import(&mut self, id_ast: IdentifierAst) {
        self.imports.push(id_ast.name);
    }

    fn expand_macro_definition(&mut self, def_macro_ast: AsmDefMacroAst) {
        let arch = self.env.current_arch();
        let reserved = self.arch_tree.reserved_names(arch);
        match self.macros.define(arch, reserved, def_macro_ast) {
            Ok(()) => {}
            Err(mut errors) => {
                self.errors.append(&mut errors);
            }
        }
    }

    fn expand_macro_invocation(&mut self, invoke_ast: AsmInvokeAst) {
        let arches = self.arch_tree.get_all_ancestors(self.env.current_arch());
        match self.macros.expand(&arches, invoke_ast) {
            Ok(statements) => {
                self.predeclare_statements(&statements);
                self.expand_statements(statements);
            }
            Err(mut errors) => {
                self.errors.append(&mut errors);
            }
        }
    }

    fn expand_label(&mut self, id_ast: IdentifierAst) {
        let full_name = self.env.look_up_symbol(&id_ast.name).unwrap();
        let Some(chunk_env) = self.env.current_chunk() else {
            self.errors.push(AsmError::DirectiveNotInSection {
                directive: "label",
                span: id_ast.span,
            });
            return;
        };
        chunk_env.add_symbol(ObjSymbol {
            name: full_name,
            exported: true, // TODO
            offset: Offset::try_from(chunk_env.total_size()).unwrap(), // TODO
        });
    }

    fn expand_named_scope(
        &mut self,
        id: IdentifierAst,
        body: Vec<AsmStmtAst>,
    ) {
        let scope_name = id.name.clone();
        self.expand_label(id);
        self.env.begin_scope(&scope_name);
        self.expand_statements(body);
        self.env.end_scope();
    }

    fn expand_reserve(&mut self, reserve_ast: AsmReserveAst) {
        if self.env.current_chunk().is_none() {
            self.errors.push(AsmError::DirectiveNotInSection {
                directive: ".RESERVE",
                span: reserve_ast.directive_span,
            });
        }
        let count: u64 = if let Some(expr_ast) = &reserve_ast.count {
            match self.typecheck_static_dir_expr_as(
                (".RESERVE", "count"),
                expr_ast,
                ExprType::Integer,
            ) {
                Some(value) => match value.unwrap_int_ref().to_u64() {
                    Some(count) => count,
                    None => {
                        self.errors.push(AsmError::DirectiveExprOutOfRange {
                            directive: ".RESERVE",
                            component: "count",
                            expr_span: expr_ast.span,
                            expr_value: value.unwrap_int_ref().clone(),
                            valid_range: bigint_range(u64::MIN, u64::MAX),
                        });
                        return;
                    }
                },
                None => return,
            }
        } else {
            1
        };
        let type_size = self.data_type_size(reserve_ast.data_type);
        if let Some(chunk_env) = self.env.current_chunk() {
            // TODO: error on overflow
            chunk_env.add_padding((type_size * count) as usize);
        }
    }

    fn expand_section(&mut self, section_ast: AsmSectionAst) {
        let name: Option<Rc<str>> = self
            .typecheck_static_dir_expr_as(
                (".SECTION", "name"),
                &section_ast.name,
                ExprType::String,
            )
            .map(|value| value.unwrap_str_ref().clone());

        let mut align: Option<Align> = None;
        let mut arch: Option<Rc<str>> = None;
        let mut fill: Option<u8> = None;
        let mut start: Option<Addr> = None;
        let mut within: Option<Align> = None;
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for (id_ast, expr_ast) in section_ast.attrs {
            self.chunk_declare_attr(&mut prev_attrs, &id_ast);
            match &*id_ast.name {
                "align" => align = Some(self.chunk_align_attr(expr_ast)),
                "arch" => arch = Some(self.chunk_arch_attr(expr_ast)),
                "fill" => fill = Some(self.chunk_fill_attr(expr_ast)),
                "start" => start = Some(self.chunk_start_attr(expr_ast)),
                "within" => within = Some(self.chunk_within_attr(expr_ast)),
                _ => {
                    self.errors.push(AsmError::InvalidAttrName {
                        directive: ".SECTION",
                        attr_name: id_ast.name,
                        attr_span: id_ast.span,
                    });
                }
            }
        }

        let chunk_index = self.next_chunk_index;
        self.next_chunk_index += 1;
        self.env.begin_chunk(chunk_index);
        if let Some(arch) = arch {
            self.set_current_arch(arch);
        }
        // TODO: don't attempt to expand statements if the arch was invalid
        self.expand_statements(section_ast.body);
        let chunk_env = self.env.end_chunk();
        // TODO: error if size is too large
        let size = Size::try_from(chunk_env.total_size()).unwrap();
        let finished_chunk = chunk_env.finish();
        if let Some(section_name) = name {
            let chunk = ObjChunk {
                section_name,
                data: finished_chunk.data,
                size,
                start,
                align: align.unwrap_or_default(),
                within,
                fill,
                symbols: finished_chunk.symbols,
                patches: finished_chunk.patches,
            };
            debug_assert!(!self.chunks.contains_key(&chunk_index));
            self.chunks.insert(chunk_index, chunk);
        }
    }

    fn expand_utf8_data(&mut self, data_ast: AsmUtf8DataAst) {
        if self.env.current_chunk().is_none() {
            self.errors.push(AsmError::DirectiveNotInSection {
                directive: ".UTF8",
                span: data_ast.directive_span,
            });
        }
        for expr_ast in data_ast.expressions {
            match self.typecheck_expression(&expr_ast) {
                Some((expr, ExprType::Integer)) => {
                    let Some(value) = expr.static_value() else {
                        self.errors.push(AsmError::DirectiveExprNotStatic {
                            directive: ".UTF8",
                            component: "value",
                            expr_span: expr_ast.span,
                        });
                        return;
                    };
                    let bigint = value.unwrap_int_ref();
                    let Some(chr) = bigint.to_u32().and_then(char::from_u32)
                    else {
                        self.errors.push(
                            AsmError::InvalidUnicodeScalarValue {
                                expr_span: expr_ast.span,
                                expr_value: bigint.clone(),
                            },
                        );
                        return;
                    };
                    if let Some(chunk_env) = self.env.current_chunk() {
                        chunk_env
                            .data_mut()
                            .extend_from_slice(chr.to_string().as_bytes());
                    }
                }
                Some((expr, ExprType::String)) => {
                    let Some(value) = expr.static_value() else {
                        self.errors.push(AsmError::DirectiveExprNotStatic {
                            directive: ".UTF8",
                            component: "value",
                            expr_span: expr_ast.span,
                        });
                        return;
                    };
                    if let Some(chunk_env) = self.env.current_chunk() {
                        chunk_env.data_mut().extend_from_slice(
                            value.unwrap_str_ref().as_bytes(),
                        );
                    }
                }
                Some((_, ty)) => {
                    self.errors.push(AsmError::DirectiveExprTypeError {
                        directive: ".UTF8",
                        component: "value",
                        expr_span: expr_ast.span,
                        expr_type: ty,
                        valid_types: vec![ExprType::String, ExprType::Integer],
                    });
                }
                None => {}
            }
        }
    }

    fn expand_int_data(&mut self, int_data: AsmIntDataAst) {
        let directive = int_data.int_type.directive();
        if self.env.current_chunk().is_none() {
            self.errors.push(AsmError::DirectiveNotInSection {
                directive,
                span: int_data.directive_span,
            });
        }
        let Some(int_type) = self.int_patch_type(int_data.int_type) else {
            self.errors.push(AsmError::ArchHasNoEndianness {
                directive,
                span: int_data.directive_span,
                arch: self.env.current_arch().clone(),
            });
            return;
        };
        for expr_ast in int_data.expressions {
            let static_value: i64 = match self.typecheck_expression(&expr_ast)
            {
                Some((mut expr, ExprType::Label)) => {
                    // TODO: If the label belongs to a chunk with an explicit
                    // start address, then the label's address value is static
                    // and no patch is necessary.
                    expr.ops.push(ObjExprOp::LabelAddr);
                    self.try_add_patch(ObjPatchData::Integer(int_type, expr));
                    0
                }
                Some((expr, ExprType::Integer)) => match expr.static_value() {
                    Some(value) => {
                        let bigint = value.unwrap_int_ref();
                        match int_type.value_in_range(bigint) {
                            Ok(value) => value,
                            Err(range) => {
                                self.errors.push(
                                    AsmError::DirectiveExprOutOfRange {
                                        directive,
                                        component: "value",
                                        expr_span: expr_ast.span,
                                        expr_value: bigint.clone(),
                                        valid_range: RangeInclusive {
                                            start: BigInt::from(range.start),
                                            last: BigInt::from(range.last),
                                        },
                                    },
                                );
                                0
                            }
                        }
                    }
                    None => {
                        let data = ObjPatchData::Integer(int_type, expr);
                        self.try_add_patch(data);
                        0
                    }
                },
                Some((_, ty)) => {
                    self.errors.push(AsmError::DirectiveExprTypeError {
                        directive,
                        component: "value",
                        expr_span: expr_ast.span,
                        expr_type: ty,
                        valid_types: vec![ExprType::Integer, ExprType::Label],
                    });
                    0
                }
                None => 0,
            };
            if let Some(chunk_env) = self.env.current_chunk() {
                let data = chunk_env.data_mut();
                match int_type {
                    ObjPatchIntType::U8 => {
                        data.push(static_value as u8);
                    }
                    ObjPatchIntType::U16be => {
                        data.push((static_value >> 8) as u8);
                        data.push(static_value as u8);
                    }
                    ObjPatchIntType::U16le => {
                        data.push(static_value as u8);
                        data.push((static_value >> 8) as u8);
                    }
                    ObjPatchIntType::U24be => {
                        data.push((static_value >> 16) as u8);
                        data.push((static_value >> 8) as u8);
                        data.push(static_value as u8);
                    }
                    ObjPatchIntType::U24le => {
                        data.push(static_value as u8);
                        data.push((static_value >> 8) as u8);
                        data.push((static_value >> 16) as u8);
                    }
                }
            }
        }
    }

    fn set_current_arch(&mut self, arch: Rc<str>) {
        debug_assert!(self.arch_tree.contains_arch(&arch));
        self.env.set_current_arch(arch);
    }

    fn data_type_size(&self, data_type: AsmDataTypeAst) -> u64 {
        match data_type {
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U8) => 1,
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U16) => 2,
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U16be) => 2,
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U16le) => 2,
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U24) => 3,
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U24be) => 3,
            AsmDataTypeAst::Int(_, AsmIntTypeAst::U24le) => 3,
        }
    }

    fn int_patch_type(
        &self,
        int_type: AsmIntTypeAst,
    ) -> Option<ObjPatchIntType> {
        match int_type {
            AsmIntTypeAst::U8 => Some(ObjPatchIntType::U8),
            AsmIntTypeAst::U16 => self.endian_patch_type(
                ObjPatchIntType::U16be,
                ObjPatchIntType::U16le,
            ),
            AsmIntTypeAst::U16be => Some(ObjPatchIntType::U16be),
            AsmIntTypeAst::U16le => Some(ObjPatchIntType::U16le),
            AsmIntTypeAst::U24 => self.endian_patch_type(
                ObjPatchIntType::U24be,
                ObjPatchIntType::U24le,
            ),
            AsmIntTypeAst::U24be => Some(ObjPatchIntType::U24be),
            AsmIntTypeAst::U24le => Some(ObjPatchIntType::U24le),
        }
    }

    fn endian_patch_type(
        &self,
        be_type: ObjPatchIntType,
        le_type: ObjPatchIntType,
    ) -> Option<ObjPatchIntType> {
        let arch = self.env.current_arch();
        match self.arch_tree.native_endianness(arch) {
            Some(Endianness::BigEndian) => Some(be_type),
            Some(Endianness::LittleEndian) => Some(le_type),
            None => None,
        }
    }

    fn chunk_align_attr(&mut self, expr_ast: ExprAst) -> Align {
        self.chunk_static_align_attr("align", expr_ast)
    }

    fn chunk_arch_attr(&mut self, expr_ast: ExprAst) -> Rc<str> {
        let expr_span = expr_ast.span;
        if let Some(arch) = self.chunk_static_str_attr("arch", expr_ast) {
            if self.arch_tree.contains_arch(&arch) {
                return arch;
            }
            self.errors.push(AsmError::UnknownArch {
                arch: arch.clone(),
                span: expr_span,
            });
        }
        self.env.current_arch().clone()
    }

    fn chunk_fill_attr(&mut self, expr_ast: ExprAst) -> u8 {
        let expr_span = expr_ast.span;
        if let Some(bigint) = self.chunk_static_int_attr("fill", expr_ast) {
            match u8::try_from(&bigint) {
                Ok(byte) => return byte,
                Err(_) => {
                    self.errors.push(AsmError::DirectiveExprOutOfRange {
                        directive: ".SECTION",
                        component: "fill",
                        expr_span,
                        expr_value: bigint,
                        valid_range: bigint_range(u8::MIN, u8::MAX),
                    })
                }
            }
        }
        u8::default()
    }

    fn chunk_start_attr(&mut self, expr_ast: ExprAst) -> Addr {
        let expr_span = expr_ast.span;
        if let Some(bigint) = self.chunk_static_int_attr("start", expr_ast) {
            match Addr::try_from(&bigint) {
                Ok(addr) => return addr,
                Err(_) => {
                    self.errors.push(AsmError::DirectiveExprOutOfRange {
                        directive: ".SECTION",
                        component: "start",
                        expr_span,
                        expr_value: bigint,
                        valid_range: bigint_range(Addr::MIN, Addr::MAX),
                    })
                }
            }
        }
        Addr::MIN
    }

    fn chunk_within_attr(&mut self, expr_ast: ExprAst) -> Align {
        self.chunk_static_align_attr("within", expr_ast)
    }

    fn chunk_static_align_attr(
        &mut self,
        attr_name: &'static str,
        expr_ast: ExprAst,
    ) -> Align {
        let expr_span = expr_ast.span;
        if let Some(bigint) = self.chunk_static_int_attr(attr_name, expr_ast) {
            match Align::try_from(&bigint) {
                Ok(align) => return align,
                Err(error) => {
                    self.errors.push(AsmError::InvalidAlignmentValue {
                        directive: ".SECTION",
                        attr_name,
                        error,
                        expr_span,
                        expr_value: bigint,
                    });
                }
            }
        }
        Align::default()
    }

    fn chunk_static_int_attr(
        &mut self,
        attr_name: &'static str,
        expr_ast: ExprAst,
    ) -> Option<BigInt> {
        self.typecheck_static_dir_expr_as(
            (".SECTION", attr_name),
            &expr_ast,
            ExprType::Integer,
        )
        .map(|value| value.unwrap_int_ref().clone())
    }

    fn chunk_static_str_attr(
        &mut self,
        attr_name: &'static str,
        expr_ast: ExprAst,
    ) -> Option<Rc<str>> {
        self.typecheck_static_dir_expr_as(
            (".SECTION", attr_name),
            &expr_ast,
            ExprType::String,
        )
        .map(|value| value.unwrap_str_ref().clone())
    }

    fn chunk_declare_attr(
        &mut self,
        prev_attrs: &mut HashMap<Rc<str>, SrcSpan>,
        id_ast: &IdentifierAst,
    ) {
        if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
            self.errors.push(AsmError::DuplicateAttrName {
                directive: ".SECTION",
                attr_name: id_ast.name.clone(),
                attr_span: id_ast.span,
                prev_span,
            });
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
        }
    }

    /// Attempts to add an `ObjPatch` to the current chunk starting at the
    /// current end of its static data. Does nothing if there is no current
    /// chunk; it is assumed that the caller will have already flagged an error
    /// in that case.
    fn try_add_patch(&mut self, data: ObjPatchData) {
        if let Some(chunk_env) = self.env.current_chunk() {
            // TODO: Error instead of crash if offset is too large.
            let offset = Offset::try_from(chunk_env.total_size()).unwrap();
            chunk_env.add_patch(ObjPatch { offset, data });
        }
    }

    fn typecheck_static_dir_expr_as(
        &mut self,
        (directive, component): (&'static str, &'static str),
        expr_ast: &ExprAst,
        required_type: ExprType,
    ) -> Option<ExprValue> {
        match self.typecheck_dir_expr_as(
            (directive, component),
            expr_ast,
            required_type,
        ) {
            Some(expr) => match expr.static_value() {
                Some(value) => Some(value.clone()),
                None => {
                    self.errors.push(AsmError::DirectiveExprNotStatic {
                        directive,
                        component,
                        expr_span: expr_ast.span,
                    });
                    None
                }
            },
            None => None,
        }
    }

    fn typecheck_dir_expr_as(
        &mut self,
        (directive, component): (&'static str, &'static str),
        expr_ast: &ExprAst,
        required_type: ExprType,
    ) -> Option<ObjExpr> {
        match self.typecheck_expression(expr_ast) {
            Some((expr, expr_type)) => {
                if expr_type == required_type {
                    Some(expr)
                } else {
                    self.errors.push(AsmError::DirectiveExprTypeError {
                        directive,
                        component,
                        expr_span: expr_ast.span,
                        expr_type,
                        valid_types: vec![required_type],
                    });
                    None
                }
            }
            None => None,
        }
    }

    fn typecheck_expression(
        &mut self,
        expr_ast: &ExprAst,
    ) -> Option<(ObjExpr, ExprType)> {
        // TODO: error if contains a name that is reserved in current arch
        match self.env.typecheck_expression(expr_ast) {
            Ok(expr_and_type) => Some(expr_and_type),
            Err(mut errors) => {
                self.errors.append(&mut errors);
                None
            }
        }
    }

    fn merge_errors(&mut self, result: AsmResult<()>) {
        match result {
            Ok(()) => {}
            Err(mut errors) => self.errors.append(&mut errors),
        }
    }

    fn finish(self) -> AsmResult<ObjFile> {
        if self.errors.is_empty() {
            Ok(ObjFile {
                chunks: self.chunks.into_values().collect(),
                imports: self.imports,
                asserts: self.asserts,
            })
        } else {
            Err(self.errors)
        }
    }
}

fn bigint_range<T: Into<BigInt>>(start: T, last: T) -> RangeInclusive<BigInt> {
    RangeInclusive { start: start.into(), last: last.into() }
}

//===========================================================================//
