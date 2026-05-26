//! Facilities for assembling source files into object files.

mod arch;
mod builtins;
mod expr;
mod macros;

use crate::addr::{Addr, Align, AlignTryFromError, Endianness, Offset, Size};
use crate::error::{SourceError, SourceResult, SrcSpan};
use crate::expr::ExprType;
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
use expr::AsmTypeEnv;
use macros::MacroTable;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

//===========================================================================//

/// Assembles an object file from source code.
pub fn assemble_source(source: &str) -> SourceResult<ObjFile> {
    assemble_ast(
        AsmModuleAst::parse_source(source)
            .map_err(SourceError::from_errors)?,
    )
}

fn assemble_ast(module: AsmModuleAst) -> SourceResult<ObjFile> {
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
    errors: Vec<SourceError>,
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
        let opt_message_expr: Option<ObjExpr> = match &assert_ast.message {
            None => None,
            Some(expr_ast) => match self.typecheck_expression(expr_ast) {
                Some((expr, ExprType::String)) => Some(expr),
                Some((_, ty)) => {
                    let message =
                        ".ASSERT message must be a string".to_string();
                    let label = format!("this expression has type {ty}");
                    self.errors.push(
                        SourceError::new(expr_ast.span, message)
                            .with_label(expr_ast.span, label),
                    );
                    None
                }
                None => None,
            },
        };
        let condition_expr: ObjExpr = match self
            .typecheck_expression(&assert_ast.condition)
        {
            Some((condition_expr, ExprType::Boolean)) => match condition_expr
                .static_value()
            {
                None => condition_expr,
                Some(value) => {
                    if value.unwrap_bool() {
                        return;
                    } else {
                        match &opt_message_expr {
                            None => {
                                let message = "Assertion failed".to_string();
                                self.errors.push(SourceError::new(
                                    assert_ast.condition.span,
                                    message,
                                ));
                                return;
                            }
                            Some(message_expr) => {
                                match message_expr.static_value() {
                                    Some(message_value) => {
                                        let message = format!(
                                            "Assertion failed: {}",
                                            message_value.unwrap_str_ref()
                                        );
                                        self.errors.push(SourceError::new(
                                            assert_ast.condition.span,
                                            message,
                                        ));
                                        return;
                                    }
                                    None => condition_expr,
                                }
                            }
                        }
                    }
                }
            },
            Some((_, ty)) => {
                let message =
                    ".ASSERT condition must be a boolean".to_string();
                let label = format!("this expression has type {ty}");
                self.errors.push(
                    SourceError::new(assert_ast.condition.span, message)
                        .with_label(assert_ast.condition.span, label),
                );
                return;
            }
            None => return,
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
            let message = "labels must be within a .SECTION".to_string();
            self.errors.push(SourceError::new(id_ast.span, message));
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
            let message =
                ".RESERVE directive must be within a .SECTION".to_string();
            self.errors
                .push(SourceError::new(reserve_ast.directive_span, message));
        }
        let count: u64 = if let Some(expr_ast) = &reserve_ast.count {
            match self.typecheck_expression(expr_ast) {
                Some((expr, ExprType::Integer)) => match expr.static_value() {
                    Some(value) => match value.unwrap_int_ref().to_u64() {
                        Some(count) => count,
                        None => {
                            let message =
                                "reserve count is too large".to_string();
                            let label = format!(
                                "this value of this expression is \
                                             {value}"
                            );
                            self.errors.push(
                                SourceError::new(expr_ast.span, message)
                                    .with_label(expr_ast.span, label),
                            );
                            return;
                        }
                    },
                    None => {
                        let message =
                            "reserve count must be static".to_string();
                        let label = "this expression isn't static".to_string();
                        self.errors.push(
                            SourceError::new(expr_ast.span, message)
                                .with_label(expr_ast.span, label),
                        );
                        return;
                    }
                },
                Some((_, ty)) => {
                    let message =
                        "reserve count must be an integer".to_string();
                    let label = format!("this expression has type {ty}");
                    self.errors.push(
                        SourceError::new(expr_ast.span, message)
                            .with_label(expr_ast.span, label),
                    );
                    return;
                }
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
        let name: Option<Rc<str>> = match self
            .typecheck_expression(&section_ast.name)
        {
            Some((expr, ExprType::String)) => match expr.static_value() {
                Some(value) => Some(value.unwrap_str_ref().clone()),
                None => {
                    let message = "section name must be static".to_string();
                    let label = "this expression isn't static".to_string();
                    self.errors.push(
                        SourceError::new(section_ast.name.span, message)
                            .with_label(section_ast.name.span, label),
                    );
                    None
                }
            },
            Some((_, ty)) => {
                let message = "section name must be a string".to_string();
                let label = format!("this expression has type {ty}");
                self.errors.push(
                    SourceError::new(section_ast.name.span, message)
                        .with_label(section_ast.name.span, label),
                );
                None
            }
            None => None,
        };

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
                _ => self.chunk_invalid_attr_error(id_ast),
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
            let message =
                ".UTF8 directive must be within a .SECTION".to_string();
            self.errors
                .push(SourceError::new(data_ast.directive_span, message));
        }
        for expr_ast in data_ast.expressions {
            match self.typecheck_expression(&expr_ast) {
                Some((expr, ExprType::Integer)) => {
                    let Some(value) = expr.static_value() else {
                        let message = ".UTF8 value must be static".to_string();
                        let label = "this expression isn't static".to_string();
                        self.errors.push(
                            SourceError::new(expr_ast.span, message)
                                .with_label(expr_ast.span, label),
                        );
                        return;
                    };
                    let bigint = value.unwrap_int_ref();
                    let Some(chr) = bigint.to_u32().and_then(char::from_u32)
                    else {
                        let message =
                            "invalid unicode scalar value".to_string();
                        let label = format!(
                            "the value of this expression is {bigint}"
                        );
                        self.errors.push(
                            SourceError::new(expr_ast.span, message)
                                .with_label(expr_ast.span, label),
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
                        let message = ".UTF8 value must be static".to_string();
                        let label = "this expression isn't static".to_string();
                        self.errors.push(
                            SourceError::new(expr_ast.span, message)
                                .with_label(expr_ast.span, label),
                        );
                        return;
                    };
                    if let Some(chunk_env) = self.env.current_chunk() {
                        chunk_env.data_mut().extend_from_slice(
                            value.unwrap_str_ref().as_bytes(),
                        );
                    }
                }
                Some((_, ty)) => {
                    let message = ".UTF8 value must be a string".to_string();
                    let label = format!("this expression has type {ty}");
                    self.errors.push(
                        SourceError::new(expr_ast.span, message)
                            .with_label(expr_ast.span, label),
                    );
                }
                None => {}
            }
        }
    }

    fn expand_int_data(&mut self, int_data: AsmIntDataAst) {
        if self.env.current_chunk().is_none() {
            let message = format!(
                "{} directive must be within a .SECTION",
                int_data.int_type.directive()
            );
            self.errors
                .push(SourceError::new(int_data.directive_span, message));
        }
        let Some(int_type) = self.int_patch_type(int_data.int_type) else {
            let message = format!(
                "Cannot use {} directive under architecture {:?}, which has \
                 no defined endianness",
                int_data.int_type.directive(),
                self.env.current_arch()
            );
            self.errors
                .push(SourceError::new(int_data.directive_span, message));
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
                                let message = format!(
                                    "{} value is statically out of range \
                                     ({}-{})",
                                    int_data.int_type.directive(),
                                    range.start(),
                                    range.end()
                                );
                                let label = format!(
                                    "the value of this expression is {bigint}"
                                );
                                self.errors.push(
                                    SourceError::new(expr_ast.span, message)
                                        .with_label(expr_ast.span, label),
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
                    let message = format!(
                        "{} value must be an integer",
                        int_data.int_type.directive()
                    );
                    let label = format!("this expression has type {ty}");
                    self.errors.push(
                        SourceError::new(expr_ast.span, message)
                            .with_label(expr_ast.span, label),
                    );
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
            self.unknown_arch_error(expr_span, &arch);
        }
        self.env.current_arch().clone()
    }

    fn chunk_fill_attr(&mut self, expr_ast: ExprAst) -> u8 {
        let expr_span = expr_ast.span;
        if let Some(bigint) = self.chunk_static_int_attr("fill", expr_ast) {
            match u8::try_from(&bigint) {
                Ok(byte) => return byte,
                Err(_) => self
                    .chunk_attr_out_of_range_error("fill", expr_span, &bigint),
            }
        }
        u8::default()
    }

    fn chunk_start_attr(&mut self, expr_ast: ExprAst) -> Addr {
        let expr_span = expr_ast.span;
        if let Some(bigint) = self.chunk_static_int_attr("start", expr_ast) {
            match Addr::try_from(&bigint) {
                Ok(addr) => return addr,
                Err(()) => self.chunk_attr_out_of_range_error(
                    "start", expr_span, &bigint,
                ),
            }
        }
        Addr::MIN
    }

    fn chunk_within_attr(&mut self, expr_ast: ExprAst) -> Align {
        self.chunk_static_align_attr("within", expr_ast)
    }

    fn chunk_static_align_attr(
        &mut self,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> Align {
        let expr_span = expr_ast.span;
        if let Some(bigint) = self.chunk_static_int_attr(attr_name, expr_ast) {
            match Align::try_from(&bigint) {
                Ok(align) => return align,
                Err(error) => {
                    let message = match error {
                        AlignTryFromError::NotAPowerOfTwo => {
                            format!("`{attr_name}` must be a power of two")
                        }
                        AlignTryFromError::TooLargePowerOfTwo => {
                            format!(
                                "`{attr_name}` must be at most {}",
                                0x8000_0000u32
                            )
                        }
                    };
                    let label =
                        format!("the value of this expression is {bigint}");
                    self.errors.push(
                        SourceError::new(expr_span, message)
                            .with_label(expr_span, label),
                    );
                }
            }
        }
        Align::default()
    }

    fn chunk_static_int_attr(
        &mut self,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> Option<BigInt> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(&expr_ast) {
            Some((expr, ExprType::Integer)) => match expr.static_value() {
                Some(value) => Some(value.clone().unwrap_int()),
                None => {
                    self.chunk_attr_non_static_error(attr_name, expr_span);
                    None
                }
            },
            Some((_, expr_type)) => {
                self.chunk_attr_type_error(
                    attr_name,
                    &ExprType::Integer,
                    expr_span,
                    &expr_type,
                );
                None
            }
            None => None,
        }
    }

    fn chunk_static_str_attr(
        &mut self,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> Option<Rc<str>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(&expr_ast) {
            Some((expr, ExprType::String)) => match expr.static_value() {
                Some(value) => Some(value.unwrap_str_ref().clone()),
                None => {
                    self.chunk_attr_non_static_error(attr_name, expr_span);
                    None
                }
            },
            Some((_, expr_type)) => {
                self.chunk_attr_type_error(
                    attr_name,
                    &ExprType::String,
                    expr_span,
                    &expr_type,
                );
                None
            }
            None => None,
        }
    }

    fn chunk_declare_attr(
        &mut self,
        prev_attrs: &mut HashMap<Rc<str>, SrcSpan>,
        id_ast: &IdentifierAst,
    ) {
        if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
            let message = format!(
                "Duplicate `{}` attribute for `.SECTION`",
                id_ast.name
            );
            let label1 = "Previously declared here".to_string();
            let label2 = "Duplicated here".to_string();
            self.errors.push(
                SourceError::new(id_ast.span, message)
                    .with_label(prev_span, label1)
                    .with_label(id_ast.span, label2),
            );
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
        }
    }

    fn chunk_attr_non_static_error(
        &mut self,
        attr_name: &str,
        expr_span: SrcSpan,
    ) {
        let message =
            format!(".SECTION `{attr_name}` attribute must be static");
        let label = "this expression isn't static".to_string();
        self.errors.push(
            SourceError::new(expr_span, message).with_label(expr_span, label),
        );
    }

    fn chunk_attr_out_of_range_error(
        &mut self,
        attr_name: &str,
        expr_span: SrcSpan,
        value: &BigInt,
    ) {
        let message = format!(".SECTION `{attr_name}` is out of range");
        let label = format!("the value of this expression is {value}");
        self.errors.push(
            SourceError::new(expr_span, message).with_label(expr_span, label),
        );
    }

    fn chunk_attr_type_error(
        &mut self,
        attr_name: &str,
        expected_type: &ExprType,
        expr_span: SrcSpan,
        actual_type: &ExprType,
    ) {
        let message =
            format!(".SECTION `{attr_name}` must have type {expected_type}");
        let label = format!("this expression has type {actual_type}");
        self.errors.push(
            SourceError::new(expr_span, message).with_label(expr_span, label),
        );
    }

    fn chunk_invalid_attr_error(&mut self, attr_id: IdentifierAst) {
        let message =
            format!("Invalid `.SECTION` attribute: `{}`", attr_id.name);
        self.errors.push(SourceError::new(attr_id.span, message));
    }

    fn unknown_arch_error(&mut self, expr_span: SrcSpan, arch: &str) {
        let message = format!("the `{arch}` architecture was never defined");
        self.errors.push(SourceError::new(expr_span, message));
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

    fn typecheck_expression(
        &mut self,
        expr_ast: &ExprAst,
    ) -> Option<(ObjExpr, ExprType)> {
        // TODO: error if contains a name that is reserved in current arch
        match self.env.typecheck_expression(expr_ast) {
            Ok(expr_and_type) => Some(expr_and_type),
            Err(errors) => {
                self.errors.extend(errors);
                None
            }
        }
    }

    fn merge_errors(&mut self, result: SourceResult<()>) {
        match result {
            Ok(()) => {}
            Err(mut errors) => self.errors.append(&mut errors),
        }
    }

    fn finish(self) -> SourceResult<ObjFile> {
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

//===========================================================================//
