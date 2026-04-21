//! Facilities for assembling source files into object files.

mod arch;
mod builtins;
mod expr;
mod macros;

use crate::addr::{Align, Offset, Size};
use crate::error::{SourceError, SourceResult, SrcSpan};
use crate::expr::ExprType;
use crate::obj::{
    ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjPatch, ObjSymbol, PatchKind,
};
use crate::parse::{
    AsmDefMacroAst, AsmInvokeAst, AsmModuleAst, AsmSectionAst, AsmStmtAst,
    ExprAst, IdentifierAst,
};
use arch::ArchTree;
use expr::AsmTypeEnv;
use macros::MacroTable;
use num_bigint::BigInt;
use std::collections::BTreeMap;
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
    assembler.scope_module(&module);
    assembler.expand_module(module);
    assembler.finish()
}

//===========================================================================//

struct Assembler {
    arch_tree: ArchTree,
    macros: MacroTable,
    env: AsmTypeEnv,
    next_chunk_index: usize,
    chunks: BTreeMap<usize, ObjChunk>,
    imports: Vec<Rc<str>>,
    errors: Vec<SourceError>,
}

impl Assembler {
    fn new() -> Assembler {
        let (arch_tree, macros) = builtins::make_builtins();
        Assembler {
            arch_tree,
            macros,
            env: AsmTypeEnv::new(),
            next_chunk_index: 0,
            chunks: BTreeMap::new(),
            imports: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Scans over a module AST (without expanding macros) and collects
    /// existing labels and scopes into the environment.
    fn scope_module(&mut self, module: &AsmModuleAst) {
        self.scope_statements(&module.statements);
    }

    /// Scans over a list of statement ASTs (without expanding macros) and
    /// collects existing labels and scopes into the environment.
    fn scope_statements(&mut self, statements: &[AsmStmtAst]) {
        for statement in statements {
            self.scope_statement(statement);
        }
    }

    /// Scans over a statement AST (without expanding macros) and collects
    /// existing labels and scopes into the environment.
    fn scope_statement(&mut self, statement: &AsmStmtAst) {
        match statement {
            AsmStmtAst::DefMacro(_) => {}
            AsmStmtAst::Import(id) => self.scope_import(id),
            AsmStmtAst::Invoke(_) => {}
            AsmStmtAst::Label(id) => self.scope_label(id),
            AsmStmtAst::Section(section) => self.scope_section(section),
            AsmStmtAst::U8(_expr) => {}
            AsmStmtAst::U16le(_expr) => {}
            AsmStmtAst::U24le(_expr) => {}
        }
    }

    fn scope_import(&mut self, id_ast: &IdentifierAst) {
        // TODO: error if name is reserved in current arch
        let result = self.env.declare_import(id_ast);
        self.merge_errors(result);
    }

    fn scope_label(&mut self, id_ast: &IdentifierAst) {
        // TODO: error if name is reserved in current arch
        let result = self.env.declare_label(id_ast);
        self.merge_errors(result);
    }

    fn scope_section(&mut self, section_ast: &AsmSectionAst) {
        self.scope_statements(&section_ast.body);
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
            AsmStmtAst::DefMacro(def) => self.expand_macro_definition(def),
            AsmStmtAst::Import(id) => self.expand_import(id),
            AsmStmtAst::Invoke(invoke) => self.expand_macro_invocation(invoke),
            AsmStmtAst::Label(id) => self.expand_label(id),
            AsmStmtAst::Section(section) => self.expand_section(section),
            AsmStmtAst::U8(expr) => self.expand_u8(expr),
            AsmStmtAst::U16le(expr) => self.expand_u16le(expr),
            AsmStmtAst::U24le(expr) => self.expand_u24le(expr),
        }
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
                self.scope_statements(&statements);
                self.expand_statements(statements);
            }
            Err(mut errors) => {
                self.errors.append(&mut errors);
            }
        }
    }

    fn expand_label(&mut self, id_ast: IdentifierAst) {
        let Some(chunk_env) = self.env.current_chunk() else {
            let message = "labels must be within a .SECTION".to_string();
            self.errors.push(SourceError::new(id_ast.span, message));
            return;
        };
        chunk_env.symbols.push(ObjSymbol {
            name: id_ast.name,
            exported: true, // TODO
            offset: Offset::try_from(chunk_env.data.len()).unwrap(),
        });
    }

    fn expand_section(&mut self, section_ast: AsmSectionAst) {
        let name: Option<Rc<str>> = match self
            .typecheck_expression(&section_ast.name)
        {
            Some((expr, ExprType::String)) => match expr.static_value() {
                Some(value) => Some(value.clone().unwrap_str()),
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

        let mut arch: Option<Rc<str>> = None;
        let align = Align::default(); // TODO: support align attribute
        let within: Option<Align> = None; // TODO: support within attribute
        let mut fill: Option<u8> = None;
        for (id_ast, expr_ast) in section_ast.attrs {
            // TODO: error if repeated attr name
            match &*id_ast.name {
                "arch" => arch = self.chunk_arch_attr(expr_ast),
                "fill" => fill = Some(self.chunk_fill_attr(expr_ast)),
                _ => {} // TODO: error for unknown attr name
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
        let size = Size::try_from(chunk_env.data.len()).unwrap();
        if let Some(section_name) = name {
            let chunk = ObjChunk {
                section_name,
                data: Box::from(chunk_env.data),
                size,
                align,
                within,
                fill,
                symbols: Rc::from(chunk_env.symbols),
                patches: Box::from(chunk_env.patches),
            };
            debug_assert!(!self.chunks.contains_key(&chunk_index));
            self.chunks.insert(chunk_index, chunk);
        }
    }

    fn expand_u16le(&mut self, expr_ast: ExprAst) {
        let word = self.expand_int_data_directive(PatchKind::U16le, expr_ast);
        if let Some(chunk_env) = self.env.current_chunk() {
            chunk_env.data.push(word as u8);
            chunk_env.data.push((word >> 8) as u8);
        }
    }

    fn expand_u24le(&mut self, expr_ast: ExprAst) {
        let long = self.expand_int_data_directive(PatchKind::U24le, expr_ast);
        if let Some(chunk_env) = self.env.current_chunk() {
            chunk_env.data.push(long as u8);
            chunk_env.data.push((long >> 8) as u8);
            chunk_env.data.push((long >> 16) as u8);
        }
    }

    fn expand_u8(&mut self, expr_ast: ExprAst) {
        let byte = self.expand_int_data_directive(PatchKind::U8, expr_ast);
        if let Some(chunk_env) = self.env.current_chunk() {
            chunk_env.data.push(byte as u8);
        }
    }

    /// Shared implementation for directives that insert an integer (of some
    /// size and signedness) into the chunk data. Returns the static integer
    /// value that should be written into the chunk data; if a link-time patch
    /// is required, this static value will just be zero, and an appropriate
    /// `ObjPatch` will be added to the current chunk.
    fn expand_int_data_directive(
        &mut self,
        kind: PatchKind,
        expr_ast: ExprAst,
    ) -> i64 {
        if self.env.current_chunk().is_none() {
            let message = format!(
                "{} directive must be within a .SECTION",
                kind.directive()
            );
            self.errors.push(SourceError::new(expr_ast.span, message));
        }
        match self.typecheck_expression(&expr_ast) {
            Some((mut expr, ExprType::Label)) => {
                // TODO: If the label belongs to a chunk with an explicit start
                // address, then the label's address value is static and no
                // patch is necessary.
                expr.ops.push(ObjExprOp::LabelAddr);
                self.try_add_patch(kind, expr);
            }
            Some((expr, ExprType::Integer)) => match expr.static_value() {
                Some(value) => {
                    let bigint = value.unwrap_int_ref();
                    match kind.value_in_range(bigint) {
                        Ok(value) => return value,
                        Err(range) => {
                            let message = format!(
                                "{} value is statically out of range ({}-{})",
                                kind.directive(),
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
                        }
                    }
                }
                None => self.try_add_patch(kind, expr),
            },
            Some((_, ty)) => {
                let message =
                    format!("{} value must be an integer", kind.directive());
                let label = format!("this expression has type {ty}");
                self.errors.push(
                    SourceError::new(expr_ast.span, message)
                        .with_label(expr_ast.span, label),
                );
            }
            None => {}
        }
        0
    }

    fn set_current_arch(&mut self, arch: Rc<str>) {
        debug_assert!(self.arch_tree.contains_arch(&arch));
        self.env.set_current_arch(arch);
    }

    fn chunk_arch_attr(&mut self, expr_ast: ExprAst) -> Option<Rc<str>> {
        let expr_span = expr_ast.span;
        if let Some(arch) = self.chunk_static_str_attr("arch", expr_ast) {
            if self.arch_tree.contains_arch(&arch) {
                return Some(arch);
            }
            self.unknown_arch_error(expr_span, &arch);
        }
        None
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
                Some(value) => Some(value.clone().unwrap_str()),
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

    fn unknown_arch_error(&mut self, expr_span: SrcSpan, arch: &str) {
        let message = format!("the `{arch}` architecture was never defined");
        self.errors.push(SourceError::new(expr_span, message));
    }

    /// Attempts to add an `ObjPatch` to the current chunk starting at the
    /// current end of its static data. Does nothing if there is no current
    /// chunk; it is assumed that the caller will have already flagged an error
    /// in that case.
    fn try_add_patch(&mut self, kind: PatchKind, expr: ObjExpr) {
        if let Some(chunk_env) = self.env.current_chunk() {
            // TODO: Error instead of crash if offset is too large.
            let offset = Offset::try_from(chunk_env.data.len()).unwrap();
            let patch = ObjPatch { offset, kind, expr };
            chunk_env.patches.push(patch);
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
            })
        } else {
            Err(self.errors)
        }
    }
}

//===========================================================================//
