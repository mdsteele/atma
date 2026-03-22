//! Facilities for assembling source files into object files.

mod expr;
mod macros;

use crate::addr::{Align, Offset, Size};
use crate::error::{SourceError, SourceResult};
use crate::expr::ExprType;
use crate::obj::{
    ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjPatch, ObjSymbol, PatchKind,
};
use crate::parse::{
    AsmDefMacroAst, AsmInvokeAst, AsmModuleAst, AsmSectionAst, AsmStmtAst,
    ExprAst, IdentifierAst,
};
use expr::AsmTypeEnv;
use macros::MacroTable;
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
    env: AsmTypeEnv,
    macros: MacroTable,
    next_chunk_index: usize,
    chunks: BTreeMap<usize, ObjChunk>,
    imports: Vec<Rc<str>>,
    errors: Vec<SourceError>,
}

impl Assembler {
    fn new() -> Assembler {
        Assembler {
            env: AsmTypeEnv::new(),
            macros: MacroTable::new(),
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
        match self.env.declare_import(id_ast) {
            Ok(()) => {}
            Err(mut errors) => self.errors.append(&mut errors),
        }
    }

    fn scope_label(&mut self, id_ast: &IdentifierAst) {
        match self.env.declare_label(id_ast) {
            Ok(()) => {}
            Err(mut errors) => self.errors.append(&mut errors),
        }
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
        match self.macros.define(def_macro_ast) {
            Ok(()) => {}
            Err(mut errors) => {
                self.errors.append(&mut errors);
            }
        }
    }

    fn expand_macro_invocation(&mut self, invoke_ast: AsmInvokeAst) {
        match self.macros.expand(invoke_ast) {
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
        if let Some(chunk_env) = self.env.current_chunk() {
            chunk_env.symbols.push(ObjSymbol {
                name: id_ast.name,
                exported: true, // TODO
                offset: Offset::try_from(chunk_env.data.len()).unwrap(),
            });
        } else {
            let message = "labels must be within a .SECTION".to_string();
            self.errors.push(SourceError::new(id_ast.span, message));
        }
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
        let align = Align::default(); // TODO: support align attribute
        let within: Option<Align> = None; // TODO: support within attribute
        let fill: Option<u8> = None; // TODO: support fill attribute
        let chunk_index = self.next_chunk_index;
        self.next_chunk_index += 1;
        self.env.begin_chunk(chunk_index);
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
        match self.env.typecheck_expression(expr_ast) {
            Ok(expr_and_type) => Some(expr_and_type),
            Err(errors) => {
                self.errors.extend(errors);
                None
            }
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
