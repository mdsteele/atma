//! Facilities for assembling source files into object files.

mod arch;
mod builtins;
mod error;
mod expr;
mod macros;

use crate::addr::{Addr, Align, Endianness, Offset, Size};
use crate::error::{Errs, SrcCache, SrcSpan};
use crate::expr::{ExprType, ExprValue};
use crate::obj::{
    ObjAssert, ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjPatch, ObjPatchData,
    ObjPatchIntType, ObjSymbol,
};
use crate::parse::{
    AsmAssertAst, AsmBinaryAst, AsmDataTypeAst, AsmDefMacroAst, AsmIntDataAst,
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
use std::path::Path;
use std::range::RangeInclusive;
use std::rc::Rc;

//===========================================================================//

/// Assembles an object file from source code.
pub fn assemble_source(
    cache: &mut dyn SrcCache,
    src_path: Rc<str>,
    source_code: &str,
) -> AsmResult<ObjFile> {
    assemble_ast(
        cache,
        src_path,
        AsmModuleAst::parse_source(source_code).map_err(|errors| {
            errors
                .into_iter()
                .map(|error| AsmError::ParseError { error })
                .collect::<Errs<_>>()
        })?,
    )
}

fn assemble_ast(
    cache: &mut dyn SrcCache,
    src_path: Rc<str>,
    module: AsmModuleAst,
) -> AsmResult<ObjFile> {
    let mut errs = Errs::<AsmError>::new();
    let mut assembler = Assembler::new(cache, src_path);
    errs.also(assembler.predeclare_module(&module));
    errs.also(assembler.expand_module(module));
    errs.result()?;
    Ok(assembler.finish())
}

//===========================================================================//

struct Assembler<'a> {
    cache: &'a mut dyn SrcCache,
    src_path: Rc<str>,
    arch_tree: ArchTree,
    macros: MacroTable,
    next_anonymous_scope_number: u32,
    env: AsmTypeEnv,
    next_chunk_index: usize,
    chunks: BTreeMap<usize, ObjChunk>,
    imports: Vec<Rc<str>>,
    asserts: Vec<ObjAssert>,
}

impl<'a> Assembler<'a> {
    fn new(cache: &'a mut dyn SrcCache, src_path: Rc<str>) -> Assembler<'a> {
        let (arch_tree, macros) = builtins::make_builtins();
        Assembler {
            cache,
            src_path,
            arch_tree,
            macros,
            next_anonymous_scope_number: 0,
            env: AsmTypeEnv::new(),
            next_chunk_index: 0,
            chunks: BTreeMap::new(),
            imports: Vec::new(),
            asserts: Vec::new(),
        }
    }

    /// Scans over a module AST (without expanding macros) and collects
    /// existing labels and scopes into the environment.
    fn predeclare_module(&mut self, module: &AsmModuleAst) -> AsmResult<()> {
        self.predeclare_statements(&module.statements)
    }

    /// Scans over a list of statement ASTs (without expanding macros) and
    /// collects existing labels and scopes into the environment.
    fn predeclare_statements(
        &mut self,
        statements: &[AsmStmtAst],
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        for statement in statements {
            errs.also(self.predeclare_statement(statement));
        }
        errs.result()
    }

    /// Scans over a statement AST (without expanding macros) and collects
    /// existing labels and scopes into the environment.
    fn predeclare_statement(
        &mut self,
        statement: &AsmStmtAst,
    ) -> AsmResult<()> {
        match statement {
            AsmStmtAst::AnonymousScope(_) => Ok(()),
            AsmStmtAst::Assert(_) => Ok(()),
            AsmStmtAst::Binary(_) => Ok(()),
            AsmStmtAst::DefMacro(_) => Ok(()),
            AsmStmtAst::Import(id) => self.predeclare_import(id),
            AsmStmtAst::IntData(_) => Ok(()),
            AsmStmtAst::Invoke(_) => Ok(()),
            AsmStmtAst::Label(id) => self.predeclare_label(id),
            AsmStmtAst::NamedScope(id, body) => {
                self.predeclare_named_scope(id, body)
            }
            AsmStmtAst::Reserve(_) => Ok(()),
            AsmStmtAst::Section(section) => self.predeclare_section(section),
            AsmStmtAst::Utf8Data(_) => Ok(()),
        }
    }

    fn predeclare_import(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        // TODO: error if name is reserved in current arch
        self.env.declare_import(id_ast)
    }

    fn predeclare_label(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        // TODO: error if name is reserved in current arch
        self.env.declare_label(id_ast)
    }

    fn predeclare_named_scope(
        &mut self,
        id: &IdentifierAst,
        body: &[AsmStmtAst],
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        errs.also(self.predeclare_label(id));
        self.env.begin_scope(&id.name);
        errs.also(self.predeclare_statements(body));
        self.env.end_scope();
        errs.result()
    }

    fn predeclare_section(
        &mut self,
        section_ast: &AsmSectionAst,
    ) -> AsmResult<()> {
        self.predeclare_statements(&section_ast.body)
    }

    /// Consumes a module AST, expanding macros and directives into chunk data.
    fn expand_module(&mut self, module: AsmModuleAst) -> AsmResult<()> {
        self.expand_statements(module.statements)
    }

    /// Consumes a list of statement ASTs, expanding macros and directives into
    /// chunk data.
    fn expand_statements(
        &mut self,
        statements: Vec<AsmStmtAst>,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        for statement in statements {
            errs.also(self.expand_statement(statement));
        }
        errs.result()
    }

    /// Consumes a statement AST, expanding macros and directives into chunk
    /// data.
    fn expand_statement(&mut self, statement: AsmStmtAst) -> AsmResult<()> {
        match statement {
            AsmStmtAst::AnonymousScope(body) => {
                self.expand_anonymous_scope(body)
            }
            AsmStmtAst::Assert(assert) => self.expand_assert(assert),
            AsmStmtAst::Binary(data) => self.expand_binary_data(data),
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

    fn expand_anonymous_scope(
        &mut self,
        body: Vec<AsmStmtAst>,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let name = format!("${:x}", self.next_anonymous_scope_number);
        self.next_anonymous_scope_number += 1;
        self.env.begin_scope(&Rc::<str>::from(name));
        errs.also(self.predeclare_statements(&body));
        errs.also(self.expand_statements(body));
        self.env.end_scope();
        errs.result()
    }

    fn expand_assert(&mut self, assert_ast: AsmAssertAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let opt_message_expr: Option<ObjExpr> =
            assert_ast.message.and_then(|expr_ast| {
                errs.ok(self.typecheck_dir_expr_as(
                    (".ASSERT", "message"),
                    &expr_ast,
                    ExprType::String,
                ))
            });
        let condition_span = assert_ast.condition.span;
        let condition_expr: ObjExpr = 'condition: {
            match errs.ok(self.typecheck_dir_expr_as(
                (".ASSERT", "condition"),
                &assert_ast.condition,
                ExprType::Boolean,
            )) {
                None => return errs.result(),
                Some(condition_expr) => match condition_expr.static_value() {
                    None => condition_expr,
                    Some(ExprValue::Boolean(true)) => return errs.result(),
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
                        errs.push(AsmError::AssertionStaticallyFailed {
                            condition_span,
                            additional_message,
                        });
                        return errs.result();
                    }
                },
            }
        };
        self.asserts.push(ObjAssert {
            condition: condition_expr,
            message: opt_message_expr,
        });
        errs.result()
    }

    fn expand_import(&mut self, id_ast: IdentifierAst) -> AsmResult<()> {
        self.imports.push(id_ast.name);
        Ok(())
    }

    fn expand_macro_definition(
        &mut self,
        def_macro_ast: AsmDefMacroAst,
    ) -> AsmResult<()> {
        let arch = self.env.current_arch();
        let reserved = self.arch_tree.reserved_names(arch);
        self.macros.define(arch, reserved, def_macro_ast)
    }

    fn expand_macro_invocation(
        &mut self,
        invoke_ast: AsmInvokeAst,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let arches = self.arch_tree.get_all_ancestors(self.env.current_arch());
        if let Some(statements) =
            errs.ok(self.macros.expand(&arches, invoke_ast))
        {
            errs.also(self.predeclare_statements(&statements));
            errs.also(self.expand_statements(statements));
        }
        errs.result()
    }

    fn expand_label(&mut self, id_ast: IdentifierAst) -> AsmResult<()> {
        let full_name = self.env.look_up_symbol(&id_ast.name).unwrap();
        let Some(chunk_env) = self.env.current_chunk() else {
            return Err(Errs::one(AsmError::DirectiveNotInSection {
                directive: "label",
                span: id_ast.span,
            }));
        };
        chunk_env.add_symbol(ObjSymbol {
            name: full_name,
            exported: true, // TODO
            offset: Offset::try_from(chunk_env.total_size()).unwrap(), // TODO
        });
        Ok(())
    }

    fn expand_named_scope(
        &mut self,
        id: IdentifierAst,
        body: Vec<AsmStmtAst>,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let scope_name = id.name.clone();
        errs.also(self.expand_label(id));
        self.env.begin_scope(&scope_name);
        errs.also(self.expand_statements(body));
        self.env.end_scope();
        errs.result()
    }

    fn expand_reserve(&mut self, reserve_ast: AsmReserveAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: ".RESERVE",
                span: reserve_ast.directive_span,
            });
        }
        let count: u64 = if let Some(expr_ast) = &reserve_ast.count {
            let Some(value) = errs.ok(self.typecheck_static_dir_expr_as(
                (".RESERVE", "count"),
                expr_ast,
                ExprType::Integer,
            )) else {
                return errs.result();
            };
            match value.unwrap_int_ref().to_u64() {
                Some(count) => count,
                None => {
                    errs.push(AsmError::DirectiveExprOutOfRange {
                        directive: ".RESERVE",
                        component: "count",
                        expr_span: expr_ast.span,
                        expr_value: value.unwrap_int_ref().clone(),
                        valid_range: bigint_range(u64::MIN, u64::MAX),
                    });
                    return errs.result();
                }
            }
        } else {
            1
        };
        let type_size = self.data_type_size(reserve_ast.data_type);
        if let Some(chunk_env) = self.env.current_chunk() {
            // TODO: error on overflow
            chunk_env.add_padding((type_size * count) as usize);
        }
        errs.result()
    }

    fn expand_section(&mut self, section_ast: AsmSectionAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let name: Option<Rc<str>> = errs
            .ok(self.typecheck_static_dir_expr_as(
                (".SECTION", "name"),
                &section_ast.name,
                ExprType::String,
            ))
            .map(|value| value.unwrap_str_ref().clone());

        let mut align: Option<Align> = None;
        let mut arch: Option<Rc<str>> = None;
        let mut fill: Option<u8> = None;
        let mut start: Option<Addr> = None;
        let mut within: Option<Align> = None;
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for (id_ast, expr_ast) in section_ast.attrs {
            errs.also(self.chunk_declare_attr(&mut prev_attrs, &id_ast));
            match &*id_ast.name {
                "align" => {
                    align = Some(
                        errs.ok_or_default(self.chunk_align_attr(expr_ast)),
                    )
                }
                "arch" => {
                    arch =
                        Some(errs.ok_or_else(
                            self.chunk_arch_attr(expr_ast),
                            || self.env.current_arch().clone(),
                        ))
                }
                "fill" => {
                    fill = Some(
                        errs.ok_or_default(self.chunk_fill_attr(expr_ast)),
                    )
                }
                "start" => {
                    start = Some(
                        errs.ok_or_default(self.chunk_start_attr(expr_ast)),
                    )
                }
                "within" => {
                    within =
                        Some(errs.ok_or(
                            self.chunk_within_attr(expr_ast),
                            Align::MAX,
                        ))
                }
                _ => {
                    errs.push(AsmError::InvalidAttrName {
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
        errs.also(self.expand_statements(section_ast.body));
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
        errs.result()
    }

    fn expand_binary_data(&mut self, data_ast: AsmBinaryAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: ".BINARY",
                span: data_ast.directive_span,
            });
        }
        match errs.ok(self.typecheck_expression(&data_ast.path_expr)) {
            Some((expr, ExprType::String)) => {
                let Some(path_value) = expr.static_value() else {
                    errs.push(AsmError::DirectiveExprNotStatic {
                        directive: ".BINARY",
                        component: "path",
                        expr_span: data_ast.path_expr.span,
                    });
                    return errs.result();
                };
                let path = self.joined_path(path_value.unwrap_str_ref());
                let Some(chunk_env) = self.env.current_chunk() else {
                    return errs.result();
                };
                let chunk_data = chunk_env.data_mut();
                match self.cache.fetch_and_write_data(&path, chunk_data) {
                    Ok(()) => {}
                    Err(error) => {
                        errs.push(AsmError::SrcCacheError {
                            path,
                            path_span: data_ast.path_expr.span,
                            error,
                        });
                    }
                }
            }
            Some((_, expr_type)) => {
                errs.push(AsmError::DirectiveExprTypeError {
                    directive: ".BINARY",
                    component: "path",
                    expr_span: data_ast.path_expr.span,
                    expr_type,
                    valid_types: vec![ExprType::String],
                });
            }
            None => {}
        }
        errs.result()
    }

    /// Given a relative path appearing in this assembly source file (e.g. in a
    /// `.BINARY` directive), join that path to this source file's parent
    /// directory.
    fn joined_path(&self, relative_path: &Rc<str>) -> Rc<str> {
        match AsRef::<Path>::as_ref(&*self.src_path).parent() {
            None => relative_path.clone(),
            Some(base_path) => {
                let joined = base_path.join(&**relative_path);
                // We can safely `unwrap()` the `to_str()` here because
                // `joined` was made from `Path`s that came from `str`s.
                Rc::<str>::from(joined.to_str().unwrap())
            }
        }
    }

    fn expand_utf8_data(&mut self, data_ast: AsmUtf8DataAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: ".UTF8",
                span: data_ast.directive_span,
            });
        }
        for expr_ast in data_ast.expressions {
            match errs.ok(self.typecheck_expression(&expr_ast)) {
                Some((expr, ExprType::Integer)) => {
                    let Some(value) = expr.static_value() else {
                        errs.push(AsmError::DirectiveExprNotStatic {
                            directive: ".UTF8",
                            component: "value",
                            expr_span: expr_ast.span,
                        });
                        return errs.result();
                    };
                    let bigint = value.unwrap_int_ref();
                    let Some(chr) = bigint.to_u32().and_then(char::from_u32)
                    else {
                        errs.push(AsmError::InvalidUnicodeScalarValue {
                            expr_span: expr_ast.span,
                            expr_value: bigint.clone(),
                        });
                        return errs.result();
                    };
                    if let Some(chunk_env) = self.env.current_chunk() {
                        chunk_env
                            .data_mut()
                            .extend_from_slice(chr.to_string().as_bytes());
                    }
                }
                Some((expr, ExprType::String)) => {
                    let Some(value) = expr.static_value() else {
                        errs.push(AsmError::DirectiveExprNotStatic {
                            directive: ".UTF8",
                            component: "value",
                            expr_span: expr_ast.span,
                        });
                        return errs.result();
                    };
                    if let Some(chunk_env) = self.env.current_chunk() {
                        chunk_env.data_mut().extend_from_slice(
                            value.unwrap_str_ref().as_bytes(),
                        );
                    }
                }
                Some((_, ty)) => {
                    errs.push(AsmError::DirectiveExprTypeError {
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
        errs.result()
    }

    fn expand_int_data(&mut self, int_data: AsmIntDataAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let directive = int_data.int_type.directive();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive,
                span: int_data.directive_span,
            });
        }
        let Some(int_type) = self.int_patch_type(int_data.int_type) else {
            errs.push(AsmError::ArchHasNoEndianness {
                directive,
                span: int_data.directive_span,
                arch: self.env.current_arch().clone(),
            });
            return errs.result();
        };
        for expr_ast in int_data.expressions {
            let static_value: i64 = match errs
                .ok(self.typecheck_expression(&expr_ast))
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
                                errs.push(AsmError::DirectiveExprOutOfRange {
                                    directive,
                                    component: "value",
                                    expr_span: expr_ast.span,
                                    expr_value: bigint.clone(),
                                    valid_range: RangeInclusive {
                                        start: BigInt::from(range.start),
                                        last: BigInt::from(range.last),
                                    },
                                });
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
                    errs.push(AsmError::DirectiveExprTypeError {
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
        errs.result()
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

    fn chunk_align_attr(&mut self, expr_ast: ExprAst) -> AsmResult<Align> {
        self.chunk_static_align_attr("align", expr_ast)
    }

    fn chunk_arch_attr(&mut self, expr_ast: ExprAst) -> AsmResult<Rc<str>> {
        let expr_span = expr_ast.span;
        let arch = self.chunk_static_str_attr("arch", expr_ast)?;
        if self.arch_tree.contains_arch(&arch) {
            Ok(arch)
        } else {
            Err(Errs::one(AsmError::UnknownArch {
                arch: arch.clone(),
                span: expr_span,
            }))
        }
    }

    fn chunk_fill_attr(&mut self, expr_ast: ExprAst) -> AsmResult<u8> {
        let expr_span = expr_ast.span;
        let bigint = self.chunk_static_int_attr("fill", expr_ast)?;
        u8::try_from(&bigint).map_err(|_| {
            Errs::one(AsmError::DirectiveExprOutOfRange {
                directive: ".SECTION",
                component: "fill",
                expr_span,
                expr_value: bigint,
                valid_range: bigint_range(u8::MIN, u8::MAX),
            })
        })
    }

    fn chunk_start_attr(&mut self, expr_ast: ExprAst) -> AsmResult<Addr> {
        let expr_span = expr_ast.span;
        let bigint = self.chunk_static_int_attr("start", expr_ast)?;
        Addr::try_from(&bigint).map_err(|_| {
            Errs::one(AsmError::DirectiveExprOutOfRange {
                directive: ".SECTION",
                component: "start",
                expr_span,
                expr_value: bigint,
                valid_range: bigint_range(Addr::MIN, Addr::MAX),
            })
        })
    }

    fn chunk_within_attr(&mut self, expr_ast: ExprAst) -> AsmResult<Align> {
        self.chunk_static_align_attr("within", expr_ast)
    }

    fn chunk_static_align_attr(
        &mut self,
        attr_name: &'static str,
        expr_ast: ExprAst,
    ) -> AsmResult<Align> {
        let expr_span = expr_ast.span;
        let bigint = self.chunk_static_int_attr(attr_name, expr_ast)?;
        Align::try_from(&bigint).map_err(|error| {
            Errs::one(AsmError::InvalidAlignmentValue {
                directive: ".SECTION",
                attr_name,
                error,
                expr_span,
                expr_value: bigint,
            })
        })
    }

    fn chunk_static_int_attr(
        &mut self,
        attr_name: &'static str,
        expr_ast: ExprAst,
    ) -> AsmResult<BigInt> {
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
    ) -> AsmResult<Rc<str>> {
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
    ) -> AsmResult<()> {
        if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
            Err(Errs::one(AsmError::DuplicateAttrName {
                directive: ".SECTION",
                attr_name: id_ast.name.clone(),
                attr_span: id_ast.span,
                prev_span,
            }))
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
            Ok(())
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
    ) -> AsmResult<ExprValue> {
        let expr = self.typecheck_dir_expr_as(
            (directive, component),
            expr_ast,
            required_type,
        )?;
        match expr.static_value() {
            Some(value) => Ok(value.clone()),
            None => Err(Errs::one(AsmError::DirectiveExprNotStatic {
                directive,
                component,
                expr_span: expr_ast.span,
            })),
        }
    }

    fn typecheck_dir_expr_as(
        &mut self,
        (directive, component): (&'static str, &'static str),
        expr_ast: &ExprAst,
        required_type: ExprType,
    ) -> AsmResult<ObjExpr> {
        let (expr, expr_type) = self.typecheck_expression(expr_ast)?;
        if expr_type == required_type {
            Ok(expr)
        } else {
            Err(Errs::one(AsmError::DirectiveExprTypeError {
                directive,
                component,
                expr_span: expr_ast.span,
                expr_type,
                valid_types: vec![required_type],
            }))
        }
    }

    fn typecheck_expression(
        &mut self,
        expr_ast: &ExprAst,
    ) -> AsmResult<(ObjExpr, ExprType)> {
        // TODO: error if contains a name that is reserved in current arch
        self.env.typecheck_expression(expr_ast)
    }

    fn finish(self) -> ObjFile {
        ObjFile {
            chunks: self.chunks.into_values().collect(),
            imports: self.imports,
            asserts: self.asserts,
        }
    }
}

fn bigint_range<T: Into<BigInt>>(start: T, last: T) -> RangeInclusive<BigInt> {
    RangeInclusive { start: start.into(), last: last.into() }
}

//===========================================================================//
