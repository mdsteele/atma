use super::env::{AsmDeclValue, AsmTypeEnv};
use super::error::{AsmError, AsmResult};
use super::int_data::{assemble_int_data, int_patch_type};
use super::macros::MacroTable;
use super::predef::make_predefined_arch_macros;
use super::repeat::typecheck_iterator;
use crate::addr::{Addr, Align, Offset, Size};
use crate::error::{Errs, SrcCache, SrcSpan};
use crate::expr::{
    ExprEvalError, ExprFunc, ExprFuncEvalError, ExprLabel,
    ExprNotStaticReason, ExprStatic, ExprType, ExprTypeError, ExprValue,
};
use crate::obj::{
    ObjChunk, ObjExpr, ObjExprOp, ObjFile, ObjImport, ObjPatchData,
    ObjPatchRelType, ObjSrcContext, ObjSrcLoc, ObjSrcParent, ObjSymbol,
};
use crate::parse::{
    AsmAssertAst, AsmBinaryAst, AsmCondAst, AsmDataTypeAst, AsmDeclareAst,
    AsmDefMacroAst, AsmEnumAst, AsmIntDataAst, AsmInvokeAst, AsmLabelAst,
    AsmModuleAst, AsmRelAddrAst, AsmRelTypeAst, AsmRepeatAst, AsmReserveAst,
    AsmScopeAst, AsmSectionAst, AsmSetAst, AsmStmtAst, AsmStructAst,
    AsmUseAst, AsmUtf8DataAst, DeclarationKind, ExprAst, IdentifierAst,
};
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
    let mut errs = Errs::<AsmError>::new();
    let mut assembler = Assembler::new(cache, src_path);
    if let Some(module) = errs.ok(assembler.env.parse_source(source_code)) {
        errs.also(assembler.predeclare_module(&module));
        errs.also(assembler.expand_module(module));
    }
    errs.result()?;
    Ok(assembler.finish())
}

//===========================================================================//

struct Assembler<'a> {
    cache: &'a mut dyn SrcCache,
    macros: MacroTable,
    env: AsmTypeEnv,
    next_chunk_index: usize,
    chunks: BTreeMap<usize, ObjChunk>,
    imports: Vec<ObjImport>,
    variables: Vec<ObjExpr>,
}

impl<'a> Assembler<'a> {
    fn new(cache: &'a mut dyn SrcCache, root_path: Rc<str>) -> Assembler<'a> {
        let (arch_tree, macros) = make_predefined_arch_macros();
        Assembler {
            cache,
            macros,
            env: AsmTypeEnv::new(root_path, arch_tree),
            next_chunk_index: 0,
            chunks: BTreeMap::new(),
            imports: Vec::new(),
            variables: Vec::new(),
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
            AsmStmtAst::Assert(_) => Ok(()),
            AsmStmtAst::Binary(_) => Ok(()),
            AsmStmtAst::Cond(_) => Ok(()),
            AsmStmtAst::Declare(_) => Ok(()),
            AsmStmtAst::DefMacro(_) => Ok(()),
            AsmStmtAst::Enum(_) => Ok(()),
            AsmStmtAst::Import(id) => self.predeclare_import(id),
            AsmStmtAst::IntData(_) => Ok(()),
            AsmStmtAst::Invoke(_) => Ok(()),
            AsmStmtAst::Label(label) => self.predeclare_label(label),
            AsmStmtAst::RelAddr(_) => Ok(()),
            AsmStmtAst::Repeat(_) => Ok(()),
            AsmStmtAst::Reserve(_) => Ok(()),
            AsmStmtAst::Scope(scope) => self.predeclare_scope(scope),
            AsmStmtAst::Section(section) => self.predeclare_section(section),
            AsmStmtAst::Set(_) => Ok(()),
            AsmStmtAst::Struct(_) => Ok(()),
            AsmStmtAst::Use(_) => Ok(()),
            AsmStmtAst::Utf8Data(_) => Ok(()),
        }
    }

    fn predeclare_import(&mut self, id_ast: &IdentifierAst) -> AsmResult<()> {
        self.env.declare_import(id_ast)
    }

    fn predeclare_label(&mut self, label_ast: &AsmLabelAst) -> AsmResult<()> {
        self.env.declare_label(label_ast)
    }

    fn predeclare_scope(&mut self, scope_ast: &AsmScopeAst) -> AsmResult<()> {
        if let Some(label_ast) = &scope_ast.label {
            let mut errs = Errs::<AsmError>::new();
            errs.also(self.predeclare_label(label_ast));
            self.env.begin_named_scope(label_ast.identifier.name.clone());
            errs.also(self.predeclare_statements(&scope_ast.body));
            self.env.end_scope();
            errs.result()
        } else {
            Ok(())
        }
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
            AsmStmtAst::Assert(ast) => self.expand_assert(ast),
            AsmStmtAst::Binary(ast) => self.expand_binary_data(ast),
            AsmStmtAst::Cond(ast) => self.expand_conditional(ast),
            AsmStmtAst::Declare(ast) => self.expand_declaration(ast),
            AsmStmtAst::DefMacro(ast) => self.expand_macro_definition(ast),
            AsmStmtAst::Enum(ast) => self.expand_enum(ast),
            AsmStmtAst::Import(id) => self.expand_import(id),
            AsmStmtAst::IntData(ast) => self.expand_int_data(ast),
            AsmStmtAst::Invoke(ast) => self.expand_macro_invocation(ast),
            AsmStmtAst::Label(ast) => self.expand_label(ast),
            AsmStmtAst::RelAddr(ast) => self.expand_rel_addr(ast),
            AsmStmtAst::Repeat(ast) => self.expand_repeat(ast),
            AsmStmtAst::Reserve(ast) => self.expand_reserve(ast),
            AsmStmtAst::Scope(ast) => self.expand_scope(ast),
            AsmStmtAst::Section(ast) => self.expand_section(ast),
            AsmStmtAst::Set(ast) => self.expand_assignment(ast),
            AsmStmtAst::Struct(ast) => self.expand_struct(ast),
            AsmStmtAst::Use(ast) => self.expand_use_file(ast),
            AsmStmtAst::Utf8Data(ast) => self.expand_utf8_data(ast),
        }
    }

    fn expand_assert(&mut self, assert_ast: AsmAssertAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let condition = errs.ok(self.typecheck_dir_expr_as(
            (".ASSERT", "condition"),
            assert_ast.condition,
            ExprType::Boolean,
        ));
        let (message_span, message_expr, message_static) =
            match assert_ast.message {
                None => {
                    let message_span = assert_ast.directive_span;
                    let message_value = ExprValue::from("Assertion failed");
                    let message_expr = ObjExpr::from(message_value.clone());
                    (message_span, message_expr, Ok(message_value))
                }
                Some(message_ast) => {
                    let message_span = message_ast.span;
                    match errs.ok(self.typecheck_dir_expr_as(
                        (".ASSERT", "message"),
                        message_ast,
                        ExprType::String,
                    )) {
                        None => {
                            let message_expr = ObjExpr::from(false);
                            let message_static =
                                Err(ExprNotStaticReason::TypeError);
                            (message_span, message_expr, message_static)
                        }
                        Some((message_expr, message_static)) => {
                            (message_span, message_expr, message_static)
                        }
                    }
                }
            };
        let failure_expr = {
            let mut message_ops = message_expr.ops;
            let mut expr = ObjExpr::from(ExprFunc::Error);
            expr.ops.append(&mut message_ops);
            expr.ops.push(ObjExprOp::Apply {
                context: self.env.current_src_context(),
                arg_span: message_span,
            });
            expr
        };
        match condition {
            None => {} // The assertion condition failed to typecheck.
            Some((_, Ok(ExprValue::Boolean(true)))) => {
                // The assertion condition statically succeeded.
            }
            Some((_, Ok(_))) => {
                // The assertion condition statically failed.
                match message_static {
                    Ok(message_value) => {
                        // Message is statically known, so we can report the
                        // failed assertion right now.
                        errs.push(AsmError::StaticEvalError {
                            context: self.env.current_src_context(),
                            error: ExprEvalError::FuncEvalError {
                                arg_span: message_span,
                                error: ExprFuncEvalError::ErrorMessage(
                                    message_value.unwrap_str(),
                                ),
                            },
                        });
                    }
                    Err(reason) => {
                        // Message is not statically known, so we have to wait
                        // until link time to report the failed assertion.
                        errs.also(
                            self.env.check_for_inevitable_eval_error(&reason),
                        );
                        self.variables.push(failure_expr);
                    }
                }
            }
            Some((mut condition_expr, Err(reason))) => {
                // The assertion condition can't be evaluated yet, so we have
                // to wait until link time to check it.
                errs.also(self.env.check_for_inevitable_eval_error(&reason));
                let mut failure_ops = failure_expr.ops;
                condition_expr.ops.push(ObjExprOp::SkipUnless(2));
                condition_expr
                    .ops
                    .push(ObjExprOp::Push(ExprValue::from(true)));
                condition_expr.ops.push(ObjExprOp::Skip(failure_ops.len()));
                condition_expr.ops.append(&mut failure_ops);
                self.variables.push(condition_expr);
            }
        }
        errs.result()
    }

    fn expand_assignment(&mut self, set_ast: AsmSetAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let expr_span = set_ast.expression.span;
        let (expr, expr_type, expr_static) =
            errs.with(self.env.typecheck_expression(set_ast.expression));
        let lvalue_span = set_ast.id.span;
        let lvalue_name = set_ast.id.name.clone();
        if let Some(lvalue_type) =
            errs.ok(self.env.typecheck_lvalue(set_ast.id))
        {
            if expr_type.is_subtype_of(&lvalue_type) {
                let decl_value = match expr_static {
                    Ok(static_value) => AsmDeclValue::Static(static_value),
                    Err(reason) => {
                        errs.also(
                            self.env.check_for_inevitable_eval_error(&reason),
                        );
                        let variable_index = self.variables.len();
                        self.variables.push(expr);
                        AsmDeclValue::Variable(variable_index, reason)
                    }
                };
                self.env.reassign_variable(lvalue_name, decl_value);
            } else {
                errs.push(AsmError::VariableTypeError {
                    expr_loc: self.env.make_loc(expr_span),
                    expr_type,
                    lvalue_loc: self.env.make_loc(lvalue_span),
                    lvalue_type,
                });
            }
        }
        errs.result()
    }

    fn expand_conditional(&mut self, cond_ast: AsmCondAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let mut selected_body_ast: Option<Vec<AsmStmtAst>> = None;
        let mut directive: &'static str = ".IF";
        for (pred_ast, body_ast) in
            std::iter::once(cond_ast.if_block).chain(cond_ast.elif_blocks)
        {
            let pred_span = pred_ast.span;
            let mut ambiguous_predicate: bool = true;
            match errs.with(self.env.typecheck_expression(pred_ast)) {
                (_, ExprType::Boolean, Ok(static_pred)) => {
                    ambiguous_predicate = false;
                    if selected_body_ast.is_none() && static_pred.unwrap_bool()
                    {
                        selected_body_ast = Some(body_ast);
                    }
                }
                (_, ExprType::Boolean, Err(reason)) => {
                    // If a previous block was already selected, then this
                    // predicate (and any future predicates) are not required
                    // to be static, in case the previously-selected predicate
                    // was guarding e.g. an evaluation error in this one.
                    if selected_body_ast.is_none() {
                        errs.push(AsmError::DirectiveExprNotStatic {
                            directive,
                            component: "predicate",
                            expr_loc: self.env.make_loc(pred_span),
                            reason,
                        });
                    }
                }
                (_, ExprType::Bottom | ExprType::Undefined, _) => {}
                (_, pred_type, _) => {
                    errs.push(AsmError::ExprTypeError {
                        context: self.env.current_src_context(),
                        error: ExprTypeError::CannotUseTypeAsPredicate {
                            expr_span: pred_span,
                            expr_type: pred_type,
                        },
                    });
                }
            }
            // If no block has been selected yet, and this predicate had a type
            // error or wasn't static, we can't know whether or not this block
            // should have been selected.  To avoid reporting spurious errors
            // in later blocks, select an imaginary empty block.
            if selected_body_ast.is_none() && ambiguous_predicate {
                selected_body_ast = Some(vec![]);
            }
            directive = ".ELIF";
        }
        if let Some(body_ast) = selected_body_ast.or(cond_ast.else_block)
            && !body_ast.is_empty()
        {
            self.env.begin_anonymous_scope();
            errs.also(self.predeclare_statements(&body_ast));
            errs.also(self.expand_statements(body_ast));
            self.env.end_scope();
        }
        errs.result()
    }

    fn expand_declaration(
        &mut self,
        declare_ast: AsmDeclareAst,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let (expr, expr_type, expr_static) =
            errs.with(self.env.typecheck_expression(declare_ast.expression));
        let decl_value = match expr_static {
            Ok(static_value) => AsmDeclValue::Static(static_value),
            Err(reason) => {
                errs.also(self.env.check_for_inevitable_eval_error(&reason));
                let variable_index = self.variables.len();
                self.variables.push(expr);
                AsmDeclValue::Variable(variable_index, reason)
            }
        };
        errs.also(self.env.declare_variable(
            declare_ast.kind,
            &declare_ast.id,
            expr_type,
            decl_value,
        ));
        errs.result()
    }

    fn expand_enum(&mut self, enum_ast: AsmEnumAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        errs.also(self.env.declare_fixed_value(
            &enum_ast.id,
            ExprType::Entity(Rc::from("enum")),
            AsmDeclValue::Static(ExprValue::Entity(enum_ast.id.name.clone())),
        ));
        self.env.begin_named_scope(enum_ast.id.name);
        let mut field_values = Vec::<ExprValue>::new();
        let mut field_value = BigInt::from(-1);
        for field_ast in enum_ast.fields {
            if let Some(expr_ast) = field_ast.expression {
                field_value = errs
                    .ok(self.typecheck_static_dir_expr_as(
                        (".ENUM", "value"),
                        expr_ast,
                        ExprType::Integer,
                    ))
                    .map(|value| value.unwrap_int())
                    .unwrap_or_default()
            } else {
                field_value += 1;
            };
            errs.also(self.env.declare_fixed_value(
                &field_ast.id,
                ExprType::Integer,
                AsmDeclValue::Static(ExprValue::Integer(field_value.clone())),
            ));
            field_values.push(ExprValue::Integer(field_value.clone()));
        }
        errs.also(
            self.env.declare_enum_values(enum_ast.id.span, field_values),
        );
        self.env.end_scope();
        errs.result()
    }

    fn expand_import(&mut self, id_ast: IdentifierAst) -> AsmResult<()> {
        self.imports.push(ObjImport {
            full_name: id_ast.name,
            loc: self.env.make_loc(id_ast.span),
        });
        Ok(())
    }

    fn expand_macro_definition(
        &mut self,
        def_macro_ast: AsmDefMacroAst,
    ) -> AsmResult<()> {
        let arch = self.env.current_arch();
        let reserved = self.env.arch_tree().reserved_names(arch);
        self.macros.define(
            self.env.current_src_context(),
            arch,
            reserved,
            def_macro_ast,
        )
    }

    fn expand_macro_invocation(
        &mut self,
        invoke_ast: AsmInvokeAst,
    ) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let context = self.env.current_src_context();
        let arches =
            self.env.arch_tree().get_all_ancestors(self.env.current_arch());
        if let Some(statements) =
            errs.ok(self.macros.expand(&context, &arches, invoke_ast))
        {
            errs.also(self.predeclare_statements(&statements));
            errs.also(self.expand_statements(statements));
        }
        errs.result()
    }

    fn expand_label(&mut self, label_ast: AsmLabelAst) -> AsmResult<()> {
        let full_name =
            self.env.current_scope().prefixed(&label_ast.identifier.name);
        let loc = self.env.make_loc(label_ast.identifier.span);
        let Some(chunk_env) = self.env.current_chunk_mut() else {
            return Err(Errs::one(AsmError::DirectiveNotInSection {
                directive: "label",
                loc,
            }));
        };
        chunk_env.add_symbol(ObjSymbol {
            name: full_name,
            loc,
            exported: label_ast.exported,
            offset: Offset::try_from(chunk_env.total_size()).unwrap(), // TODO
        });
        Ok(())
    }

    fn expand_scope(&mut self, scope_ast: AsmScopeAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if let Some(label_ast) = scope_ast.label {
            let name = label_ast.identifier.name.clone();
            errs.also(self.expand_label(label_ast));
            self.env.begin_named_scope(name);
        } else {
            self.env.begin_anonymous_scope();
            errs.also(self.predeclare_statements(&scope_ast.body));
        }
        errs.also(self.expand_statements(scope_ast.body));
        self.env.end_scope();
        errs.result()
    }

    fn expand_repeat(&mut self, repeat_ast: AsmRepeatAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let expr_loc = self.env.make_loc(repeat_ast.expression.span);
        let (_, expr_type, expr_static) =
            errs.with(self.env.typecheck_expression(repeat_ast.expression));
        let (item_type, iterator) =
            errs.with(typecheck_iterator(expr_loc, expr_type, expr_static));
        for value in iterator {
            self.env.begin_anonymous_scope();
            if let Some(id) = &repeat_ast.id {
                errs.also(self.env.declare_variable(
                    DeclarationKind::Let,
                    id,
                    item_type.clone(),
                    AsmDeclValue::Static(value),
                ));
            }
            errs.also(self.predeclare_statements(&repeat_ast.body));
            errs.also(self.expand_statements(repeat_ast.body.clone()));
            self.env.end_scope();
        }
        errs.result()
    }

    fn expand_reserve(&mut self, reserve_ast: AsmReserveAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: ".RESERVE",
                loc: self.env.make_loc(reserve_ast.directive_span),
            });
        }
        let type_size = errs
            .ok(self.env.data_type_size(reserve_ast.data_type))
            .unwrap_or_default();
        let Some(count) = errs.ok(self.typecheck_data_type_count(
            ".RESERVE",
            "count",
            reserve_ast.count,
        )) else {
            return errs.result();
        };
        if let Some(chunk_env) = self.env.current_chunk_mut() {
            // TODO: handle overflow
            errs.also(chunk_env.append_padding(
                usize::try_from(type_size).unwrap() * (count as usize),
            ));
        }
        errs.result()
    }

    fn expand_section(&mut self, section_ast: AsmSectionAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let section_name_loc = self.env.make_loc(section_ast.name.span);
        let name: Option<Rc<str>> = errs
            .ok(self.typecheck_static_dir_expr_as(
                (".SECTION", "name"),
                section_ast.name,
                ExprType::String,
            ))
            .map(|value| value.unwrap_str());

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
                        attr_loc: self.env.make_loc(id_ast.span),
                    });
                }
            }
        }

        let chunk_index = self.next_chunk_index;
        self.next_chunk_index += 1;
        self.env.begin_chunk(chunk_index, start, fill);
        if let Some(arch) = arch {
            self.env.set_current_arch(arch);
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
                section_name_loc,
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

    fn expand_struct(&mut self, struct_ast: AsmStructAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        errs.also(self.env.verify_not_builtin_or_reserved(&struct_ast.id));
        let mut prev_fields = HashMap::<Rc<str>, SrcSpan>::new();
        let mut fields: Vec<(IdentifierAst, Offset)> =
            Vec::with_capacity(struct_ast.fields.len());
        let mut size = Size::ZERO;
        for field_ast in struct_ast.fields {
            errs.also(self.env.verify_not_builtin_or_reserved(&field_ast.id));
            if let Some(prev_span) = prev_fields
                .insert(field_ast.id.name.clone(), field_ast.id.span)
            {
                errs.push(AsmError::DuplicateStructField {
                    struct_name: struct_ast.id.name.clone(),
                    field_name: field_ast.id.name.clone(),
                    field_loc: self.env.make_loc(field_ast.id.span),
                    prev_loc: self.env.make_loc(prev_span),
                });
            } else {
                let field_offset = match Offset::try_from(u128::from(size)) {
                    Ok(offset) => offset,
                    Err(()) => {
                        eprintln!("TODO: report overflow error");
                        Offset::MAX
                    }
                };
                fields.push((field_ast.id, field_offset));
            }
            let field_size = errs
                .ok(self.typecheck_data_array_size(
                    ".STRUCT field",
                    field_ast.data_type,
                    field_ast.count,
                ))
                .unwrap_or_default();
            size = size.checked_add(field_size).unwrap_or_else(|| {
                eprintln!("TODO: report overflow error");
                Size::MAX
            });
        }
        errs.also(self.env.declare_struct(struct_ast.id, fields, size));
        errs.result()
    }

    fn expand_use_file(&mut self, use_ast: AsmUseAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if !self.env.is_at_top_level() {
            errs.push(AsmError::DirectiveNotAtTopLevel {
                directive: ".USE",
                loc: self.env.make_loc(use_ast.directive_span),
            });
        }
        let path_span = use_ast.path.span;
        if let Some(path) =
            errs.ok(self.typecheck_static_path_expr(".USE", use_ast.path))
        {
            // TODO: skip if we've already used this path
            match self.cache.fetch_or_get_cached_utf8(&path) {
                Ok(source_code) => {
                    let context = Rc::new(ObjSrcContext {
                        path,
                        parent: ObjSrcParent::Use(ObjSrcLoc {
                            span: path_span,
                            context: self.env.current_src_context(),
                        }),
                    });
                    self.env.push_src_context(context);
                    if let Some(module) =
                        errs.ok(self.env.parse_source(source_code))
                        && self.env.is_at_top_level()
                    {
                        errs.also(self.predeclare_module(&module));
                        errs.also(self.expand_module(module));
                    }
                    self.env.pop_src_context();
                }
                Err(error) => {
                    errs.push(AsmError::SrcCacheError {
                        path,
                        path_loc: self.env.make_loc(path_span),
                        error,
                    });
                }
            }
        }
        errs.result()
    }

    fn expand_binary_data(&mut self, data_ast: AsmBinaryAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: ".BINARY",
                loc: self.env.make_loc(data_ast.directive_span),
            });
        }
        let path_span = data_ast.path.span;
        if let Some(path) =
            errs.ok(self.typecheck_static_path_expr(".BINARY", data_ast.path))
            && let Some(chunk_env) = self.env.current_chunk_mut()
        {
            let chunk_data = chunk_env.data_mut();
            match self.cache.fetch_and_write_data(&path, chunk_data) {
                Ok(()) => {}
                Err(error) => {
                    errs.push(AsmError::SrcCacheError {
                        path,
                        path_loc: self.env.make_loc(path_span),
                        error,
                    });
                }
            }
        }
        errs.result()
    }

    fn expand_utf8_data(&mut self, data_ast: AsmUtf8DataAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: ".UTF8",
                loc: self.env.make_loc(data_ast.directive_span),
            });
        }
        for expr_ast in data_ast.expressions {
            errs.also(self.expand_utf8_data_expr(expr_ast));
        }
        errs.result()
    }

    fn expand_utf8_data_expr(&mut self, expr_ast: ExprAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let expr_span = expr_ast.span;
        match errs.with(self.env.typecheck_expression(expr_ast)) {
            (_, ExprType::Undefined, _) => {}
            (_, ExprType::Integer, Ok(value)) => {
                let bigint = value.unwrap_int_ref();
                let Some(chr) = bigint.to_u32().and_then(char::from_u32)
                else {
                    errs.push(AsmError::InvalidUnicodeScalarValue {
                        expr_loc: self.env.make_loc(expr_span),
                        expr_value: bigint.clone(),
                    });
                    return errs.result();
                };
                if let Some(chunk_env) = self.env.current_chunk_mut() {
                    chunk_env
                        .data_mut()
                        .extend_from_slice(chr.to_string().as_bytes());
                }
            }
            (_, ExprType::String, Ok(value)) => {
                if let Some(chunk_env) = self.env.current_chunk_mut() {
                    chunk_env
                        .data_mut()
                        .extend_from_slice(value.unwrap_str_ref().as_bytes());
                }
            }
            (
                _,
                ExprType::Bottom | ExprType::Integer | ExprType::String,
                Err(reason),
            ) => {
                errs.push(AsmError::DirectiveExprNotStatic {
                    directive: ".UTF8",
                    component: "value",
                    expr_loc: self.env.make_loc(expr_span),
                    reason,
                });
            }
            (_, expr_type, _) => {
                errs.push(AsmError::DirectiveExprTypeError {
                    directive: ".UTF8",
                    component: "value",
                    expr_loc: self.env.make_loc(expr_span),
                    expr_type,
                    valid_types: vec![ExprType::String, ExprType::Integer],
                });
            }
        }
        errs.result()
    }

    fn expand_int_data(&mut self, int_data: AsmIntDataAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive: int_data.int_type.directive(),
                loc: self.env.make_loc(int_data.directive_span),
            });
        }
        if let Some(int_type) = errs.ok(int_patch_type(&self.env, &int_data)) {
            for expr_ast in int_data.expressions {
                errs.also(assemble_int_data(
                    &mut self.env,
                    int_data.int_type.directive(),
                    int_type,
                    expr_ast,
                ));
            }
        }
        errs.result()
    }

    fn expand_rel_addr(&mut self, rel_addr: AsmRelAddrAst) -> AsmResult<()> {
        let mut errs = Errs::<AsmError>::new();
        let directive = rel_addr.rel_type.directive();
        if self.env.current_chunk().is_none() {
            errs.push(AsmError::DirectiveNotInSection {
                directive,
                loc: self.env.make_loc(rel_addr.directive_span),
            });
        }
        let rel_type = self.rel_patch_type(rel_addr.rel_type);
        let (dest_expr, dest_static) = errs.with(self.typecheck_rel_expr(
            directive,
            "destination address",
            rel_addr.dest_expr,
        ));
        let (base_expr, base_static) = errs.with(self.typecheck_rel_expr(
            directive,
            "base address",
            rel_addr.base_expr,
        ));
        // TODO: Error if destination is statically out of range.
        if let (Some(dest_label), Some(base_label)) =
            (dest_static, base_static)
            && let Ok(delta_bigint) = dest_label.try_subtract(&base_label)
            && let Ok(delta) = rel_type.delta_value_in_range(&delta_bigint)
        {
            if let Some(chunk_env) = self.env.current_chunk_mut() {
                rel_type.append_delta(delta, chunk_env.data_mut());
            }
        } else {
            if let (Some(dest), Some(base)) = (dest_expr, base_expr)
                && let Some(chunk_env) = self.env.current_chunk_mut()
            {
                let data = ObjPatchData::Relative(rel_type, dest, base);
                errs.also(chunk_env.append_patch(data));
            }
        };
        errs.result()
    }

    fn typecheck_rel_expr(
        &self,
        directive: &'static str,
        component: &'static str,
        expr_ast: ExprAst,
    ) -> ((Option<ObjExpr>, Option<ExprLabel>), Errs<AsmError>) {
        let mut errs = Errs::<AsmError>::new();
        let expr_span = expr_ast.span;
        let ret = match errs.with(self.env.typecheck_expression(expr_ast)) {
            (_, ExprType::Undefined, _) => (None, None),
            (expr, ExprType::Label, Ok(value)) => {
                (Some(expr), Some(value.unwrap_label()))
            }
            (expr, ExprType::Integer, Ok(value)) => {
                if let Some(chunk_env) = self.env.current_chunk() {
                    let label = ExprLabel::ChunkAbsolute {
                        chunk_index: chunk_env.chunk_index(),
                        address: value.unwrap_int(),
                    };
                    (Some(expr), Some(label))
                } else {
                    (Some(expr), None)
                }
            }
            (
                expr,
                ExprType::Bottom | ExprType::Integer | ExprType::Label,
                Err(reason),
            ) => {
                errs.also(self.env.check_for_inevitable_eval_error(&reason));
                (Some(expr), None)
            }
            (_, expr_type, _) => {
                errs.push(AsmError::DirectiveExprTypeError {
                    directive,
                    component,
                    expr_loc: self.env.make_loc(expr_span),
                    expr_type,
                    valid_types: vec![ExprType::Integer, ExprType::Label],
                });
                (None, None)
            }
        };
        (ret, errs)
    }

    fn rel_patch_type(&self, rel_type: AsmRelTypeAst) -> ObjPatchRelType {
        match rel_type {
            AsmRelTypeAst::Addr16Rel8 => ObjPatchRelType::Addr16Rel8,
            AsmRelTypeAst::Addr16Rel16le => ObjPatchRelType::Addr16Rel16le,
            AsmRelTypeAst::Addr16RelLink => ObjPatchRelType::Addr16RelLink,
        }
    }

    fn chunk_align_attr(&mut self, expr_ast: ExprAst) -> AsmResult<Align> {
        self.chunk_static_align_attr("align", expr_ast)
    }

    fn chunk_arch_attr(&mut self, expr_ast: ExprAst) -> AsmResult<Rc<str>> {
        let expr_span = expr_ast.span;
        let arch = self.chunk_static_str_attr("arch", expr_ast)?;
        if self.env.arch_tree().contains_arch(&arch) {
            Ok(arch)
        } else {
            Err(Errs::one(AsmError::UnknownArch {
                arch: arch.clone(),
                loc: self.env.make_loc(expr_span),
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
                expr_loc: self.env.make_loc(expr_span),
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
                expr_loc: self.env.make_loc(expr_span),
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
                expr_loc: self.env.make_loc(expr_span),
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
            expr_ast,
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
            expr_ast,
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
                attr_loc: self.env.make_loc(id_ast.span),
                prev_loc: self.env.make_loc(prev_span),
            }))
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
            Ok(())
        }
    }

    fn typecheck_static_path_expr(
        &mut self,
        directive: &'static str,
        expr_ast: ExprAst,
    ) -> AsmResult<Rc<str>> {
        let value = self.typecheck_static_dir_expr_as(
            (directive, "path"),
            expr_ast,
            ExprType::String,
        )?;
        Ok(self.joined_path(value.unwrap_str_ref()))
    }

    /// Given a relative path appearing in this assembly source file (e.g. in a
    /// `.BINARY` directive), join that path to this source file's parent
    /// directory.
    fn joined_path(&self, relative_path: &Rc<str>) -> Rc<str> {
        let src_path = &self.env.current_src_context().path;
        match AsRef::<Path>::as_ref(&**src_path).parent() {
            None => relative_path.clone(),
            Some(base_path) => {
                let joined = base_path.join(&**relative_path);
                // We can safely `unwrap()` the `to_str()` here because
                // `joined` was made from `Path`s that came from `str`s.
                Rc::<str>::from(joined.to_str().unwrap())
            }
        }
    }

    fn typecheck_data_array_size(
        &self,
        directive: &'static str,
        data_type: AsmDataTypeAst,
        count_expr: Option<ExprAst>,
    ) -> AsmResult<Size> {
        let type_size = self.env.data_type_size(data_type)?;
        let count =
            self.typecheck_data_type_count(directive, "count", count_expr)?;
        // TODO: error on overflow
        Ok(Size::try_from(u128::from(type_size) * u128::from(count)).unwrap())
    }

    fn typecheck_data_type_count(
        &self,
        directive: &'static str,
        component: &'static str,
        count_expr: Option<ExprAst>,
    ) -> AsmResult<u64> {
        if let Some(expr_ast) = count_expr {
            let expr_span = expr_ast.span;
            let expr_value = self.typecheck_static_dir_expr_as(
                (directive, component),
                expr_ast,
                ExprType::Integer,
            )?;
            let expr_value = expr_value.unwrap_int();
            expr_value.to_u64().ok_or_else(|| {
                Errs::one(AsmError::DirectiveExprOutOfRange {
                    directive,
                    component,
                    expr_loc: self.env.make_loc(expr_span),
                    expr_value,
                    valid_range: bigint_range(u64::MIN, u64::MAX),
                })
            })
        } else {
            Ok(1)
        }
    }

    fn typecheck_static_dir_expr_as(
        &self,
        (directive, component): (&'static str, &'static str),
        expr_ast: ExprAst,
        required_type: ExprType,
    ) -> AsmResult<ExprValue> {
        let expr_span = expr_ast.span;
        let (_, expr_static) = self.typecheck_dir_expr_as(
            (directive, component),
            expr_ast,
            required_type,
        )?;
        match expr_static {
            Ok(value) => Ok(value),
            Err(reason) => Err(Errs::one(AsmError::DirectiveExprNotStatic {
                directive,
                component,
                expr_loc: self.env.make_loc(expr_span),
                reason,
            })),
        }
    }

    fn typecheck_dir_expr_as(
        &self,
        (directive, component): (&'static str, &'static str),
        expr_ast: ExprAst,
        required_type: ExprType,
    ) -> AsmResult<(ObjExpr, ExprStatic)> {
        let mut errs = Errs::<AsmError>::new();
        let expr_span = expr_ast.span;
        let (expr, expr_type, expr_static) =
            errs.with(self.env.typecheck_expression(expr_ast));
        if !expr_type.is_subtype_of(&required_type) {
            errs.push(AsmError::DirectiveExprTypeError {
                directive,
                component,
                expr_loc: self.env.make_loc(expr_span),
                expr_type,
                valid_types: vec![required_type],
            });
        }
        errs.result()?;
        Ok((expr, expr_static))
    }

    fn finish(self) -> ObjFile {
        ObjFile {
            chunks: self.chunks.into_values().collect(),
            imports: self.imports,
            variables: self.variables,
        }
    }
}

fn bigint_range<T: Into<BigInt>>(start: T, last: T) -> RangeInclusive<BigInt> {
    RangeInclusive { start: start.into(), last: last.into() }
}

//===========================================================================//
