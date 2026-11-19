use super::attrs::{
    ENTITY_ADDRSPACE, ENTITY_REGION, ENTITY_SECTION, LinkAddrspaceAttrs,
    LinkChecksumAttrs, LinkExportAttrs, LinkRegionAttrs, LinkSectionAttrs,
};
use super::checksum::{ChecksumConfig, ChecksumRange};
use super::env::ConfigTypeEnv;
use super::error::{ConfigAttr, ConfigError, ConfigResult};
use super::variable::ConfigVariableOr;
use super::{
    AddrspaceConfig, ExportConfig, ImportConfig, LinkConfig, RegionConfig,
    SectionConfig,
};
use crate::addr::{Addr, Align, BitWidth, Range};
use crate::error::{Errs, SrcCache, SrcSpan};
use crate::expr::{
    ExprEvalError, ExprFunc, ExprFuncEvalError, ExprNotStaticReason,
    ExprStatic, ExprType, ExprTypeError, ExprValue,
};
use crate::obj::{ObjExpr, ObjExprOp, ObjSrcContext, ObjSrcLoc, ObjSrcParent};
use crate::parse::{
    ExprAst, IdentifierAst, LinkAssertAst, LinkBlockAst, LinkConfigAst,
    LinkDirectiveAst, LinkEntryAst,
};
use num_bigint::BigInt;
use std::path::Path;
use std::rc::Rc;

//===========================================================================//

pub(super) struct ConfigBuilder<'a> {
    cache: &'a mut dyn SrcCache,
    env: ConfigTypeEnv,
    config: LinkConfig,
}

impl<'a> ConfigBuilder<'a> {
    pub(super) fn new(
        cache: &'a mut dyn SrcCache,
        root_path: Rc<str>,
    ) -> ConfigBuilder<'a> {
        ConfigBuilder {
            cache,
            env: ConfigTypeEnv::new(root_path),
            config: LinkConfig {
                addrspaces: Vec::new(),
                regions: Vec::new(),
                sections: Vec::new(),
                imports: Vec::new(),
                variables: Vec::new(),
                exports: Vec::new(),
                checksums: Vec::new(),
            },
        }
    }

    pub(super) fn build(
        mut self,
        source_code: &str,
    ) -> ConfigResult<LinkConfig> {
        let config_ast = self.env.parse_source(source_code)?;
        self.visit_config(config_ast)?;
        Ok(self.config)
    }

    fn visit_config(&mut self, config_ast: LinkConfigAst) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for dir_ast in config_ast.directives {
            errs.also(self.visit_directive(dir_ast));
        }
        errs.result()
    }

    fn visit_directive(
        &mut self,
        dir_ast: LinkDirectiveAst,
    ) -> ConfigResult<()> {
        match dir_ast {
            LinkDirectiveAst::Addrspaces(block) => {
                self.visit_addrspaces_dir(block)
            }
            LinkDirectiveAst::Assert(ast) => self.visit_assert_dir(ast),
            LinkDirectiveAst::Checksums(block) => {
                self.visit_checksums_dir(block)
            }
            LinkDirectiveAst::Exports(block) => self.visit_exports_dir(block),
            LinkDirectiveAst::Import(ids) => self.visit_import_dir(ids),
            LinkDirectiveAst::Let(id, expr) => self.visit_let_dir(id, expr),
            LinkDirectiveAst::Regions(block) => self.visit_regions_dir(block),
            LinkDirectiveAst::Sections(block) => {
                self.visit_sections_dir(block)
            }
            LinkDirectiveAst::Use(expr) => self.visit_use_dir(expr),
        }
    }

    fn visit_addrspaces_dir(
        &mut self,
        block: LinkBlockAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let base_attrs = errs.with(LinkAddrspaceAttrs::build(
            &self.env,
            &Rc::from(LinkBlockAst::ADDRSPACES_DIRECTIVE),
            block.base_attrs,
        ));
        for entry in block.entries {
            errs.also(self.visit_addrspaces_entry(&base_attrs, entry));
        }
        errs.result()
    }

    fn visit_addrspaces_entry(
        &mut self,
        base_attrs: &LinkAddrspaceAttrs,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        errs.also(self.declare_entry(ENTITY_ADDRSPACE, &entry.id));
        let attrs = errs.with(base_attrs.with_overrides(
            &self.env,
            &entry.id.name,
            entry.attrs,
        ));
        if attrs.bits.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::AddrspaceBits,
            });
        }
        self.config.addrspaces.push(AddrspaceConfig {
            name: entry.id.name,
            bit_width: attrs.bits.unwrap_or(BitWidth::MAX),
            fill: attrs.fill.unwrap_or_default(),
        });
        errs.result()
    }

    fn visit_assert_dir(
        &mut self,
        assert_ast: LinkAssertAst,
    ) -> ConfigResult<()> {
        let (
            (condition_expr, condition_static),
            (message_span, message_expr, message_static),
        ) = Errs::join_results(
            self.typecheck_predicate(assert_ast.condition),
            self.typecheck_assert_message(
                assert_ast.directive_span,
                assert_ast.message,
            ),
        )?;
        let failure_expr = {
            let mut expr = message_expr;
            let func_value = ExprValue::from(ExprFunc::Error);
            expr.ops.insert(0, ObjExprOp::Push(func_value));
            expr.ops.push(ObjExprOp::Apply {
                context: self.env.current_src_context(),
                arg_span: message_span,
            });
            expr
        };
        let mut errs = Errs::<ConfigError>::new();
        match condition_static {
            Ok(ExprValue::Boolean(true)) => {
                // The assertion condition statically succeeded.
            }
            Ok(_) => {
                // The assertion condition statically failed.
                match message_static {
                    Ok(message_value) => {
                        // Message is statically known, so we can report the
                        // failed assertion right now.
                        errs.push(ConfigError::StaticEvalError {
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
                        self.config.variables.push(failure_expr);
                    }
                }
            }
            Err(reason) => {
                // The assertion condition can't be evaluated yet, so we have
                // to wait until link time to check it.
                errs.also(self.env.check_for_inevitable_eval_error(&reason));
                let mut variable_expr = condition_expr;
                let mut failure_ops = failure_expr.ops;
                variable_expr.ops.push(ObjExprOp::SkipUnless(2));
                variable_expr.ops.push(ObjExprOp::Push(ExprValue::from(true)));
                variable_expr.ops.push(ObjExprOp::Skip(failure_ops.len()));
                variable_expr.ops.append(&mut failure_ops);
                self.config.variables.push(variable_expr);
            }
        }
        errs.result()
    }

    fn typecheck_assert_message(
        &self,
        directive_span: SrcSpan,
        opt_message_ast: Option<ExprAst>,
    ) -> ConfigResult<(SrcSpan, ObjExpr, ExprStatic)> {
        if let Some(message_ast) = opt_message_ast {
            let message_span = message_ast.span;
            let (expr, expr_static) =
                self.typecheck_error_message(message_ast)?;
            Ok((message_span, expr, expr_static))
        } else {
            let value = ExprValue::from("Assertion failed");
            let expr = ObjExpr::constant(
                value.clone(),
                self.env.make_loc(directive_span),
            );
            Ok((directive_span, expr, Ok(value)))
        }
    }

    fn visit_checksums_dir(
        &mut self,
        block: LinkBlockAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let base_attrs = errs.with(LinkChecksumAttrs::build(
            &self.env,
            &mut self.config.variables,
            &Rc::from(LinkBlockAst::CHECKSUMS_DIRECTIVE),
            block.base_attrs,
        ));
        for entry in block.entries {
            errs.also(self.visit_checksums_entry(&base_attrs, entry));
        }
        errs.result()
    }

    fn visit_checksums_entry(
        &mut self,
        base_attrs: &LinkChecksumAttrs,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        if self.env.get_export(&entry.id.name).is_none() {
            errs.also(self.import_symbol(entry.id.clone()));
        }
        let attrs = errs.with(base_attrs.with_overrides(
            &self.env,
            &mut self.config.variables,
            &entry.id.name,
            entry.attrs,
        ));
        if attrs.sum.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::ChecksumSum,
            });
        }
        let start = attrs.start.unwrap_or_default();
        let range = match (attrs.end, attrs.size) {
            (None, None) => ChecksumRange::From { start },
            (Some((_, end)), None) => ChecksumRange::FromTo { start, end },
            (None, Some((_, size))) => ChecksumRange::FromSize { start, size },
            (Some(_), Some(_)) => {
                // This is invalid; an error will have already been reported.
                ChecksumRange::default()
            }
        };
        let sum_formats = attrs
            .sum
            .unwrap_or_else(|| ConfigVariableOr::Static(Rc::from([])));
        let unit_format = attrs.unit.unwrap_or_default();
        self.config.checksums.push(ChecksumConfig {
            name: entry.id.name,
            sum_formats,
            unit_format,
            range,
        });
        errs.result()
    }

    fn visit_exports_dir(&mut self, block: LinkBlockAst) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let base_attrs = errs.with(LinkExportAttrs::build(
            &self.env,
            &mut self.config.variables,
            &Rc::from(LinkBlockAst::EXPORTS_DIRECTIVE),
            block.base_attrs,
        ));
        for entry in block.entries {
            errs.also(self.visit_exports_entry(&base_attrs, entry));
        }
        errs.result()
    }

    fn visit_exports_entry(
        &mut self,
        base_attrs: &LinkExportAttrs,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        if let Some(import_loc) = self.env.get_import(&entry.id.name) {
            errs.push(ConfigError::ExportImportedSymbol {
                symbol_name: entry.id.name.clone(),
                import_loc,
                export_loc: self.env.make_loc(entry.id.span),
            });
        }
        if let Some(prev_loc) = self.env.get_export(&entry.id.name) {
            errs.push(ConfigError::DuplicateExport {
                symbol_name: entry.id.name.clone(),
                export_loc: self.env.make_loc(entry.id.span),
                prev_loc,
            });
        }

        let attrs = errs.with(base_attrs.with_overrides(
            &self.env,
            &mut self.config.variables,
            &entry.id.name,
            entry.attrs,
        ));
        if attrs.space.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::ExportSpace,
            });
        }
        if attrs.addr.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::ExportAddr,
            });
        }
        let space = attrs.space.unwrap_or_default();
        let addr = attrs.addr.unwrap_or_default();
        self.config.exports.push(ExportConfig {
            name: entry.id.name.clone(),
            name_loc: self.env.make_loc(entry.id.span),
            space: space.clone(),
            address: addr,
        });
        self.env.add_export(entry.id, space, addr);
        errs.result()
    }

    fn visit_import_dir(
        &mut self,
        id_asts: Vec<IdentifierAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for id_ast in id_asts {
            errs.also(self.import_symbol(id_ast));
        }
        errs.result()
    }

    fn import_symbol(&mut self, id_ast: IdentifierAst) -> ConfigResult<()> {
        if let Some(export_loc) = self.env.get_export(&id_ast.name) {
            return Err(Errs::one(ConfigError::ExportImportedSymbol {
                symbol_name: id_ast.name.clone(),
                import_loc: self.env.make_loc(id_ast.span),
                export_loc,
            }));
        }
        if self.env.get_import(&id_ast.name).is_none() {
            self.config.imports.push(ImportConfig {
                name: id_ast.name.clone(),
                loc: self.env.make_loc(id_ast.span),
            });
        }
        self.env.add_import(id_ast);
        Ok(())
    }

    fn visit_let_dir(
        &mut self,
        id_ast: IdentifierAst,
        expr_ast: ExprAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let (expr_type, value) = match errs.with(self.typecheck_expr(expr_ast))
        {
            (_, expr_type, Ok(static_value)) => {
                (expr_type, ConfigVariableOr::Static(static_value))
            }
            (expr, expr_type, Err(reason)) => {
                errs.also(self.env.check_for_inevitable_eval_error(&reason));
                let variable = ConfigVariableOr::new_variable(
                    &mut self.config.variables,
                    expr,
                );
                (expr_type, variable)
            }
        };
        self.env.add_declaration(
            id_ast.name.clone(),
            self.env.make_loc(id_ast.span),
            expr_type,
            value,
        );
        errs.result()
    }

    fn visit_regions_dir(&mut self, block: LinkBlockAst) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let base_attrs = errs.with(LinkRegionAttrs::build(
            &self.env,
            &Rc::from(LinkBlockAst::REGIONS_DIRECTIVE),
            block.base_attrs,
        ));
        for entry in block.entries {
            let region =
                errs.with(self.visit_region_entry(&base_attrs, entry));
            self.config.regions.push(region);
        }
        errs.result()
    }

    fn visit_region_entry(
        &mut self,
        base_attrs: &LinkRegionAttrs,
        entry: LinkEntryAst,
    ) -> (RegionConfig, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        errs.also(self.declare_entry(ENTITY_REGION, &entry.id));
        let attrs = errs.with(base_attrs.with_overrides(
            &self.env,
            &entry.id.name,
            entry.attrs,
        ));
        let addrspace_width = if let Some(space) = &attrs.space {
            self.config
                .addrspaces
                .iter()
                .find(|addrspace| space == &addrspace.name)
                .map(|addrspace| addrspace.bit_width)
                .unwrap_or(BitWidth::MAX)
        } else {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::RegionSpace,
            });
            BitWidth::MAX
        };
        let addrspace_range = Range::with_bits(addrspace_width);
        if attrs.start.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::RegionStart,
            });
        }
        if attrs.size.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::RegionSize,
            });
        }
        let range = match (attrs.start, attrs.size) {
            (Some(start), Some(size)) => match start.range_with_size(size) {
                Some(range) if addrspace_range.is_superset(range) => range,
                _ => {
                    errs.push(ConfigError::RegionRangeOverflow {
                        entry_name: entry.id.name.clone(),
                        entry_loc: self.env.make_loc(entry.id.span),
                        start,
                        size,
                        bit_width: addrspace_width,
                    });
                    start.range_within(Align::MAX)
                }
            },
            (Some(start), None) => start.range_within(Align::MAX),
            (None, Some(size)) => Addr::MIN.range_with_size(size).unwrap(),
            (None, None) => Range::FULL,
        };
        let region = RegionConfig {
            name: entry.id.name,
            name_loc: self.env.make_loc(entry.id.span),
            space: attrs.space.unwrap_or_default(),
            range,
            fill: attrs.fill,
            kind: attrs.kind.unwrap_or_default(),
        };
        (region, errs)
    }

    fn visit_sections_dir(&mut self, block: LinkBlockAst) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let base_attrs = errs.with(LinkSectionAttrs::build(
            &self.env,
            &Rc::from(LinkBlockAst::SECTIONS_DIRECTIVE),
            block.base_attrs,
        ));
        for entry in block.entries {
            errs.also(self.visit_section_entry(&base_attrs, entry));
        }
        errs.result()
    }

    fn visit_section_entry(
        &mut self,
        base_attrs: &LinkSectionAttrs,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        errs.also(self.declare_entry(ENTITY_SECTION, &entry.id));
        let attrs = errs.with(base_attrs.with_overrides(
            &self.env,
            &entry.id.name,
            entry.attrs,
        ));
        if attrs.region.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::SectionRegion,
            });
        }
        let section_index = self.config.sections.len();
        let name_loc = self.env.make_loc(entry.id.span);
        self.env.add_declaration(
            Rc::from(format!("{}::%size", entry.id.name)),
            name_loc.clone(),
            ExprType::Integer,
            match attrs.size {
                Some((_span, size)) => {
                    let value = ExprValue::Integer(BigInt::from(size));
                    ConfigVariableOr::Static(value)
                }
                None => ConfigVariableOr::new_variable(
                    &mut self.config.variables,
                    ObjExpr {
                        ops: vec![ObjExprOp::SectionSize(section_index)],
                        loc: name_loc.clone(),
                    },
                ),
            },
        );
        self.env.add_declaration(
            Rc::from(format!("{}::%start", entry.id.name)),
            name_loc.clone(),
            ExprType::Integer,
            match attrs.start {
                Some(start) => {
                    let value = ExprValue::Integer(BigInt::from(start));
                    ConfigVariableOr::Static(value)
                }
                None => ConfigVariableOr::new_variable(
                    &mut self.config.variables,
                    ObjExpr {
                        ops: vec![ObjExprOp::SectionStart(section_index)],
                        loc: name_loc.clone(),
                    },
                ),
            },
        );
        self.config.sections.push(SectionConfig {
            name: entry.id.name,
            name_loc,
            region: attrs.region.unwrap_or_default(),
            start: attrs.start,
            size: attrs.size.map(|(_span, size)| size),
            align: attrs.align.unwrap_or_default(),
            within: attrs.within,
            fill: attrs.fill.map(|(_span, fill)| fill),
            mirror: attrs
                .mirror
                .map(|(span, name)| (self.env.make_loc(span), name)),
        });
        errs.result()
    }

    fn visit_use_dir(&mut self, expr_ast: ExprAst) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let expr_span = expr_ast.span;
        match errs.with(self.typecheck_expr(expr_ast)) {
            (_, ExprType::Undefined, _) => {}
            (_, ExprType::String, Ok(path_value)) => {
                let path = self.joined_path(path_value.unwrap_str_ref());
                match self.cache.fetch_or_get_cached_utf8(&path) {
                    Ok(source_code) => {
                        let context = Rc::new(ObjSrcContext {
                            path,
                            parent: ObjSrcParent::Use(ObjSrcLoc {
                                span: expr_span,
                                context: self.env.current_src_context(),
                            }),
                        });
                        self.env.push_src_context(context);
                        if let Some(config_ast) =
                            errs.ok(self.env.parse_source(source_code))
                        {
                            errs.also(self.visit_config(config_ast));
                        }
                        self.env.pop_src_context();
                    }
                    Err(error) => {
                        errs.push(ConfigError::SrcCacheError {
                            path,
                            path_loc: self.env.make_loc(expr_span),
                            error,
                        });
                    }
                }
            }
            (_, ExprType::String | ExprType::Bottom, Err(reason)) => errs
                .push(ConfigError::PathNotStatic {
                    expr_loc: self.env.make_loc(expr_span),
                    reason,
                }),
            (_, expr_type, _) => errs.push(ConfigError::PathTypeError {
                expr_loc: self.env.make_loc(expr_span),
                expr_type,
            }),
        }
        errs.result()
    }

    fn declare_entry(
        &mut self,
        entry_kind: &str,
        entry_id: &IdentifierAst,
    ) -> ConfigResult<()> {
        if let Some(decl) = self.env.get_declaration(&entry_id.name) {
            Err(Errs::one(ConfigError::DuplicateEntryName {
                entry_name: entry_id.name.clone(),
                entry_loc: self.env.make_loc(entry_id.span),
                prev_loc: decl.id_loc.clone(),
            }))
        } else {
            let value = ExprValue::Entity(entry_id.name.clone());
            self.env.add_declaration(
                entry_id.name.clone(),
                self.env.make_loc(entry_id.span),
                ExprType::Entity(Rc::from(entry_kind)),
                ConfigVariableOr::Static(value),
            );
            Ok(())
        }
    }

    pub fn typecheck_expr(
        &self,
        expr_ast: ExprAst,
    ) -> ((ObjExpr, ExprType, ExprStatic), Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        let expr_span = expr_ast.span;
        let typechecked = errs
            .ok(self.env.typecheck_expression(expr_ast))
            .unwrap_or_else(|| {
                let ops = vec![ObjExprOp::Push(ExprValue::FALSE)];
                let loc = self.env.make_loc(expr_span);
                let reason = ExprNotStaticReason::TypeError;
                (ObjExpr { ops, loc }, ExprType::Undefined, Err(reason))
            });
        (typechecked, errs)
    }

    fn typecheck_error_message(
        &self,
        expr_ast: ExprAst,
    ) -> ConfigResult<(ObjExpr, ExprStatic)> {
        let mut errs = Errs::<ConfigError>::new();
        let expr_span = expr_ast.span;
        let (expr, expr_type, expr_static) =
            errs.with(self.typecheck_expr(expr_ast));
        if !expr_type.is_subtype_of(&ExprType::String) {
            errs.push(ConfigError::ExprTypeError {
                context: self.env.current_src_context(),
                error: ExprTypeError::CannotUseTypeAsErrorMessage {
                    expr_span,
                    expr_type,
                },
            });
        }
        errs.result()?;
        Ok((expr, expr_static))
    }

    fn typecheck_predicate(
        &self,
        expr_ast: ExprAst,
    ) -> ConfigResult<(ObjExpr, ExprStatic)> {
        let mut errs = Errs::<ConfigError>::new();
        let expr_span = expr_ast.span;
        let (expr, expr_type, expr_static) =
            errs.with(self.typecheck_expr(expr_ast));
        if !expr_type.is_subtype_of(&ExprType::Boolean) {
            errs.push(ConfigError::ExprTypeError {
                context: self.env.current_src_context(),
                error: ExprTypeError::CannotUseTypeAsPredicate {
                    expr_span,
                    expr_type,
                },
            });
        }
        errs.result()?;
        Ok((expr, expr_static))
    }

    /// Given a relative path appearing in the current source context (e.g. in
    /// a `use` statement), join that path to the current source file's parent
    /// directory.
    fn joined_path(&self, relative_path: &Rc<str>) -> Rc<str> {
        let context = self.env.current_src_context();
        match AsRef::<Path>::as_ref(&*context.path).parent() {
            None => relative_path.clone(),
            Some(base_path) => {
                let joined = base_path.join(&**relative_path);
                // We can safely `unwrap()` the `to_str()` here because
                // `joined` was made from `Path`s that came from `str`s.
                Rc::<str>::from(joined.to_str().unwrap())
            }
        }
    }
}

//===========================================================================//
