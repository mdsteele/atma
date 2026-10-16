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
use crate::addr::{Addr, Align, Range};
use crate::error::{Errs, SrcCache};
use crate::expr::{ExprType, ExprValue};
use crate::obj::{ObjSrcContext, ObjSrcLoc, ObjSrcParent};
use crate::parse::{
    ExprAst, IdentifierAst, LinkBlockAst, LinkConfigAst, LinkDirectiveAst,
    LinkEntryAst,
};
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
        let attrs = errs.with(LinkAddrspaceAttrs::build(
            &self.env,
            &entry.id.name,
            entry.attrs,
        ));
        let attrs = base_attrs.clone_with_overrides(attrs);
        if attrs.bits.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::AddrspaceBits,
            });
        }
        self.config.addrspaces.push(AddrspaceConfig {
            name: entry.id.name,
            bits: attrs.bits.unwrap_or(u32::BITS),
            fill: attrs.fill.unwrap_or_default(),
        });
        errs.result()
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
        let attrs = errs.with(LinkChecksumAttrs::build(
            &self.env,
            &mut self.config.variables,
            &entry.id.name,
            entry.attrs,
        ));
        let attrs = base_attrs.clone_with_overrides(attrs);
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
            (Some((end_span, _)), Some((size_span, _))) => {
                errs.push(ConfigError::MutuallyExclusiveAttrs {
                    entry_loc: self.env.make_loc(entry.id.span),
                    attribute_1: ConfigAttr::ChecksumSize,
                    attribute_2: ConfigAttr::ChecksumEnd,
                    attr_span_1: size_span,
                    attr_span_2: end_span,
                });
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

        let attrs = errs.with(LinkExportAttrs::build(
            &self.env,
            &mut self.config.variables,
            &entry.id.name,
            entry.attrs,
        ));
        let attrs = base_attrs.clone_with_overrides(attrs);
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
        let (expr_type, value) =
            match errs.ok(self.env.typecheck_expression(expr_ast)) {
                Some((_, expr_type, Ok(static_value))) => {
                    (expr_type, ConfigVariableOr::Static(static_value))
                }
                Some((expr, expr_type, Err(_))) => (
                    expr_type,
                    ConfigVariableOr::new_variable(
                        &mut self.config.variables,
                        expr,
                    ),
                ),
                None => (
                    ExprType::Undefined,
                    ConfigVariableOr::Static(ExprValue::Boolean(false)),
                ),
            };
        self.env.add_declaration(id_ast, expr_type, value);
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
        let attrs = errs.with(LinkRegionAttrs::build(
            &self.env,
            &entry.id.name,
            entry.attrs,
        ));
        let attrs = base_attrs.clone_with_overrides(attrs);
        let addrspace_bits = if let Some(space) = &attrs.space {
            self.config
                .addrspaces
                .iter()
                .find(|addrspace| space == &addrspace.name)
                .map(|addrspace| addrspace.bits)
                .unwrap_or(Addr::BITS)
        } else {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::RegionSpace,
            });
            Addr::BITS
        };
        let addrspace_range = Range::with_bits(addrspace_bits);
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
                        bits: addrspace_bits,
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
        let attrs = errs.with(LinkSectionAttrs::build(
            &self.env,
            &entry.id.name,
            entry.attrs,
        ));
        let attrs = base_attrs.clone_with_overrides(attrs);
        if attrs.region.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_loc: self.env.make_loc(entry.id.span),
                attribute: ConfigAttr::SectionRegion,
            });
        }
        self.config.sections.push(SectionConfig {
            name: entry.id.name,
            name_loc: self.env.make_loc(entry.id.span),
            region: attrs.region.unwrap_or_default(),
            start: attrs.start,
            size: attrs.size,
            align: attrs.align.unwrap_or_default(),
            within: attrs.within,
            fill: attrs.fill,
        });
        errs.result()
    }

    fn visit_use_dir(&mut self, expr_ast: ExprAst) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        let expr_span = expr_ast.span;
        match errs.ok(self.env.typecheck_expression(expr_ast)) {
            None => {}
            Some((_, ExprType::String, Ok(path_value))) => {
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
            Some((_, ExprType::String | ExprType::Bottom, Err(reason))) => {
                errs.push(ConfigError::PathNotStatic {
                    expr_loc: self.env.make_loc(expr_span),
                    reason,
                })
            }
            Some((_, expr_type, _)) => errs.push(ConfigError::PathTypeError {
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
                entry_id.clone(),
                ExprType::Entity(Rc::from(entry_kind)),
                ConfigVariableOr::Static(value),
            );
            Ok(())
        }
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
