use super::checksum::{ChecksumConfig, ChecksumFormat, ChecksumRange};
use super::env::LinkTypeEnv;
use super::error::{ConfigAttr, ConfigEntryKind, ConfigError, ConfigResult};
use super::{
    AddrspaceConfig, ConfigVariableOr, ExportConfig, LinkConfig, RegionConfig,
    SectionConfig,
};
use crate::addr::{Addr, Align, Range, Size};
use crate::error::{Errs, SrcSpan};
use crate::expr::{ExprType, ExprValue};
use crate::obj::ObjExpr;
use crate::parse::{
    ExprAst, IdentifierAst, LinkConfigAst, LinkDirectiveAst, LinkEntryAst,
};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

const ENTITY_ADDRSPACE: &str = "address space";
const ENTITY_MEMORY: &str = "memory region";
const ENTITY_SECTION: &str = "section";

type PrevAttrs = HashMap<Rc<str>, SrcSpan>;

//===========================================================================//

pub(super) struct ConfigBuilder {
    env: LinkTypeEnv,
    config: LinkConfig,
}

impl ConfigBuilder {
    pub(super) fn new() -> ConfigBuilder {
        ConfigBuilder {
            env: LinkTypeEnv::new(),
            config: LinkConfig {
                addrspaces: Vec::new(),
                bss: Vec::new(),
                memory: Vec::new(),
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
        ast: LinkConfigAst,
    ) -> ConfigResult<LinkConfig> {
        let mut errs = Errs::<ConfigError>::new();
        for dir_ast in ast.directives {
            errs.also(self.visit_directive(dir_ast));
        }
        errs.result()?;
        Ok(self.config)
    }

    fn visit_directive(
        &mut self,
        dir_ast: LinkDirectiveAst,
    ) -> ConfigResult<()> {
        match dir_ast {
            LinkDirectiveAst::Addrspaces(entries) => {
                self.visit_addrspaces_dir(entries)
            }
            LinkDirectiveAst::Bss(entries) => self.visit_bss_dir(entries),
            LinkDirectiveAst::Checksums(entries) => {
                self.visit_checksums_dir(entries)
            }
            LinkDirectiveAst::Exports(entries) => {
                self.visit_exports_dir(entries)
            }
            LinkDirectiveAst::Import(ids) => self.visit_import_dir(ids),
            LinkDirectiveAst::Let(id, expr) => self.visit_let_dir(id, expr),
            LinkDirectiveAst::Memory(entries) => {
                self.visit_memory_dir(entries)
            }
            LinkDirectiveAst::Sections(entries) => {
                self.visit_sections_dir(entries)
            }
        }
    }

    fn visit_addrspaces_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for entry in entries {
            errs.also(self.visit_addrspaces_entry(entry));
        }
        errs.result()
    }

    fn visit_addrspaces_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        errs.also(self.declare_entry(ENTITY_ADDRSPACE, &entry.id));
        let mut bits: Option<u32> = None;
        let mut fill: Option<u8> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            errs.also(self.declare_attr(
                &entry.id.name,
                &mut prev_attrs,
                &id_ast,
            ));
            match &*id_ast.name {
                "bits" => {
                    bits = Some(
                        errs.ok_or_default(self.addrspace_bits_attr(expr_ast)),
                    )
                }
                "fill" => {
                    fill = Some(
                        errs.ok_or_default(self.addrspace_fill_attr(expr_ast)),
                    )
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Addrspace,
                    attr_name: id_ast.name.clone(),
                    attr_span: id_ast.span,
                }),
            }
        }
        if bits.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::AddrspaceBits,
            });
        }
        self.config.addrspaces.push(AddrspaceConfig {
            name: entry.id.name,
            bits: bits.unwrap_or(u32::BITS),
            fill: fill.unwrap_or_default(),
        });
        errs.result()
    }

    fn addrspace_bits_attr(&self, expr_ast: ExprAst) -> ConfigResult<u32> {
        let expr_span = expr_ast.span;
        let bigint =
            self.static_int_attr(ConfigAttr::AddrspaceBits, expr_ast)?;
        match bigint.to_u32() {
            Some(int) if (1..=Addr::BITS).contains(&int) => Ok(int),
            _ => Err(Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::AddrspaceBits,
                expr_span,
                value: bigint,
            })),
        }
    }

    fn addrspace_fill_attr(&self, expr_ast: ExprAst) -> ConfigResult<u8> {
        self.static_fill_attr(ConfigAttr::AddrspaceFill, expr_ast)
    }

    fn visit_checksums_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for entry in entries {
            errs.also(self.visit_checksums_entry(entry));
        }
        errs.result()
    }

    fn visit_checksums_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        if self.env.get_export(&entry.id.name).is_none() {
            errs.also(self.import_symbol(entry.id.clone()));
        }
        let mut sum: Option<ConfigVariableOr<Rc<[ChecksumFormat]>>> = None;
        let mut unit: Option<ConfigVariableOr<ChecksumFormat>> = None;
        let mut start: Option<ConfigVariableOr<u64>> = None;
        let mut end: Option<ConfigVariableOr<u64>> = None;
        let mut size: Option<ConfigVariableOr<u64>> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            errs.also(self.declare_attr(
                &entry.id.name,
                &mut prev_attrs,
                &id_ast,
            ));
            match &*id_ast.name {
                "sum" => {
                    sum =
                        Some(errs.ok_or_else(
                            self.checksum_sum_attr(expr_ast),
                            || ConfigVariableOr::Static(Rc::from([])),
                        ))
                }
                "unit" => {
                    unit = Some(errs.ok_or(
                        self.checksum_unit_attr(expr_ast),
                        ConfigVariableOr::Static(ChecksumFormat::U8),
                    ))
                }
                "start" => {
                    start = Some(
                        errs.ok_or_default(self.checksum_start_attr(expr_ast)),
                    )
                }
                "end" => {
                    end = Some(
                        errs.ok_or_default(self.checksum_end_attr(expr_ast)),
                    )
                }
                "size" => {
                    size = Some(
                        errs.ok_or_default(self.checksum_size_attr(expr_ast)),
                    )
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Checksum,
                    attr_name: id_ast.name.clone(),
                    attr_span: id_ast.span,
                }),
            }
        }
        if sum.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::ChecksumSum,
            });
        }
        let start = start.unwrap_or_default();
        let range = match (end, size) {
            (None, None) => ChecksumRange::From { start },
            (Some(end), None) => ChecksumRange::FromTo { start, end },
            (None, Some(size)) => ChecksumRange::FromSize { start, size },
            (Some(_end), Some(_size)) => {
                errs.push(ConfigError::MutuallyExclusiveAttrs {
                    attribute_1: ConfigAttr::ChecksumSize,
                    attribute_2: ConfigAttr::ChecksumEnd,
                    attr_span_1: *prev_attrs.get("size").unwrap(),
                    attr_span_2: *prev_attrs.get("end").unwrap(),
                    entry_span: entry.id.span,
                });
                ChecksumRange::default()
            }
        };
        self.config.checksums.push(ChecksumConfig {
            name: entry.id.name,
            sum_formats: sum
                .unwrap_or_else(|| ConfigVariableOr::Static(Rc::from([]))),
            unit_format: unit
                .unwrap_or(ConfigVariableOr::Static(ChecksumFormat::U8)),
            range,
        });
        errs.result()
    }

    fn checksum_sum_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<Rc<[ChecksumFormat]>>> {
        self.variable_checksum_formats_attr(ConfigAttr::ChecksumSum, expr_ast)
    }

    fn checksum_unit_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<ChecksumFormat>> {
        self.variable_checksum_format_attr(ConfigAttr::ChecksumUnit, expr_ast)
    }

    fn checksum_start_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        self.variable_u64_attr(ConfigAttr::ChecksumStart, expr_ast)
    }

    fn checksum_end_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        self.variable_u64_attr(ConfigAttr::ChecksumEnd, expr_ast)
    }

    fn checksum_size_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        self.variable_u64_attr(ConfigAttr::ChecksumSize, expr_ast)
    }

    fn visit_exports_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for entry in entries {
            errs.also(self.visit_exports_entry(entry));
        }
        errs.result()
    }

    fn visit_exports_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        if let Some(import_span) = self.env.get_import(&entry.id.name) {
            errs.push(ConfigError::ExportImportedSymbol {
                symbol_name: entry.id.name.clone(),
                import_span,
                export_span: entry.id.span,
            });
        }
        if let Some(prev_span) = self.env.get_export(&entry.id.name) {
            errs.push(ConfigError::DuplicateExport {
                symbol_name: entry.id.name.clone(),
                export_span: entry.id.span,
                prev_span,
            });
        }
        let mut space: Option<Rc<str>> = None;
        let mut addr: Option<ConfigVariableOr<Addr>> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            errs.also(self.declare_attr(
                &entry.id.name,
                &mut prev_attrs,
                &id_ast,
            ));
            match &*id_ast.name {
                "addr" => {
                    addr = Some(
                        errs.ok_or_default(self.export_addr_attr(expr_ast)),
                    )
                }
                "space" => {
                    space = Some(
                        errs.ok_or_default(self.export_space_attr(expr_ast)),
                    )
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Export,
                    attr_name: id_ast.name.clone(),
                    attr_span: id_ast.span,
                }),
            }
        }
        if space.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::ChecksumSum,
            });
        }
        if addr.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::ExportAddr,
            });
        }
        let space = space.unwrap_or_default();
        let address = addr.unwrap_or_default();
        self.config.exports.push(ExportConfig {
            name: entry.id.name.clone(),
            space: space.clone(),
            address,
        });
        self.env.add_export(entry.id, space, address);
        errs.result()
    }

    fn export_addr_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<Addr>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Some(static_value)) => {
                let bigint = static_value.unwrap_int_ref();
                let addr = Addr::try_from(bigint).map_err(|()| {
                    Errs::one(ConfigError::OutOfRangeAttr {
                        attribute: ConfigAttr::ExportAddr,
                        expr_span,
                        value: bigint.clone(),
                    })
                })?;
                Ok(ConfigVariableOr::Static(addr))
            }
            (expr, ExprType::Integer, None) => Ok(self.add_variable(expr)),
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute: ConfigAttr::ExportAddr,
                expr_span,
                expr_type,
                valid_types: vec![ExprType::Integer],
            })),
        }
    }

    fn export_space_attr(&self, expr_ast: ExprAst) -> ConfigResult<Rc<str>> {
        self.static_entity_attr(
            ConfigAttr::ExportSpace,
            expr_ast,
            ENTITY_ADDRSPACE,
        )
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
        if let Some(export_span) = self.env.get_export(&id_ast.name) {
            return Err(Errs::one(ConfigError::ExportImportedSymbol {
                symbol_name: id_ast.name.clone(),
                import_span: id_ast.span,
                export_span,
            }));
        }
        if self.env.get_import(&id_ast.name).is_none() {
            self.config.imports.push(id_ast.name.clone());
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
        let (ty, value) = match errs.ok(self.typecheck_expression(expr_ast)) {
            Some((_expr, ty, Some(static_value))) => {
                (ty, ConfigVariableOr::Static(static_value))
            }
            Some((expr, ty, None)) => (ty, self.add_variable(expr)),
            None => (
                ExprType::Bottom,
                ConfigVariableOr::Static(ExprValue::Boolean(false)),
            ),
        };
        self.env.add_declaration(id_ast, ty, value);
        errs.result()
    }

    fn add_variable<T>(&mut self, expr: ObjExpr) -> ConfigVariableOr<T> {
        let index = self.config.variables.len();
        self.config.variables.push(expr);
        ConfigVariableOr::Variable(index)
    }

    fn visit_bss_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for entry in entries {
            let region = errs.with(self.visit_region_entry(entry));
            self.config.bss.push(region);
        }
        errs.result()
    }

    fn visit_memory_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for entry in entries {
            let region = errs.with(self.visit_region_entry(entry));
            self.config.memory.push(region);
        }
        errs.result()
    }

    fn visit_region_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> (RegionConfig, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        errs.also(self.declare_entry(ENTITY_MEMORY, &entry.id));
        let mut space: Option<Rc<str>> = None;
        let mut start: Option<Addr> = None;
        let mut size: Option<Size> = None;
        let mut fill: Option<u8> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            errs.also(self.declare_attr(
                &entry.id.name,
                &mut prev_attrs,
                &id_ast,
            ));
            match &*id_ast.name {
                "fill" => {
                    fill = Some(
                        errs.ok_or_default(self.memory_fill_attr(expr_ast)),
                    )
                }
                "size" => {
                    size = Some(errs.ok_or(
                        self.memory_size_attr(expr_ast),
                        Size::from(1u32),
                    ))
                }
                "space" => {
                    space = Some(
                        errs.ok_or_default(self.memory_space_attr(expr_ast)),
                    )
                }
                "start" => {
                    start = Some(
                        errs.ok_or_default(self.memory_start_attr(expr_ast)),
                    )
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Region,
                    attr_name: id_ast.name.clone(),
                    attr_span: id_ast.span,
                }),
            }
        }
        let addrspace_bits = if let Some(space) = &space {
            self.config
                .addrspaces
                .iter()
                .find(|addrspace| space == &addrspace.name)
                .map(|addrspace| addrspace.bits)
                .unwrap_or(Addr::BITS)
        } else {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::RegionSpace,
            });
            Addr::BITS
        };
        let addrspace_range = Range::with_bits(addrspace_bits);
        if start.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::RegionStart,
            });
        }
        if size.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::RegionSize,
            });
        }
        let range = match (start, size) {
            (Some(start), Some(size)) => match start.range_with_size(size) {
                Some(range) if addrspace_range.is_superset(range) => range,
                _ => {
                    errs.push(ConfigError::RegionRangeOverflow {
                        entry_name: entry.id.name.clone(),
                        entry_span: entry.id.span,
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
            space: space.unwrap_or_default(),
            range,
            fill,
        };
        (region, errs)
    }

    fn memory_fill_attr(&self, expr_ast: ExprAst) -> ConfigResult<u8> {
        self.static_fill_attr(ConfigAttr::RegionFill, expr_ast)
    }

    fn memory_size_attr(&self, expr_ast: ExprAst) -> ConfigResult<Size> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(ConfigAttr::RegionSize, expr_ast)?;
        match Size::try_from(&bigint) {
            Ok(size) if size > Size::ZERO => Ok(size),
            _ => Err(Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::RegionSize,
                expr_span,
                value: bigint,
            })),
        }
    }

    fn memory_space_attr(&self, expr_ast: ExprAst) -> ConfigResult<Rc<str>> {
        self.static_entity_attr(
            ConfigAttr::RegionSpace,
            expr_ast,
            ENTITY_ADDRSPACE,
        )
    }

    fn memory_start_attr(&self, expr_ast: ExprAst) -> ConfigResult<Addr> {
        let expr_span = expr_ast.span;
        // TODO: allow this to be a static exported label
        let bigint =
            self.static_int_attr(ConfigAttr::RegionStart, expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::RegionStart,
                expr_span,
                value: bigint,
            })
        })
    }

    fn visit_sections_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        for entry in entries {
            errs.also(self.visit_section_entry(entry));
        }
        errs.result()
    }

    fn visit_section_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> ConfigResult<()> {
        let mut errs = Errs::<ConfigError>::new();
        errs.also(self.declare_entry(ENTITY_SECTION, &entry.id));
        let mut region: Option<Rc<str>> = None;
        let mut start: Option<Addr> = None;
        let mut size: Option<Size> = None;
        let mut align: Option<Align> = None;
        let mut within: Option<Align> = None;
        let mut fill: Option<u8> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            errs.also(self.declare_attr(
                &entry.id.name,
                &mut prev_attrs,
                &id_ast,
            ));
            match &*id_ast.name {
                "align" => {
                    align = Some(
                        errs.ok_or_default(self.section_align_attr(expr_ast)),
                    )
                }
                "fill" => {
                    fill = Some(
                        errs.ok_or_default(self.section_fill_attr(expr_ast)),
                    )
                }
                "region" => {
                    region = Some(
                        errs.ok_or_default(self.section_region_attr(expr_ast)),
                    )
                }
                "size" => {
                    size = Some(errs.ok_or(
                        self.section_size_attr(expr_ast),
                        Size::from(1u32),
                    ))
                }
                "start" => {
                    start = Some(
                        errs.ok_or_default(self.section_start_attr(expr_ast)),
                    )
                }
                "within" => {
                    within =
                        Some(errs.ok_or(
                            self.section_within_attr(expr_ast),
                            Align::MAX,
                        ))
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Section,
                    attr_name: id_ast.name.clone(),
                    attr_span: id_ast.span,
                }),
            }
        }
        if region.is_none() {
            errs.push(ConfigError::MissingAttr {
                entry_name: entry.id.name.clone(),
                entry_span: entry.id.span,
                attribute: ConfigAttr::SectionRegion,
            });
        }
        self.config.sections.push(SectionConfig {
            name: entry.id.name,
            region: region.unwrap_or_default(),
            start,
            size,
            align: align.unwrap_or_default(),
            within,
            fill,
        });
        errs.result()
    }

    fn section_align_attr(&self, expr_ast: ExprAst) -> ConfigResult<Align> {
        self.static_align_attr(ConfigAttr::SectionAlign, expr_ast)
    }

    fn section_fill_attr(&self, expr_ast: ExprAst) -> ConfigResult<u8> {
        self.static_fill_attr(ConfigAttr::SectionFill, expr_ast)
    }

    fn section_region_attr(&self, expr_ast: ExprAst) -> ConfigResult<Rc<str>> {
        self.static_entity_attr(
            ConfigAttr::SectionRegion,
            expr_ast,
            ENTITY_MEMORY,
        )
    }

    fn section_start_attr(&self, expr_ast: ExprAst) -> ConfigResult<Addr> {
        let expr_span = expr_ast.span;
        // TODO: allow this to be a static exported label
        let bigint =
            self.static_int_attr(ConfigAttr::SectionStart, expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::SectionStart,
                expr_span,
                value: bigint,
            })
        })
    }

    fn section_size_attr(&self, expr_ast: ExprAst) -> ConfigResult<Size> {
        let expr_span = expr_ast.span;
        let bigint =
            self.static_int_attr(ConfigAttr::SectionSize, expr_ast)?;
        match Size::try_from(&bigint) {
            Ok(size) if size > Size::ZERO => Ok(size),
            _ => Err(Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::SectionSize,
                expr_span,
                value: bigint,
            })),
        }
    }

    fn section_within_attr(&self, expr_ast: ExprAst) -> ConfigResult<Align> {
        self.static_align_attr(ConfigAttr::SectionWithin, expr_ast)
    }

    fn static_align_attr(
        &self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<Align> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(attribute, expr_ast)?;
        Align::try_from(&bigint).map_err(|error| {
            Errs::one(ConfigError::InvalidAlignmentAttr {
                attribute,
                error,
                expr_span,
                expr_value: bigint,
            })
        })
    }

    fn static_entity_attr(
        &self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
        entity_kind: &str,
    ) -> ConfigResult<Rc<str>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Entity(kind), static_value)
                if &*kind == entity_kind =>
            {
                if let Some(value) = static_value {
                    Ok(value.unwrap_entity())
                } else {
                    Err(Errs::one(ConfigError::NonStaticAttr {
                        attribute,
                        expr_span,
                    }))
                }
            }
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute,
                expr_span,
                expr_type,
                valid_types: vec![ExprType::Entity(Rc::from(entity_kind))],
            })),
        }
    }

    fn variable_checksum_formats_attr(
        &mut self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<Rc<[ChecksumFormat]>>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::String, Some(static_value)) => {
                let format = self.parse_format(
                    static_value.unwrap_str_ref(),
                    attribute,
                    expr_span,
                )?;
                Ok(ConfigVariableOr::Static(Rc::from([format])))
            }
            (_, ExprType::List(inner), Some(static_value))
                if *inner == ExprType::String =>
            {
                let mut errs = Errs::<ConfigError>::new();
                let items = static_value.unwrap_list();
                let mut formats =
                    Vec::<ChecksumFormat>::with_capacity(items.len());
                for item in items.iter() {
                    if let Some(format) = errs.ok(self.parse_format(
                        item.unwrap_str_ref(),
                        attribute,
                        expr_span,
                    )) {
                        formats.push(format);
                    }
                }
                errs.result()?;
                debug_assert_eq!(formats.len(), formats.capacity());
                Ok(ConfigVariableOr::Static(Rc::from(
                    formats.into_boxed_slice(),
                )))
            }
            (expr, ExprType::String, None) => Ok(self.add_variable(expr)),
            (expr, ExprType::List(inner), None)
                if *inner == ExprType::String =>
            {
                Ok(self.add_variable(expr))
            }
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute,
                expr_span,
                expr_type,
                valid_types: vec![
                    ExprType::String,
                    ExprType::List(Rc::from(ExprType::String)),
                ],
            })),
        }
    }

    fn variable_checksum_format_attr(
        &mut self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<ChecksumFormat>> {
        let expr_span = expr_ast.span;
        let variable = self.variable_str_attr(attribute, expr_ast)?;
        variable.try_map_static(|string| {
            self.parse_format(&string, attribute, expr_span)
        })
    }

    fn parse_format(
        &mut self,
        string: &Rc<str>,
        attribute: ConfigAttr,
        expr_span: SrcSpan,
    ) -> ConfigResult<ChecksumFormat> {
        string.parse::<ChecksumFormat>().map_err(|()| {
            Errs::one(ConfigError::InvalidChecksumFormatAttr {
                attribute,
                expr_span,
                expr_value: string.clone(),
            })
        })
    }

    fn static_fill_attr(
        &self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<u8> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(attribute, expr_ast)?;
        u8::try_from(&bigint).map_err(|_| {
            Errs::one(ConfigError::OutOfRangeAttr {
                attribute,
                expr_span,
                value: bigint,
            })
        })
    }

    fn variable_u64_attr(
        &mut self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Some(static_value)) => {
                let bigint = static_value.unwrap_int_ref();
                let uint64 = bigint.to_u64().ok_or_else(|| {
                    Errs::one(ConfigError::OutOfRangeAttr {
                        attribute,
                        expr_span,
                        value: bigint.clone(),
                    })
                })?;
                Ok(ConfigVariableOr::Static(uint64))
            }
            (_, ExprType::Label, Some(static_value)) => {
                Ok(self.add_variable(ObjExpr::from(static_value)))
            }
            (expr, ExprType::Integer | ExprType::Label, None) => {
                Ok(self.add_variable(expr))
            }
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute,
                expr_span,
                expr_type,
                valid_types: vec![ExprType::Integer, ExprType::Label],
            })),
        }
    }

    fn static_int_attr(
        &self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<BigInt> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Some(value)) => Ok(value.unwrap_int()),
            (_, ExprType::Integer, None) => {
                Err(Errs::one(ConfigError::NonStaticAttr {
                    attribute,
                    expr_span,
                }))
            }
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute,
                expr_span,
                expr_type,
                valid_types: vec![ExprType::Integer],
            })),
        }
    }

    fn variable_str_attr(
        &mut self,
        attribute: ConfigAttr,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<Rc<str>>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::String, Some(static_value)) => {
                Ok(ConfigVariableOr::Static(static_value.unwrap_str()))
            }
            (expr, ExprType::String, None) => Ok(self.add_variable(expr)),
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute,
                expr_span,
                expr_type,
                valid_types: vec![ExprType::String],
            })),
        }
    }

    fn typecheck_expression(
        &self,
        expr_ast: ExprAst,
    ) -> ConfigResult<(ObjExpr, ExprType, Option<ExprValue>)> {
        self.env.typecheck_expression(&expr_ast)
    }

    fn declare_entry(
        &mut self,
        entry_kind: &str,
        entry_id: &IdentifierAst,
    ) -> ConfigResult<()> {
        if let Some(decl) = self.env.get_declaration(&entry_id.name) {
            Err(Errs::one(ConfigError::DuplicateEntryName {
                entry_name: entry_id.name.clone(),
                entry_span: entry_id.span,
                prev_span: decl.id_span,
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

    fn declare_attr(
        &mut self,
        entry_name: &Rc<str>,
        prev_attrs: &mut PrevAttrs,
        id_ast: &IdentifierAst,
    ) -> ConfigResult<()> {
        if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
            Err(Errs::one(ConfigError::DuplicateAttrName {
                entry_name: entry_name.clone(),
                attr_name: id_ast.name.clone(),
                attr_span: id_ast.span,
                prev_span,
            }))
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
            Ok(())
        }
    }
}

//===========================================================================//
