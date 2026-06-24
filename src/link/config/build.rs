use super::checksum::{ChecksumConfig, ChecksumFormat, ChecksumRange};
use super::env::LinkTypeEnv;
use super::{
    AddrspaceConfig, ConfigVariableOr, ExportConfig, LinkConfig, RegionConfig,
    SectionConfig,
};
use crate::addr::{Addr, Align, AlignTryFromError, Range, Size};
use crate::error::{Errs, SourceError, SourceResult, SrcSpan};
use crate::expr::{ExprLabel, ExprType, ExprValue};
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
const ENTITY_CHECKSUM: &str = "checksum";
const ENTITY_EXPORT: &str = "exported symbol";
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
    ) -> SourceResult<LinkConfig> {
        let mut errors = Errs::<SourceError>::new();
        for dir_ast in ast.directives {
            errors.also(self.visit_directive(dir_ast));
        }
        errors.result()?;
        Ok(self.config)
    }

    fn visit_directive(
        &mut self,
        dir_ast: LinkDirectiveAst,
    ) -> SourceResult<()> {
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
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            errs.also(self.visit_addrspaces_entry(entry));
        }
        errs.result()
    }

    fn visit_addrspaces_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
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
                _ => errs
                    .push(self.invalid_attr_error(ENTITY_ADDRSPACE, id_ast)),
            }
        }
        if bits.is_none() {
            errs.push(self.missing_attr_error("bits", &entry.id));
        }
        self.config.addrspaces.push(AddrspaceConfig {
            name: entry.id.name,
            bits: bits.unwrap_or(u32::BITS),
            fill: fill.unwrap_or_default(),
        });
        errs.result()
    }

    fn addrspace_bits_attr(&mut self, expr_ast: ExprAst) -> SourceResult<u32> {
        let expr_span = expr_ast.span;
        let bigint =
            self.static_int_attr(ENTITY_ADDRSPACE, "bits", expr_ast)?;
        match bigint.to_u32() {
            Some(int) if (1..=Addr::BITS).contains(&int) => Ok(int),
            _ => Err(Errs::one(self.out_of_range_attr_error(
                ENTITY_ADDRSPACE,
                "bits",
                expr_span,
                &bigint,
            ))),
        }
    }

    fn addrspace_fill_attr(&mut self, expr_ast: ExprAst) -> SourceResult<u8> {
        self.static_fill_attr(ENTITY_ADDRSPACE, expr_ast)
    }

    fn visit_checksums_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            errs.also(self.visit_checksums_entry(entry));
        }
        errs.result()
    }

    fn visit_checksums_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        if !self.symbol_is_exported(&entry.id.name) {
            errs.also(self.import_symbol(entry.id.clone()));
        }
        let mut sum: Option<ChecksumFormat> = None;
        let mut unit: Option<ChecksumFormat> = None;
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
                    sum = Some(errs.ok_or(
                        self.checksum_sum_attr(expr_ast),
                        ChecksumFormat::U8,
                    ))
                }
                "unit" => {
                    unit = Some(errs.ok_or(
                        self.checksum_unit_attr(expr_ast),
                        ChecksumFormat::U8,
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
                _ => errs.push(self.invalid_attr_error(ENTITY_EXPORT, id_ast)),
            }
        }
        if sum.is_none() {
            errs.push(self.missing_attr_error("sum", &entry.id));
        }
        let start = start.unwrap_or_default();
        let range = match (end, size) {
            (None, None) => ChecksumRange::From { start },
            (Some(end), None) => ChecksumRange::FromTo { start, end },
            (None, Some(size)) => ChecksumRange::FromSize { start, size },
            (Some(_end), Some(_size)) => {
                let message =
                    "a checksum cannot specify both `size` and `end`"
                        .to_string();
                let end_label = "`end` specified here".to_string();
                let size_label = "`size` specified here".to_string();
                let end_span = *prev_attrs.get("end").unwrap();
                let size_span = *prev_attrs.get("size").unwrap();
                errs.push(
                    SourceError::new(entry.id.span, message)
                        .with_label(size_span, size_label)
                        .with_label(end_span, end_label),
                );
                ChecksumRange::default()
            }
        };
        self.config.checksums.push(ChecksumConfig {
            name: entry.id.name,
            sum_format: sum.unwrap_or(ChecksumFormat::U8),
            unit_format: unit.unwrap_or(ChecksumFormat::U8),
            range,
        });
        errs.result()
    }

    fn checksum_sum_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<ChecksumFormat> {
        self.static_checksum_format_attr(ENTITY_CHECKSUM, "sum", expr_ast)
    }

    fn checksum_unit_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<ChecksumFormat> {
        self.static_checksum_format_attr(ENTITY_CHECKSUM, "unit", expr_ast)
    }

    fn checksum_start_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<ConfigVariableOr<u64>> {
        self.variable_u64_attr(ENTITY_CHECKSUM, "start", expr_ast)
    }

    fn checksum_end_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<ConfigVariableOr<u64>> {
        self.variable_u64_attr(ENTITY_CHECKSUM, "end", expr_ast)
    }

    fn checksum_size_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<ConfigVariableOr<u64>> {
        self.variable_u64_attr(ENTITY_CHECKSUM, "size", expr_ast)
    }

    fn visit_exports_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            errs.also(self.visit_exports_entry(entry));
        }
        errs.result()
    }

    fn visit_exports_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        if self.symbol_is_imported(&entry.id.name) {
            let message =
                "cannot import and export the same symbol".to_string();
            // TODO: indicate where the symbol was previously imported
            errs.push(SourceError::new(entry.id.span, message));
        }
        if self.symbol_is_exported(&entry.id.name) {
            let message = "cannot export the same symbol twice".to_string();
            // TODO: indicate where the symbol was previously exported
            errs.push(SourceError::new(entry.id.span, message));
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
                _ => errs.push(self.invalid_attr_error(ENTITY_EXPORT, id_ast)),
            }
        }
        if space.is_none() {
            errs.push(self.missing_attr_error("space", &entry.id));
        }
        if addr.is_none() {
            errs.push(self.missing_attr_error("addr", &entry.id));
        }
        let space = space.unwrap_or_default();
        let address = addr.unwrap_or_default();
        self.config.exports.push(ExportConfig {
            name: entry.id.name.clone(),
            space: space.clone(),
            address,
        });
        let address_value = address.map(|addr| {
            ExprValue::Label(ExprLabel::AddrAbsolute {
                space,
                address: BigInt::from(addr),
            })
        });
        self.env.add_declaration(entry.id, ExprType::Label, address_value);
        errs.result()
    }

    fn export_addr_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<ConfigVariableOr<Addr>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Some(static_value)) => {
                let bigint = static_value.unwrap_int_ref();
                let addr = Addr::try_from(bigint).map_err(|()| {
                    Errs::one(self.out_of_range_attr_error(
                        ENTITY_EXPORT,
                        "addr",
                        expr_span,
                        bigint,
                    ))
                })?;
                Ok(ConfigVariableOr::Static(addr))
            }
            (expr, ExprType::Integer, None) => {
                let index = self.config.variables.len();
                self.config.variables.push(expr);
                Ok(ConfigVariableOr::Variable(index))
            }
            (_, expr_type, _) => Err(Errs::one(self.int_attr_type_error(
                ENTITY_EXPORT,
                "addr",
                expr_span,
                &expr_type,
            ))),
        }
    }

    fn export_space_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<Rc<str>> {
        self.static_entity_attr(
            ENTITY_EXPORT,
            "space",
            expr_ast,
            ENTITY_ADDRSPACE,
        )
    }

    fn visit_import_dir(
        &mut self,
        id_asts: Vec<IdentifierAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for id_ast in id_asts {
            errs.also(self.import_symbol(id_ast));
        }
        errs.result()
    }

    fn import_symbol(&mut self, id_ast: IdentifierAst) -> SourceResult<()> {
        if self.symbol_is_exported(&id_ast.name) {
            let message =
                "cannot import and export the same symbol".to_string();
            // TODO: indicate where the symbol was previously exported
            return Err(Errs::one(SourceError::new(id_ast.span, message)));
        }
        if !self.symbol_is_imported(&id_ast.name) {
            self.config.imports.push(id_ast.name.clone());
        }
        let label = ExprLabel::SymbolRelative {
            name: id_ast.name.clone(),
            offset: BigInt::ZERO,
        };
        let value = ConfigVariableOr::Static(ExprValue::Label(label));
        self.env.add_declaration(id_ast, ExprType::Label, value);
        Ok(())
    }

    fn symbol_is_imported(&self, name: &str) -> bool {
        self.config.imports.iter().any(|import| name == &**import)
    }

    fn symbol_is_exported(&self, name: &str) -> bool {
        self.config.exports.iter().any(|export| name == &*export.name)
    }

    fn visit_let_dir(
        &mut self,
        id_ast: IdentifierAst,
        expr_ast: ExprAst,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        let (var_type, value) = match self.typecheck_expression(expr_ast) {
            Ok((_expr, var_type, Some(static_value))) => {
                (var_type, ConfigVariableOr::Static(static_value))
            }
            Ok((expr, var_type, None)) => {
                let index = self.config.variables.len();
                self.config.variables.push(expr);
                (var_type, ConfigVariableOr::Variable(index))
            }
            Err(errors) => {
                errs.append(errors);
                (
                    ExprType::Bottom,
                    ConfigVariableOr::Static(ExprValue::Boolean(false)),
                )
            }
        };
        self.env.add_declaration(id_ast, var_type, value);
        errs.result()
    }

    fn visit_bss_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            let region = errs.with(self.visit_region_entry(entry));
            self.config.bss.push(region);
        }
        errs.result()
    }

    fn visit_memory_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            let region = errs.with(self.visit_region_entry(entry));
            self.config.memory.push(region);
        }
        errs.result()
    }

    fn visit_region_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> (RegionConfig, Errs<SourceError>) {
        let mut errs = Errs::<SourceError>::new();
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
                _ => errs.push(self.invalid_attr_error(ENTITY_MEMORY, id_ast)),
            }
        }
        if space.is_none() {
            errs.push(self.missing_attr_error("space", &entry.id));
        }
        if start.is_none() {
            errs.push(self.missing_attr_error("start", &entry.id));
        }
        if size.is_none() {
            errs.push(self.missing_attr_error("size", &entry.id));
        }
        let range = match (start, size) {
            (Some(start), Some(size)) => {
                match start.range_with_size(size) {
                    Some(range) => range,
                    None => {
                        errs.push(self.memory_range_overflow_error(
                            &entry.id, start, size,
                        ));
                        start.range_within(Align::MAX)
                    }
                }
            }
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

    fn memory_fill_attr(&mut self, expr_ast: ExprAst) -> SourceResult<u8> {
        self.static_fill_attr(ENTITY_MEMORY, expr_ast)
    }

    fn memory_size_attr(&mut self, expr_ast: ExprAst) -> SourceResult<Size> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(ENTITY_MEMORY, "size", expr_ast)?;
        match Size::try_from(&bigint) {
            Ok(size) if size > Size::ZERO => Ok(size),
            _ => Err(Errs::one(self.out_of_range_attr_error(
                ENTITY_MEMORY,
                "size",
                expr_span,
                &bigint,
            ))),
        }
    }

    fn memory_space_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<Rc<str>> {
        self.static_entity_attr(
            ENTITY_MEMORY,
            "space",
            expr_ast,
            ENTITY_ADDRSPACE,
        )
    }

    fn memory_start_attr(&mut self, expr_ast: ExprAst) -> SourceResult<Addr> {
        let expr_span = expr_ast.span;
        // TODO: allow this to be a static exported label
        let bigint = self.static_int_attr(ENTITY_MEMORY, "start", expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(self.out_of_range_attr_error(
                ENTITY_MEMORY,
                "start",
                expr_span,
                &bigint,
            ))
        })
    }

    fn visit_sections_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            errs.also(self.visit_section_entry(entry));
        }
        errs.result()
    }

    fn visit_section_entry(
        &mut self,
        entry: LinkEntryAst,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
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
                _ => {
                    errs.push(self.invalid_attr_error(ENTITY_SECTION, id_ast))
                }
            }
        }
        if region.is_none() {
            errs.push(self.missing_attr_error("region", &entry.id));
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

    fn section_align_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<Align> {
        self.static_align_attr(ENTITY_SECTION, "align", expr_ast)
    }

    fn section_fill_attr(&mut self, expr_ast: ExprAst) -> SourceResult<u8> {
        self.static_fill_attr(ENTITY_SECTION, expr_ast)
    }

    fn section_region_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<Rc<str>> {
        self.static_entity_attr(
            ENTITY_SECTION,
            "region",
            expr_ast,
            ENTITY_MEMORY,
        )
    }

    fn section_start_attr(&mut self, expr_ast: ExprAst) -> SourceResult<Addr> {
        let expr_span = expr_ast.span;
        // TODO: allow this to be a static exported label
        let bigint =
            self.static_int_attr(ENTITY_SECTION, "start", expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(self.out_of_range_attr_error(
                ENTITY_SECTION,
                "start",
                expr_span,
                &bigint,
            ))
        })
    }

    fn section_size_attr(&mut self, expr_ast: ExprAst) -> SourceResult<Size> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(ENTITY_SECTION, "size", expr_ast)?;
        match Size::try_from(&bigint) {
            Ok(size) if size > Size::ZERO => Ok(size),
            _ => Err(Errs::one(self.out_of_range_attr_error(
                ENTITY_SECTION,
                "size",
                expr_span,
                &bigint,
            ))),
        }
    }

    fn section_within_attr(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<Align> {
        self.static_align_attr(ENTITY_SECTION, "within", expr_ast)
    }

    fn static_align_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> SourceResult<Align> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(entry_kind, attr_name, expr_ast)?;
        Align::try_from(&bigint).map_err(|error| {
            let message = match error {
                AlignTryFromError::NotAPowerOfTwo => {
                    format!("`{attr_name}` must be a power of two")
                }
                AlignTryFromError::TooLargePowerOfTwo => {
                    format!("`{attr_name}` must be at most {}", 0x8000_0000u32)
                }
            };
            let label = format!("the value of this expression is {bigint}");
            Errs::one(
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label),
            )
        })
    }

    fn static_entity_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
        entity_kind: &str,
    ) -> SourceResult<Rc<str>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Entity(kind), static_value)
                if &*kind == entity_kind =>
            {
                if let Some(value) = static_value {
                    Ok(value.unwrap_entity())
                } else {
                    Err(Errs::one(self.non_static_attr_error(
                        entry_kind, attr_name, expr_span,
                    )))
                }
            }
            (_, expr_type, _) => {
                let message = format!(
                    "{entry_kind} `{attr_name}` must have type {entity_kind}"
                );
                let label = format!("this expression has type {expr_type}");
                Err(Errs::one(
                    SourceError::new(expr_span, message)
                        .with_label(expr_span, label),
                ))
            }
        }
    }

    fn static_checksum_format_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> SourceResult<ChecksumFormat> {
        let expr_span = expr_ast.span;
        let string = self.static_str_attr(entry_kind, attr_name, expr_ast)?;
        string.parse::<ChecksumFormat>().map_err(|()| {
            let message = format!(
                "{entry_kind} `{attr_name}` must be a valid checksum format"
            );
            let label = format!("this value of this expression is {string}");
            Errs::one(
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label),
            )
        })
    }

    fn static_fill_attr(
        &mut self,
        entry_kind: &str,
        expr_ast: ExprAst,
    ) -> SourceResult<u8> {
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(entry_kind, "fill", expr_ast)?;
        u8::try_from(&bigint).map_err(|_| {
            Errs::one(self.out_of_range_attr_error(
                entry_kind, "fill", expr_span, &bigint,
            ))
        })
    }

    fn variable_u64_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> SourceResult<ConfigVariableOr<u64>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Some(static_value)) => {
                let bigint = static_value.unwrap_int_ref();
                let uint64 = bigint.to_u64().ok_or_else(|| {
                    Errs::one(self.out_of_range_attr_error(
                        entry_kind, attr_name, expr_span, bigint,
                    ))
                })?;
                Ok(ConfigVariableOr::Static(uint64))
            }
            (_, ExprType::Label, Some(static_value)) => {
                let index = self.config.variables.len();
                self.config.variables.push(ObjExpr::from(static_value));
                Ok(ConfigVariableOr::Variable(index))
            }
            (expr, ExprType::Integer | ExprType::Label, None) => {
                let index = self.config.variables.len();
                self.config.variables.push(expr);
                Ok(ConfigVariableOr::Variable(index))
            }
            // TODO: int_attr_type_error is wrong, since this can also be a
            // label
            (_, expr_type, _) => Err(Errs::one(self.int_attr_type_error(
                entry_kind, attr_name, expr_span, &expr_type,
            ))),
        }
    }

    fn static_int_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> SourceResult<BigInt> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Some(value)) => Ok(value.unwrap_int()),
            (_, ExprType::Integer, None) => Err(Errs::one(
                self.non_static_attr_error(entry_kind, attr_name, expr_span),
            )),
            (_, expr_type, _) => Err(Errs::one(self.int_attr_type_error(
                entry_kind, attr_name, expr_span, &expr_type,
            ))),
        }
    }

    fn static_str_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> SourceResult<Rc<str>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast)? {
            (_, ExprType::String, Some(value)) => Ok(value.unwrap_str()),
            (_, ExprType::String, None) => Err(Errs::one(
                self.non_static_attr_error(entry_kind, attr_name, expr_span),
            )),
            (_, expr_type, _) => {
                let message =
                    format!("{entry_kind} `{attr_name}` must be a string");
                let label = format!("this expression has type {expr_type}");
                Err(Errs::one(
                    SourceError::new(expr_span, message)
                        .with_label(expr_span, label),
                ))
            }
        }
    }

    fn typecheck_expression(
        &mut self,
        expr_ast: ExprAst,
    ) -> SourceResult<(ObjExpr, ExprType, Option<ExprValue>)> {
        self.env.typecheck_expression(&expr_ast)
    }

    fn declare_entry(
        &mut self,
        entry_kind: &str,
        entry_id: &IdentifierAst,
    ) -> SourceResult<()> {
        if let Some(decl) = self.env.get_declaration(&entry_id.name) {
            Err(Errs::one(self.duplicate_id_error(entry_id, decl.id_span)))
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
        entry_name: &str,
        prev_attrs: &mut PrevAttrs,
        id_ast: &IdentifierAst,
    ) -> SourceResult<()> {
        if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
            let message = format!(
                "Duplicate `{}` attribute for `{entry_name}`",
                id_ast.name
            );
            let label1 = "Previously declared here".to_string();
            let label2 = "Duplicated here".to_string();
            Err(Errs::one(
                SourceError::new(id_ast.span, message)
                    .with_label(prev_span, label1)
                    .with_label(id_ast.span, label2),
            ))
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
            Ok(())
        }
    }

    fn int_attr_type_error(
        &self,
        entry_kind: &str,
        attr_name: &str,
        expr_span: SrcSpan,
        expr_type: &ExprType,
    ) -> SourceError {
        let message = format!("{entry_kind} `{attr_name}` must be an integer");
        let label = format!("this expression has type {expr_type}");
        SourceError::new(expr_span, message).with_label(expr_span, label)
    }

    fn duplicate_id_error(
        &self,
        id: &IdentifierAst,
        prev_span: SrcSpan,
    ) -> SourceError {
        let message = format!("`{}` was already declared", id.name);
        let label1 = "Previously declared here".to_string();
        let label2 = "Declared again here".to_string();
        SourceError::new(id.span, message)
            .with_label(prev_span, label1)
            .with_label(id.span, label2)
    }

    fn invalid_attr_error(
        &self,
        entry_kind: &str,
        attr_id: IdentifierAst,
    ) -> SourceError {
        let message =
            format!("Invalid {entry_kind} attribute: `{}`", attr_id.name);
        SourceError::new(attr_id.span, message)
    }

    fn memory_range_overflow_error(
        &mut self,
        entry_id: &IdentifierAst,
        start: Addr,
        size: Size,
    ) -> SourceError {
        let message = format!(
            "Memory range overflows address size \
             (start=${start:x}, size=${size:x})"
        );
        SourceError::new(entry_id.span, message)
    }

    fn missing_attr_error(
        &self,
        attr: &str,
        entry_id: &IdentifierAst,
    ) -> SourceError {
        let message = format!("Missing required `{attr}` attribute");
        SourceError::new(entry_id.span, message)
    }

    fn non_static_attr_error(
        &self,
        entry_kind: &str,
        attr_name: &str,
        expr_span: SrcSpan,
    ) -> SourceError {
        let message =
            format!("{entry_kind} `{attr_name}` attribute must be static");
        let label = "this expression isn't static".to_string();
        SourceError::new(expr_span, message).with_label(expr_span, label)
    }

    fn out_of_range_attr_error(
        &self,
        entry_kind: &str,
        attr_name: &str,
        expr_span: SrcSpan,
        value: &BigInt,
    ) -> SourceError {
        let message = format!("{entry_kind} `{attr_name}` is out of range");
        let label = format!("the value of this expression is {value}");
        SourceError::new(expr_span, message).with_label(expr_span, label)
    }
}

//===========================================================================//
