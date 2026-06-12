use super::error::LinkResult;
use super::expr::LinkTypeEnv;
use super::fragment::LinkFragment;
use super::patch::PatchedFile;
use super::positioned::PositionedBinary;
use crate::addr::{Addr, Align, AlignTryFromError, Range, Size};
use crate::error::{Errs, SourceError, SourceResult, SrcSpan};
use crate::expr::{ExprType, ExprValue};
use crate::obj::{ObjExpr, ObjFile};
use crate::parse::{
    ExprAst, IdentifierAst, LinkConfigAst, LinkDirectiveAst, LinkEntryAst,
};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

const ENTITY_ADDRSPACE: &str = "address space";
const ENTITY_EXPORT: &str = "exported symbol";
const ENTITY_MEMORY: &str = "memory region";
const ENTITY_SECTION: &str = "section";

type PrevAttrs = HashMap<Rc<str>, SrcSpan>;

//===========================================================================//

struct ConfigBuilder {
    env: LinkTypeEnv,
    addrspaces: Vec<AddrspaceConfig>,
    bss: Vec<RegionConfig>,
    memory: Vec<RegionConfig>,
    sections: Vec<SectionConfig>,
    exports: Vec<ExportConfig>,
}

impl ConfigBuilder {
    pub(crate) fn new() -> ConfigBuilder {
        ConfigBuilder {
            env: LinkTypeEnv::new(),
            addrspaces: Vec::new(),
            bss: Vec::new(),
            memory: Vec::new(),
            sections: Vec::new(),
            exports: Vec::new(),
        }
    }

    pub(crate) fn build(
        mut self,
        ast: LinkConfigAst,
    ) -> SourceResult<LinkConfig> {
        let mut errors = Errs::<SourceError>::new();
        for dir_ast in ast.directives {
            errors.also(self.visit_directive(dir_ast));
        }
        errors.result()?;
        Ok(LinkConfig {
            addrspaces: self.addrspaces,
            bss: self.bss,
            memory: self.memory,
            sections: self.sections,
            exports: self.exports,
        })
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
            LinkDirectiveAst::Exports(entries) => {
                self.visit_exports_dir(entries)
            }
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
        self.addrspaces.push(AddrspaceConfig {
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
        // TODO: error if this name is already exported
        let mut space: Option<Rc<str>> = None;
        let mut addr: Option<Addr> = None;
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
        self.exports.push(ExportConfig {
            name: entry.id.name,
            space: space.unwrap_or_default(),
            address: addr.unwrap_or_default(),
        });
        errs.result()
    }

    fn export_addr_attr(&mut self, expr_ast: ExprAst) -> SourceResult<Addr> {
        // TODO: allow addr to be non-static
        let expr_span = expr_ast.span;
        let bigint = self.static_int_attr(ENTITY_EXPORT, "addr", expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(self.out_of_range_attr_error(
                ENTITY_EXPORT,
                "addr",
                expr_span,
                &bigint,
            ))
        })
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

    fn visit_let_dir(
        &mut self,
        id_ast: IdentifierAst,
        expr_ast: ExprAst,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        let (var_type, static_value) =
            match self.typecheck_expression(expr_ast) {
                Ok((_expr, var_type, static_value)) => {
                    // TODO: save expr to be evaluated later
                    (var_type, static_value)
                }
                Err(errors) => {
                    errs.append(errors);
                    (ExprType::Bottom, None)
                }
            };
        self.env.add_declaration(id_ast, var_type, static_value);
        errs.result()
    }

    fn visit_bss_dir(
        &mut self,
        entries: Vec<LinkEntryAst>,
    ) -> SourceResult<()> {
        let mut errs = Errs::<SourceError>::new();
        for entry in entries {
            let region = errs.with(self.visit_region_entry(entry));
            self.bss.push(region);
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
            self.memory.push(region);
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
        self.sections.push(SectionConfig {
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
                    "{entry_kind} `{attr_name}` must have \
                                       type {entity_kind}"
                );
                let label = format!("this expression has type {expr_type}");
                Err(Errs::one(
                    SourceError::new(expr_span, message)
                        .with_label(expr_span, label),
                ))
            }
        }
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
            (_, expr_type, _) => {
                let message =
                    format!("{entry_kind} `{attr_name}` must be an integer");
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
            self.env.add_declaration(
                entry_id.clone(),
                ExprType::Entity(Rc::from(entry_kind)),
                Some(ExprValue::Entity(entry_id.name.clone())),
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

    fn duplicate_id_error(
        &mut self,
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
        &mut self,
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
        &mut self,
        attr: &str,
        entry_id: &IdentifierAst,
    ) -> SourceError {
        let message = format!("Missing required `{attr}` attribute");
        SourceError::new(entry_id.span, message)
    }

    fn non_static_attr_error(
        &mut self,
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
        &mut self,
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

/// A linker configuration for a binary.
#[derive(Debug)]
pub struct LinkConfig {
    /// Configurations for the address spaces that memory regions exist in.
    pub addrspaces: Vec<AddrspaceConfig>,
    /// Configurations for memory regions that padding-only sections can be
    /// placed within, and that will not be included in the final binary.
    pub bss: Vec<RegionConfig>,
    /// Configurations for memory regions that data sections can be placed
    /// within, and that will be included in the final binary.
    pub memory: Vec<RegionConfig>,
    /// Configurations for the data sections to be linked together.
    pub sections: Vec<SectionConfig>,
    /// Symbols defined and exported by the linker config.
    pub exports: Vec<ExportConfig>,
}

impl LinkConfig {
    /// Sources source code into a linker configuration.
    pub fn from_source(source: &str) -> SourceResult<LinkConfig> {
        let ast = LinkConfigAst::parse_source(source)
            .map_err(SourceError::from_errors)?;
        ConfigBuilder::new().build(ast)
    }

    /// Given a set of object files, patches all chunks and returns them in the
    /// order in which they appear in the final binary.
    pub fn link_objects(
        &self,
        object_files: Vec<ObjFile>,
    ) -> LinkResult<Vec<LinkFragment>> {
        let positioned_binary =
            PositionedBinary::position(self, &object_files)?;
        let patched_files = PatchedFile::patch_all(
            object_files,
            &positioned_binary.file_chunk_metadata,
            &positioned_binary.exported_symbols,
        )?;
        Ok(LinkFragment::from_patched_files(patched_files, &positioned_binary))
    }
}

//===========================================================================//

/// A linker configuration for a single address space.
#[derive(Debug)]
pub struct AddrspaceConfig {
    /// The name of this address space.
    pub name: Rc<str>,
    /// The number of address bits for this address space.
    pub bits: u32,
    /// Any padded portions of memory regions in this address space will be
    /// filled with this byte value by default.
    pub fill: u8,
}

//===========================================================================//

/// A linker configuration for a single memory region.
#[derive(Debug)]
pub struct RegionConfig {
    /// The name of this memory region.
    pub name: Rc<str>,
    /// The name of the address space that this memory region exists in.
    pub space: Rc<str>,
    /// The range of addresses covered by this memory region.
    pub range: Range,
    /// If set, then any padded portions of the memory region will be filled
    /// with this byte value. Otherwise, they will be filled with this region's
    /// address space's fill byte.
    pub fill: Option<u8>,
}

//===========================================================================//

/// A linker configuration for a single data section.
#[derive(Debug)]
pub struct SectionConfig {
    /// The name of this section.
    pub name: Rc<str>,
    /// The name of the memory region that this section should be placed in.
    pub region: Rc<str>,
    /// If set, then the section must start at exactly this address.
    pub start: Option<Addr>,
    /// If set, then the section will have exactly this size.
    pub size: Option<Size>,
    /// The required alignment for this section, within its address space.
    pub align: Align,
    /// If set, then this entire section must not cross any alignment boundary
    /// of this size within its address space.
    pub within: Option<Align>,
    /// If set, then any padded portions of the section will be filled with
    /// this byte value. Otherwise, they will be filled with this sections's
    /// memory region's fill byte.
    pub fill: Option<u8>,
}

//===========================================================================//

/// A linker configuration for a single exported symbol.
#[derive(Debug)]
pub struct ExportConfig {
    /// The name of the exported symbol.
    pub name: Rc<str>,
    /// The name of the address space that this symbol exists in.
    pub space: Rc<str>,
    /// The address of the symbol.
    pub address: Addr, // TODO: allow this to be a non-static expression
}

//===========================================================================//
