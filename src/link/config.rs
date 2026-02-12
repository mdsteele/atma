use super::error::LinkError;
use super::expr::LinkTypeEnv;
use super::fragment::LinkFragment;
use super::patch::PatchedFile;
use super::positioned::PositionedBinary;
use crate::addr::{Addr, Align, AlignTryFromError, Range, Size};
use crate::expr::{ExprType, ExprValue};
use crate::obj::{ObjExpr, ObjFile};
use crate::parse::{
    ExprAst, IdentifierAst, LinkConfigAst, LinkDirectiveAst, LinkEntryAst,
    ParseError, ParseResult, SrcSpan,
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

struct ConfigBuilder {
    env: LinkTypeEnv,
    addrspaces: Vec<AddrspaceConfig>,
    memory: Vec<MemoryConfig>,
    sections: Vec<SectionConfig>,
    errors: Vec<ParseError>,
}

impl ConfigBuilder {
    pub(crate) fn new() -> ConfigBuilder {
        ConfigBuilder {
            env: LinkTypeEnv::new(),
            addrspaces: Vec::new(),
            memory: Vec::new(),
            sections: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub(crate) fn finish(self) -> ParseResult<LinkConfig> {
        if self.errors.is_empty() {
            Ok(LinkConfig {
                addrspaces: self.addrspaces,
                memory: self.memory,
                sections: self.sections,
            })
        } else {
            Err(self.errors)
        }
    }

    pub(crate) fn visit_directive(&mut self, dir_ast: LinkDirectiveAst) {
        match dir_ast {
            LinkDirectiveAst::Addrspaces(entries) => {
                self.visit_addrspaces_dir(entries)
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

    fn visit_addrspaces_dir(&mut self, entries: Vec<LinkEntryAst>) {
        for entry in entries {
            self.visit_addrspaces_entry(entry);
        }
    }

    fn visit_addrspaces_entry(&mut self, entry: LinkEntryAst) {
        self.declare_entry(ENTITY_ADDRSPACE, &entry.id);
        let mut bits: Option<u32> = None;
        let mut fill: Option<u8> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            self.declare_attr(&entry.id.name, &mut prev_attrs, &id_ast);
            match &*id_ast.name {
                "bits" => bits = Some(self.addrspace_bits_attr(expr_ast)),
                "fill" => fill = Some(self.addrspace_fill_attr(expr_ast)),
                _ => self.invalid_attr_error(ENTITY_ADDRSPACE, id_ast),
            }
        }
        if bits.is_none() {
            self.missing_attr_error("bits", &entry.id);
        }
        self.addrspaces.push(AddrspaceConfig {
            name: entry.id.name,
            bits: bits.unwrap_or(u32::BITS),
            fill: fill.unwrap_or_default(),
        });
    }

    fn addrspace_bits_attr(&mut self, expr_ast: ExprAst) -> u32 {
        let expr_span = expr_ast.span;
        let mut bits = Addr::BITS;
        if let Some(bigint) =
            self.static_int_attr(ENTITY_ADDRSPACE, "bits", expr_ast)
        {
            match bigint.to_u32() {
                Some(int) if (1..=Addr::BITS).contains(&int) => {
                    bits = int;
                }
                _ => self.out_of_range_attr_error(
                    ENTITY_ADDRSPACE,
                    "bits",
                    expr_span,
                    &bigint,
                ),
            }
        }
        bits
    }

    fn addrspace_fill_attr(&mut self, expr_ast: ExprAst) -> u8 {
        self.static_fill_attr(ENTITY_ADDRSPACE, expr_ast)
    }

    fn visit_let_dir(&mut self, id_ast: IdentifierAst, expr_ast: ExprAst) {
        let (_expr, var_type, static_value) =
            self.typecheck_expression(expr_ast);
        // TODO: save expr to be evaluated later
        self.env.add_declaration(id_ast, var_type, static_value);
    }

    fn visit_memory_dir(&mut self, entries: Vec<LinkEntryAst>) {
        for entry in entries {
            self.visit_memory_entry(entry);
        }
    }

    fn visit_memory_entry(&mut self, entry: LinkEntryAst) {
        self.declare_entry(ENTITY_MEMORY, &entry.id);
        let mut space: Option<Rc<str>> = None;
        let mut start: Option<Addr> = None;
        let mut size: Option<Size> = None;
        let mut fill: Option<u8> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            self.declare_attr(&entry.id.name, &mut prev_attrs, &id_ast);
            match &*id_ast.name {
                "fill" => fill = Some(self.memory_fill_attr(expr_ast)),
                "size" => size = Some(self.memory_size_attr(expr_ast)),
                "space" => space = Some(self.memory_space_attr(expr_ast)),
                "start" => start = Some(self.memory_start_attr(expr_ast)),
                _ => self.invalid_attr_error(ENTITY_MEMORY, id_ast),
            }
        }
        if space.is_none() {
            self.missing_attr_error("space", &entry.id);
        }
        if start.is_none() {
            self.missing_attr_error("start", &entry.id);
        }
        if size.is_none() {
            self.missing_attr_error("size", &entry.id);
        }
        let range = match (start, size) {
            (Some(start), Some(size)) => match start.range_with_size(size) {
                Some(range) => range,
                None => {
                    self.memory_range_overflow_error(&entry.id, start, size);
                    start.range_within(Align::MAX)
                }
            },
            (Some(start), None) => start.range_within(Align::MAX),
            (None, Some(size)) => Addr::MIN.range_with_size(size).unwrap(),
            (None, None) => Range::FULL,
        };
        self.memory.push(MemoryConfig {
            name: entry.id.name,
            space: space.unwrap_or_default(),
            range,
            fill,
        });
    }

    fn memory_fill_attr(&mut self, expr_ast: ExprAst) -> u8 {
        self.static_fill_attr(ENTITY_MEMORY, expr_ast)
    }

    fn memory_size_attr(&mut self, expr_ast: ExprAst) -> Size {
        let expr_span = expr_ast.span;
        if let Some(bigint) =
            self.static_int_attr(ENTITY_MEMORY, "size", expr_ast)
        {
            match Size::try_from(&bigint) {
                Ok(size) if size > Size::ZERO => return size,
                _ => self.out_of_range_attr_error(
                    ENTITY_MEMORY,
                    "size",
                    expr_span,
                    &bigint,
                ),
            }
        }
        Size::from(1u8)
    }

    fn memory_space_attr(&mut self, expr_ast: ExprAst) -> Rc<str> {
        self.static_entity_attr(
            ENTITY_MEMORY,
            "space",
            expr_ast,
            ENTITY_ADDRSPACE,
        )
        .unwrap_or_default()
    }

    fn memory_start_attr(&mut self, expr_ast: ExprAst) -> Addr {
        let expr_span = expr_ast.span;
        if let Some(bigint) =
            self.static_int_attr(ENTITY_MEMORY, "start", expr_ast)
        {
            match Addr::try_from(&bigint) {
                Ok(addr) => return addr,
                Err(()) => self.out_of_range_attr_error(
                    ENTITY_MEMORY,
                    "start",
                    expr_span,
                    &bigint,
                ),
            }
        }
        Addr::MIN
    }

    fn visit_sections_dir(&mut self, entries: Vec<LinkEntryAst>) {
        for entry in entries {
            self.visit_section_entry(entry);
        }
    }

    fn visit_section_entry(&mut self, entry: LinkEntryAst) {
        self.declare_entry(ENTITY_SECTION, &entry.id);
        let mut load: Option<Rc<str>> = None;
        let mut start: Option<Addr> = None;
        let mut align: Option<Align> = None;
        let mut within: Option<Align> = None;
        let mut fill: Option<u8> = None;
        let mut prev_attrs = PrevAttrs::new();
        for (id_ast, expr_ast) in entry.attrs {
            self.declare_attr(&entry.id.name, &mut prev_attrs, &id_ast);
            match &*id_ast.name {
                "align" => align = Some(self.section_align_attr(expr_ast)),
                "fill" => fill = Some(self.section_fill_attr(expr_ast)),
                "load" => load = Some(self.section_load_attr(expr_ast)),
                "start" => start = Some(self.section_start_attr(expr_ast)),
                "within" => within = Some(self.section_within_attr(expr_ast)),
                _ => self.invalid_attr_error(ENTITY_SECTION, id_ast),
            }
        }
        if load.is_none() {
            self.missing_attr_error("load", &entry.id);
        }
        self.sections.push(SectionConfig {
            name: entry.id.name,
            load: load.unwrap_or_default(),
            start,
            align: align.unwrap_or_default(),
            within,
            fill,
        });
    }

    fn section_align_attr(&mut self, expr_ast: ExprAst) -> Align {
        self.static_align_attr(ENTITY_SECTION, "align", expr_ast)
    }

    fn section_fill_attr(&mut self, expr_ast: ExprAst) -> u8 {
        self.static_fill_attr(ENTITY_SECTION, expr_ast)
    }

    fn section_load_attr(&mut self, expr_ast: ExprAst) -> Rc<str> {
        self.static_entity_attr(
            ENTITY_SECTION,
            "load",
            expr_ast,
            ENTITY_MEMORY,
        )
        .unwrap_or_default()
    }

    fn section_start_attr(&mut self, expr_ast: ExprAst) -> Addr {
        let expr_span = expr_ast.span;
        if let Some(bigint) =
            self.static_int_attr(ENTITY_SECTION, "start", expr_ast)
        {
            match Addr::try_from(&bigint) {
                Ok(addr) => return addr,
                Err(()) => self.out_of_range_attr_error(
                    ENTITY_SECTION,
                    "start",
                    expr_span,
                    &bigint,
                ),
            }
        }
        Addr::default()
    }

    fn section_within_attr(&mut self, expr_ast: ExprAst) -> Align {
        self.static_align_attr(ENTITY_SECTION, "within", expr_ast)
    }

    fn static_align_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> Align {
        let expr_span = expr_ast.span;
        if let Some(bigint) =
            self.static_int_attr(entry_kind, attr_name, expr_ast)
        {
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
                        ParseError::new(expr_span, message)
                            .with_label(expr_span, label),
                    );
                }
            }
        }
        Align::default()
    }

    fn static_entity_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
        entity_kind: &str,
    ) -> Option<Rc<str>> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast) {
            (_, ExprType::Entity(kind), static_value)
                if &*kind == entity_kind =>
            {
                if let Some(value) = static_value {
                    Some(value.unwrap_entity())
                } else {
                    self.non_static_attr_error(
                        entry_kind, attr_name, expr_span,
                    );
                    None
                }
            }
            (_, ExprType::Bottom, _) => None,
            (_, expr_type, _) => {
                let message = format!(
                    "{entry_kind} `{attr_name}` must have \
                                       type {entity_kind}"
                );
                let label = format!("this expression has type {expr_type}");
                self.errors.push(
                    ParseError::new(expr_span, message)
                        .with_label(expr_span, label),
                );
                None
            }
        }
    }

    fn static_fill_attr(&mut self, entry_kind: &str, expr_ast: ExprAst) -> u8 {
        let expr_span = expr_ast.span;
        if let Some(bigint) =
            self.static_int_attr(entry_kind, "fill", expr_ast)
        {
            match u8::try_from(&bigint) {
                Ok(byte) => return byte,
                Err(_) => self.out_of_range_attr_error(
                    entry_kind, "fill", expr_span, &bigint,
                ),
            }
        }
        u8::default()
    }

    fn static_int_attr(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_ast: ExprAst,
    ) -> Option<BigInt> {
        let expr_span = expr_ast.span;
        match self.typecheck_expression(expr_ast) {
            (_, ExprType::Integer, Some(value)) => Some(value.unwrap_int()),
            (_, ExprType::Integer, None) => {
                self.non_static_attr_error(entry_kind, attr_name, expr_span);
                None
            }
            (_, ExprType::Bottom, _) => None,
            (_, expr_type, _) => {
                let message =
                    format!("{entry_kind} `{attr_name}` must be an integer");
                let label = format!("this expression has type {expr_type}");
                self.errors.push(
                    ParseError::new(expr_span, message)
                        .with_label(expr_span, label),
                );
                None
            }
        }
    }

    fn typecheck_expression(
        &mut self,
        expr_ast: ExprAst,
    ) -> (ObjExpr, ExprType, Option<ExprValue>) {
        match self.env.typecheck_expression(&expr_ast) {
            Ok(typechecked) => typechecked,
            Err(mut errors) => {
                self.errors.append(&mut errors);
                (ObjExpr::from(false), ExprType::Bottom, None)
            }
        }
    }

    fn declare_entry(&mut self, entry_kind: &str, entry_id: &IdentifierAst) {
        if let Some(decl) = self.env.get_declaration(&entry_id.name) {
            self.duplicate_id_error(entry_id, decl.id_span);
        } else {
            self.env.add_declaration(
                entry_id.clone(),
                ExprType::Entity(Rc::from(entry_kind)),
                Some(ExprValue::Entity(entry_id.name.clone())),
            );
        }
    }

    fn declare_attr(
        &mut self,
        entry_name: &str,
        prev_attrs: &mut PrevAttrs,
        id_ast: &IdentifierAst,
    ) {
        if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
            let message = format!(
                "Duplicate `{}` attribute for `{entry_name}`",
                id_ast.name
            );
            let label1 = "Previously declared here".to_string();
            let label2 = "Duplicated here".to_string();
            self.errors.push(
                ParseError::new(id_ast.span, message)
                    .with_label(prev_span, label1)
                    .with_label(id_ast.span, label2),
            );
        } else {
            prev_attrs.insert(id_ast.name.clone(), id_ast.span);
        }
    }

    fn duplicate_id_error(&mut self, id: &IdentifierAst, prev_span: SrcSpan) {
        let message = format!("`{}` was already declared", id.name);
        let label1 = "Previously declared here".to_string();
        let label2 = "Declared again here".to_string();
        self.errors.push(
            ParseError::new(id.span, message)
                .with_label(prev_span, label1)
                .with_label(id.span, label2),
        );
    }

    fn invalid_attr_error(
        &mut self,
        entry_kind: &str,
        attr_id: IdentifierAst,
    ) {
        let message =
            format!("Invalid {entry_kind} attribute: `{}`", attr_id.name);
        self.errors.push(ParseError::new(attr_id.span, message));
    }

    fn memory_range_overflow_error(
        &mut self,
        entry_id: &IdentifierAst,
        start: Addr,
        size: Size,
    ) {
        let message = format!(
            "Memory range overflows address size \
             (start=${start:x}, size=${size:x})"
        );
        self.errors.push(ParseError::new(entry_id.span, message));
    }

    fn missing_attr_error(&mut self, attr: &str, entry_id: &IdentifierAst) {
        let message = format!("Missing required `{attr}` attribute");
        self.errors.push(ParseError::new(entry_id.span, message));
    }

    fn non_static_attr_error(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_span: SrcSpan,
    ) {
        let message =
            format!("{entry_kind} `{attr_name}` attribute must be static");
        let label = "this expression isn't static".to_string();
        self.errors.push(
            ParseError::new(expr_span, message).with_label(expr_span, label),
        );
    }

    fn out_of_range_attr_error(
        &mut self,
        entry_kind: &str,
        attr_name: &str,
        expr_span: SrcSpan,
        value: &BigInt,
    ) {
        let message = format!("{entry_kind} `{attr_name}` is out of range");
        let label = format!("the value of this expression is {value}");
        self.errors.push(
            ParseError::new(expr_span, message).with_label(expr_span, label),
        );
    }
}

//===========================================================================//

/// A linker configuration for a binary.
#[derive(Debug)]
pub struct LinkConfig {
    /// Configurations for the address spaces that memory regions exist in.
    pub addrspaces: Vec<AddrspaceConfig>,
    /// Configurations for the memory regions that sections will be placed
    /// within.
    pub memory: Vec<MemoryConfig>,
    /// Configurations for the data sections to be linked together.
    pub sections: Vec<SectionConfig>,
}

impl LinkConfig {
    /// Parses source code into a linker configuration.
    pub fn from_source(source: &str) -> ParseResult<LinkConfig> {
        let ast = LinkConfigAst::parse_source(source)?;
        let mut builder = ConfigBuilder::new();
        for dir_ast in ast.directives {
            builder.visit_directive(dir_ast);
        }
        builder.finish()
    }

    /// Given a set of object files, patches all chunks and returns them in the
    /// order in which they appear in the final binary.
    pub fn link_objects(
        &self,
        object_files: Vec<ObjFile>,
    ) -> Result<Vec<LinkFragment>, Vec<LinkError>> {
        let positioned_binary =
            PositionedBinary::position(self, &object_files)?;
        let patched_files = PatchedFile::patch_all(
            object_files,
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
pub struct MemoryConfig {
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
    /// The name of the memory region that this section should be loaded into.
    pub load: Rc<str>,
    /// If set, then the section must start at exactly this address.
    pub start: Option<Addr>,
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
