use super::RegionKind;
use super::builtin::{
    ENTITY_REGION_KIND, REGION_KIND_BSS, REGION_KIND_DATA, REGION_KIND_OMIT,
};
use super::checksum::ChecksumFormat;
use super::env::ConfigTypeEnv;
use super::error::{ConfigAttr, ConfigEntryKind, ConfigError, ConfigResult};
use super::variable::ConfigVariableOr;
use crate::addr::{Addr, Align, Size};
use crate::error::{Errs, SrcSpan};
use crate::expr::ExprType;
use crate::obj::ObjExpr;
use crate::parse::{AttributeAst, ExprAst, IdentifierAst};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) const ENTITY_ADDRSPACE: &str = "address space";
pub(super) const ENTITY_REGION: &str = "memory region";
pub(super) const ENTITY_SECTION: &str = "section";

//===========================================================================//

#[derive(Default)]
pub(super) struct LinkAddrspaceAttrs {
    pub bits: Option<u32>,
    pub fill: Option<u8>,
}

impl LinkAddrspaceAttrs {
    pub(super) fn clone_with_overrides(&self, overrides: Self) -> Self {
        Self {
            bits: self.bits.or(overrides.bits),
            fill: self.fill.or(overrides.fill),
        }
    }

    pub(super) fn build(
        env: &ConfigTypeEnv,
        entry_name: &Rc<str>,
        attrs_ast: Vec<AttributeAst>,
    ) -> (Self, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        let mut attrs = LinkAddrspaceAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for attr_ast in attrs_ast {
            errs.also(declare_attr(
                env,
                entry_name,
                &mut prev_attrs,
                &attr_ast.id,
            ));
            match &*attr_ast.id.name {
                "bits" => {
                    let result = Self::bits_attr(env, attr_ast.expr);
                    attrs.bits = Some(errs.ok_or_default(result));
                }
                "fill" => {
                    let result = Self::fill_attr(env, attr_ast.expr);
                    attrs.fill = Some(errs.ok_or_default(result));
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Addrspace,
                    attr_name: attr_ast.id.name.clone(),
                    attr_loc: env.make_loc(attr_ast.id.span),
                }),
            }
        }
        (attrs, errs)
    }

    fn bits_attr(env: &ConfigTypeEnv, expr_ast: ExprAst) -> ConfigResult<u32> {
        let expr_span = expr_ast.span;
        let bigint =
            static_int_attr(env, ConfigAttr::AddrspaceBits, expr_ast)?;
        match bigint.to_u32() {
            Some(int) if (1..=Addr::BITS).contains(&int) => Ok(int),
            _ => Err(Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::AddrspaceBits,
                expr_loc: env.make_loc(expr_span),
                value: bigint,
            })),
        }
    }

    fn fill_attr(env: &ConfigTypeEnv, expr_ast: ExprAst) -> ConfigResult<u8> {
        static_byte_attr(env, ConfigAttr::AddrspaceFill, expr_ast)
    }
}

//===========================================================================//

#[derive(Default)]
pub(super) struct LinkChecksumAttrs {
    pub sum: Option<ConfigVariableOr<Rc<[ChecksumFormat]>>>,
    pub unit: Option<ConfigVariableOr<ChecksumFormat>>,
    pub start: Option<ConfigVariableOr<u64>>,
    pub end: Option<(SrcSpan, ConfigVariableOr<u64>)>,
    pub size: Option<(SrcSpan, ConfigVariableOr<u64>)>,
}

impl LinkChecksumAttrs {
    pub(super) fn clone_with_overrides(&self, overrides: Self) -> Self {
        Self {
            sum: self.sum.clone().or(overrides.sum),
            unit: self.unit.or(overrides.unit),
            start: self.start.or(overrides.start),
            end: self.end.or(overrides.end),
            size: self.size.or(overrides.size),
        }
    }

    pub(super) fn build(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        entry_name: &Rc<str>,
        attrs_ast: Vec<AttributeAst>,
    ) -> (Self, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        let mut attrs = LinkChecksumAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for attr_ast in attrs_ast {
            errs.also(declare_attr(
                env,
                entry_name,
                &mut prev_attrs,
                &attr_ast.id,
            ));
            match &*attr_ast.id.name {
                "sum" => {
                    attrs.sum = Some(errs.ok_or_else(
                        Self::sum_attr(env, vars, attr_ast.expr),
                        || ConfigVariableOr::Static(Rc::from([])),
                    ))
                }
                "unit" => {
                    let result = Self::unit_attr(env, vars, attr_ast.expr);
                    attrs.unit = Some(errs.ok_or_default(result));
                }
                "start" => {
                    let result = Self::start_attr(env, vars, attr_ast.expr);
                    attrs.start = Some(errs.ok_or_default(result));
                }
                "end" => {
                    let result = Self::end_attr(env, vars, attr_ast.expr);
                    let end = errs.ok_or_default(result);
                    attrs.end = Some((attr_ast.id.span, end));
                }
                "size" => {
                    let result = Self::size_attr(env, vars, attr_ast.expr);
                    let size = errs.ok_or_default(result);
                    attrs.size = Some((attr_ast.id.span, size));
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Checksum,
                    attr_name: attr_ast.id.name.clone(),
                    attr_loc: env.make_loc(attr_ast.id.span),
                }),
            }
        }
        (attrs, errs)
    }

    fn sum_attr(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<Rc<[ChecksumFormat]>>> {
        variable_checksum_formats_attr(
            env,
            vars,
            ConfigAttr::ChecksumSum,
            expr_ast,
        )
    }

    fn unit_attr(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<ChecksumFormat>> {
        variable_checksum_format_attr(
            env,
            vars,
            ConfigAttr::ChecksumUnit,
            expr_ast,
        )
    }

    fn start_attr(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        variable_u64_attr(env, vars, ConfigAttr::ChecksumStart, expr_ast)
    }

    fn end_attr(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        variable_u64_attr(env, vars, ConfigAttr::ChecksumEnd, expr_ast)
    }

    fn size_attr(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<u64>> {
        variable_u64_attr(env, vars, ConfigAttr::ChecksumSize, expr_ast)
    }
}

//===========================================================================//

#[derive(Default)]
pub(super) struct LinkExportAttrs {
    pub space: Option<Rc<str>>,
    pub addr: Option<ConfigVariableOr<Addr>>,
}

impl LinkExportAttrs {
    pub(super) fn clone_with_overrides(&self, overrides: Self) -> Self {
        Self {
            space: self.space.clone().or(overrides.space),
            addr: self.addr.or(overrides.addr),
        }
    }

    pub(super) fn build(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        entry_name: &Rc<str>,
        attrs_ast: Vec<AttributeAst>,
    ) -> (Self, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        let mut attrs = LinkExportAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for attr_ast in attrs_ast {
            errs.also(declare_attr(
                env,
                entry_name,
                &mut prev_attrs,
                &attr_ast.id,
            ));
            match &*attr_ast.id.name {
                "addr" => {
                    let result = Self::addr_attr(env, vars, attr_ast.expr);
                    attrs.addr = Some(errs.ok_or_default(result));
                }
                "space" => {
                    let result = Self::space_attr(env, attr_ast.expr);
                    attrs.space = Some(errs.ok_or_default(result));
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Export,
                    attr_name: attr_ast.id.name.clone(),
                    attr_loc: env.make_loc(attr_ast.id.span),
                }),
            }
        }
        (attrs, errs)
    }

    fn addr_attr(
        env: &ConfigTypeEnv,
        vars: &mut Vec<ObjExpr>,
        expr_ast: ExprAst,
    ) -> ConfigResult<ConfigVariableOr<Addr>> {
        let expr_span = expr_ast.span;
        match env.typecheck_expression(expr_ast)? {
            (_, ExprType::Integer, Ok(static_value)) => {
                let bigint = static_value.unwrap_int_ref();
                let addr = Addr::try_from(bigint).map_err(|()| {
                    Errs::one(ConfigError::OutOfRangeAttr {
                        attribute: ConfigAttr::ExportAddr,
                        expr_loc: env.make_loc(expr_span),
                        value: bigint.clone(),
                    })
                })?;
                Ok(ConfigVariableOr::Static(addr))
            }
            (expr, ExprType::Integer, Err(_)) => {
                Ok(ConfigVariableOr::new_variable(vars, expr))
            }
            (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
                attribute: ConfigAttr::ExportAddr,
                expr_loc: env.make_loc(expr_span),
                expr_type,
                valid_types: vec![ExprType::Integer],
            })),
        }
    }

    fn space_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Rc<str>> {
        static_entity_attr(
            env,
            ConfigAttr::ExportSpace,
            expr_ast,
            ENTITY_ADDRSPACE,
        )
    }
}

//===========================================================================//

#[derive(Default)]
pub(super) struct LinkRegionAttrs {
    pub space: Option<Rc<str>>,
    pub start: Option<Addr>,
    pub size: Option<Size>,
    pub fill: Option<u8>,
    pub kind: Option<RegionKind>,
}

impl LinkRegionAttrs {
    pub(super) fn clone_with_overrides(&self, overrides: Self) -> Self {
        Self {
            space: self.space.clone().or(overrides.space),
            start: self.start.or(overrides.start),
            size: self.size.or(overrides.size),
            fill: self.fill.or(overrides.fill),
            kind: self.kind.or(overrides.kind),
        }
    }

    pub(super) fn build(
        env: &ConfigTypeEnv,
        entry_name: &Rc<str>,
        attrs_ast: Vec<AttributeAst>,
    ) -> (Self, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        let mut attrs = LinkRegionAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for attr_ast in attrs_ast {
            errs.also(declare_attr(
                env,
                entry_name,
                &mut prev_attrs,
                &attr_ast.id,
            ));
            match &*attr_ast.id.name {
                "fill" => {
                    let result = Self::fill_attr(env, attr_ast.expr);
                    attrs.fill = Some(errs.ok_or_default(result));
                }
                "kind" => {
                    let result = Self::kind_attr(env, attr_ast.expr);
                    attrs.kind = Some(errs.ok_or_default(result));
                }
                "size" => {
                    let result = Self::size_attr(env, attr_ast.expr);
                    attrs.size = Some(errs.ok_or(result, Size::from(1u32)));
                }
                "space" => {
                    let result = Self::space_attr(env, attr_ast.expr);
                    attrs.space = Some(errs.ok_or_default(result));
                }
                "start" => {
                    let result = Self::start_attr(env, attr_ast.expr);
                    attrs.start = Some(errs.ok_or_default(result));
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Region,
                    attr_name: attr_ast.id.name.clone(),
                    attr_loc: env.make_loc(attr_ast.id.span),
                }),
            }
        }
        (attrs, errs)
    }

    fn fill_attr(env: &ConfigTypeEnv, expr_ast: ExprAst) -> ConfigResult<u8> {
        static_byte_attr(env, ConfigAttr::RegionFill, expr_ast)
    }

    fn kind_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<RegionKind> {
        let kind_str = static_entity_attr(
            env,
            ConfigAttr::RegionKind,
            expr_ast,
            ENTITY_REGION_KIND,
        )?;
        match &*kind_str {
            REGION_KIND_BSS => Ok(RegionKind::Bss),
            REGION_KIND_DATA => Ok(RegionKind::Data),
            REGION_KIND_OMIT => Ok(RegionKind::Omit),
            _ => unreachable!(),
        }
    }

    fn size_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Size> {
        let expr_span = expr_ast.span;
        let bigint = static_int_attr(env, ConfigAttr::RegionSize, expr_ast)?;
        match Size::try_from(&bigint) {
            Ok(size) if size > Size::ZERO => Ok(size),
            _ => Err(Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::RegionSize,
                expr_loc: env.make_loc(expr_span),
                value: bigint,
            })),
        }
    }

    fn space_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Rc<str>> {
        static_entity_attr(
            env,
            ConfigAttr::RegionSpace,
            expr_ast,
            ENTITY_ADDRSPACE,
        )
    }

    fn start_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Addr> {
        let expr_span = expr_ast.span;
        let bigint = static_int_attr(env, ConfigAttr::RegionStart, expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::RegionStart,
                expr_loc: env.make_loc(expr_span),
                value: bigint,
            })
        })
    }
}

//===========================================================================//

#[derive(Default)]
pub(super) struct LinkSectionAttrs {
    pub region: Option<Rc<str>>,
    pub start: Option<Addr>,
    pub size: Option<Size>,
    pub align: Option<Align>,
    pub within: Option<Align>,
    pub fill: Option<u8>,
}

impl LinkSectionAttrs {
    pub(super) fn clone_with_overrides(&self, overrides: Self) -> Self {
        Self {
            region: self.region.clone().or(overrides.region),
            start: self.start.or(overrides.start),
            size: self.size.or(overrides.size),
            align: self.align.or(overrides.align),
            within: self.within.or(overrides.within),
            fill: self.fill.or(overrides.fill),
        }
    }

    pub(super) fn build(
        env: &ConfigTypeEnv,
        entry_name: &Rc<str>,
        attrs_ast: Vec<AttributeAst>,
    ) -> (Self, Errs<ConfigError>) {
        let mut errs = Errs::<ConfigError>::new();
        let mut attrs = LinkSectionAttrs::default();
        let mut prev_attrs = HashMap::<Rc<str>, SrcSpan>::new();
        for attr_ast in attrs_ast {
            errs.also(declare_attr(
                env,
                entry_name,
                &mut prev_attrs,
                &attr_ast.id,
            ));
            match &*attr_ast.id.name {
                "align" => {
                    let result = Self::align_attr(env, attr_ast.expr);
                    attrs.align = Some(errs.ok_or_default(result));
                }
                "fill" => {
                    let result = Self::fill_attr(env, attr_ast.expr);
                    attrs.fill = Some(errs.ok_or_default(result));
                }
                "region" => {
                    let result = Self::region_attr(env, attr_ast.expr);
                    attrs.region = Some(errs.ok_or_default(result));
                }
                "size" => {
                    let result = Self::size_attr(env, attr_ast.expr);
                    attrs.size = Some(errs.ok_or(result, Size::from(1u32)));
                }
                "start" => {
                    let result = Self::start_attr(env, attr_ast.expr);
                    attrs.start = Some(errs.ok_or_default(result));
                }
                "within" => {
                    let result = Self::within_attr(env, attr_ast.expr);
                    attrs.within = Some(errs.ok_or(result, Align::MAX));
                }
                _ => errs.push(ConfigError::InvalidAttrName {
                    entry_kind: ConfigEntryKind::Section,
                    attr_name: attr_ast.id.name.clone(),
                    attr_loc: env.make_loc(attr_ast.id.span),
                }),
            }
        }
        (attrs, errs)
    }

    fn align_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Align> {
        static_align_attr(env, ConfigAttr::SectionAlign, expr_ast)
    }

    fn fill_attr(env: &ConfigTypeEnv, expr_ast: ExprAst) -> ConfigResult<u8> {
        static_byte_attr(env, ConfigAttr::SectionFill, expr_ast)
    }

    fn region_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Rc<str>> {
        static_entity_attr(
            env,
            ConfigAttr::SectionRegion,
            expr_ast,
            ENTITY_REGION,
        )
    }

    fn start_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Addr> {
        let expr_span = expr_ast.span;
        // TODO: allow this to be a static exported label
        let bigint = static_int_attr(env, ConfigAttr::SectionStart, expr_ast)?;
        Addr::try_from(&bigint).map_err(|()| {
            Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::SectionStart,
                expr_loc: env.make_loc(expr_span),
                value: bigint,
            })
        })
    }

    fn size_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Size> {
        let expr_span = expr_ast.span;
        let bigint = static_int_attr(env, ConfigAttr::SectionSize, expr_ast)?;
        match Size::try_from(&bigint) {
            Ok(size) if size > Size::ZERO => Ok(size),
            _ => Err(Errs::one(ConfigError::OutOfRangeAttr {
                attribute: ConfigAttr::SectionSize,
                expr_loc: env.make_loc(expr_span),
                value: bigint,
            })),
        }
    }

    fn within_attr(
        env: &ConfigTypeEnv,
        expr_ast: ExprAst,
    ) -> ConfigResult<Align> {
        static_align_attr(env, ConfigAttr::SectionWithin, expr_ast)
    }
}

//===========================================================================//

fn declare_attr(
    env: &ConfigTypeEnv,
    entry_name: &Rc<str>,
    prev_attrs: &mut HashMap<Rc<str>, SrcSpan>,
    id_ast: &IdentifierAst,
) -> ConfigResult<()> {
    if let Some(&prev_span) = prev_attrs.get(&id_ast.name) {
        Err(Errs::one(ConfigError::DuplicateAttrName {
            entry_name: entry_name.clone(),
            attr_name: id_ast.name.clone(),
            attr_loc: env.make_loc(id_ast.span),
            prev_span,
        }))
    } else {
        prev_attrs.insert(id_ast.name.clone(), id_ast.span);
        Ok(())
    }
}

fn parse_checksum_format(
    env: &ConfigTypeEnv,
    string: &Rc<str>,
    attribute: ConfigAttr,
    expr_span: SrcSpan,
) -> ConfigResult<ChecksumFormat> {
    string.parse::<ChecksumFormat>().map_err(|()| {
        Errs::one(ConfigError::InvalidChecksumFormatAttr {
            attribute,
            expr_loc: env.make_loc(expr_span),
            expr_value: string.clone(),
        })
    })
}

//===========================================================================//

fn static_align_attr(
    env: &ConfigTypeEnv,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<Align> {
    let expr_span = expr_ast.span;
    let bigint = static_int_attr(env, attribute, expr_ast)?;
    Align::try_from(&bigint).map_err(|error| {
        Errs::one(ConfigError::InvalidAlignmentAttr {
            attribute,
            error,
            expr_loc: env.make_loc(expr_span),
            expr_value: bigint,
        })
    })
}

fn static_byte_attr(
    env: &ConfigTypeEnv,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<u8> {
    let expr_span = expr_ast.span;
    let bigint = static_int_attr(env, attribute, expr_ast)?;
    u8::try_from(&bigint).map_err(|_| {
        Errs::one(ConfigError::OutOfRangeAttr {
            attribute,
            expr_loc: env.make_loc(expr_span),
            value: bigint,
        })
    })
}

fn static_entity_attr(
    env: &ConfigTypeEnv,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
    entity_kind: &str,
) -> ConfigResult<Rc<str>> {
    let expr_span = expr_ast.span;
    let (_, expr_type, expr_static) = env.typecheck_expression(expr_ast)?;
    let expected_type = ExprType::Entity(Rc::from(entity_kind));
    if expr_type.is_subtype_of(&expected_type) {
        match expr_static {
            Ok(value) => Ok(value.unwrap_entity()),
            Err(reason) => Err(Errs::one(ConfigError::NonStaticAttr {
                attribute,
                expr_loc: env.make_loc(expr_span),
                reason,
            })),
        }
    } else {
        Err(Errs::one(ConfigError::AttrTypeError {
            attribute,
            expr_loc: env.make_loc(expr_span),
            expr_type,
            valid_types: vec![expected_type],
        }))
    }
}

fn static_int_attr(
    env: &ConfigTypeEnv,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<BigInt> {
    let expr_span = expr_ast.span;
    match env.typecheck_expression(expr_ast)? {
        (_, ExprType::Integer, Ok(value)) => Ok(value.unwrap_int()),
        (_, ExprType::Integer | ExprType::Bottom, Err(reason)) => {
            Err(Errs::one(ConfigError::NonStaticAttr {
                attribute,
                expr_loc: env.make_loc(expr_span),
                reason,
            }))
        }
        (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
            attribute,
            expr_loc: env.make_loc(expr_span),
            expr_type,
            valid_types: vec![ExprType::Integer],
        })),
    }
}

//===========================================================================//

fn variable_checksum_formats_attr(
    env: &ConfigTypeEnv,
    vars: &mut Vec<ObjExpr>,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<ConfigVariableOr<Rc<[ChecksumFormat]>>> {
    let expr_span = expr_ast.span;
    match env.typecheck_expression(expr_ast)? {
        (_, ExprType::String, Ok(static_value)) => {
            let format = parse_checksum_format(
                env,
                static_value.unwrap_str_ref(),
                attribute,
                expr_span,
            )?;
            Ok(ConfigVariableOr::Static(Rc::from([format])))
        }
        (_, ExprType::List(inner), Ok(static_value))
            if *inner == ExprType::String =>
        {
            let mut errs = Errs::<ConfigError>::new();
            let items = static_value.unwrap_list();
            let mut formats =
                Vec::<ChecksumFormat>::with_capacity(items.len());
            for item in items.iter() {
                if let Some(format) = errs.ok(parse_checksum_format(
                    env,
                    item.unwrap_str_ref(),
                    attribute,
                    expr_span,
                )) {
                    formats.push(format);
                }
            }
            errs.result()?;
            debug_assert_eq!(formats.len(), formats.capacity());
            Ok(ConfigVariableOr::Static(Rc::from(formats.into_boxed_slice())))
        }
        (expr, ExprType::String, Err(_)) => {
            Ok(ConfigVariableOr::new_variable(vars, expr))
        }
        (expr, ExprType::List(inner), Err(_))
            if *inner == ExprType::String =>
        {
            Ok(ConfigVariableOr::new_variable(vars, expr))
        }
        (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
            attribute,
            expr_loc: env.make_loc(expr_span),
            expr_type,
            valid_types: vec![
                ExprType::String,
                ExprType::List(Rc::from(ExprType::String)),
            ],
        })),
    }
}

fn variable_checksum_format_attr(
    env: &ConfigTypeEnv,
    vars: &mut Vec<ObjExpr>,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<ConfigVariableOr<ChecksumFormat>> {
    let expr_span = expr_ast.span;
    match variable_str_attr(env, vars, attribute, expr_ast)? {
        ConfigVariableOr::Variable(index) => {
            Ok(ConfigVariableOr::Variable(index))
        }
        ConfigVariableOr::Static(string) => Ok(ConfigVariableOr::Static(
            parse_checksum_format(env, &string, attribute, expr_span)?,
        )),
    }
}

fn variable_u64_attr(
    env: &ConfigTypeEnv,
    vars: &mut Vec<ObjExpr>,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<ConfigVariableOr<u64>> {
    let expr_span = expr_ast.span;
    match env.typecheck_expression(expr_ast)? {
        (_, ExprType::Integer, Ok(static_value)) => {
            let bigint = static_value.unwrap_int_ref();
            let uint64 = bigint.to_u64().ok_or_else(|| {
                Errs::one(ConfigError::OutOfRangeAttr {
                    attribute,
                    expr_loc: env.make_loc(expr_span),
                    value: bigint.clone(),
                })
            })?;
            Ok(ConfigVariableOr::Static(uint64))
        }
        (expr, ExprType::Integer | ExprType::Label, _) => {
            Ok(ConfigVariableOr::new_variable(vars, expr))
        }
        (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
            attribute,
            expr_loc: env.make_loc(expr_span),
            expr_type,
            valid_types: vec![ExprType::Integer, ExprType::Label],
        })),
    }
}

fn variable_str_attr(
    env: &ConfigTypeEnv,
    vars: &mut Vec<ObjExpr>,
    attribute: ConfigAttr,
    expr_ast: ExprAst,
) -> ConfigResult<ConfigVariableOr<Rc<str>>> {
    let expr_span = expr_ast.span;
    match env.typecheck_expression(expr_ast)? {
        (_, ExprType::String, Ok(static_value)) => {
            Ok(ConfigVariableOr::Static(static_value.unwrap_str()))
        }
        (expr, ExprType::String, Err(_)) => {
            Ok(ConfigVariableOr::new_variable(vars, expr))
        }
        (_, expr_type, _) => Err(Errs::one(ConfigError::AttrTypeError {
            attribute,
            expr_loc: env.make_loc(expr_span),
            expr_type,
            valid_types: vec![ExprType::String],
        })),
    }
}

//===========================================================================//
