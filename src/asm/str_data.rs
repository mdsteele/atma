use super::charmap::Charmap;
use super::check::{bigint_range, typecheck_static_dir_expr_as};
use super::env::AsmTypeEnv;
use super::error::{AsmError, AsmResult};
use crate::error::{Errs, SrcSpan};
use crate::expr::{ExprStatic, ExprType, ExprValue};
use crate::parse::{AsmCharmapAst, AsmStrType, ExprAst};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::collections::HashMap;
use std::range::RangeInclusive;
use std::rc::Rc;

//===========================================================================//

pub(super) fn define_charmap(
    env: &mut AsmTypeEnv,
    charmap_ast: AsmCharmapAst,
) -> AsmResult<()> {
    let name_span = charmap_ast.name.span;
    let (name, charmap) = Errs::join_results(
        typecheck_charmap_name(env, charmap_ast.name),
        build_charmap(env, charmap_ast.parent, charmap_ast.mappings),
    )?;
    env.add_charmap(name, name_span, charmap);
    Ok(())
}

fn typecheck_charmap_name(
    env: &AsmTypeEnv,
    name_ast: ExprAst,
) -> AsmResult<Rc<str>> {
    let name_span = name_ast.span;
    let name_value = typecheck_static_dir_expr_as(
        env,
        (AsmCharmapAst::DIRECTIVE, "name"),
        name_ast,
        ExprType::String,
    )?;
    let name_string = name_value.unwrap_str();
    if let Some((prev_span, _)) = env.get_charmap(&name_string) {
        return Err(Errs::one(AsmError::CharmapAlreadyDefined {
            name: name_string,
            name_loc: env.make_loc(name_span),
            prev_loc: env.make_loc(prev_span),
        }));
    }
    Ok(name_string)
}

fn build_charmap(
    env: &AsmTypeEnv,
    parent: Option<ExprAst>,
    mappings: Vec<(ExprAst, ExprAst)>,
) -> AsmResult<Charmap> {
    let mut errs = Errs::<AsmError>::new();
    let mut charmap = errs.with(clone_parent_charmap(env, parent));
    let mut prev_keys = HashMap::<Rc<str>, SrcSpan>::new();
    for (from_ast, to_ast) in mappings {
        errs.also(add_charmap_mapping(
            env,
            &mut charmap,
            &mut prev_keys,
            from_ast,
            to_ast,
        ));
    }
    errs.result()?;
    Ok(charmap)
}

fn clone_parent_charmap(
    env: &AsmTypeEnv,
    parent: Option<ExprAst>,
) -> (Charmap, Errs<AsmError>) {
    let mut errs = Errs::<AsmError>::new();
    let charmap = if let Some(parent_expr) = parent
        && let parent_span = parent_expr.span
        && let Some(parent_value) = errs.ok(typecheck_static_dir_expr_as(
            env,
            (AsmCharmapAst::DIRECTIVE, "parent"),
            parent_expr,
            ExprType::String,
        ))
        && let Some(parent_charmap) =
            errs.ok(get_charmap(env, parent_span, parent_value.unwrap_str()))
    {
        parent_charmap.clone()
    } else {
        Charmap::new()
    };
    (charmap, errs)
}

fn get_charmap(
    env: &AsmTypeEnv,
    name_span: SrcSpan,
    name: Rc<str>,
) -> AsmResult<&Charmap> {
    match env.get_charmap(&name) {
        Some((_, charmap)) => Ok(charmap),
        None => Err(Errs::one(AsmError::UnknownCharmap {
            charmap: name,
            loc: env.make_loc(name_span),
        })),
    }
}

fn add_charmap_mapping(
    env: &AsmTypeEnv,
    charmap: &mut Charmap,
    prev_keys: &mut HashMap<Rc<str>, SrcSpan>,
    from_ast: ExprAst,
    to_ast: ExprAst,
) -> AsmResult<()> {
    let from_span = from_ast.span;
    let to_span = to_ast.span;
    let ((from_type, from_static), (to_type, to_static)) = Errs::join_results(
        typecheck_mapping_expr(env, from_ast),
        typecheck_mapping_expr(env, to_ast),
    )?;
    let mapping_type =
        get_mapping_type(env, from_span, from_type, to_span, to_type)?;
    let (from_value, to_value) = Errs::join_results(
        get_static_mapping_value(env, from_span, from_static),
        get_static_mapping_value(env, to_span, to_static),
    )?;
    match mapping_type {
        MappingType::StringToInteger => map_string_to_integer(
            env, charmap, prev_keys, from_span, from_value, to_span, to_value,
        ),
        MappingType::StringToList => map_string_to_list(
            env, charmap, prev_keys, from_span, from_value, to_span, to_value,
        ),
        MappingType::RangeToRange => map_range_to_range(
            env, charmap, prev_keys, from_span, from_value, to_span, to_value,
        ),
    }
}

fn typecheck_mapping_expr(
    env: &AsmTypeEnv,
    expr_ast: ExprAst,
) -> AsmResult<(ExprType, ExprStatic)> {
    let ((_, expr_type, expr_static), errs) =
        env.typecheck_expression(expr_ast);
    errs.result()?;
    Ok((expr_type, expr_static))
}

fn map_string_to_integer(
    env: &AsmTypeEnv,
    charmap: &mut Charmap,
    prev_keys: &mut HashMap<Rc<str>, SrcSpan>,
    from_span: SrcSpan,
    from_value: ExprValue,
    to_span: SrcSpan,
    to_value: ExprValue,
) -> AsmResult<()> {
    let mut errs = Errs::<AsmError>::new();
    let from_string = from_value.unwrap_str();
    let to_byte = errs.ok_or_default(mapping_byte(env, to_span, &to_value));
    errs.also(insert_mapping(
        env,
        charmap,
        prev_keys,
        from_span,
        from_string,
        &[to_byte],
    ));
    errs.result()
}

fn map_string_to_list(
    env: &AsmTypeEnv,
    charmap: &mut Charmap,
    prev_keys: &mut HashMap<Rc<str>, SrcSpan>,
    from_span: SrcSpan,
    from_value: ExprValue,
    to_span: SrcSpan,
    to_value: ExprValue,
) -> AsmResult<()> {
    let mut errs = Errs::<AsmError>::new();
    let from_string = from_value.unwrap_str();
    let to_list = to_value.unwrap_list();
    let mut to_bytes = Vec::<u8>::with_capacity(to_list.len());
    for to_item in to_list.iter() {
        let Some(to_byte) = errs.ok(mapping_byte(env, to_span, to_item))
        else {
            break;
        };
        to_bytes.push(to_byte);
    }
    errs.also(insert_mapping(
        env,
        charmap,
        prev_keys,
        from_span,
        from_string,
        to_bytes.as_slice(),
    ));
    errs.result()
}

fn map_range_to_range(
    env: &AsmTypeEnv,
    charmap: &mut Charmap,
    prev_keys: &mut HashMap<Rc<str>, SrcSpan>,
    from_span: SrcSpan,
    from_value: ExprValue,
    to_span: SrcSpan,
    to_value: ExprValue,
) -> AsmResult<()> {
    let (chars, bytes) =
        mapping_range_pair(env, from_span, from_value, to_span, to_value)?;
    let mut buffer = String::with_capacity(char::MAX_LEN_UTF8);
    for (from_char, to_byte) in chars.into_iter().zip(bytes) {
        buffer.clear();
        buffer.push(from_char);
        let from_string = Rc::<str>::from(buffer.as_str());
        insert_mapping(
            env,
            charmap,
            prev_keys,
            from_span,
            from_string,
            &[to_byte],
        )?;
    }
    debug_assert_eq!(buffer.capacity(), char::MAX_LEN_UTF8);
    Ok(())
}

fn mapping_range_pair(
    env: &AsmTypeEnv,
    from_span: SrcSpan,
    from_value: ExprValue,
    to_span: SrcSpan,
    to_value: ExprValue,
) -> AsmResult<(RangeInclusive<char>, RangeInclusive<u8>)> {
    let (char_range, byte_range) = Errs::join_results(
        mapping_char_range(env, from_span, from_value),
        mapping_byte_range(env, to_span, to_value),
    )?;
    let char_range_len = get_char_range_len(char_range);
    let byte_range_len = get_byte_range_len(byte_range);
    if char_range_len != byte_range_len {
        return Err(Errs::one(
            AsmError::CharmapRangeMappingWithUnequalLengths {
                char_range_loc: env.make_loc(from_span),
                char_range_len,
                byte_range_loc: env.make_loc(to_span),
                byte_range_len,
            },
        ));
    }
    Ok((char_range, byte_range))
}

fn mapping_char_range(
    env: &AsmTypeEnv,
    expr_span: SrcSpan,
    tuple_value: ExprValue,
) -> AsmResult<RangeInclusive<char>> {
    let tuple_items = tuple_value.unwrap_tuple();
    debug_assert_eq!(tuple_items.len(), 2);
    let (start, last) = Errs::join_results(
        mapping_char(env, expr_span, &tuple_items[0]),
        mapping_char(env, expr_span, &tuple_items[1]),
    )?;
    if start > last {
        return Err(Errs::one(AsmError::CharmapRangeMappingEmptyCharRange {
            range_loc: env.make_loc(expr_span),
            range_start: start,
            range_last: last,
        }));
    }
    Ok(RangeInclusive { start, last })
}

fn mapping_char(
    env: &AsmTypeEnv,
    expr_span: SrcSpan,
    string_value: &ExprValue,
) -> AsmResult<char> {
    let string = string_value.unwrap_str_ref();
    let mut chars = string.chars();
    if let Some(chr) = chars.next()
        && chars.next().is_none()
    {
        Ok(chr)
    } else {
        Err(Errs::one(AsmError::CharmapRangeMappingInvalidCharEndpoint {
            range_loc: env.make_loc(expr_span),
            range_endpoint: string.clone(),
        }))
    }
}

fn mapping_byte_range(
    env: &AsmTypeEnv,
    expr_span: SrcSpan,
    tuple_value: ExprValue,
) -> AsmResult<RangeInclusive<u8>> {
    let tuple_items = tuple_value.unwrap_tuple();
    debug_assert_eq!(tuple_items.len(), 2);
    let (start, last) = Errs::join_results(
        mapping_byte(env, expr_span, &tuple_items[0]),
        mapping_byte(env, expr_span, &tuple_items[1]),
    )?;
    if start > last {
        return Err(Errs::one(AsmError::CharmapRangeMappingEmptyByteRange {
            range_loc: env.make_loc(expr_span),
            range_start: start,
            range_last: last,
        }));
    }
    Ok(RangeInclusive { start, last })
}

fn mapping_byte(
    env: &AsmTypeEnv,
    expr_span: SrcSpan,
    expr_value: &ExprValue,
) -> AsmResult<u8> {
    let bigint = expr_value.unwrap_int_ref();
    if let Some(to_byte) = bigint.to_u8() {
        Ok(to_byte)
    } else {
        Err(Errs::one(AsmError::DirectiveExprOutOfRange {
            directive: AsmCharmapAst::DIRECTIVE,
            component: "byte",
            expr_loc: env.make_loc(expr_span),
            expr_value: bigint.clone(),
            valid_range: bigint_range(u8::MIN, u8::MAX),
        }))
    }
}

fn insert_mapping(
    env: &AsmTypeEnv,
    charmap: &mut Charmap,
    prev_keys: &mut HashMap<Rc<str>, SrcSpan>,
    from_span: SrcSpan,
    from_string: Rc<str>,
    to_bytes: &[u8],
) -> AsmResult<()> {
    if let Some(&prev_span) = prev_keys.get(&from_string) {
        return Err(Errs::one(AsmError::CharmapKeyConflict {
            key_loc: env.make_loc(from_span),
            key_string: from_string.clone(),
            prev_loc: env.make_loc(prev_span),
            prev_string: from_string,
        }));
    }
    charmap.try_insert(from_span, &from_string, to_bytes).map_err(
        |(prev_span, prev_key)| {
            Errs::one(AsmError::CharmapKeyConflict {
                key_loc: env.make_loc(from_span),
                key_string: from_string.clone(),
                prev_loc: env.make_loc(prev_span),
                prev_string: prev_key,
            })
        },
    )?;
    prev_keys.entry(from_string).or_insert(from_span);
    Ok(())
}

/// Types of mappings that can appear in a charmap definition.
#[derive(Clone, Copy)]
enum MappingType {
    /// Maps a string to a single byte.
    StringToInteger,
    /// Maps a string to a list of bytes.
    StringToList,
    /// Maps a range of characters to a range of bytes.
    RangeToRange,
}

fn get_mapping_type(
    env: &AsmTypeEnv,
    from_span: SrcSpan,
    from_type: ExprType,
    to_span: SrcSpan,
    to_type: ExprType,
) -> AsmResult<MappingType> {
    match (from_type, to_type) {
        (
            ExprType::String | ExprType::Bottom,
            ExprType::Integer | ExprType::Bottom,
        ) => Ok(MappingType::StringToInteger),
        (ExprType::String | ExprType::Bottom, ExprType::List(item_type))
            if item_type.is_subtype_of(&ExprType::Integer) =>
        {
            Ok(MappingType::StringToList)
        }
        (ExprType::Tuple(from_types), ExprType::Tuple(to_types))
            if matches!(*from_types, [ExprType::String, ExprType::String])
                && matches!(
                    *to_types,
                    [ExprType::Integer, ExprType::Integer]
                ) =>
        {
            Ok(MappingType::RangeToRange)
        }
        (ExprType::Tuple(from_types), ExprType::Bottom)
            if matches!(*from_types, [ExprType::String, ExprType::String]) =>
        {
            Ok(MappingType::RangeToRange)
        }
        (ExprType::Bottom, ExprType::Tuple(to_types))
            if matches!(*to_types, [ExprType::Integer, ExprType::Integer]) =>
        {
            Ok(MappingType::RangeToRange)
        }
        (from_type, to_type) => {
            Err(Errs::one(AsmError::CharmapMappingTypeError {
                from_loc: env.make_loc(from_span),
                from_type,
                to_loc: env.make_loc(to_span),
                to_type,
            }))
        }
    }
}

fn get_static_mapping_value(
    env: &AsmTypeEnv,
    expr_span: SrcSpan,
    expr_static: ExprStatic,
) -> AsmResult<ExprValue> {
    expr_static.map_err(|reason| {
        Errs::one(AsmError::DirectiveExprNotStatic {
            directive: AsmCharmapAst::DIRECTIVE,
            component: "mapping",
            expr_loc: env.make_loc(expr_span),
            reason,
        })
    })
}
//===========================================================================//

fn get_byte_range_len(range: RangeInclusive<u8>) -> u32 {
    u32::from(range.last) + 1 - u32::from(range.start)
}

fn get_char_range_len(range: RangeInclusive<char>) -> u32 {
    let mut len = u32::from(range.last) + 1 - u32::from(range.start);
    // Subtract out the 0xd800-0xdfff surrogate gap if the range spans over it.
    if range.start <= '\u{d7ff}' && range.last >= '\u{e000}' {
        len -= 0x800;
    }
    len
}

//===========================================================================//

pub(super) fn assemble_str_data(
    env: &mut AsmTypeEnv,
    str_type: AsmStrType,
    expr_ast: ExprAst,
) -> AsmResult<()> {
    let mut errs = Errs::<AsmError>::new();
    let expr_span = expr_ast.span;
    match errs.with(env.typecheck_expression(expr_ast)) {
        (_, ExprType::Undefined, _) => {}
        (_, ExprType::Integer, Ok(value)) => {
            errs.also(assemble_str_data_bigint(
                env,
                str_type,
                expr_span,
                value.unwrap_int_ref(),
            ));
        }
        (_, ExprType::String, Ok(value)) => {
            errs.also(assemble_str_data_string(
                env,
                str_type,
                expr_span,
                value.unwrap_str_ref(),
            ));
        }
        (
            _,
            ExprType::Bottom | ExprType::Integer | ExprType::String,
            Err(reason),
        ) => {
            errs.push(AsmError::DirectiveExprNotStatic {
                directive: str_type.directive(),
                component: "value",
                expr_loc: env.make_loc(expr_span),
                reason,
            });
        }
        (_, expr_type, _) => {
            errs.push(AsmError::DirectiveExprTypeError {
                directive: str_type.directive(),
                component: "value",
                expr_loc: env.make_loc(expr_span),
                expr_type,
                valid_types: vec![ExprType::String, ExprType::Integer],
            });
        }
    }
    errs.result()
}

fn assemble_str_data_bigint(
    env: &mut AsmTypeEnv,
    str_type: AsmStrType,
    expr_span: SrcSpan,
    bigint: &BigInt,
) -> AsmResult<()> {
    match str_type {
        AsmStrType::Ascii => {
            if let Some(byte) = bigint.to_u8()
                && byte < 0x80
            {
                env.append_chunk_data(&[byte])
            } else {
                Err(Errs::one(AsmError::InvalidAsciiValue {
                    expr_loc: env.make_loc(expr_span),
                    expr_value: bigint.clone(),
                }))
            }
        }
        AsmStrType::Chars => {
            if let Some(byte) = bigint.to_u8() {
                env.append_chunk_data(&[byte])
            } else {
                Err(Errs::one(AsmError::DirectiveExprOutOfRange {
                    directive: str_type.directive(),
                    component: "byte",
                    expr_loc: env.make_loc(expr_span),
                    expr_value: bigint.clone(),
                    valid_range: bigint_range(u8::MIN, u8::MAX),
                }))
            }
        }
        AsmStrType::Utf8 => {
            if let Some(chr) = bigint.to_u32().and_then(char::from_u32) {
                env.append_chunk_data(chr.to_string().as_bytes())
            } else {
                Err(Errs::one(AsmError::InvalidUnicodeScalarValue {
                    expr_loc: env.make_loc(expr_span),
                    expr_value: bigint.clone(),
                }))
            }
        }
    }
}

fn assemble_str_data_string(
    env: &mut AsmTypeEnv,
    str_type: AsmStrType,
    expr_span: SrcSpan,
    string: &str,
) -> AsmResult<()> {
    match str_type {
        AsmStrType::Ascii => {
            if let Some(chr) = string.chars().find(|chr| !chr.is_ascii()) {
                Err(Errs::one(AsmError::InvalidAsciiString {
                    expr_loc: env.make_loc(expr_span),
                    non_ascii_char: chr,
                }))
            } else {
                env.append_chunk_data(string.as_bytes())
            }
        }
        AsmStrType::Chars => {
            if let Some((name, charmap)) = env.current_charmap() {
                let bytes =
                    charmap.translate(string).map_err(|unmatched| {
                        Errs::one(AsmError::NoMatchingCharmapMapping {
                            charmap: name.clone(),
                            expr_loc: env.make_loc(expr_span),
                            unmatched: Rc::from(unmatched),
                        })
                    })?;
                env.append_chunk_data(bytes.as_slice())
            } else {
                Err(Errs::one(AsmError::NoCharmapSet {
                    expr_loc: env.make_loc(expr_span),
                }))
            }
        }
        AsmStrType::Utf8 => env.append_chunk_data(string.as_bytes()),
    }
}

//===========================================================================//
