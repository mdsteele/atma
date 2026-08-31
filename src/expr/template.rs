use super::error::{ExprTypeError, ExprTypeResult};
use super::value::{ExprType, ExprValue};
use crate::error::{Errs, SrcSpan};
use crate::obj::{BinaryIo, Decoder, Encoder};
use chumsky::error::Rich;
use chumsky::inspector::SimpleState;
use chumsky::{self, IterParser, Parser, primitive};
use num_bigint::{BigUint, Sign};
use std::collections::HashSet;
use std::fmt::Write;
use std::io;
use std::rc::Rc;

//===========================================================================//

const FLAG_ZERO_PAD: u8 = 1 << 0;
const FLAG_ALWAYS_SIGN: u8 = 1 << 1;
const FLAG_ALT_FORMAT: u8 = 1 << 2;

const TAG_KIND_DEFAULT: u8 = 0x00;
const TAG_KIND_DEBUG: u8 = 0x01;
const TAG_KIND_BINARY: u8 = 0x02;
const TAG_KIND_LOWER_HEX: u8 = 0x03;
const TAG_KIND_UPPER_HEX: u8 = 0x04;

//===========================================================================//

/// A string interpolation template.
#[derive(Debug, Eq, PartialEq)]
pub struct Template {
    prefix: String,
    slots: Vec<(Slot, String)>,
}

impl Template {
    pub(super) fn build(
        template_span: SrcSpan,
        template_string: Rc<str>,
    ) -> ExprTypeResult<Self> {
        Self::parse(&template_string).map_err(|error| {
            Errs::one(ExprTypeError::InterpolationTemplateParseError {
                template_span,
                template_string,
                error,
            })
        })
    }

    fn parse(template_string: &str) -> Result<Self, TemplateParseError> {
        let mut state = SimpleState(0usize);
        let template = Self::parser()
            .parse_with_state(template_string, &mut state)
            .into_result()
            .map_err(|error| {
                TemplateParseError::ParseFailed(format!("{:?}", error))
            })?;
        let indices = template
            .slots
            .iter()
            .map(|(slot, _)| slot.index)
            .collect::<HashSet<usize>>();
        for index in 0..template.num_args() {
            if !indices.contains(&index) {
                return Err(TemplateParseError::SkippedArgIndex(index));
            }
        }
        Ok(template)
    }

    pub(super) fn typecheck(
        &self,
        op_span: SrcSpan,
        template_span: SrcSpan,
        arg_span: SrcSpan,
        arg_type: ExprType,
    ) -> ExprTypeResult<()> {
        self.typecheck_internal(&arg_type).map_err(|param_type| {
            Errs::one(ExprTypeError::CannotInterpolateTypeIntoTemplate {
                op_span,
                template_span,
                arg_span,
                arg_type,
                param_type,
            })
        })
    }

    fn typecheck_internal(&self, rhs_type: &ExprType) -> Result<(), ExprType> {
        let mut expected_items = vec![ExprType::Undefined; self.num_args()];
        for (slot, _) in &self.slots {
            slot.restrict_type(&mut expected_items);
        }
        let expected_type = if expected_items.len() == 1 {
            expected_items.pop().unwrap()
        } else {
            ExprType::Tuple(Rc::from(expected_items))
        };
        if !rhs_type.is_subtype_of(&expected_type) {
            return Err(expected_type);
        }
        Ok(())
    }

    pub(super) fn parser<'a>() -> impl Parser<'a, &'a str, Self, Extra<'a>> {
        Self::non_slot()
            .then(
                Slot::parser()
                    .then(Self::non_slot())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(prefix, slots)| Self { prefix, slots })
    }

    pub(super) fn non_slot<'a>() -> impl Parser<'a, &'a str, String, Extra<'a>>
    {
        primitive::choice((
            primitive::just("{{").ignored(),
            primitive::just("}}").ignored(),
            primitive::any().filter(|&c| c != '{' && c != '}').ignored(),
        ))
        .repeated()
        .to_slice()
        .map(|s: &str| s.to_string())
    }

    pub(crate) fn format(
        &self,
        arg: ExprValue,
    ) -> Result<Rc<str>, TemplateEvalError> {
        match arg {
            ExprValue::Tuple(items) => self.format_args(&items),
            other => self.format_args(&[other]),
        }
    }

    fn format_args(
        &self,
        args: &[ExprValue],
    ) -> Result<Rc<str>, TemplateEvalError> {
        let mut string = self.prefix.clone();
        for (slot, suffix) in &self.slots {
            slot.format(args, &mut string)?;
            string.push_str(suffix);
        }
        Ok(Rc::from(string))
    }

    fn num_args(&self) -> usize {
        self.slots.iter().fold(0, |n, (slot, _)| n.max(slot.index + 1))
    }
}

impl BinaryIo for Template {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let prefix = String::read_from(decoder)?;
        let slots = Vec::<(Slot, String)>::read_from(decoder)?;
        Ok(Self { prefix, slots })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.prefix.write_to(encoder)?;
        self.slots.write_to(encoder)
    }
}

//===========================================================================//

#[derive(Debug, Eq, PartialEq)]
struct Slot {
    index: usize,
    options: FormatOptions,
}

impl Slot {
    fn parser<'a>() -> impl Parser<'a, &'a str, Self, Extra<'a>> {
        chumsky::text::digits(10)
            .to_slice()
            .try_map(|digits: &str, span| {
                digits
                    .parse::<u16>()
                    .map_err(|_| Rich::custom(span, "invalid slot index"))
            })
            .or_not()
            .then(
                primitive::just(':')
                    .ignore_then(FormatOptions::parser())
                    .or_not(),
            )
            .map_with(|(index, options), extra| {
                let index = match index {
                    Some(index) => usize::from(index),
                    None => {
                        let state: &mut SimpleState<usize> = extra.state();
                        let index = state.0;
                        state.0 += 1;
                        index
                    }
                };
                let options = options.unwrap_or_default();
                Slot { index, options }
            })
            .delimited_by(primitive::just('{'), primitive::just('}'))
    }

    fn restrict_type(&self, items: &mut [ExprType]) {
        self.options.restrict_type(&mut items[self.index])
    }

    fn format(
        &self,
        args: &[ExprValue],
        out: &mut String,
    ) -> Result<(), TemplateEvalError> {
        if self.index < args.len() {
            self.options.format(&args[self.index], out)
        } else {
            Err(TemplateEvalError::SlotIndexOutOfBounds)
        }
    }
}

impl BinaryIo for Slot {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let index = usize::read_from(decoder)?;
        let options = FormatOptions::read_from(decoder)?;
        Ok(Self { index, options })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.index.write_to(encoder)?;
        self.options.write_to(encoder)
    }
}

//===========================================================================//

#[derive(Debug, Default, Eq, PartialEq)]
struct FormatOptions {
    flags: u8,
    width: u16,
    kind: FormatKind,
}

impl FormatOptions {
    fn parser<'a>() -> impl Parser<'a, &'a str, Self, Extra<'a>> {
        let flags_parser = primitive::group((
            primitive::just('+').to(FLAG_ALWAYS_SIGN).or_not(),
            primitive::just('#').to(FLAG_ALT_FORMAT).or_not(),
            primitive::just('0').to(FLAG_ZERO_PAD).or_not(),
        ))
        .map(|(f1, f2, f3)| {
            f1.unwrap_or_default()
                | f2.unwrap_or_default()
                | f3.unwrap_or_default()
        });
        let width_parser = chumsky::text::int(10)
            .try_map(|digits: &str, span| {
                digits
                    .parse::<u16>()
                    .map_err(|_| Rich::custom(span, "invalid width"))
            })
            .or_not()
            .map(Option::unwrap_or_default);
        let kind_parser =
            FormatKind::parser().or_not().map(Option::unwrap_or_default);
        primitive::group((flags_parser, width_parser, kind_parser))
            .map(|(flags, width, kind)| Self { flags, width, kind })
    }

    fn restrict_type(&self, item: &mut ExprType) {
        if 0 != self.flags & (FLAG_ZERO_PAD | FLAG_ALWAYS_SIGN) {
            *item = ExprType::Integer;
        }
        self.kind.restrict_type(item);
    }

    fn format(
        &self,
        arg: &ExprValue,
        out: &mut String,
    ) -> Result<(), TemplateEvalError> {
        let width = usize::from(self.width);
        match arg {
            ExprValue::String(string) => match self.kind {
                FormatKind::Debug => {
                    write!(out, "{:width$?}", string).unwrap();
                }
                _ => write!(out, "{:width$}", string).unwrap(),
            },
            ExprValue::Integer(int) => {
                let mut prefix = String::new();
                if int.sign() == Sign::Minus {
                    prefix.push('-');
                } else if 0 != self.flags & FLAG_ALWAYS_SIGN {
                    prefix.push('+');
                }
                if 0 != self.flags & FLAG_ALT_FORMAT {
                    prefix.push_str(self.kind.alt_prefix());
                }
                let magnitude = self.kind.format_uint(int.magnitude());
                let len = prefix.len() + magnitude.len();
                out.reserve(width.max(len));
                if len < width {
                    if 0 != self.flags & FLAG_ZERO_PAD {
                        prefix.reserve(width - len);
                        for _ in 0..(width - len) {
                            prefix.push('0');
                        }
                    } else {
                        for _ in 0..(width - len) {
                            out.push(' ');
                        }
                    }
                }
                out.push_str(&prefix);
                out.push_str(&magnitude);
            }
            other => write!(out, "{:width$}", other).unwrap(),
        }
        Ok(())
    }
}

impl BinaryIo for FormatOptions {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        let flags = u8::read_from(decoder)?;
        let width = u16::read_from(decoder)?;
        let kind = FormatKind::read_from(decoder)?;
        Ok(Self { width, flags, kind })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.flags.write_to(encoder)?;
        self.width.write_to(encoder)?;
        self.kind.write_to(encoder)
    }
}

//===========================================================================//

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum FormatKind {
    #[default]
    Default,
    Debug,
    Binary,
    LowerHex,
    UpperHex,
}

impl FormatKind {
    fn parser<'a>() -> impl Parser<'a, &'a str, Self, Extra<'a>> {
        primitive::choice((
            primitive::just('?').to(Self::Debug),
            primitive::just('b').to(Self::Binary),
            primitive::just('x').to(Self::LowerHex),
            primitive::just('X').to(Self::UpperHex),
        ))
    }

    fn restrict_type(self, item: &mut ExprType) {
        match self {
            Self::Default | Self::Debug => {}
            Self::Binary | Self::LowerHex | Self::UpperHex => {
                *item = ExprType::Integer
            }
        }
    }

    fn alt_prefix(self) -> &'static str {
        match self {
            Self::Default => "",
            Self::Debug => "",
            Self::Binary => "%",
            Self::LowerHex => "$",
            Self::UpperHex => "$",
        }
    }

    fn format_uint(self, uint: &BigUint) -> String {
        match self {
            Self::Default | Self::Debug => format!("{}", uint),
            Self::Binary => format!("{:b}", uint),
            Self::LowerHex => format!("{:x}", uint),
            Self::UpperHex => format!("{:X}", uint),
        }
    }
}

impl BinaryIo for FormatKind {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_KIND_DEFAULT => Ok(Self::Default),
            TAG_KIND_DEBUG => Ok(Self::Debug),
            TAG_KIND_BINARY => Ok(Self::Binary),
            TAG_KIND_LOWER_HEX => Ok(Self::LowerHex),
            TAG_KIND_UPPER_HEX => Ok(Self::UpperHex),
            byte => Err(io::Error::other(format!(
                "unknown FormatKind tag: 0x{:02x}",
                byte
            ))),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let tag = match self {
            Self::Default => TAG_KIND_DEFAULT,
            Self::Debug => TAG_KIND_DEBUG,
            Self::Binary => TAG_KIND_BINARY,
            Self::LowerHex => TAG_KIND_LOWER_HEX,
            Self::UpperHex => TAG_KIND_UPPER_HEX,
        };
        tag.write_to(encoder)
    }
}

//===========================================================================//

/// An error encountered while parsing an interpolation template string.
#[derive(Debug)]
pub enum TemplateParseError {
    /// Encountered the given parse error.
    ParseFailed(String),
    /// The given argument index was skipped in the template string.
    SkippedArgIndex(usize),
}

impl TemplateParseError {
    pub(super) fn into_label_string(self) -> String {
        match self {
            Self::ParseFailed(message) => message,
            Self::SkippedArgIndex(index) => {
                format!("skipped argument {{{index}}}")
            }
        }
    }
}

//===========================================================================//

#[derive(Debug)]
pub(crate) enum TemplateEvalError {
    SlotIndexOutOfBounds,
}

//===========================================================================//

type ParseError<'a> = Rich<'a, char>;
type Extra<'a> = chumsky::extra::Full<ParseError<'a>, SimpleState<usize>, ()>;

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{FormatKind, Template};
    use crate::obj::assert_round_trips;

    #[test]
    fn template_round_trip() {
        assert_round_trips(Template::parse("").unwrap());
        assert_round_trips(Template::parse("foobar{}baz").unwrap());
        assert_round_trips(Template::parse("<{1:+#06x}-{:?}>").unwrap());
    }

    #[test]
    fn format_kind_round_trip() {
        assert_round_trips(FormatKind::Default);
        assert_round_trips(FormatKind::Debug);
        assert_round_trips(FormatKind::Binary);
        assert_round_trips(FormatKind::LowerHex);
        assert_round_trips(FormatKind::UpperHex);
    }
}

//===========================================================================//
