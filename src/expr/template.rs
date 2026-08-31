use super::error::{ExprTypeError, ExprTypeResult};
use super::value::{ExprType, ExprValue};
use crate::error::{Errs, SrcSpan};
use crate::obj::{BinaryIo, Decoder, Encoder};
use chumsky::error::Rich;
use chumsky::inspector::SimpleState;
use chumsky::{self, IterParser, Parser, primitive};
use std::collections::HashSet;
use std::fmt::Write;
use std::io;
use std::rc::Rc;

//===========================================================================//

const TAG_FORMAT_DEFAULT: u8 = 0x00;
const TAG_FORMAT_DEBUG: u8 = 0x01;
const TAG_FORMAT_BINARY: u8 = 0x02;
const TAG_FORMAT_LOWER_HEX: u8 = 0x03;
const TAG_FORMAT_UPPER_HEX: u8 = 0x04;

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
        Self::build_internal(&template_string).map_err(|error| {
            Errs::one(ExprTypeError::InterpolationTemplateParseError {
                template_span,
                template_string,
                error,
            })
        })
    }

    fn build_internal(
        template_string: &str,
    ) -> Result<Self, TemplateParseError> {
        let template = Self::parse(template_string)?;
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

    fn parse(source: &str) -> Result<Self, TemplateParseError> {
        let mut state = SimpleState(0usize);
        Self::parser()
            .parse_with_state(source, &mut state)
            .into_result()
            .map_err(|error| {
                TemplateParseError::ParseFailed(format!("{:?}", error))
            })
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
    formatter: Formatter,
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
                primitive::just(':').ignore_then(Formatter::parser()).or_not(),
            )
            .map_with(|(index, formatter), extra| {
                let index = match index {
                    Some(index) => usize::from(index),
                    None => {
                        let state: &mut SimpleState<usize> = extra.state();
                        let index = state.0;
                        state.0 += 1;
                        index
                    }
                };
                let formatter = formatter.unwrap_or_default();
                Slot { index, formatter }
            })
            .delimited_by(primitive::just('{'), primitive::just('}'))
    }

    fn restrict_type(&self, items: &mut [ExprType]) {
        self.formatter.restrict_type(&mut items[self.index])
    }

    fn format(
        &self,
        args: &[ExprValue],
        out: &mut String,
    ) -> Result<(), TemplateEvalError> {
        if self.index < args.len() {
            self.formatter.format(&args[self.index], out)
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
        let formatter = Formatter::read_from(decoder)?;
        Ok(Self { index, formatter })
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        self.index.write_to(encoder)?;
        self.formatter.write_to(encoder)
    }
}

//===========================================================================//

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum Formatter {
    #[default]
    Default,
    Debug,
    Binary,
    LowerHex,
    UpperHex,
    // TODO: more options
}

impl Formatter {
    fn parser<'a>() -> impl Parser<'a, &'a str, Self, Extra<'a>> {
        // TODO: support other format attributes (e.g. padding)
        primitive::choice((
            primitive::just('?').to(Formatter::Debug),
            primitive::just('b').to(Formatter::Binary),
            primitive::just('x').to(Formatter::LowerHex),
            primitive::just('X').to(Formatter::UpperHex),
            primitive::empty().to(Formatter::Default),
        ))
    }

    fn restrict_type(&self, item: &mut ExprType) {
        match self {
            Self::Default | Self::Debug => {}
            Self::Binary | Self::LowerHex | Self::UpperHex => {
                *item = ExprType::Integer
            }
        }
    }

    fn format(
        &self,
        arg: &ExprValue,
        out: &mut String,
    ) -> Result<(), TemplateEvalError> {
        match self {
            Self::Default => match arg {
                ExprValue::String(string) => {
                    write!(out, "{}", string).unwrap()
                }
                other => write!(out, "{}", other).unwrap(),
            },
            Self::Debug => {
                write!(out, "{}", arg).unwrap();
            }
            Self::Binary => match arg {
                ExprValue::Integer(int) => write!(out, "{:b}", int).unwrap(),
                _ => return Err(TemplateEvalError::InvalidType),
            },
            Self::LowerHex => match arg {
                ExprValue::Integer(int) => write!(out, "{:x}", int).unwrap(),
                _ => return Err(TemplateEvalError::InvalidType),
            },
            Self::UpperHex => match arg {
                ExprValue::Integer(int) => write!(out, "{:X}", int).unwrap(),
                _ => return Err(TemplateEvalError::InvalidType),
            },
        }
        Ok(())
    }
}

impl BinaryIo for Formatter {
    fn read_from<R: io::BufRead>(
        decoder: &mut Decoder<R>,
    ) -> io::Result<Self> {
        match u8::read_from(decoder)? {
            TAG_FORMAT_DEFAULT => Ok(Self::Default),
            TAG_FORMAT_DEBUG => Ok(Self::Debug),
            TAG_FORMAT_BINARY => Ok(Self::Binary),
            TAG_FORMAT_LOWER_HEX => Ok(Self::LowerHex),
            TAG_FORMAT_UPPER_HEX => Ok(Self::UpperHex),
            byte => Err(io::Error::other(format!(
                "unknown formatter tag: 0x{:02x}",
                byte
            ))),
        }
    }

    fn write_to<W: io::Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> io::Result<()> {
        let tag = match self {
            Self::Default => TAG_FORMAT_DEFAULT,
            Self::Debug => TAG_FORMAT_DEBUG,
            Self::Binary => TAG_FORMAT_BINARY,
            Self::LowerHex => TAG_FORMAT_LOWER_HEX,
            Self::UpperHex => TAG_FORMAT_UPPER_HEX,
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
    InvalidType,
    SlotIndexOutOfBounds,
}

//===========================================================================//

type ParseError<'a> = Rich<'a, char>;
type Extra<'a> = chumsky::extra::Full<ParseError<'a>, SimpleState<usize>, ()>;

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::Formatter;
    use crate::obj::assert_round_trips;

    #[test]
    fn formatter_round_trips() {
        assert_round_trips(Formatter::Default);
        assert_round_trips(Formatter::Debug);
        assert_round_trips(Formatter::Binary);
        assert_round_trips(Formatter::LowerHex);
        assert_round_trips(Formatter::UpperHex);
    }
}

//===========================================================================//
