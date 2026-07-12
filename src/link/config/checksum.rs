use super::super::eval::LinkEvalEnv;
use super::ConfigVariableOr;
use crate::error::Errs;
use crate::expr::{ExprLabel, ExprValue};
use crate::link::{LinkError, LinkResult, LinkedBinary};
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use std::fmt;
use std::rc::Rc;

//===========================================================================//

/// A linker configuration for a checksum to be calculated and stored in the
/// final binary.
#[derive(Debug)]
pub struct ChecksumConfig {
    /// The name of the destination symbol at which to store the computed
    /// checksum.
    pub name: Rc<str>,
    /// The formats to use for storing the computed sum.
    pub sum_formats: ConfigVariableOr<Rc<[ChecksumFormat]>>,
    /// The format to use for reading units of data to be summed.
    pub unit_format: ConfigVariableOr<ChecksumFormat>,
    /// The range of bytes in the binary over which to compute the checksum.
    pub range: ChecksumRange<ConfigVariableOr<u64>>,
}

impl ChecksumConfig {
    pub(super) fn calculate_and_write(
        &self,
        binary: &mut LinkedBinary,
        eval_env: &LinkEvalEnv,
    ) -> LinkResult<()> {
        let sum_formats = resolve_formats(self.sum_formats.clone(), eval_env)?;
        let sums_size: u64 =
            sum_formats.iter().map(|format| u64::from(format.size())).sum();
        let unit_format = resolve_format(self.unit_format, eval_env)?;
        let Some(offset) = binary.get_symbol_offset(&self.name) else {
            return Err(Errs::one(LinkError::Misc)); // TODO: error details
        };
        let binary_size = binary.size();
        if offset + sums_size > binary_size {
            return Err(Errs::one(LinkError::Misc)); // TODO: error details
        }
        let (start, end) = match self.range {
            ChecksumRange::From { start } => {
                let start = resolve_range(start, binary, eval_env)?;
                (start, binary_size)
            }
            ChecksumRange::FromTo { start, end } => {
                // TODO: allow parallel errors
                let start = resolve_range(start, binary, eval_env)?;
                let end = resolve_range(end, binary, eval_env)?;
                (start, end)
            }
            ChecksumRange::FromSize { start, size } => {
                // TODO: allow parallel errors
                let start = resolve_range(start, binary, eval_env)?;
                let size = resolve_range(size, binary, eval_env)?;
                (start, start + size) // TODO: check for overflow
            }
        };
        if start > end || end > binary_size {
            return Err(Errs::one(LinkError::InvalidChecksumRange {
                checksum_symbol: self.name.clone(),
                binary_size,
                start,
                end,
            }));
        }
        let checksum = binary.calculate_checksum(start, end, unit_format);
        let mut offset = offset;
        for &sum_format in sum_formats.iter() {
            binary.store_checksum(checksum, sum_format, offset);
            offset += u64::from(sum_format.size());
        }
        Ok(())
    }
}

fn resolve_format(
    variable: ConfigVariableOr<ChecksumFormat>,
    eval_env: &LinkEvalEnv,
) -> LinkResult<ChecksumFormat> {
    eval_env.resolve(variable, |value| match value {
        ExprValue::String(string) => parse_format(string),
        _ => Err(Errs::one(LinkError::MalformedPatchExpression)),
    })
}

fn resolve_formats(
    variable: ConfigVariableOr<Rc<[ChecksumFormat]>>,
    eval_env: &LinkEvalEnv,
) -> LinkResult<Rc<[ChecksumFormat]>> {
    eval_env.resolve(variable, |value| match value {
        ExprValue::String(string) => Ok(Rc::from([parse_format(string)?])),
        ExprValue::List(items) => {
            let mut errs = Errs::<LinkError>::new();
            let mut formats =
                Vec::<ChecksumFormat>::with_capacity(items.len());
            for item in items.iter() {
                match item {
                    ExprValue::String(string) => {
                        if let Some(format) = errs.ok(parse_format(string)) {
                            formats.push(format);
                        }
                    }
                    _ => {
                        errs.push(LinkError::MalformedPatchExpression);
                    }
                }
            }
            errs.result()?;
            debug_assert_eq!(formats.len(), formats.capacity());
            Ok(Rc::from(formats.into_boxed_slice()))
        }
        _ => Err(Errs::one(LinkError::MalformedPatchExpression)),
    })
}

fn parse_format(string: &str) -> LinkResult<ChecksumFormat> {
    // TODO: error details
    string.parse::<ChecksumFormat>().map_err(|()| Errs::one(LinkError::Misc))
}

fn resolve_range(
    variable: ConfigVariableOr<u64>,
    binary: &LinkedBinary,
    eval_env: &LinkEvalEnv,
) -> LinkResult<u64> {
    eval_env.resolve(variable, |value| match value {
        ExprValue::Integer(bigint) => {
            // TODO: error details
            bigint.to_u64().ok_or_else(|| Errs::one(LinkError::Misc))
        }
        ExprValue::Label(ExprLabel::SymbolRelative { name, offset }) => {
            let binary_offset =
                binary.get_symbol_offset(name).ok_or_else(|| {
                    Errs::one(LinkError::Misc) // TODO: error details
                })?;
            let binary_offset = BigInt::from(binary_offset) + offset;
            binary_offset.to_u64().ok_or_else(|| Errs::one(LinkError::Misc))
        }
        ExprValue::Label(_label) => {
            // TODO: error details: checksum expressions can only use imported
            // symbols, not exported symbols.
            Err(Errs::one(LinkError::Misc))
        }
        _ => Err(Errs::one(LinkError::MalformedPatchExpression)),
    })
}

//===========================================================================//

/// A format for reading/writing a checksum, or a unit of checksum data, in the
/// finished binary.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ChecksumFormat {
    /// An unsigned 8-bit integer.
    U8,
    /// The complement of an unsigned 8-bit integer.
    U8Cpl,
    /// An unsigned 16-bit integer, stored big-endian.
    U16be,
    /// The complement of an unsigned 16-bit integer, stored big-endian.
    U16beCpl,
    /// An unsigned 16-bit integer, stored little-endian.
    U16le,
    /// The complement of an unsigned 16-bit integer, stored little-endian.
    U16leCpl,
}

impl ChecksumFormat {
    /// A list of all `ChecksumFormat` values.
    pub const ALL: &[ChecksumFormat] = &[
        ChecksumFormat::U8,
        ChecksumFormat::U8Cpl,
        ChecksumFormat::U16be,
        ChecksumFormat::U16beCpl,
        ChecksumFormat::U16le,
        ChecksumFormat::U16leCpl,
    ];

    /// Returns the size, in bytes, of a checksum or unit of checksum data with
    /// this format.
    pub fn size(self) -> u32 {
        match self {
            Self::U8 | Self::U8Cpl => 1,
            Self::U16be | Self::U16beCpl | Self::U16le | Self::U16leCpl => 2,
        }
    }

    fn string(self) -> &'static str {
        match self {
            Self::U8 => "u8",
            Self::U8Cpl => "~u8",
            Self::U16be => "u16be",
            Self::U16beCpl => "~u16be",
            Self::U16le => "u16le",
            Self::U16leCpl => "~u16le",
        }
    }
}

impl fmt::Display for ChecksumFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.string())
    }
}

impl std::str::FromStr for ChecksumFormat {
    type Err = ();

    fn from_str(string: &str) -> Result<Self, ()> {
        match string {
            "u8" => Ok(Self::U8),
            "~u8" => Ok(Self::U8Cpl),
            "u16be" => Ok(Self::U16be),
            "~u16be" => Ok(Self::U16beCpl),
            "u16le" => Ok(Self::U16le),
            "~u16le" => Ok(Self::U16leCpl),
            _ => Err(()),
        }
    }
}

//===========================================================================//

/// A range of bytes in a linked binary over which to compute a checksum.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ChecksumRange<T> {
    /// From the given byte offset (inclusive) to the end of the binary.
    From {
        /// The byte offset for the start of the range.
        start: T,
    },
    /// From the `start` byte offset (inclusive) to the `end` byte offset
    /// (exclusive).
    FromTo {
        /// The byte offset for the start of the range.
        start: T,
        /// The byte offset for the (exclusive) endpoint of the range.
        end: T,
    },
    /// From the `start` byte offset (inclusive) to a byte offset of `(start +
    /// size)` (exclusive).
    FromSize {
        /// The byte offset for the start of the range.
        start: T,
        /// The number of bytes in the range.
        size: T,
    },
}

impl<T: Default> Default for ChecksumRange<T> {
    fn default() -> Self {
        Self::From { start: T::default() }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ChecksumFormat, ChecksumRange};

    #[test]
    fn checksum_format_string_round_trip() {
        for format in ChecksumFormat::ALL {
            assert_eq!(format.to_string().parse(), Ok(*format));
        }
    }

    #[test]
    fn checksum_range_default_is_whole_binary() {
        assert_eq!(ChecksumRange::default(), ChecksumRange::From { start: 0 });
    }
}

//===========================================================================//
