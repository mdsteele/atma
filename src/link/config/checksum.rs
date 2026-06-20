use crate::error::Errs;
use crate::link::{LinkError, LinkResult, LinkedBinary};
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
    /// The format to use for storing the computed sum.
    pub sum_format: ChecksumFormat,
    /// The format to use for reading units of data to be summed.
    pub unit_format: ChecksumFormat,
    /// The range of bytes in the binary over which to compute the checksum.
    pub range: ChecksumRange,
}

impl ChecksumConfig {
    pub(super) fn calculate_and_write(
        &self,
        binary: &mut LinkedBinary,
    ) -> LinkResult<()> {
        let Some(offset) = binary.get_symbol_offset(&self.name) else {
            return Err(Errs::one(LinkError::Misc)); // TODO: error details
        };
        let binary_size = binary.size();
        if offset + u64::from(self.sum_format.size()) > binary_size {
            return Err(Errs::one(LinkError::Misc)); // TODO: error details
        }
        let (start, end) = match self.range {
            ChecksumRange::From { start } => (start, binary_size),
            ChecksumRange::FromTo { start, end } => (start, end),
            ChecksumRange::FromSize { start, size } => (start, start + size),
        };
        if start > end || end > binary_size {
            return Err(Errs::one(LinkError::InvalidChecksumRange {
                checksum_symbol: self.name.clone(),
                binary_size,
                start,
                end,
            }));
        }
        let checksum = binary.calculate_checksum(start, end, self.unit_format);
        binary.store_checksum(checksum, self.sum_format, offset);
        Ok(())
    }
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
pub enum ChecksumRange {
    /// From the given byte offset (inclusive) to the end of the binary.
    From { start: u64 },
    /// From the `start` byte offset (inclusive) to the `end` byte offset
    /// (exclusive).
    ///
    /// If `start >= end`, then this represents an empty range.
    FromTo { start: u64, end: u64 },
    /// From the `start` byte offset (inclusive) to a byte offset of `(start +
    /// size)` (exclusive).
    ///
    /// If `size` is zero, then this represents an empty range.
    FromSize { start: u64, size: u64 },
}

impl Default for ChecksumRange {
    fn default() -> Self {
        Self::From { start: 0 }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{ChecksumFormat, ChecksumRange};

    #[test]
    fn checksum_format_string_round_trip() {
        let formats = [
            ChecksumFormat::U8,
            ChecksumFormat::U8Cpl,
            ChecksumFormat::U16be,
            ChecksumFormat::U16beCpl,
            ChecksumFormat::U16le,
            ChecksumFormat::U16leCpl,
        ];
        for format in formats {
            assert_eq!(format.to_string().parse(), Ok(format));
        }
    }

    #[test]
    fn checksum_range_default_is_whole_binary() {
        assert_eq!(ChecksumRange::default(), ChecksumRange::From { start: 0 });
    }
}

//===========================================================================//
