use super::checksum::ChecksumFormat;
use crate::addr::{Addr, Align, AlignTryFromError, Size};
use crate::error::{Errs, SourceError, SrcSpan, ToSourceError};
use crate::expr::{ExprType, ExprTypeError};
use crate::parse::ParseError;
use num_bigint::BigInt;
use std::fmt;
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for constructing a linker config.
pub type ConfigResult<T> = Result<T, Errs<ConfigError>>;

//===========================================================================//

/// An error encountered while constructing a linker config.
#[derive(Debug)]
pub enum ConfigError {
    /// A config attribute was given an expression with the wrong type.
    AttrTypeError {
        /// The config attribute.
        attribute: ConfigAttr,
        /// The source code span for the expresion.
        expr_span: SrcSpan,
        /// The actual type of the expression.
        expr_type: ExprType,
        /// The permissible types for the expression.
        valid_types: Vec<ExprType>,
    },
    /// A config entry was given two attributes with the same name.
    DuplicateAttrName {
        /// The name of the entry in which the attribute is duplicated.
        entry_name: Rc<str>,
        /// The duplicated attribute name.
        attr_name: Rc<str>,
        /// The source code span for the duplicate instance of this attribute
        /// name.
        attr_span: SrcSpan,
        /// The source code span for the earlier instance of this attribute
        /// name.
        prev_span: SrcSpan,
    },
    /// Declared two config entires with the same name.
    DuplicateEntryName {
        /// The duplicated entry name.
        entry_name: Rc<str>,
        /// The source code span for the duplicate instance of this entry name.
        entry_span: SrcSpan,
        /// The source code span for the earlier instance of this entry name.
        prev_span: SrcSpan,
    },
    /// Tried to export a symbol that was already exported.
    DuplicateExport {
        /// The symbol name.
        symbol_name: Rc<str>,
        /// The source code span for the duplicate instance of the export.
        export_span: SrcSpan,
        /// The source code span for the earlier instance of the export.
        prev_span: SrcSpan,
    },
    /// Tried to both import and export the same symbol.
    ExportImportedSymbol {
        /// The symbol name.
        symbol_name: Rc<str>,
        /// The source code span for the earlier import.
        import_span: SrcSpan,
        /// The source code span for the export.
        export_span: SrcSpan,
    },
    /// An expression failed to typecheck.
    ExprTypeError {
        /// The typechecking error.
        error: ExprTypeError,
    },
    /// An alignment attribute had an invalid value.
    InvalidAlignmentAttr {
        /// The attribute.
        attribute: ConfigAttr,
        /// The reason that the expression value was invalid.
        error: AlignTryFromError,
        /// The source code span for the expression that evaluated to an
        /// invalid alignment value.
        expr_span: SrcSpan,
        /// The value of the expression.
        expr_value: BigInt,
    },
    /// A config entry included an unknown attribute name.
    InvalidAttrName {
        /// The kind of entry for which this attribute name is invalid.
        entry_kind: ConfigEntryKind,
        /// The invalid attribute name.
        attr_name: Rc<str>,
        /// The source code span for the invalid attribute name.
        attr_span: SrcSpan,
    },
    /// An checksum format attribute had an invalid value.
    InvalidChecksumFormatAttr {
        /// The checksum format attribute.
        attribute: ConfigAttr,
        /// The source code span for the expression that evaluated to an
        /// invalid checksum format string.
        expr_span: SrcSpan,
        /// The value of the expression.
        expr_value: Rc<str>,
    },
    /// A config entry was missing a required attribute.
    MissingAttr {
        /// The name of the entry for which the attribute is missing.
        entry_name: Rc<str>,
        /// The source code span for the name of the entry.
        entry_span: SrcSpan,
        /// The missing attribute.
        attribute: ConfigAttr,
    },
    /// A config entry contained two attributes that cannot be used together.
    MutuallyExclusiveAttrs {
        /// The first attribute.
        attribute_1: ConfigAttr,
        /// The second attribute.
        attribute_2: ConfigAttr,
        /// The source code span for the first attribute name.
        attr_span_1: SrcSpan,
        /// The source code span for the second attribute name.
        attr_span_2: SrcSpan,
        /// The source code span for the name of the entry that contains the
        /// two mutually exclusive attributes.
        entry_span: SrcSpan,
    },
    /// A config attribute that requires a static value was given a non-static
    /// expression.
    NonStaticAttr {
        /// The config attribute.
        attribute: ConfigAttr,
        /// The source code span for the non-static expression.
        expr_span: SrcSpan,
    },
    /// A config attribute was given an integer value outside of its valid
    /// range.
    OutOfRangeAttr {
        /// The config attribute.
        attribute: ConfigAttr,
        /// The source code span for the expression.
        expr_span: SrcSpan,
        /// The out-of-range value.
        value: BigInt,
    },
    /// An piece of the linker config failed to parse.
    ParseError {
        /// The parse error.
        error: ParseError,
    },
    /// A memory region's address range extended beyond the bit width of its
    /// address space.
    RegionRangeOverflow {
        /// The name of the memory region entry.
        entry_name: Rc<str>,
        /// The source code span for the name of the memory region entry.
        entry_span: SrcSpan,
        /// The start address of the memory region.
        start: Addr,
        /// The size of the memory region.
        size: Size,
        /// The bit width of the memory region's address space.
        bits: u32,
    },
}

impl From<ExprTypeError> for ConfigError {
    fn from(error: ExprTypeError) -> Self {
        Self::ExprTypeError { error }
    }
}

impl From<ParseError> for ConfigError {
    fn from(error: ParseError) -> Self {
        Self::ParseError { error }
    }
}

impl ToSourceError for ConfigError {
    fn to_source_error(self) -> SourceError {
        match self {
            Self::AttrTypeError {
                attribute,
                expr_span,
                expr_type,
                valid_types,
            } => {
                let message = format!(
                    "{} `{}` attribute must have type {}",
                    attribute.entry_kind(),
                    attribute.attr_name(),
                    valid_types
                        .iter()
                        .map(ExprType::to_string)
                        .collect::<Vec<_>>()
                        .join(" or "),
                );
                let label = format!("this expression has type {expr_type}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::DuplicateAttrName {
                entry_name,
                attr_name,
                attr_span,
                prev_span,
            } => {
                let message = format!(
                    "Duplicate `{attr_name}` attribute for `{entry_name}`"
                );
                let label1 = "Previously declared here";
                let label2 = "Duplicated here";
                SourceError::new(attr_span, message)
                    .with_label(prev_span, label1)
                    .with_label(attr_span, label2)
            }
            Self::DuplicateEntryName { entry_name, entry_span, prev_span } => {
                let message = format!("`{entry_name}` was already declared");
                let label1 = "Previously declared here";
                let label2 = "Declared again here";
                SourceError::new(entry_span, message)
                    .with_label(prev_span, label1)
                    .with_label(entry_span, label2)
            }
            Self::DuplicateExport { symbol_name, export_span, prev_span } => {
                let message = format!("`{symbol_name}` was already exported");
                let label1 = "Previously exported here";
                let label2 = "Exported again here";
                SourceError::new(export_span, message)
                    .with_label(prev_span, label1)
                    .with_label(export_span, label2)
            }
            Self::ExportImportedSymbol {
                symbol_name,
                import_span,
                export_span,
            } => {
                let message =
                    format!("`{symbol_name}` is both imported and exported");
                let label1 = "Imported here";
                let label2 = "Exported here";
                SourceError::new(export_span, message)
                    .with_label(import_span, label1)
                    .with_label(export_span, label2)
            }
            Self::ExprTypeError { error } => error.to_source_error(),
            Self::InvalidAlignmentAttr {
                attribute,
                error,
                expr_span,
                expr_value,
            } => {
                let message = match error {
                    AlignTryFromError::NotAPowerOfTwo => {
                        format!(
                            "{} `{}` attribute must be a power of two",
                            attribute.entry_kind(),
                            attribute.attr_name()
                        )
                    }
                    AlignTryFromError::TooLargePowerOfTwo => {
                        format!(
                            "{} `{}` attribute must be at most ${:x}",
                            attribute.entry_kind(),
                            attribute.attr_name(),
                            Align::MAX
                        )
                    }
                };
                let label =
                    format!("the value of this expression is ${expr_value:x}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::InvalidAttrName { entry_kind, attr_name, attr_span } => {
                let message =
                    format!("Invalid {entry_kind} attribute: `{attr_name}`");
                let note = format!(
                    "Valid {entry_kind} attributes are:\n{}",
                    entry_kind
                        .attributes()
                        .iter()
                        .map(|attr| format!("`{}`", attr.attr_name()))
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                SourceError::new(attr_span, message).with_note(note)
            }
            Self::InvalidChecksumFormatAttr {
                attribute,
                expr_span,
                expr_value,
            } => {
                let message = format!(
                    "{} `{}` must use a valid checksum format",
                    attribute.entry_kind(),
                    attribute.attr_name()
                );
                let label =
                    format!("this value of this expression is {expr_value:?}");
                let note = format!(
                    "Valid checksum format strings are:\n{}",
                    ChecksumFormat::ALL
                        .iter()
                        .map(|fmt| format!("\"{fmt}\""))
                        .collect::<Vec<_>>()
                        .join(", "),
                );
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
                    .with_note(note)
            }
            Self::MissingAttr { entry_name, entry_span, attribute } => {
                let message = format!(
                    "Missing required `{}` attribute for `{entry_name}`",
                    attribute.attr_name()
                );
                SourceError::new(entry_span, message)
            }
            Self::MutuallyExclusiveAttrs {
                attribute_1,
                attribute_2,
                attr_span_1,
                attr_span_2,
                entry_span,
            } => {
                let message = format!(
                    "a {} cannot specify both `{}` and `{}`",
                    attribute_1.entry_kind(),
                    attribute_1.attr_name(),
                    attribute_2.attr_name()
                );
                SourceError::new(entry_span, message)
                    .with_label(attr_span_1, "")
                    .with_label(attr_span_2, "")
            }
            Self::NonStaticAttr { attribute, expr_span } => {
                let message = format!(
                    "{} `{}` attribute must be static",
                    attribute.entry_kind(),
                    attribute.attr_name()
                );
                let label = "this expression isn't static";
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::OutOfRangeAttr { attribute, expr_span, value } => {
                // TODO: provide the valid range of values
                let message = format!(
                    "{} `{}` is out of range",
                    attribute.entry_kind(),
                    attribute.attr_name()
                );
                let label = format!("the value of this expression is {value}");
                SourceError::new(expr_span, message)
                    .with_label(expr_span, label)
            }
            Self::ParseError { error } => error.to_source_error(),
            Self::RegionRangeOverflow {
                entry_name,
                entry_span,
                start,
                size,
                bits,
            } => {
                let message = format!(
                    "`{entry_name}` memory range overflows {bits}-bit address \
                     size (start=${start:x}, size=${size:x})"
                );
                SourceError::new(entry_span, message)
            }
        }
    }
}

//===========================================================================//

/// A kind of config entry that can appear in a linker config.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConfigEntryKind {
    /// An address space entry.
    Addrspace,
    /// A checksum entry.
    Checksum,
    /// An export entry.
    Export,
    /// A region entry.
    Region,
    /// A section entry.
    Section,
}

impl ConfigEntryKind {
    /// Returns the human-readable name of this kind of config entry
    /// (e.g. `"address space"`).
    pub fn name(self) -> &'static str {
        match self {
            Self::Addrspace => "address space",
            Self::Checksum => "checksum",
            Self::Export => "exported symbol",
            Self::Region => "memory region",
            Self::Section => "section",
        }
    }

    /// Returns a list of all attributes for this entry kind.
    pub fn attributes(self) -> &'static [ConfigAttr] {
        match self {
            Self::Addrspace => {
                &[ConfigAttr::AddrspaceBits, ConfigAttr::AddrspaceFill]
            }
            Self::Checksum => &[
                ConfigAttr::ChecksumEnd,
                ConfigAttr::ChecksumSize,
                ConfigAttr::ChecksumStart,
                ConfigAttr::ChecksumSum,
                ConfigAttr::ChecksumUnit,
            ],
            Self::Export => &[ConfigAttr::ExportAddr, ConfigAttr::ExportSpace],
            Self::Region => &[
                ConfigAttr::RegionFill,
                ConfigAttr::RegionSize,
                ConfigAttr::RegionSpace,
                ConfigAttr::RegionStart,
            ],
            Self::Section => &[
                ConfigAttr::SectionAlign,
                ConfigAttr::SectionFill,
                ConfigAttr::SectionRegion,
                ConfigAttr::SectionSize,
                ConfigAttr::SectionStart,
                ConfigAttr::SectionWithin,
            ],
        }
    }
}

impl fmt::Display for ConfigEntryKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        f.write_str(self.name())
    }
}

//===========================================================================//

/// A kind of config attribute that can appear in a linker config.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConfigAttr {
    /// The `bits` attribute of an address space entry.
    AddrspaceBits,
    /// The `fill` attribute of an address space entry.
    AddrspaceFill,
    /// The `end` attribute of a checksum entry.
    ChecksumEnd,
    /// The `size` attribute of a checksum entry.
    ChecksumSize,
    /// The `start` attribute of a checksum entry.
    ChecksumStart,
    /// The `sum` attribute of a checksum entry.
    ChecksumSum,
    /// The `unit` attribute of a checksum entry.
    ChecksumUnit,
    /// The `addr` attribute of an export entry.
    ExportAddr,
    /// The `space` attribute of an export entry.
    ExportSpace,
    /// The `fill` attribute of a memory region entry.
    RegionFill,
    /// The `size` attribute of a memory region entry.
    RegionSize,
    /// The `space` attribute of a memory region entry.
    RegionSpace,
    /// The `start` attribute of a memory region entry.
    RegionStart,
    /// The `align` attribute of a section entry.
    SectionAlign,
    /// The `fill` attribute of a section entry.
    SectionFill,
    /// The `region` attribute of a section entry.
    SectionRegion,
    /// The `size` attribute of a section entry.
    SectionSize,
    /// The `start` attribute of a section entry.
    SectionStart,
    /// The `within` attribute of a section entry.
    SectionWithin,
}

impl ConfigAttr {
    /// Returns the kind of config entry that uses this attribute.
    pub fn entry_kind(self) -> ConfigEntryKind {
        match self {
            Self::AddrspaceBits | Self::AddrspaceFill => {
                ConfigEntryKind::Addrspace
            }
            Self::ChecksumEnd
            | Self::ChecksumSize
            | Self::ChecksumStart
            | Self::ChecksumSum
            | Self::ChecksumUnit => ConfigEntryKind::Checksum,
            Self::ExportAddr | Self::ExportSpace => ConfigEntryKind::Export,
            Self::RegionFill
            | Self::RegionSize
            | Self::RegionSpace
            | Self::RegionStart => ConfigEntryKind::Region,
            Self::SectionAlign
            | Self::SectionFill
            | Self::SectionRegion
            | Self::SectionSize
            | Self::SectionStart
            | Self::SectionWithin => ConfigEntryKind::Section,
        }
    }

    /// Returns the identifier name for the attribute (e.g. `"addr"`).
    pub fn attr_name(self) -> &'static str {
        match self {
            Self::ExportAddr => "addr",
            Self::SectionAlign => "align",
            Self::AddrspaceBits => "bits",
            Self::ChecksumEnd => "end",
            Self::AddrspaceFill | Self::RegionFill | Self::SectionFill => {
                "fill"
            }
            Self::SectionRegion => "region",
            Self::ChecksumSize | Self::RegionSize | Self::SectionSize => {
                "size"
            }
            Self::ExportSpace | Self::RegionSpace => "space",
            Self::ChecksumStart | Self::RegionStart | Self::SectionStart => {
                "start"
            }
            Self::ChecksumSum => "sum",
            Self::ChecksumUnit => "unit",
            Self::SectionWithin => "within",
        }
    }
}

//===========================================================================//
