use crate::addr::Size;
use crate::error::{Errs, SourceError};
use crate::expr::ExprEvalError;
use crate::obj::{ObjPatchIntType, ObjSrcContext, ObjSrcLoc};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

/// A specialized `Result` type for linking object files.
pub type LinkResult<T> = Result<T, Errs<LinkError>>;

//===========================================================================//

/// An error encountered during linking.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LinkError {
    /// A miscellaneous error.
    Misc, // TODO: remove this
    /// An assertion condition evaluated to false.
    AssertionFailed {
        /// An additional message provided by the assertion.
        message: Option<Rc<str>>,
    },
    /// The final binary size would be too large to create.
    BinaryTooLarge {
        /// The name of the memory region that would not fit in the final
        /// binary.
        region_name: Rc<str>,
    },
    /// A chunk was unable to be arranged within its section, given the
    /// constraints.
    ChunkCannotBePlaced,
    /// A chunk in an object file has a declared size smaller than its data
    /// payload.
    ChunkDataLargerThanSize {
        /// The length of the chunk's data payload.
        chunk_data_len: usize,
        /// The declared size of the chunk.
        chunk_size: Size,
    },
    /// A chunk in an object file belongs to a section that is not declared in
    /// the linker config.
    ChunkSectionDoesNotExist {
        /// The name of the nonexistent linker section to which the chunk
        /// should belong.
        section_name: Rc<str>,
        /// The source code location for the expression in the chunk
        /// declaration that evaluated to the nonexistent section name.
        section_name_loc: ObjSrcLoc,
    },
    /// A chunk in a BSS memory region contained non-padding data.
    DataInBssChunk,
    /// An error occurred while evaluating an expression at link time.
    ExprEvalError {
        /// The context in which the evaluation error occurred.
        context: Rc<ObjSrcContext>,
        /// The evaluation error.
        error: ExprEvalError,
    },
    /// A chunk in a BSS memory region had an explicit fill byte set.
    FillByteOnBssChunk,
    /// A BSS memory region had an explicit fill byte set.
    FillByteOnBssRegion,
    /// A section in a BSS memory region had an explicit fill byte set.
    FillByteOnBssSection,
    /// A checksum range went beyond the size of the binary.
    InvalidChecksumRange {
        /// The symbol name where the checksum is to be stored.
        checksum_symbol: Rc<str>,
        /// The size of the binary, in bytes.
        binary_size: u64,
        /// The byte offset for the start of the checksum range.
        start: u64,
        /// The byte offset for the end of the checksum range.
        end: u64,
    },
    /// A patch's expression was malformed in some way (e.g. stack
    /// underflow). This shouldn't happen for valid object files (as the
    /// assembler should have generated a valid expression).
    MalformedPatchExpression,
    /// A patch's offset/size was out of range for the size of the chunk
    /// data. This shouldn't happen for valid object files (as the assembler
    /// should have generated a valid patch offset).
    PatchOffsetOutOfRange,
    /// An integer patch expression evaluated to a value that is out of range
    /// for that patch's integer type.
    PatchValueOutOfRange {
        /// The kind of patch that failed.
        int_type: ObjPatchIntType,
        /// The out-of-range expression value.
        value: BigInt,
    },
    /// A patch expression evaluated to a value of the wrong type. This
    /// shouldn't happen for valid object files (as the assembler should have
    /// caught the type error).
    PatchValueWrongType,
    /// A section was unable to be positioned within its memory region, given
    /// the constraints.
    SectionCannotBePlaced {
        /// The name of the memory region that the section didn't fit into.
        region_name: Rc<str>,
        /// The linker config source code location where the region was
        /// declared.
        region_loc: ObjSrcLoc,
        /// The name of the section that couldn't be placed.
        section_name: Rc<str>,
        /// The linker config source code location where the section was
        /// declared.
        section_loc: ObjSrcLoc,
    },
    /// A section contained no data.
    SectionIsEmpty {
        /// The name of the empty section.
        section_name: Rc<str>,
        /// The linker config source code location where the section was
        /// declared.
        section_loc: ObjSrcLoc,
    },
    /// Two symbols were exported with the same name.
    SymbolExportCollision {
        /// The fully qualified name shared by the symbols.
        symbol_name: Rc<str>,
        /// The source code location for the duplicate instance of a symbol
        /// with this name being exported.
        export_loc: ObjSrcLoc,
        /// The source code location for the earlier instance of a symbol with
        /// this name being exported.
        prev_loc: ObjSrcLoc,
    },
    /// An object file imported a symbol that was never exported by any other
    /// object file.
    SymbolImportUnresolved {
        /// The fully qualified name of the imported symbol.
        symbol_name: Rc<str>,
        /// The source code location where the symbol was imported.
        import_loc: ObjSrcLoc,
    },
}

impl LinkError {
    /// Converts the error into a `SourceError`.
    pub fn to_source_error(self) -> SourceError {
        match self {
            Self::ChunkSectionDoesNotExist {
                section_name,
                section_name_loc,
            } => {
                let message = format!(
                    "section {section_name:?} was never declared in the \
                     linker config"
                );
                SourceError::new(section_name_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*section_name_loc.context)
            }
            Self::ExprEvalError { context, error } => {
                error.to_source_error(&context.path).with_context(&*context)
            }
            Self::SectionCannotBePlaced {
                region_name,
                region_loc,
                section_name,
                section_loc,
            } => {
                let message = format!(
                    "unable to place section {section_name:?} anywhere in \
                     {region_name:?}"
                );
                let region_label =
                    format!("region {region_name:?} was declared here");
                let section_label =
                    format!("section {section_name:?} was declared here");
                SourceError::new(section_loc.primary(), message)
                    .with_label(region_loc.primary(), region_label)
                    .with_context(&*region_loc.context)
                    .with_primary_label(section_label)
                    .with_context(&*section_loc.context)
            }
            Self::SectionIsEmpty { section_name, section_loc } => {
                let message =
                    format!("section {section_name:?} has a size of zero");
                SourceError::new(section_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*section_loc.context)
            }
            Self::SymbolExportCollision {
                symbol_name,
                export_loc,
                prev_loc,
            } => {
                let message = format!(
                    "symbol `{symbol_name}` was exported more than once"
                );
                let prev_label = "Previously exported here";
                let export_label = "Exported again here";
                SourceError::new(export_loc.primary(), message)
                    .with_label(prev_loc.primary(), prev_label)
                    .with_context(&*prev_loc.context)
                    .with_primary_label(export_label)
                    .with_context(&*export_loc.context)
            }
            Self::SymbolImportUnresolved { symbol_name, import_loc } => {
                let message = format!(
                    "imported symbol `{symbol_name}` was never exported \
                     anywhere"
                );
                SourceError::new(import_loc.primary(), message)
                    .with_primary_label("")
                    .with_context(&*import_loc.context)
            }
            other => todo!("{other:?}"),
        }
    }
}

//===========================================================================//
