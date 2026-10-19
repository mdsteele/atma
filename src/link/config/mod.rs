mod attrs;
mod build;
mod builtin;
mod checksum;
mod env;
mod error;
mod variable;

use super::binary::LinkedBinary;
use super::error::{LinkError, LinkResult};
use super::eval::{LinkEvalEnv, LinkSymbolContext};
use super::patch::PatchedFile;
use super::positioned::PositionedBinary;
use super::types::{AbsoluteLabel, AddressSpace};
use crate::addr::{Addr, Align, Range, Size};
use crate::error::{Errs, SrcCache};
use crate::expr::ExprValue;
use crate::obj::{ObjExpr, ObjFile, ObjSrcLoc};
use build::ConfigBuilder;
pub use checksum::{ChecksumConfig, ChecksumFormat, ChecksumRange};
pub use error::{ConfigAttr, ConfigEntryKind, ConfigError, ConfigResult};
use std::collections::HashMap;
use std::rc::Rc;
pub use variable::ConfigVariableOr;

//===========================================================================//

/// A linker configuration for a binary.
#[derive(Debug)]
pub struct LinkConfig {
    /// Configurations for the address spaces that memory regions exist in.
    pub addrspaces: Vec<AddrspaceConfig>,
    /// Configurations for memory regions that sections can be placed
    /// within.
    pub regions: Vec<RegionConfig>,
    /// Configurations for the data sections to be linked together.
    pub sections: Vec<SectionConfig>,
    /// Names of symbols that are imported into this configuration from object
    /// files.  These symbols can be used to define non-static variables,
    /// exports, and checksums.
    pub imports: Vec<ImportConfig>,
    /// Local non-static variables declared in this configuration, which are to
    /// be evaluated (in order) after all chunks and sections have been
    /// positioned in their memory regions, and can then be used by export and
    /// checksum configurations (or by later variables).
    pub variables: Vec<ObjExpr>,
    /// Symbols defined and exported by the linker config.
    pub exports: Vec<ExportConfig>,
    /// Configurations for checksums to be calculated and written into the
    /// binary after the rest of the binary has been linked.
    pub checksums: Vec<ChecksumConfig>,
}

impl LinkConfig {
    /// Sources source code into a linker configuration.
    pub fn from_source(
        cache: &mut dyn SrcCache,
        src_path: Rc<str>,
        source_code: &str,
    ) -> ConfigResult<LinkConfig> {
        ConfigBuilder::new(cache, src_path).build(source_code)
    }

    /// Given a set of object files, patches all chunks and returns them in the
    /// order in which they appear in the final binary.
    pub fn link_objects(
        &self,
        object_files: Vec<ObjFile>,
    ) -> LinkResult<LinkedBinary> {
        let mut positioned_binary =
            PositionedBinary::position(self, &object_files)?;
        let mut eval_env = LinkEvalEnv::new();
        self.resolve_symbols(&mut positioned_binary, &mut eval_env)?;
        let patched_files =
            PatchedFile::patch_all(object_files, &positioned_binary)?;
        let mut binary = LinkedBinary::from_patched_files(
            patched_files,
            &positioned_binary,
        );
        for checksum in &self.checksums {
            checksum.calculate_and_write(&mut binary, &eval_env)?;
        }
        Ok(binary)
    }

    fn resolve_symbols(
        &self,
        positioned_binary: &mut PositionedBinary,
        eval_env: &mut LinkEvalEnv,
    ) -> LinkResult<()> {
        let mut errs = Errs::<LinkError>::new();
        let symbol_addrs = errs.with(self.resolve_imports(positioned_binary));
        errs.also(self.evaluate_variables(
            symbol_addrs,
            positioned_binary,
            eval_env,
        ));
        for export in &self.exports {
            errs.also(self.export_symbol(export, positioned_binary, eval_env));
        }
        errs.result()
    }

    fn resolve_imports(
        &self,
        positioned_binary: &PositionedBinary,
    ) -> (HashMap<Rc<str>, Option<AbsoluteLabel>>, Errs<LinkError>) {
        let mut errs = Errs::<LinkError>::new();
        let mut symbol_addrs =
            HashMap::<Rc<str>, Option<AbsoluteLabel>>::new();
        for import in &self.imports {
            let opt_absolute_label = positioned_binary
                .external_symbols
                .get(&import.name)
                .map(|symbol| symbol.absolute.clone());
            if opt_absolute_label.is_none() {
                errs.push(LinkError::SymbolImportUnresolved {
                    symbol_name: import.name.clone(),
                    import_loc: import.loc.clone(),
                });
            }
            symbol_addrs.insert(import.name.clone(), opt_absolute_label);
        }
        (symbol_addrs, errs)
    }

    fn evaluate_variables(
        &self,
        symbol_addrs: HashMap<Rc<str>, Option<AbsoluteLabel>>,
        positioned_binary: &PositionedBinary,
        eval_env: &mut LinkEvalEnv,
    ) -> LinkResult<()> {
        let mut errs = Errs::<LinkError>::new();
        let symbol_context = LinkSymbolContext {
            address_space_widths: &positioned_binary.address_space_widths,
            chunk_metadata: &[], // a linker config has no chunks
            symbol_addrs,
        };
        for expr in &self.variables {
            errs.also(eval_env.evaluate_variable(expr, &symbol_context));
        }
        errs.result()
    }

    fn export_symbol(
        &self,
        export: &ExportConfig,
        positioned_binary: &mut PositionedBinary,
        eval_env: &mut LinkEvalEnv,
    ) -> LinkResult<()> {
        let symbol_addr =
            eval_env.resolve(export.address, |value| match value {
                ExprValue::Integer(bigint) => Ok(Addr::wrap_bigint(bigint)),
                _ => Err(Errs::one(LinkError::MalformedPatchExpression {
                    message: Rc::from("link export addr is not an integer"),
                })),
            })?;
        let absolute = AbsoluteLabel {
            space: AddressSpace {
                name: export.space.clone(),
                width: positioned_binary.address_space_widths[&export.space],
            },
            address: symbol_addr,
        };
        positioned_binary.add_external_symbol(
            &export.name,
            &export.name_loc,
            absolute,
        )
    }
}

//===========================================================================//

/// A linker configuration for a single address space.
#[derive(Debug)]
pub struct AddrspaceConfig {
    /// The name of this address space.
    pub name: Rc<str>,
    /// The bit width of this address space.
    pub width: Align,
    /// Any padded portions of non-BSS memory regions in this address space
    /// will be filled with this byte value by default.
    pub fill: u8,
}

//===========================================================================//

/// A linker configuration for a single memory region.
#[derive(Debug)]
pub struct RegionConfig {
    /// The name of this memory region.
    pub name: Rc<str>,
    /// The linker config source code location where the memory region was
    /// declared.
    pub name_loc: ObjSrcLoc,
    /// The name of the address space that this memory region exists in.
    pub space: Rc<str>,
    /// The range of addresses covered by this memory region.
    pub range: Range,
    /// If set, then any padded portions of the memory region will be filled
    /// with this byte value. Otherwise, they will be filled with this region's
    /// address space's fill byte.
    pub fill: Option<u8>,
    /// What kind of memory region this is.
    pub kind: RegionKind,
}

//===========================================================================//

/// Describes a kind of memory region, and controls how that region is handled
/// during linking.
#[derive(Clone, Copy, Debug, Default, Hash, Eq, PartialEq)]
pub enum RegionKind {
    /// A memory region that contains data, and that should be included in the
    /// final binary.  This is the default.
    #[default]
    Data,
    /// A memory region that contains data, but that should not be included in
    /// the final binary.
    Omit,
    /// A memory region that contains no initial data (only reserved space and
    /// padding), and that should not be included in the final binary.
    Bss,
}

//===========================================================================//

/// A linker configuration for a single data section.
#[derive(Debug)]
pub struct SectionConfig {
    /// The name of this section.
    pub name: Rc<str>,
    /// The linker config source code location where the section was
    /// declared.
    pub name_loc: ObjSrcLoc,
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

/// A linker configuration for a single imported symbol.
#[derive(Debug)]
pub struct ImportConfig {
    /// The name of the imported symbol.
    pub name: Rc<str>,
    /// The linker config source location for the import.
    pub loc: ObjSrcLoc,
}

//===========================================================================//

/// A linker configuration for a single exported symbol.
#[derive(Debug)]
pub struct ExportConfig {
    /// The name of the exported symbol.
    pub name: Rc<str>,
    /// The linker config source code location where the export was declared.
    pub name_loc: ObjSrcLoc,
    /// The name of the address space that this symbol exists in.
    pub space: Rc<str>,
    /// The address of the symbol.
    pub address: ConfigVariableOr<Addr>,
}

//===========================================================================//
