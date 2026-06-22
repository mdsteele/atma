mod build;
mod checksum;
mod env;

use crate::error::Errs;
use super::binary::LinkedBinary;
use super::error::{LinkError, LinkResult};
use super::eval::{LinkEvalEnv, LinkSymbolContext};
use super::patch::PatchedFile;
use super::types::AbsoluteLabel;
use super::positioned::PositionedBinary;
use crate::addr::{Addr, Align, Range, Size};
use crate::error::{SourceError, SourceResult};
use crate::expr::ExprValue;
use crate::obj::{ObjExpr, ObjFile};
use crate::parse::LinkConfigAst;
use build::ConfigBuilder;
pub use checksum::{ChecksumConfig, ChecksumFormat, ChecksumRange};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

/// A linker configuration for a binary.
#[derive(Debug)]
pub struct LinkConfig {
    /// Configurations for the address spaces that memory regions exist in.
    pub addrspaces: Vec<AddrspaceConfig>,
    /// Configurations for memory regions that padding-only sections can be
    /// placed within, and that will not be included in the final binary.
    pub bss: Vec<RegionConfig>,
    /// Configurations for memory regions that data sections can be placed
    /// within, and that will be included in the final binary.
    pub memory: Vec<RegionConfig>,
    /// Configurations for the data sections to be linked together.
    pub sections: Vec<SectionConfig>,
    /// Local non-static variables declared in this configuration, which can be
    /// evaluated after all chunks and sections have been positioned in their
    /// memory regions, and then used by export and checksum expressions.
    pub variables: Vec<ObjExpr>,
    /// Symbols defined and exported by the linker config.
    pub exports: Vec<ExportConfig>,
    /// Configurations for checksums to be calculated and written into the
    /// binary after the rest of the binary has been linked.
    pub checksums: Vec<ChecksumConfig>,
}

impl LinkConfig {
    /// Sources source code into a linker configuration.
    pub fn from_source(source: &str) -> SourceResult<LinkConfig> {
        let ast = LinkConfigAst::parse_source(source)
            .map_err(SourceError::from_errors)?;
        ConfigBuilder::new().build(ast)
    }

    /// Given a set of object files, patches all chunks and returns them in the
    /// order in which they appear in the final binary.
    pub fn link_objects(
        &self,
        object_files: Vec<ObjFile>,
    ) -> LinkResult<LinkedBinary> {
        let mut positioned_binary =
            PositionedBinary::position(self, &object_files)?;
        self.export_config_symbols(&mut positioned_binary)?;
        let patched_files =
            PatchedFile::patch_all(object_files, &positioned_binary)?;
        let mut binary = LinkedBinary::from_patched_files(
            patched_files,
            &positioned_binary,
        );
        for checksum in &self.checksums {
            checksum.calculate_and_write(&mut binary)?;
        }
        Ok(binary)
    }

    fn export_config_symbols(
        &self,
        positioned_binary: &mut PositionedBinary,
    ) -> LinkResult<()> {
        let eval_context = LinkSymbolContext {
            chunk_metadata: &[],
            symbol_addrs: HashMap::new(), // TODO: gather imported symbols
        };
        let mut eval_env = LinkEvalEnv::new();
        // TODO: An exported symbol should also be visible to subsequent .LET
        // directives, but right now this evaluates all .LETs before all
        // .EXPORTs.
        for expr in &self.variables {
            let value = eval_env.evaluate_expression(expr, &eval_context)?;
            eval_env.push_variable(value);
        }
        let mut errs = Errs::<LinkError>::new();
        for export in &self.exports {
            let Some(addr_value) = errs.ok(eval_env.evaluate_expression(&export.address, &eval_context)) else {
                continue;
            };
            let symbol_addr = match addr_value {
                ExprValue::Integer(value) => Addr::wrap_bigint(&value),
                _ => {
                    errs.push(LinkError::MalformedPatchExpression);
                    continue;
                }
            };
            let collision = positioned_binary.exported_symbols.insert(
                export.name.clone(),
                AbsoluteLabel {
                    space: export.space.clone(),
                    address: symbol_addr,
                },
            );
            if collision.is_some() {
                errs.push(LinkError::SymbolExportCollision {
                    symbol_name: export.name.clone(),
                });
            }
        }
        errs.result()
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
pub struct RegionConfig {
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

/// A linker configuration for a single exported symbol.
#[derive(Debug)]
pub struct ExportConfig {
    /// The name of the exported symbol.
    pub name: Rc<str>,
    /// The name of the address space that this symbol exists in.
    pub space: Rc<str>,
    /// An expression that evaluates to the address of the symbol.
    pub address: ObjExpr,
}

//===========================================================================//
