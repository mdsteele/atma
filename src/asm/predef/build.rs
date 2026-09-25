use super::super::arch::ArchTree;
use super::super::macros::MacroTable;
use super::addrmode::AddrMode;
use super::pool::RcPool;
use crate::obj::ObjSrcContext;
use crate::parse::AsmDefMacroAst;
use std::rc::Rc;

//===========================================================================//

pub(super) struct BuiltinBuilder {
    pub(super) arch_tree: ArchTree,
    pub(super) macros: MacroTable,
    pub(super) context: Rc<ObjSrcContext>,
    pub(super) pool: RcPool,
}

impl BuiltinBuilder {
    pub fn add_macros<A: AddrMode>(
        &mut self,
        arch: &'static str,
        macros: &[(&'static str, &[u8], A)],
    ) {
        let arch = self.pool.string(arch);
        for (name, prefix_bytes, addr_mode) in macros {
            self.add_macro(&arch, name, prefix_bytes, addr_mode);
        }
    }

    fn add_macro<A: AddrMode>(
        &mut self,
        arch: &Rc<str>,
        name: &'static str,
        prefix_bytes: &[u8],
        addr_mode: &A,
    ) {
        let params = addr_mode.macro_args(&mut self.pool);
        let body = addr_mode.macro_body(&mut self.pool, prefix_bytes);
        let definition =
            AsmDefMacroAst { id: self.pool.standard_id(name), params, body };
        let reserved = self.arch_tree.reserved_names(arch);
        self.macros
            .define(self.context.clone(), arch, reserved, definition)
            .unwrap();
    }
}

//===========================================================================//
