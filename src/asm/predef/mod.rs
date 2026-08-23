mod build;
mod mos6502;
mod pool;
mod sm83;
mod spc700;
mod superfx;
mod w65c816;

use self::build::BuiltinBuilder;
use self::pool::RcPool;
use super::arch::ArchTree;
use super::macros::MacroTable;
use crate::addr::Endianness;
use crate::obj::{ObjSrcContext, ObjSrcParent};
use std::rc::Rc;

//===========================================================================//

pub(super) fn make_predefined_arch_macros() -> (ArchTree, MacroTable) {
    let mut pool = RcPool::new();
    let mut arch_tree = ArchTree::new();
    define_arch(
        &mut pool,
        &mut arch_tree,
        mos6502::ARCH_65XX,
        ArchTree::ROOT_ARCH_NAME,
        mos6502::RES_65XX,
        Some(Endianness::LittleEndian),
    );
    define_arch(
        &mut pool,
        &mut arch_tree,
        mos6502::ARCH_6502,
        mos6502::ARCH_65XX,
        mos6502::RES_6502,
        Some(Endianness::LittleEndian),
    );
    define_arch(
        &mut pool,
        &mut arch_tree,
        w65c816::ARCH_65C816,
        mos6502::ARCH_65XX,
        w65c816::RES_65C816,
        Some(Endianness::LittleEndian),
    );
    define_arch(
        &mut pool,
        &mut arch_tree,
        sm83::ARCH_SM83,
        ArchTree::ROOT_ARCH_NAME,
        sm83::RES_SM83,
        Some(Endianness::LittleEndian),
    );
    define_arch(
        &mut pool,
        &mut arch_tree,
        spc700::ARCH_SPC700,
        ArchTree::ROOT_ARCH_NAME,
        spc700::RES_SPC700,
        Some(Endianness::LittleEndian),
    );
    define_arch(
        &mut pool,
        &mut arch_tree,
        superfx::ARCH_SUPERFX,
        ArchTree::ROOT_ARCH_NAME,
        superfx::RES_SUPERFX,
        Some(Endianness::LittleEndian),
    );
    let mut builder = BuiltinBuilder {
        arch_tree,
        macros: MacroTable::new(),
        context: Rc::new(ObjSrcContext {
            path: Rc::from(""), // internal definition
            parent: ObjSrcParent::Root,
        }),
        pool,
    };
    builder.add_macros(mos6502::ARCH_65XX, mos6502::MACROS_65XX);
    builder.add_macros(mos6502::ARCH_6502, mos6502::MACROS_6502);
    builder.add_macros(w65c816::ARCH_65C816, w65c816::MACROS_65C816);
    builder.add_macros(sm83::ARCH_SM83, sm83::MACROS_SM83);
    builder.add_macros(spc700::ARCH_SPC700, spc700::MACROS_SPC700);
    builder.add_macros(superfx::ARCH_SUPERFX, superfx::MACROS_SUPERFX);
    (builder.arch_tree, builder.macros)
}

fn define_arch(
    pool: &mut RcPool,
    arch_tree: &mut ArchTree,
    arch_name: &'static str,
    parent_name: &'static str,
    reserved: &'static [&'static str],
    endianness: Option<Endianness>,
) {
    let reserved =
        reserved.iter().map(|reg| pool.string(reg)).collect::<Vec<Rc<str>>>();
    arch_tree
        .define_arch(
            pool.string(arch_name),
            parent_name,
            &reserved,
            endianness,
        )
        .unwrap();
}

//===========================================================================//
