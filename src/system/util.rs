use crate::addr::Addr;
use crate::bus::{SimBus, new_labeled_bus, new_rom_bus};
use crate::parse::SymbolTable;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) fn new_rom_bus_with_binary_symbols(
    rom_data: Box<[u8]>,
    rom_start: u64,
    space: &str,
    symbols: &SymbolTable,
) -> Box<dyn SimBus> {
    let rom_data_len = rom_data.len() as u64;
    let labels = symbols.entries.iter().filter_map(|symbol| {
        symbol.binary_offset.and_then(|offset| {
            if offset >= rom_start
                && offset - rom_start < rom_data_len
                && Rc::as_ref(&symbol.space) == space
                && let Ok(addr) = Addr::try_from(offset - rom_start)
            {
                Some((addr, symbol.name.clone()))
            } else {
                None
            }
        })
    });
    new_labeled_bus(new_rom_bus(rom_data), labels)
}

pub(super) fn with_offset_free_symbols(
    inner_bus: Box<dyn SimBus>,
    space: &str,
    symbols: &SymbolTable,
) -> Box<dyn SimBus> {
    let mut labels = HashMap::<Addr, Rc<str>>::new();
    for symbol in &symbols.entries {
        if symbol.binary_offset.is_some() || Rc::as_ref(&symbol.space) != space
        {
            continue;
        }
        if symbol.is_external || !labels.contains_key(&symbol.address) {
            labels.insert(symbol.address, symbol.name.clone());
        }
    }
    new_labeled_bus(inner_bus, labels)
}

pub(super) fn with_symbols(
    inner_bus: Box<dyn SimBus>,
    space: &str,
    symbols: &SymbolTable,
) -> Box<dyn SimBus> {
    let mut labels = HashMap::<Addr, Rc<str>>::new();
    for symbol in &symbols.entries {
        if Rc::as_ref(&symbol.space) != space {
            continue;
        }
        if symbol.is_external || !labels.contains_key(&symbol.address) {
            labels.insert(symbol.address, symbol.name.clone());
        }
    }
    new_labeled_bus(inner_bus, labels)
}

//===========================================================================//
