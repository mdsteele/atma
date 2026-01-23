use crate::bus::{Addr, SimBus, WatchKind};
use crate::proc::SimBreak;

//===========================================================================//

pub(crate) fn pack(hi: u8, lo: u8) -> u16 {
    ((hi as u16) << 8) | (lo as u16)
}

pub(crate) fn unpack(word: u16) -> (u8, u8) {
    ((word >> 8) as u8, (word & 0xff) as u8)
}

pub(crate) fn watch(
    bus: &dyn SimBus,
    addr: Addr,
    kind: WatchKind,
) -> Result<(), SimBreak> {
    if let Some(id) = bus.watchpoint_at(addr, kind) {
        Err(SimBreak::Watchpoint(kind, id))
    } else {
        Ok(())
    }
}

//===========================================================================//
