use super::{SimBus, WatchId, WatchKind};
use bimap::BiHashMap;

//===========================================================================//

/// Returns a simulated bus that's not connected to anything.  There are no
/// address lines, all writes will be ignored, and all reads will return open
/// bus.
pub fn new_null_bus() -> Box<dyn SimBus> {
    Box::new(NullBus::new())
}

struct NullBus {
    watchpoints: BiHashMap<WatchKind, WatchId>,
}

impl NullBus {
    pub fn new() -> NullBus {
        NullBus { watchpoints: BiHashMap::new() }
    }
}

impl SimBus for NullBus {
    fn description(&self) -> String {
        "null bus".to_string()
    }

    fn label_at(&self, _addr: u32) -> Option<&str> {
        None
    }

    fn watchpoint_at(&self, _addr: u32, kind: WatchKind) -> Option<WatchId> {
        self.watchpoints.get_by_left(&kind).cloned()
    }

    fn watch_address(&mut self, _addr: u32, kind: WatchKind) -> WatchId {
        match self.watchpoints.get_by_left(&kind) {
            Some(id) => *id,
            None => {
                let id = WatchId::create();
                self.watchpoints.insert(kind, id);
                id
            }
        }
    }

    fn watch_label(
        &mut self,
        _label: &str,
        _kind: WatchKind,
    ) -> Option<WatchId> {
        None
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, _addr: u32) -> u8 {
        0
    }

    fn read_byte(&mut self, _addr: u32) -> u8 {
        0
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {}
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::new_null_bus;

    #[test]
    fn description() {
        assert_eq!(new_null_bus().description(), "null bus");
    }
}

//===========================================================================//
