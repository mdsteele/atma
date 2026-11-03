use super::{SimBus, WatchId, WatchKind};
use crate::addr::{Addr, BitWidth};
use std::cell::RefCell;
use std::rc::Rc;

//===========================================================================//

pub(super) struct SharedBus {
    pub(super) shared: Rc<RefCell<Box<dyn SimBus>>>,
}

impl SimBus for SharedBus {
    fn description(&self) -> String {
        self.shared.borrow().description()
    }

    fn bit_width(&self) -> BitWidth {
        self.shared.borrow().bit_width()
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        self.shared.borrow().label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        self.shared.borrow().watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        self.shared.borrow_mut().watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.shared.borrow_mut().watch_label(label, kind)
    }

    fn unwatch(&mut self, id: WatchId) {
        self.shared.borrow_mut().unwatch(id)
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        self.shared.borrow().peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.shared.borrow_mut().read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        self.shared.borrow_mut().write_byte(addr, data)
    }
}

//===========================================================================//
