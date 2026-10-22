use super::{SimBus, WatchId, WatchKind};
use crate::addr::{Addr, BitWidth};
use bimap::BiHashMap;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

//===========================================================================//

/// Wraps another `SimBus` and applies labels to it.
pub fn new_labeled_bus<I>(inner: Box<dyn SimBus>, labels: I) -> Box<dyn SimBus>
where
    I: IntoIterator<Item = (Addr, Rc<str>)>,
{
    let bit_width = inner.bit_width();
    let address_labels = HashMap::from_iter(
        labels.into_iter().map(|(addr, label)| (bit_width.wrap(addr), label)),
    );
    if address_labels.is_empty() {
        return inner;
    }
    let labels = HashSet::from_iter(address_labels.values().cloned());
    let label_watchpoints = BiHashMap::new();
    Box::new(LabeledBus { inner, labels, address_labels, label_watchpoints })
}

//===========================================================================//

struct LabeledBus {
    inner: Box<dyn SimBus>,
    labels: HashSet<Rc<str>>,
    address_labels: HashMap<Addr, Rc<str>>,
    label_watchpoints: BiHashMap<(Rc<str>, WatchKind), WatchId>,
}

impl SimBus for LabeledBus {
    fn description(&self) -> String {
        self.inner.description()
    }

    fn bit_width(&self) -> BitWidth {
        self.inner.bit_width()
    }

    fn label_at(&self, addr: Addr) -> Option<&str> {
        match self.address_labels.get(&self.bit_width().wrap(addr)) {
            Some(label) => Some(Rc::as_ref(label)),
            None => self.inner.label_at(addr),
        }
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        if let Some(label) =
            self.address_labels.get(&self.bit_width().wrap(addr))
            && let Some(&id) =
                self.label_watchpoints.get_by_left(&(label.clone(), kind))
        {
            Some(id)
        } else {
            self.inner.watchpoint_at(addr, kind)
        }
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        self.inner.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        if let Some(label_rc) = self.labels.get(label) {
            let id = WatchId::create();
            self.label_watchpoints.insert((label_rc.clone(), kind), id);
            Some(id)
        } else {
            self.inner.watch_label(label, kind)
        }
    }

    fn unwatch(&mut self, id: WatchId) {
        self.inner.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        self.inner.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.inner.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        self.inner.write_byte(addr, data)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::new_labeled_bus;
    use crate::addr::{Addr, BitWidth};
    use crate::bus::new_open_bus;
    use std::collections::HashMap;
    use std::rc::Rc;

    #[test]
    fn labels() {
        let mut labels = HashMap::new();
        labels.insert(Addr::from(0x0100u16), Rc::from("foo"));
        labels.insert(Addr::from(0x0200u16), Rc::from("bar"));
        let bus = new_labeled_bus(new_open_bus(BitWidth::SIXTEEN), labels);
        assert_eq!(bus.label_at(Addr::from(0x0100u16)), Some("foo"));
        assert_eq!(bus.label_at(Addr::from(0x0200u16)), Some("bar"));
        assert_eq!(bus.label_at(Addr::from(0x0300u16)), None);
    }

    #[test]
    fn delegate_labels() {
        let mut labels = HashMap::new();
        labels.insert(Addr::from(0x0100u16), Rc::from("foo"));
        labels.insert(Addr::from(0x0200u16), Rc::from("bar"));
        let bus = new_labeled_bus(new_open_bus(BitWidth::SIXTEEN), labels);
        let mut labels = HashMap::new();
        labels.insert(Addr::from(0x0200u16), Rc::from("quux"));
        labels.insert(Addr::from(0x0300u16), Rc::from("baz"));
        let bus = new_labeled_bus(bus, labels);
        assert_eq!(bus.label_at(Addr::from(0x0100u16)), Some("foo"));
        assert_eq!(bus.label_at(Addr::from(0x0200u16)), Some("quux"));
        assert_eq!(bus.label_at(Addr::from(0x0300u16)), Some("baz"));
        assert_eq!(bus.label_at(Addr::from(0x0400u16)), None);
    }
}

//===========================================================================//
