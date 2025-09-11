use super::{SimBus, WatchId, WatchKind};
use bimap::BiHashMap;

//===========================================================================//

/// Wraps another `SimBus` and applies labels to it.
pub struct LabeledBus {
    inner: Box<dyn SimBus>,
    labels: BiHashMap<String, u32>,
}

impl LabeledBus {
    /// Wraps the given bus by applying the given set of labels to it.
    pub fn new<I>(inner: Box<dyn SimBus>, labels: I) -> LabeledBus
    where
        I: IntoIterator<Item = (String, u32)>,
    {
        LabeledBus { inner, labels: BiHashMap::from_iter(labels) }
    }
}

impl SimBus for LabeledBus {
    fn description(&self) -> String {
        self.inner.description()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match self.labels.get_by_right(&addr) {
            Some(label) => Some(label.as_str()),
            None => self.inner.label_at(addr),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        self.inner.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        self.inner.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        if let Some(&addr) = self.labels.get_by_left(label) {
            Some(self.inner.watch_address(addr, kind))
        } else {
            self.inner.watch_label(label, kind)
        }
    }

    fn unwatch(&mut self, id: WatchId) {
        self.inner.unwatch(id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        self.inner.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        self.inner.read_byte(addr)
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        self.inner.write_byte(addr, data)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::LabeledBus;
    use crate::bus::{SimBus, new_null_bus};
    use std::collections::HashMap;

    #[test]
    fn labels() {
        let mut labels = HashMap::new();
        labels.insert("foo".to_string(), 0x100);
        labels.insert("bar".to_string(), 0x200);
        let bus = LabeledBus::new(new_null_bus(), labels);
        assert_eq!(bus.label_at(0x100), Some("foo"));
        assert_eq!(bus.label_at(0x200), Some("bar"));
        assert_eq!(bus.label_at(0x300), None);
    }

    #[test]
    fn delegate_labels() {
        let mut labels = HashMap::new();
        labels.insert("foo".to_string(), 0x100);
        labels.insert("bar".to_string(), 0x200);
        let bus = Box::new(LabeledBus::new(new_null_bus(), labels));
        let mut labels = HashMap::new();
        labels.insert("quux".to_string(), 0x200);
        labels.insert("baz".to_string(), 0x300);
        let bus = LabeledBus::new(bus, labels);
        assert_eq!(bus.label_at(0x100), Some("foo"));
        assert_eq!(bus.label_at(0x200), Some("quux"));
        assert_eq!(bus.label_at(0x300), Some("baz"));
        assert_eq!(bus.label_at(0x400), None);
    }
}

//===========================================================================//
