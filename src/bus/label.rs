use super::SimBus;
use std::collections::HashMap;

//===========================================================================//

/// Wraps another `SimBus` and applies labels to it.
pub struct LabeledBus {
    inner: Box<dyn SimBus>,
    labels: HashMap<u32, String>,
}

impl LabeledBus {
    /// Wraps the given bus by applying the given set of labels to it.
    pub fn new(
        inner: Box<dyn SimBus>,
        labels: HashMap<u32, String>,
    ) -> LabeledBus {
        LabeledBus { inner, labels }
    }
}

impl SimBus for LabeledBus {
    fn description(&self) -> String {
        self.inner.description()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match self.labels.get(&addr) {
            Some(label) => Some(label.as_str()),
            None => self.inner.label_at(addr),
        }
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
    use crate::bus::{NullBus, SimBus};
    use std::collections::HashMap;

    #[test]
    fn labels() {
        let bus = Box::new(NullBus::new());
        let mut labels = HashMap::new();
        labels.insert(0x100, "foo".to_string());
        labels.insert(0x200, "bar".to_string());
        let bus = LabeledBus::new(bus, labels);
        assert_eq!(bus.label_at(0x100), Some("foo"));
        assert_eq!(bus.label_at(0x200), Some("bar"));
        assert_eq!(bus.label_at(0x300), None);
    }

    #[test]
    fn delegate_labels() {
        let bus = Box::new(NullBus::new());
        let mut labels = HashMap::new();
        labels.insert(0x100, "foo".to_string());
        labels.insert(0x200, "bar".to_string());
        let bus = Box::new(LabeledBus::new(bus, labels));
        let mut labels = HashMap::new();
        labels.insert(0x200, "quux".to_string());
        labels.insert(0x300, "baz".to_string());
        let bus = LabeledBus::new(bus, labels);
        assert_eq!(bus.label_at(0x100), Some("foo"));
        assert_eq!(bus.label_at(0x200), Some("quux"));
        assert_eq!(bus.label_at(0x300), Some("baz"));
        assert_eq!(bus.label_at(0x400), None);
    }
}

//===========================================================================//
