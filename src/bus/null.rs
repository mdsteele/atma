use super::SimBus;

//===========================================================================//

/// A simulated bus that's not connected to anything.  All writes are ignored,
/// and all reads return zero.
pub struct NullBus {
    _null: (),
}

impl NullBus {
    /// Wraps the given bus by applying the given set of labels to it.
    pub fn new() -> NullBus {
        NullBus { _null: () }
    }
}

impl SimBus for NullBus {
    fn description(&self) -> String {
        "null bus".to_string()
    }

    fn label_at(&self, _addr: u32) -> Option<&str> {
        None
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
    use super::{NullBus, SimBus};

    #[test]
    fn description() {
        assert_eq!(NullBus::new().description(), "null bus");
    }
}

//===========================================================================//
