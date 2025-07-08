use super::SimBus;

//===========================================================================//

/// Returns a simulated bus that's not connected to anything.  All writes will
/// be ignored, and all reads will return zero.
pub fn null_bus() -> Box<dyn SimBus> {
    Box::new(NullBus {})
}

struct NullBus {}

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
    use super::null_bus;

    #[test]
    fn description() {
        assert_eq!(null_bus().description(), "null bus");
    }
}

//===========================================================================//
