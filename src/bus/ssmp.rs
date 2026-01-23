use super::{Addr, SimBus, WatchId, WatchKind, new_rom_bus};
use bimap::BiHashMap;

//===========================================================================//

const SSMP_BOOT_ROM: [u8; 64] = [
    0xCD, 0xEF, 0xBD, 0xE8, 0x00, 0xC6, 0x1D, 0xD0, 0xFC, 0x8F, 0xAA, 0xF4,
    0x8F, 0xBB, 0xF5, 0x78, 0xCC, 0xF4, 0xD0, 0xFB, 0x2F, 0x19, 0xEB, 0xF4,
    0xD0, 0xFC, 0x7E, 0xF4, 0xD0, 0x0B, 0xE4, 0xF5, 0xCB, 0xF4, 0xD7, 0x00,
    0xFC, 0xD0, 0xF3, 0xAB, 0x01, 0x10, 0xEF, 0x7E, 0xF4, 0x10, 0xEB, 0xBA,
    0xF6, 0xDA, 0x00, 0xBA, 0xF4, 0xC4, 0xF4, 0xDD, 0x5D, 0xD0, 0xDB, 0x1F,
    0x00, 0x00, 0xC0, 0xFF,
];

const CONTROL_FLAG_ENABLE_BOOT_ROM: u8 = 0b1000_0000;
const CONTROL_FLAG_CLEAR_CPUI23: u8 = 0b0010_0000;
const CONTROL_FLAG_CLEAR_CPUI01: u8 = 0b0001_0000;

const INITIAL_CONTROL_FLAGS: u8 = CONTROL_FLAG_ENABLE_BOOT_ROM
    | CONTROL_FLAG_CLEAR_CPUI23
    | CONTROL_FLAG_CLEAR_CPUI01;

//===========================================================================//

/// Returns a simulated SNES S-SMP memory bus.
pub fn new_ssmp_bus(ram_bus: Box<dyn SimBus>) -> Box<dyn SimBus> {
    Box::new(SsmpBus::with_ram(ram_bus))
}

//===========================================================================//

/// A simulated SNES S-SMP memory bus.
struct SsmpBus {
    ram: Box<dyn SimBus>,
    hw_regs: SsmpRegBus,
    boot_rom: Box<dyn SimBus>,
}

impl SsmpBus {
    fn with_ram(ram_bus: Box<dyn SimBus>) -> SsmpBus {
        SsmpBus {
            ram: ram_bus,
            hw_regs: SsmpRegBus::new(),
            boot_rom: new_rom_bus(Box::new(SSMP_BOOT_ROM)),
        }
    }

    fn sub_bus(&self, addr: Addr) -> &dyn SimBus {
        let addr = addr.as_u16();
        match addr {
            0x00f0..0x0100 => &self.hw_regs,
            _ => {
                if addr >= 0xffc0 && self.hw_regs.boot_rom_enabled() {
                    &*self.boot_rom
                } else {
                    &*self.ram
                }
            }
        }
    }

    fn sub_bus_mut(&mut self, addr: Addr) -> &mut dyn SimBus {
        let addr = addr.as_u16();
        match addr {
            0x00f0..0x0100 => &mut self.hw_regs,
            _ => {
                if addr >= 0xffc0 && self.hw_regs.boot_rom_enabled() {
                    &mut *self.boot_rom
                } else {
                    &mut *self.ram
                }
            }
        }
    }
}

impl SimBus for SsmpBus {
    fn description(&self) -> String {
        format!("S-SMP with {}", self.ram.description())
    }

    fn label_at(&self, addr: Addr) -> Option<&str> {
        self.sub_bus(addr).label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        self.sub_bus(addr).watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        self.sub_bus_mut(addr).watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.ram
            .watch_label(label, kind)
            .or_else(|| self.hw_regs.watch_label(label, kind))
            .or_else(|| self.boot_rom.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.ram.unwatch(id);
        self.hw_regs.unwatch(id);
        self.boot_rom.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        self.sub_bus(addr).peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.sub_bus_mut(addr).read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        match addr.as_u16() {
            0x00f0..0x0100 => {
                self.hw_regs.write_byte(addr, data);
            }
            0xffc0.. => {
                if self.hw_regs.boot_rom_enabled() {
                    self.boot_rom.write_byte(addr, data);
                }
            }
            _ => {}
        }
        self.ram.write_byte(addr, data);
    }
}

//===========================================================================//

pub struct SsmpRegBus {
    watchpoints: BiHashMap<(u16, WatchKind), WatchId>,
    reg_control: u8,
    reg_dspaddr: u8,
    reg_auxio4: u8,
    reg_auxio5: u8,
}

impl SsmpRegBus {
    pub fn new() -> SsmpRegBus {
        SsmpRegBus {
            watchpoints: BiHashMap::new(),
            reg_control: INITIAL_CONTROL_FLAGS,
            reg_dspaddr: 0,
            reg_auxio4: 0,
            reg_auxio5: 0,
        }
    }

    pub fn boot_rom_enabled(&self) -> bool {
        (self.reg_control & CONTROL_FLAG_ENABLE_BOOT_ROM) != 0
    }
}

impl SimBus for SsmpRegBus {
    fn description(&self) -> String {
        "S-SMP registers".to_string()
    }

    fn label_at(&self, addr: Addr) -> Option<&str> {
        let addr = addr.as_u16();
        match addr {
            0x00f0 => Some("TEST"),
            0x00f1 => Some("CONTROL"),
            0x00f2 => Some("DSPADDR"),
            0x00f3 => Some("DSPDATA"),
            0x00f4 => Some("CPUIO0"),
            0x00f5 => Some("CPUIO1"),
            0x00f6 => Some("CPUIO2"),
            0x00f7 => Some("CPUIO3"),
            0x00f8 => Some("AUXIO4"),
            0x00f9 => Some("AUXIO5"),
            0x00fa => Some("T0DIV"),
            0x00fb => Some("T1DIV"),
            0x00fc => Some("T2DIV"),
            0x00fd => Some("T0OUT"),
            0x00fe => Some("T1OUT"),
            0x00ff => Some("T2OUT"),
            _ => None,
        }
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let addr = addr.as_u16();
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let addr = addr.as_u16();
        match self.watchpoints.get_by_left(&(addr, kind)) {
            Some(id) => *id,
            None => {
                let id = WatchId::create();
                self.watchpoints.insert((addr, kind), id);
                id
            }
        }
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        let addr: u8 = match label {
            "TEST" => 0x00f0,
            "CONTROL" => 0x00f1,
            "DSPADDR" => 0x00f2,
            "DSPDATA" => 0x00f3,
            "CPUIO0" => 0x00f4,
            "CPUIO1" => 0x00f5,
            "CPUIO2" => 0x00f6,
            "CPUIO3" => 0x00f7,
            "AUXIO4" => 0x00f8,
            "AUXIO5" => 0x00f9,
            "T0DIV" => 0x00fa,
            "T1DIV" => 0x00fb,
            "T2DIV" => 0x00fc,
            "T0OUT" => 0x00fd,
            "T1OUT" => 0x00fe,
            "T2OUT" => 0x00ff,
            _ => return None,
        };
        Some(self.watch_address(Addr::from(addr), kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        match addr.as_u16() {
            0x00f2 => self.reg_dspaddr,
            0x00f8 => self.reg_auxio4,
            0x00f9 => self.reg_auxio5,
            _ => 0, // TODO
        }
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        match addr.as_u16() {
            0x00f1 => self.reg_control = data,
            0x00f2 => self.reg_dspaddr = data,
            0x00f8 => self.reg_auxio4 = data,
            0x00f9 => self.reg_auxio5 = data,
            _ => {} // TODO
        }
    }
}

//===========================================================================//
