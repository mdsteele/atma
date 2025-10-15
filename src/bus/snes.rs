use super::{SimBus, WatchId, WatchKind, new_ram_bus};
use bimap::BiHashMap;

//===========================================================================//

/// Returns a simulated SNES CPU memory bus.
pub fn new_snes_bus(cartridge_bus: Box<dyn SimBus>) -> Box<dyn SimBus> {
    Box::new(SnesBus::with_cartridge(cartridge_bus))
}

//===========================================================================//

/// A simulated SNES CPU memory bus.
struct SnesBus {
    wram: Box<dyn SimBus>,
    cpu_regs: CpuRegBus,
    dma_regs: DmaRegBus,
    joy_regs: JoyRegBus,
    ppu_regs: PpuRegBus,
    cart: Box<dyn SimBus>,
}

impl SnesBus {
    fn with_cartridge(cartridge_bus: Box<dyn SimBus>) -> SnesBus {
        SnesBus {
            wram: new_ram_bus(Box::new([0u8; 0x20000])),
            cpu_regs: CpuRegBus::new(),
            dma_regs: DmaRegBus::new(),
            joy_regs: JoyRegBus::new(),
            ppu_regs: PpuRegBus::new(),
            cart: cartridge_bus,
        }
    }

    fn sub_bus(&self, addr: u32) -> (&dyn SimBus, u32) {
        let bank = (addr >> 16) as u8;
        let abs = addr as u16;
        match bank {
            0x00..0x40 | 0x80..0xc0 => match abs {
                0x0000..0x2000 => (&*self.wram, u32::from(abs)),
                0x2100..0x2200 => (&self.ppu_regs, addr),
                0x4016..0x4018 => (&self.joy_regs, addr),
                0x4200..0x4220 => (&self.cpu_regs, addr),
                0x4300..0x4380 => (&self.dma_regs, addr),
                _ => (&*self.cart, addr),
            },
            0x7e..0x80 => (&*self.wram, addr),
            _ => (&*self.cart, addr),
        }
    }

    fn sub_bus_mut(&mut self, addr: u32) -> (&mut dyn SimBus, u32) {
        let bank = (addr >> 16) as u8;
        let abs = addr as u16;
        match bank {
            0x00..0x40 | 0x80..0xc0 => match abs {
                0x0000..0x2000 => (&mut *self.wram, u32::from(abs)),
                0x2100..0x2200 => (&mut self.ppu_regs, addr),
                0x4016..0x4018 => (&mut self.joy_regs, addr),
                0x4200..0x4220 => (&mut self.cpu_regs, addr),
                0x4300..0x4380 => (&mut self.dma_regs, addr),
                _ => (&mut *self.cart, addr),
            },
            0x7e..0x80 => (&mut *self.wram, addr),
            _ => (&mut *self.cart, addr),
        }
    }
}

impl SimBus for SnesBus {
    fn description(&self) -> String {
        format!("SNES with {}", self.cart.description())
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        let (bus, addr) = self.sub_bus(addr);
        bus.label_at(addr)
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        let (bus, addr) = self.sub_bus(addr);
        bus.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.cart
            .watch_label(label, kind)
            .or_else(|| self.wram.watch_label(label, kind))
            .or_else(|| self.cpu_regs.watch_label(label, kind))
            .or_else(|| self.dma_regs.watch_label(label, kind))
            .or_else(|| self.joy_regs.watch_label(label, kind))
            .or_else(|| self.ppu_regs.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.wram.unwatch(id);
        self.cpu_regs.unwatch(id);
        self.dma_regs.unwatch(id);
        self.joy_regs.unwatch(id);
        self.ppu_regs.unwatch(id);
        self.cart.unwatch(id);
    }

    fn peek_byte(&self, addr: u32) -> u8 {
        let (bus, addr) = self.sub_bus(addr);
        bus.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: u32) -> u8 {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.read_byte(addr)
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.write_byte(addr, data);
    }
}

//===========================================================================//

pub struct CpuRegBus {
    watchpoints: BiHashMap<(u8, WatchKind), WatchId>,
}

impl CpuRegBus {
    pub fn new() -> CpuRegBus {
        CpuRegBus { watchpoints: BiHashMap::new() }
    }
}

impl SimBus for CpuRegBus {
    fn description(&self) -> String {
        "SNES CPU registers".to_string()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        let addr = (addr as u8) & 0x1f;
        match addr {
            0x00 => Some("NMITIMEN"),
            0x01 => Some("WRIO"),
            0x02 => Some("WRMPYA"),
            0x03 => Some("WRMPYB"),
            0x04 => Some("WRDIVL"),
            0x05 => Some("WRDIVH"),
            0x06 => Some("WRDIVB"),
            0x07 => Some("HTIMEL"),
            0x08 => Some("HTIMEH"),
            0x09 => Some("VTIMEL"),
            0x0a => Some("VTIMEH"),
            0x0b => Some("MDMAEN"),
            0x0c => Some("HDMAEN"),
            0x0d => Some("MEMSEL"),
            0x10 => Some("RDNMI"),
            0x11 => Some("TIMEUP"),
            0x12 => Some("HVBJOY"),
            0x13 => Some("RDIO"),
            0x14 => Some("RDDIVL"),
            0x15 => Some("RDDIVH"),
            0x16 => Some("RDMPYL"),
            0x17 => Some("RDMPYH"),
            0x18 => Some("JOY1L"),
            0x19 => Some("JOY1H"),
            0x1a => Some("JOY2L"),
            0x1b => Some("JOY2H"),
            0x1c => Some("JOY3L"),
            0x1d => Some("JOY3H"),
            0x1e => Some("JOY4L"),
            0x1f => Some("JOY4H"),
            _ => unreachable!(),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        let addr = (addr as u8) & 0x1f;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        let addr = (addr as u8) & 0x1f;
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
            "NMITIMEN" => 0x00,
            "WRIO" => 0x01,
            "WRMPYA" => 0x02,
            "WRMPYB" => 0x03,
            "WRDIVL" => 0x04,
            "WRDIVH" => 0x05,
            "WRDIVB" => 0x06,
            "HTIMEL" => 0x07,
            "HTIMEH" => 0x08,
            "VTIMEL" => 0x09,
            "VTIMEH" => 0x0a,
            "MDMAEN" => 0x0b,
            "HDMAEN" => 0x0c,
            "MEMSEL" => 0x0d,
            "RDNMI" => 0x10,
            "TIMEUP" => 0x11,
            "HVBJOY" => 0x12,
            "RDIO" => 0x13,
            "RDDIVL" => 0x14,
            "RDDIVH" => 0x15,
            "RDMPYL" => 0x16,
            "RDMPYH" => 0x17,
            "JOY1L" => 0x18,
            "JOY1H" => 0x19,
            "JOY2L" => 0x1a,
            "JOY2H" => 0x1b,
            "JOY3L" => 0x1c,
            "JOY3H" => 0x1d,
            "JOY4L" => 0x1e,
            "JOY4H" => 0x1f,
            _ => return None,
        };
        Some(self.watch_address(u32::from(addr), kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn read_byte(&mut self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {
        // TODO
    }
}

//===========================================================================//

pub struct DmaRegBus {
    watchpoints: BiHashMap<(u8, WatchKind), WatchId>,
}

impl DmaRegBus {
    pub fn new() -> DmaRegBus {
        DmaRegBus { watchpoints: BiHashMap::new() }
    }
}

impl SimBus for DmaRegBus {
    fn description(&self) -> String {
        "SNES DMA registers".to_string()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr as u8 {
            0x00 => Some("DMAP0"),
            0x10 => Some("DMAP1"),
            0x20 => Some("DMAP2"),
            0x30 => Some("DMAP3"),
            0x40 => Some("DMAP4"),
            0x50 => Some("DMAP5"),
            0x60 => Some("DMAP6"),
            0x70 => Some("DMAP7"),
            0x01 => Some("BBAD0"),
            0x11 => Some("BBAD1"),
            0x21 => Some("BBAD2"),
            0x31 => Some("BBAD3"),
            0x41 => Some("BBAD4"),
            0x51 => Some("BBAD5"),
            0x61 => Some("BBAD6"),
            0x71 => Some("BBAD7"),
            0x02 => Some("A1T0L"),
            0x12 => Some("A1T1L"),
            0x22 => Some("A1T2L"),
            0x32 => Some("A1T3L"),
            0x42 => Some("A1T4L"),
            0x52 => Some("A1T5L"),
            0x62 => Some("A1T6L"),
            0x72 => Some("A1T7L"),
            0x03 => Some("A1T0H"),
            0x13 => Some("A1T1H"),
            0x23 => Some("A1T2H"),
            0x33 => Some("A1T3H"),
            0x43 => Some("A1T4H"),
            0x53 => Some("A1T5H"),
            0x63 => Some("A1T6H"),
            0x73 => Some("A1T7H"),
            0x04 => Some("A1B0"),
            0x14 => Some("A1B1"),
            0x24 => Some("A1B2"),
            0x34 => Some("A1B3"),
            0x44 => Some("A1B4"),
            0x54 => Some("A1B5"),
            0x64 => Some("A1B6"),
            0x74 => Some("A1B7"),
            0x05 => Some("DAS0L"),
            0x15 => Some("DAS1L"),
            0x25 => Some("DAS2L"),
            0x35 => Some("DAS3L"),
            0x45 => Some("DAS4L"),
            0x55 => Some("DAS5L"),
            0x65 => Some("DAS6L"),
            0x75 => Some("DAS7L"),
            0x06 => Some("DAS0H"),
            0x16 => Some("DAS1H"),
            0x26 => Some("DAS2H"),
            0x36 => Some("DAS3H"),
            0x46 => Some("DAS4H"),
            0x56 => Some("DAS5H"),
            0x66 => Some("DAS6H"),
            0x76 => Some("DAS7H"),
            0x07 => Some("DASB0"),
            0x17 => Some("DASB1"),
            0x27 => Some("DASB2"),
            0x37 => Some("DASB3"),
            0x47 => Some("DASB4"),
            0x57 => Some("DASB5"),
            0x67 => Some("DASB6"),
            0x77 => Some("DASB7"),
            0x08 => Some("A2A0L"),
            0x18 => Some("A2A1L"),
            0x28 => Some("A2A2L"),
            0x38 => Some("A2A3L"),
            0x48 => Some("A2A4L"),
            0x58 => Some("A2A5L"),
            0x68 => Some("A2A6L"),
            0x78 => Some("A2A7L"),
            0x09 => Some("A2A0H"),
            0x19 => Some("A2A1H"),
            0x29 => Some("A2A2H"),
            0x39 => Some("A2A3H"),
            0x49 => Some("A2A4H"),
            0x59 => Some("A2A5H"),
            0x69 => Some("A2A6H"),
            0x79 => Some("A2A7H"),
            0x0a => Some("NTLR0"),
            0x1a => Some("NTLR1"),
            0x2a => Some("NTLR2"),
            0x3a => Some("NTLR3"),
            0x4a => Some("NTLR4"),
            0x5a => Some("NTLR5"),
            0x6a => Some("NTLR6"),
            0x7a => Some("NTLR7"),
            _ => None,
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        let addr = addr as u8;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        let addr = addr as u8;
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
            "DMAP0" => 0x00,
            "DMAP1" => 0x10,
            "DMAP2" => 0x20,
            "DMAP3" => 0x30,
            "DMAP4" => 0x40,
            "DMAP5" => 0x50,
            "DMAP6" => 0x60,
            "DMAP7" => 0x70,
            "BBAD0" => 0x01,
            "BBAD1" => 0x11,
            "BBAD2" => 0x21,
            "BBAD3" => 0x31,
            "BBAD4" => 0x41,
            "BBAD5" => 0x51,
            "BBAD6" => 0x61,
            "BBAD7" => 0x71,
            "A1T0L" => 0x02,
            "A1T1L" => 0x12,
            "A1T2L" => 0x22,
            "A1T3L" => 0x32,
            "A1T4L" => 0x42,
            "A1T5L" => 0x52,
            "A1T6L" => 0x62,
            "A1T7L" => 0x72,
            "A1T0H" => 0x03,
            "A1T1H" => 0x13,
            "A1T2H" => 0x23,
            "A1T3H" => 0x33,
            "A1T4H" => 0x43,
            "A1T5H" => 0x53,
            "A1T6H" => 0x63,
            "A1T7H" => 0x73,
            "A1B0" => 0x04,
            "A1B1" => 0x14,
            "A1B2" => 0x24,
            "A1B3" => 0x34,
            "A1B4" => 0x44,
            "A1B5" => 0x54,
            "A1B6" => 0x64,
            "A1B7" => 0x74,
            "DAS0L" => 0x05,
            "DAS1L" => 0x15,
            "DAS2L" => 0x25,
            "DAS3L" => 0x35,
            "DAS4L" => 0x45,
            "DAS5L" => 0x55,
            "DAS6L" => 0x65,
            "DAS7L" => 0x75,
            "DAS0H" => 0x06,
            "DAS1H" => 0x16,
            "DAS2H" => 0x26,
            "DAS3H" => 0x36,
            "DAS4H" => 0x46,
            "DAS5H" => 0x56,
            "DAS6H" => 0x66,
            "DAS7H" => 0x76,
            "DASB0" => 0x07,
            "DASB1" => 0x17,
            "DASB2" => 0x27,
            "DASB3" => 0x37,
            "DASB4" => 0x47,
            "DASB5" => 0x57,
            "DASB6" => 0x67,
            "DASB7" => 0x77,
            "A2A0L" => 0x08,
            "A2A1L" => 0x18,
            "A2A2L" => 0x28,
            "A2A3L" => 0x38,
            "A2A4L" => 0x48,
            "A2A5L" => 0x58,
            "A2A6L" => 0x68,
            "A2A7L" => 0x78,
            "A2A0H" => 0x09,
            "A2A1H" => 0x19,
            "A2A2H" => 0x29,
            "A2A3H" => 0x39,
            "A2A4H" => 0x49,
            "A2A5H" => 0x59,
            "A2A6H" => 0x69,
            "A2A7H" => 0x79,
            "NTLR0" => 0x0a,
            "NTLR1" => 0x1a,
            "NTLR2" => 0x2a,
            "NTLR3" => 0x3a,
            "NTLR4" => 0x4a,
            "NTLR5" => 0x5a,
            "NTLR6" => 0x6a,
            "NTLR7" => 0x7a,
            _ => return None,
        };
        Some(self.watch_address(u32::from(addr), kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn read_byte(&mut self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {
        // TODO
    }
}

//===========================================================================//

pub struct JoyRegBus {
    watchpoints: BiHashMap<(u8, WatchKind), WatchId>,
}

impl JoyRegBus {
    pub fn new() -> JoyRegBus {
        JoyRegBus { watchpoints: BiHashMap::new() }
    }
}

impl SimBus for JoyRegBus {
    fn description(&self) -> String {
        "SNES Joypad registers".to_string()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        let addr = (addr as u8) & 0x1;
        match addr {
            0 => Some("JOYSER0"),
            1 => Some("JOYSER1"),
            _ => unreachable!(),
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        let addr = (addr as u8) & 0x1;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        let addr = (addr as u8) & 0x1;
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
        let addr: u32 = match label {
            "JOYSER0" => 0,
            "JOYSER1" => 1,
            _ => return None,
        };
        Some(self.watch_address(addr, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn read_byte(&mut self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {
        // TODO
    }
}

//===========================================================================//

pub struct PpuRegBus {
    watchpoints: BiHashMap<(u8, WatchKind), WatchId>,
}

impl PpuRegBus {
    pub fn new() -> PpuRegBus {
        PpuRegBus { watchpoints: BiHashMap::new() }
    }
}

impl SimBus for PpuRegBus {
    fn description(&self) -> String {
        "SNES PPU registers".to_string()
    }

    fn label_at(&self, addr: u32) -> Option<&str> {
        match addr as u8 {
            0x00 => Some("INIDISP"),
            0x01 => Some("OBSEL"),
            0x02 => Some("OAMADDL"),
            0x03 => Some("OAMADDH"),
            0x04 => Some("OAMDATA"),
            0x05 => Some("BGMODE"),
            0x06 => Some("MOSAIC"),
            0x07 => Some("BG1SC"),
            0x08 => Some("BG2SC"),
            0x09 => Some("BG3SC"),
            0x0A => Some("BG4SC"),
            0x0B => Some("BG12NBA"),
            0x0C => Some("BG34NBA"),
            0x0D => Some("BG1HOFS"),
            0x0E => Some("BG1VOFS"),
            0x0F => Some("BG2HOFS"),
            0x10 => Some("BG2VOFS"),
            0x11 => Some("BG3HOFS"),
            0x12 => Some("BG3VOFS"),
            0x13 => Some("BG4HOFS"),
            0x14 => Some("BG4VOFS"),
            0x15 => Some("VMAIN"),
            0x16 => Some("VMADDL"),
            0x17 => Some("VMADDH"),
            0x18 => Some("VMDATAL"),
            0x19 => Some("VMDATAH"),
            0x1A => Some("M7SEL"),
            0x1B => Some("M7A"),
            0x1C => Some("M7B"),
            0x1D => Some("M7C"),
            0x1E => Some("M7D"),
            0x1F => Some("M7X"),
            0x20 => Some("M7Y"),
            0x21 => Some("CGADD"),
            0x22 => Some("CGDATA"),
            0x23 => Some("W12SEL"),
            0x24 => Some("W34SEL"),
            0x25 => Some("WOBJSEL"),
            0x26 => Some("WH0"),
            0x27 => Some("WH1"),
            0x28 => Some("WH2"),
            0x29 => Some("WH3"),
            0x2A => Some("WBGLOG"),
            0x2B => Some("WOBJLOG"),
            0x2C => Some("TM"),
            0x2D => Some("TS"),
            0x2E => Some("TMW"),
            0x2F => Some("TSW"),
            0x30 => Some("CGWSEL"),
            0x31 => Some("CGADSUB"),
            0x32 => Some("COLDATA"),
            0x33 => Some("SETINI"),
            0x34 => Some("MPYL"),
            0x35 => Some("MPYM"),
            0x36 => Some("MPYH"),
            0x37 => Some("SLHV"),
            0x38 => Some("OAMDATAREAD"),
            0x39 => Some("VMDATALREAD"),
            0x3A => Some("VMDATAHREAD"),
            0x3B => Some("CGDATAREAD"),
            0x3C => Some("OPHCT"),
            0x3D => Some("OPVCT"),
            0x3E => Some("STAT77"),
            0x3F => Some("STAT78"),
            0x40 => Some("APUIO0"),
            0x41 => Some("APUIO1"),
            0x42 => Some("APUIO2"),
            0x43 => Some("APUIO3"),
            0x80 => Some("WMDATA"),
            0x81 => Some("WMADDL"),
            0x82 => Some("WMADDM"),
            0x83 => Some("WMADDH"),
            _ => None,
        }
    }

    fn watchpoint_at(&self, addr: u32, kind: WatchKind) -> Option<WatchId> {
        let addr = addr as u8;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: u32, kind: WatchKind) -> WatchId {
        let addr = addr as u8;
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
            "INIDISP" => 0x00,
            "OBSEL" => 0x01,
            "OAMADDL" => 0x02,
            "OAMADDH" => 0x03,
            "OAMDATA" => 0x04,
            "BGMODE" => 0x05,
            "MOSAIC" => 0x06,
            "BG1SC" => 0x07,
            "BG2SC" => 0x08,
            "BG3SC" => 0x09,
            "BG4SC" => 0x0A,
            "BG12NBA" => 0x0B,
            "BG34NBA" => 0x0C,
            "BG1HOFS" => 0x0D,
            "BG1VOFS" => 0x0E,
            "BG2HOFS" => 0x0F,
            "BG2VOFS" => 0x10,
            "BG3HOFS" => 0x11,
            "BG3VOFS" => 0x12,
            "BG4HOFS" => 0x13,
            "BG4VOFS" => 0x14,
            "VMAIN" => 0x15,
            "VMADDL" => 0x16,
            "VMADDH" => 0x17,
            "VMDATAL" => 0x18,
            "VMDATAH" => 0x19,
            "M7SEL" => 0x1A,
            "M7A" => 0x1B,
            "M7B" => 0x1C,
            "M7C" => 0x1D,
            "M7D" => 0x1E,
            "M7X" => 0x1F,
            "M7Y" => 0x20,
            "CGADD" => 0x21,
            "CGDATA" => 0x22,
            "W12SEL" => 0x23,
            "W34SEL" => 0x24,
            "WOBJSEL" => 0x25,
            "WH0" => 0x26,
            "WH1" => 0x27,
            "WH2" => 0x28,
            "WH3" => 0x29,
            "WBGLOG" => 0x2A,
            "WOBJLOG" => 0x2B,
            "TM" => 0x2C,
            "TS" => 0x2D,
            "TMW" => 0x2E,
            "TSW" => 0x2F,
            "CGWSEL" => 0x30,
            "CGADSUB" => 0x31,
            "COLDATA" => 0x32,
            "SETINI" => 0x33,
            "MPYL" => 0x34,
            "MPYM" => 0x35,
            "MPYH" => 0x36,
            "SLHV" => 0x37,
            "OAMDATAREAD" => 0x38,
            "VMDATALREAD" => 0x39,
            "VMDATAHREAD" => 0x3A,
            "CGDATAREAD" => 0x3B,
            "OPHCT" => 0x3C,
            "OPVCT" => 0x3D,
            "STAT77" => 0x3E,
            "STAT78" => 0x3F,
            "APUIO0" => 0x40,
            "APUIO1" => 0x41,
            "APUIO2" => 0x42,
            "APUIO3" => 0x43,
            "WMDATA" => 0x80,
            "WMADDL" => 0x81,
            "WMADDM" => 0x82,
            "WMADDH" => 0x83,
            _ => return None,
        };
        Some(self.watch_address(u32::from(addr), kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn read_byte(&mut self, _addr: u32) -> u8 {
        0 // TODO
    }

    fn write_byte(&mut self, _addr: u32, _data: u8) {
        // TODO
    }
}

//===========================================================================//
