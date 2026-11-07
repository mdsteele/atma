use super::ram::new_open_bus;
use super::shared::SharedBus;
use super::{SimBus, WatchId, WatchKind};
use crate::addr::{Addr, BitWidth};
use bimap::BiHashMap;
use static_assertions::const_assert;
use std::cell::RefCell;
use std::rc::Rc;

//===========================================================================//

/// Returns a pair `(cart_bus, superfx_bus)` of simulated memory busses for an
/// SNES GSU cartridge.  The `cart_bus` is for the cartridge as seen by the
/// SNES CPU, and the `superfx_bus` is the bus for the SuperFX processor.
pub fn new_snes_gsu_busses(
    rom_bus: Box<dyn SimBus>,
    sram_bus: Box<dyn SimBus>,
    superfx_state: Rc<RefCell<SuperFxState>>,
) -> (Box<dyn SimBus>, Box<dyn SimBus>) {
    let shared_bus: Box<dyn SimBus> = Box::new(GsuSharedBus {
        rom: rom_bus,
        sram: sram_bus,
        open: new_open_bus(BitWidth::TWENTY_FOUR),
    });
    let shared = Rc::new(RefCell::new(shared_bus));
    let cart_bus = Box::new(GsuSnesCartBus {
        shared: SharedBus { shared: shared.clone() },
        superfx_regs: SuperFxRegBus::new(superfx_state),
    });
    let superfx_bus: Box<dyn SimBus> =
        Box::new(GsuSuperFxBus { shared: SharedBus { shared } });
    (cart_bus, superfx_bus)
}

//===========================================================================//

/// Represents state used by a SuperFX processor that is also accessible
/// through SNES CPU bus hardware registers.
pub struct SuperFxState {
    /// General purpose registers.
    pub(crate) regs: [u16; 16],
    /// The Status Flags Register.
    pub(crate) sfr: u16,
    /// The Backup RAM Register.  This is a 1-bit register that controls
    /// whether back-up RAM can be written.
    ///
    /// Note that GSU back-up RAM is currently unsupported in ATMA, because no
    /// commercial games used it (see
    /// <https://forums.nesdev.org/viewtopic.php?t=5964&start=15>).
    pub(crate) bramr: u8,
    /// The Program Bank Register (upper 8 bits of the 24-bit PC).
    pub(crate) pbr: u8,
    /// The ROM Bank Register, used to determine the upper 8 bits of cartridge
    /// ROM addresses used by certain SuperFX instructions.
    ///
    /// This is an 8-bit register, according to SNES Book II page 2-4-5, even
    /// though page 2-3-4 indicates that only values from 0x00 through 0x5f are
    /// valid (which would seem to suggest that only seven bits are normally
    /// used in practice).
    pub(crate) rombr: u8,
    /// The Config Register.
    pub(crate) cfgr: u8,
    /// The Screen Base Register, which controls where the plotted image is
    /// stored in SRAM.  According to SNES Book II page 2-8-3 and fullsnes
    /// section "SNES Cart GSU-n Bitmap I/O Ports", the starting address of the
    /// plotted image is equal to 0x700000 + 0x400 * SCBR.
    pub(crate) scbr: u8,
    /// The Clock Select Register, which controls whether the higher clock
    /// speed and pipelining are enabled.  This is a 1-bit register.
    pub(crate) clsr: u8,
    /// The Screen Mode Register.
    pub(crate) scmr: u8,
    /// The RAM Bank Register, used to determine the upper 8 bits of cartridge
    /// RAM addresses used by certain SuperFX instructions.
    ///
    /// This is a 1-bit register: according to SNES Book II page 2-4-6, the
    /// upper 7 bits are always zero when the register is read.  However, a
    /// value of 0 actually corresponds to bank 0x70, and a value of 1
    /// corresponds to bank 0x71.
    pub(crate) rambr: u8,
    /// The Cache Base Register.
    pub(crate) cbr: u16,
    /// A bitfield indicating which of the 32 16-byte cache lines are valid (1)
    /// or invalid (0).
    pub(crate) cache_flags: u32,
    /// The 512-byte instruction cache.
    pub(crate) cache: [u8; 512],
}

impl SuperFxState {
    // Flags for the Status Flags Register.  These are described in:
    //   * https://sneslab.net/wiki/Super_FX#Status/Flag_Register_(SFR)
    //   * fullsnes section "SNES Cart GSU-n General I/O Ports"
    //   * SNES Book II page 2-4-4
    pub(crate) const FLAG_Z: u16 = 1 << 1;
    pub(crate) const FLAG_CY: u16 = 1 << 2;
    pub(crate) const FLAG_S: u16 = 1 << 3;
    pub(crate) const FLAG_OV: u16 = 1 << 4;
    pub(crate) const FLAG_GO: u16 = 1 << 5;
    pub(crate) const FLAG_R: u16 = 1 << 6;
    pub(crate) const FLAG_ALT2: u16 = 1 << 8;
    pub(crate) const FLAG_ALT1: u16 = 1 << 9;
    pub(crate) const FLAG_IL: u16 = 1 << 10;
    pub(crate) const FLAG_IH: u16 = 1 << 11;
    pub(crate) const FLAG_B: u16 = 1 << 12;
    pub(crate) const FLAG_IRQ: u16 = 1 << 15;

    // Flags for the Config Register.  These are described in:
    //   * fullsnes section "SNES Cart GSU-n General I/O Ports"
    //   * SNES Book II page 2-4-11
    pub(crate) const CFG_MSO: u8 = 1 << 5;
    pub(crate) const CFG_IRQ: u8 = 1 << 7;

    // Flags for the Screen Mode Register.  These are described in:
    //   * https://sneslab.net/wiki/Super_FX#Screen_Mode_Register_(SCMR)
    //   * fullsnes section "SNES Cart GSU-n Bitmap I/O Ports"
    //   * SNES Book II page 2-4-8
    pub(crate) const SCM_MD0: u8 = 1 << 0;
    pub(crate) const SCM_MD1: u8 = 1 << 1;
    pub(crate) const SCM_HT0: u8 = 1 << 2;
    pub(crate) const SCM_RAN: u8 = 1 << 3;
    pub(crate) const SCM_RON: u8 = 1 << 4;
    pub(crate) const SCM_HT1: u8 = 1 << 5;

    pub(crate) const SFR_MASK: u16 = Self::FLAG_Z
        | Self::FLAG_CY
        | Self::FLAG_S
        | Self::FLAG_OV
        | Self::FLAG_R
        | Self::FLAG_ALT1
        | Self::FLAG_ALT2
        | Self::FLAG_IL
        | Self::FLAG_IH
        | Self::FLAG_B
        | Self::FLAG_IRQ;
    pub(crate) const BRAMR_MASK: u8 = 0x01;
    pub(crate) const CFGR_MASK: u8 = Self::CFG_MSO | Self::CFG_IRQ;
    pub(crate) const CLSR_MASK: u8 = 0x01;
    pub(crate) const SCMR_MASK: u8 = Self::SCM_MD0
        | Self::SCM_MD1
        | Self::SCM_HT0
        | Self::SCM_RAN
        | Self::SCM_RON
        | Self::SCM_HT1;
    pub(crate) const RAMBR_MASK: u8 = 0x01;
    pub(crate) const CBR_MASK: u16 = 0xfff0;
}

impl Default for SuperFxState {
    fn default() -> Self {
        Self {
            // According to SNES Book II pages 2-4-1 through 2-4-3, every
            // register's initial value is either 0x0000 or "invalid".  For
            // now, we'll just set them all to zero.
            regs: [0u16; 16],
            // According to SNES Book II page 2-4-4, SFR's initial value is
            // 0x0000.
            sfr: 0x0000,
            // According to SNES Book II page 2-4-10, BRAMR's initial value is
            // 0x00.
            bramr: 0x00,
            // According to SNES Book II page 2-4-5, PBR's initial value is
            // undefined.
            pbr: u8::MAX,
            // According to SNES Book II page 2-9-104, ROMBR's initial value is
            // "invalid"; according to page 2-4-5, its default value is
            // "undefined".  Page 2-4-5 also indicates that ROMBR is eight bits
            // wide, but page 2-3-4 says that ROMBR may only be used to specify
            // banks 0x00 through 0x5f.  Therefore, we use 0xff as an "invalid"
            // initial value.
            rombr: u8::MAX,
            // According to SNES Book II page 2-4-11, CFGR's initial value is
            // 0x00.
            cfgr: 0x00,
            // According to SNES Book II page 2-4-5, SCBR's initial value is
            // undefined.
            scbr: u8::MAX,
            // According to SNES Book II page 2-4-11, CLSR's initial value is
            // 0x00.
            clsr: 0x00,
            // According to SNES Book II page 2-4-8, SCMR's initial value is
            // 0x00.
            scmr: 0x00,
            // According to SNES Book II page 2-9-101, RAMBR's initial value is
            // "invalid"; according to page 2-4-6, its default value is
            // "undefined".  fullsnes section "SNES Cart GSU-n General I/O
            // Ports" points out that existing games have at most 64 KiB of
            // cartridge RAM, so in practice this should always be set to zero.
            // Therefore, we initialize to its maximum, nonzero value as the
            // least "valid" value available.
            rambr: Self::RAMBR_MASK,
            // According to SNES Book II page 2-4-6, CBR's initial value is
            // 0x0000.
            cbr: 0x0000,
            cache_flags: 0,
            cache: [0u8; 512],
        }
    }
}

//===========================================================================//

/// A simulated memory bus for an SNES GSU (SuperFX) cartridge, as seen from
/// the SNES CPU.
struct GsuSnesCartBus {
    shared: SharedBus,
    superfx_regs: SuperFxRegBus,
}

impl GsuSnesCartBus {
    fn sub_bus(&self, addr: Addr) -> (&dyn SimBus, Addr) {
        let bank = (addr >> 16).as_u8();
        match bank {
            0x00..0x40 | 0x80..0xc0 => match addr.as_u16() {
                0x3000..0x4000 => (&self.superfx_regs, addr),
                0x6000..0x8000 => {
                    let base = addr & Addr::from(0x1fffu16);
                    (&self.shared, Addr::from(0x700000u32) | base)
                }
                _ => (&self.shared, addr),
            },
            // Theoretically, we should map banks 0x78 and 0x79 to back-up RAM,
            // and (most of) banks 0x80 and beyond to ROM addresses above 2 MB,
            // but no commercial GSU games used these featuers (see
            // https://forums.nesdev.org/viewtopic.php?t=5964&start=15), and
            // most emulators don't support them, so we don't currently bother
            // to support them here either.
            _ => (&self.shared, addr),
        }
    }

    fn sub_bus_mut(&mut self, addr: Addr) -> (&mut dyn SimBus, Addr) {
        let bank = (addr >> 16).as_u8();
        match bank {
            0x00..0x40 | 0x80..0xc0 => match addr.as_u16() {
                0x3000..0x4000 => (&mut self.superfx_regs, addr),
                0x6000..0x8000 => {
                    let base = addr & Addr::from(0x1fffu16);
                    (&mut self.shared, Addr::from(0x700000u32) | base)
                }
                _ => (&mut self.shared, addr),
            },
            _ => (&mut self.shared, addr),
        }
    }
}

impl SimBus for GsuSnesCartBus {
    fn description(&self) -> String {
        format!("GSU cartridge with {}", self.shared.description())
    }

    fn bit_width(&self) -> BitWidth {
        BitWidth::TWENTY_FOUR
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        let (bus, addr) = self.sub_bus(addr);
        bus.label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let (bus, addr) = self.sub_bus(addr);
        bus.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.shared
            .watch_label(label, kind)
            .or_else(|| self.superfx_regs.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.shared.unwatch(id);
        self.superfx_regs.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus(addr);
        bus.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.write_byte(addr, data);
    }
}

//===========================================================================//

/// A simulated memory bus for SuperFX processor running on an SNES GSU
/// cartridge.
struct GsuSuperFxBus {
    shared: SharedBus,
}

impl SimBus for GsuSuperFxBus {
    fn description(&self) -> String {
        format!("GSU with {}", self.shared.description())
    }

    fn bit_width(&self) -> BitWidth {
        BitWidth::TWENTY_FOUR
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        self.shared.label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        self.shared.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        self.shared.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.shared.watch_label(label, kind)
    }

    fn unwatch(&mut self, id: WatchId) {
        self.shared.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        self.shared.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.shared.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        self.shared.write_byte(addr, data);
    }
}

//===========================================================================//

/// A simulated memory bus serving the ROM and SRAM chips on an SNES GSU
/// cartridge that are shared between the SNES CPU bus and the SuperFX bus.
struct GsuSharedBus {
    rom: Box<dyn SimBus>,
    sram: Box<dyn SimBus>,
    open: Box<dyn SimBus>,
}

impl GsuSharedBus {
    fn sub_bus(&self, addr: Addr) -> (&dyn SimBus, Addr) {
        let bank = (addr >> 16).as_u8();
        match bank {
            0x00..0x40 => match addr.as_u16() {
                0x8000.. => {
                    let base = addr & Addr::from(0x7fffu16);
                    (&*self.rom, (Addr::from(bank) << 15) | base)
                }
                _ => (&*self.open, addr),
            },
            0x40..0x60 => (&*self.rom, addr),
            0x70..0x72 => (&*self.sram, addr),
            0x60..0x70 | 0x72.. => (&*self.open, addr),
        }
    }

    fn sub_bus_mut(&mut self, addr: Addr) -> (&mut dyn SimBus, Addr) {
        let bank = (addr >> 16).as_u8();
        match bank {
            0x00..0x40 => match addr.as_u16() {
                0x8000.. => {
                    let base = addr & Addr::from(0x7fffu16);
                    (&mut *self.rom, (Addr::from(bank) << 15) | base)
                }
                _ => (&mut *self.open, addr),
            },
            0x40..0x60 => (&mut *self.rom, addr),
            0x70..0x72 => (&mut *self.sram, addr),
            0x60..0x70 | 0x72.. => (&mut *self.open, addr),
        }
    }
}

impl SimBus for GsuSharedBus {
    fn description(&self) -> String {
        format!("{} and {}", self.rom.description(), self.sram.description())
    }

    fn bit_width(&self) -> BitWidth {
        BitWidth::TWENTY_FOUR
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        let (bus, addr) = self.sub_bus(addr);
        bus.label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let (bus, addr) = self.sub_bus(addr);
        bus.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.rom
            .watch_label(label, kind)
            .or_else(|| self.sram.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.rom.unwatch(id);
        self.sram.unwatch(id);
        self.open.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus(addr);
        bus.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.write_byte(addr, data);
    }
}

//===========================================================================//

/// Provides access to SuperFX registers/cache to the SNES CPU.
struct SuperFxRegBus {
    gen_regs: SuperFxGenRegBus,
    ctrl_regs: SuperFxCtrlRegBus,
    cache: SuperFxCacheBus,
    open: Box<dyn SimBus>,
}

impl SuperFxRegBus {
    const BIT_WIDTH: BitWidth = BitWidth::new(12);
    const ADDR_MASK: u16 = 0xfff;

    pub fn new(state: Rc<RefCell<SuperFxState>>) -> Self {
        let gen_regs = SuperFxGenRegBus::new(state.clone());
        let ctrl_regs = SuperFxCtrlRegBus::new(state.clone());
        let cache = SuperFxCacheBus::new(state);
        let open = new_open_bus(Self::BIT_WIDTH);
        Self { gen_regs, ctrl_regs, cache, open }
    }

    fn sub_bus(&self, addr: Addr) -> (&dyn SimBus, Addr) {
        let masked = addr.as_u16() & Self::ADDR_MASK;
        // See https://forums.nesdev.org/viewtopic.php?t=5964&start=45.  For
        // now we just simulate the GSU-2 mirroring, not the more complicated
        // mirroring apparently used by older GSU revisions.
        match masked {
            0x000..0x100 | 0x300..0x500 => match masked & 0x3f {
                0x00..0x20 => (&self.gen_regs, addr),
                0x20.. => (&self.ctrl_regs, addr),
            },
            0x100..0x300 => (&self.cache, Addr::from(masked - 0x100)),
            0x500.. => (&*self.open, addr),
        }
    }

    fn sub_bus_mut(&mut self, addr: Addr) -> (&mut dyn SimBus, Addr) {
        let masked = addr.as_u16() & Self::ADDR_MASK;
        match masked {
            0x000..0x100 | 0x300..0x500 => match masked & 0x3f {
                0x00..0x20 => (&mut self.gen_regs, addr),
                0x20.. => (&mut self.ctrl_regs, addr),
            },
            0x100..0x300 => (&mut self.cache, Addr::from(masked - 0x100)),
            0x500.. => (&mut *self.open, addr),
        }
    }
}

impl SimBus for SuperFxRegBus {
    fn description(&self) -> String {
        "SuperFX registers".to_string()
    }

    fn bit_width(&self) -> BitWidth {
        Self::BIT_WIDTH
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        let (bus, addr) = self.sub_bus(addr);
        bus.label_at(addr)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let (bus, addr) = self.sub_bus(addr);
        bus.watchpoint_at(addr, kind)
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.watch_address(addr, kind)
    }

    fn watch_label(
        &mut self,
        label: &str,
        kind: WatchKind,
    ) -> Option<WatchId> {
        self.gen_regs
            .watch_label(label, kind)
            .or_else(|| self.ctrl_regs.watch_label(label, kind))
            .or_else(|| self.cache.watch_label(label, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.gen_regs.unwatch(id);
        self.ctrl_regs.unwatch(id);
        self.cache.unwatch(id);
        self.open.unwatch(id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus(addr);
        bus.peek_byte(addr)
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.read_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let (bus, addr) = self.sub_bus_mut(addr);
        bus.write_byte(addr, data);
    }
}

//===========================================================================//

/// Provides access to SuperFX general-purpose registers to the SNES CPU.
struct SuperFxGenRegBus {
    state: Rc<RefCell<SuperFxState>>,
    watchpoints: BiHashMap<(u8, WatchKind), WatchId>,
}

impl SuperFxGenRegBus {
    const BIT_WIDTH: BitWidth = BitWidth::new(5);
    const ADDR_MASK: u8 = 0x1f;

    pub fn new(state: Rc<RefCell<SuperFxState>>) -> Self {
        Self { state, watchpoints: BiHashMap::new() }
    }
}

impl SimBus for SuperFxGenRegBus {
    fn description(&self) -> String {
        "SuperFX general-purpose registers".to_string()
    }

    fn bit_width(&self) -> BitWidth {
        Self::BIT_WIDTH
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        let addr = addr.as_u8() & Self::ADDR_MASK;
        let label = match addr {
            0x00 => "R0",
            0x01 => "R0H",
            0x02 => "R1",
            0x03 => "R1H",
            0x04 => "R2",
            0x05 => "R2H",
            0x06 => "R3",
            0x07 => "R3H",
            0x08 => "R4",
            0x09 => "R4H",
            0x0a => "R5",
            0x0b => "R5H",
            0x0c => "R6",
            0x0d => "R6H",
            0x0e => "R7",
            0x0f => "R7H",
            0x10 => "R8",
            0x11 => "R8H",
            0x12 => "R9",
            0x13 => "R9H",
            0x14 => "R10",
            0x15 => "R10H",
            0x16 => "R11",
            0x17 => "R11H",
            0x18 => "R12",
            0x19 => "R12H",
            0x1a => "R13",
            0x1b => "R13H",
            0x1c => "R14",
            0x1d => "R14H",
            0x1e => "R15",
            0x1f => "R15H",
            _ => unreachable!(),
        };
        Some(label.to_string())
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let addr = addr.as_u8() & Self::ADDR_MASK;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let addr = addr.as_u8() & Self::ADDR_MASK;
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
        let addr: Addr = match label {
            "R0" => Addr::from(0x00u8),
            "R0H" => Addr::from(0x01u8),
            "R1" => Addr::from(0x02u8),
            "R1H" => Addr::from(0x03u8),
            "R2" => Addr::from(0x04u8),
            "R2H" => Addr::from(0x05u8),
            "R3" => Addr::from(0x06u8),
            "R3H" => Addr::from(0x07u8),
            "R4" => Addr::from(0x08u8),
            "R4H" => Addr::from(0x09u8),
            "R5" => Addr::from(0x0au8),
            "R5H" => Addr::from(0x0bu8),
            "R6" => Addr::from(0x0cu8),
            "R6H" => Addr::from(0x0du8),
            "R7" => Addr::from(0x0eu8),
            "R7H" => Addr::from(0x0fu8),
            "R8" => Addr::from(0x10u8),
            "R8H" => Addr::from(0x11u8),
            "R9" => Addr::from(0x12u8),
            "R9H" => Addr::from(0x13u8),
            "R10" => Addr::from(0x14u8),
            "R10H" => Addr::from(0x15u8),
            "R11" => Addr::from(0x16u8),
            "R11H" => Addr::from(0x17u8),
            "R12" => Addr::from(0x18u8),
            "R12H" => Addr::from(0x19u8),
            "R13" => Addr::from(0x1au8),
            "R13H" => Addr::from(0x1bu8),
            "R14" => Addr::from(0x1cu8),
            "R14H" => Addr::from(0x1du8),
            "R15" => Addr::from(0x1eu8),
            "R15H" => Addr::from(0x1fu8),
            _ => return None,
        };
        Some(self.watch_address(addr, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        let addr = addr.as_u8();
        let index = usize::from((addr >> 1) & 0x0f);
        let reg = self.state.borrow().regs[index];
        if addr & 0x01 == 0 { reg as u8 } else { (reg >> 8) as u8 }
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let addr = addr.as_u8();
        let index = usize::from((addr >> 1) & 0x0f);
        let reg = &mut self.state.borrow_mut().regs[index];
        if addr & 0x01 == 0 {
            *reg = (0xff00 & *reg) | u16::from(data);
        } else {
            *reg = (u16::from(data) << 8) | (0x00ff & *reg);
        }
    }
}

//===========================================================================//

/// Provides access to SuperFX control registers to the SNES CPU.
struct SuperFxCtrlRegBus {
    state: Rc<RefCell<SuperFxState>>,
    watchpoints: BiHashMap<(u8, WatchKind), WatchId>,
}

impl SuperFxCtrlRegBus {
    const BIT_WIDTH: BitWidth = BitWidth::new(4);
    const ADDR_MASK: u8 = 0x0f;

    pub fn new(state: Rc<RefCell<SuperFxState>>) -> Self {
        Self { state, watchpoints: BiHashMap::new() }
    }
}

impl SimBus for SuperFxCtrlRegBus {
    fn description(&self) -> String {
        "SuperFX control registers".to_string()
    }

    fn bit_width(&self) -> BitWidth {
        Self::BIT_WIDTH
    }

    fn label_at(&self, addr: Addr) -> Option<String> {
        let addr = addr.as_u8() & Self::ADDR_MASK;
        let label = match addr {
            0x00 => Some("SFR"),
            0x01 => Some("SFRH"),
            0x02 => None,
            0x03 => Some("BRAMR"),
            0x04 => Some("PBR"),
            0x05 => None,
            0x06 => Some("ROMBR"),
            0x07 => Some("CFGR"),
            0x08 => Some("SCBR"),
            0x09 => Some("CLSR"),
            0x0a => Some("SCMR"),
            0x0b => Some("VCR"),
            0x0c => Some("RAMBR"),
            0x0d => None,
            0x0e => Some("CBR"),
            0x0f => Some("CBRH"),
            _ => unreachable!(),
        };
        label.map(str::to_string)
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let addr = addr.as_u8() & Self::ADDR_MASK;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let addr = addr.as_u8() & Self::ADDR_MASK;
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
        let addr: Addr = match label {
            "SFR" => Addr::from(0x00u8),
            "SFRH" => Addr::from(0x01u8),
            "BRAMR" => Addr::from(0x03u8),
            "PBR" => Addr::from(0x04u8),
            "ROMBR" => Addr::from(0x06u8),
            "CFGR" => Addr::from(0x07u8),
            "SCBR" => Addr::from(0x08u8),
            "CLSR" => Addr::from(0x09u8),
            "SCMR" => Addr::from(0x0au8),
            "VCR" => Addr::from(0x0bu8),
            "RAMBR" => Addr::from(0x0cu8),
            "CBR" => Addr::from(0x0eu8),
            "CBRH" => Addr::from(0x0fu8),
            _ => return None,
        };
        Some(self.watch_address(addr, kind))
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        // According to https://forums.nesdev.org/viewtopic.php?t=5964&start=45
        // write-only and unmapped control register addresses return zero (not
        // open bus).
        let addr = addr.as_u8() & Self::ADDR_MASK;
        match addr {
            0x00 => self.state.borrow().sfr as u8, // SFR (lo)
            0x01 => (self.state.borrow().sfr >> 8) as u8, // SFR (hi)
            0x02 => 0, // unused address, returns zero (not open bus!)
            0x03 => self.state.borrow().bramr, // BRAMR
            0x04 => self.state.borrow().pbr, // PBR
            0x05 => 0, // unused address, returns zero (not open bus!)
            0x06 => self.state.borrow().rombr, // ROMBR
            0x07 => 0, // CFGR, write only, returns zero (not open bus!)
            0x08 => 0, // SCBR, write only, returns zero (not open bus!)
            0x09 => 0, // CLSR, write only, returns zero (not open bus!)
            0x0a => 0, // SCMR, write only, returns zero (not open bus!)
            // MesenCE seems to always return 0x04 for the VCR, regardless of
            // which GSU revision it is (except for the modern FX3).  See
            // https://github.com/nesdev-org/MesenCE/blob/
            // 4b8a196e299b3de297b204fd1208d2981562c89b/Core/SNES/Coprocessors/
            // GSU/Gsu.cpp#L519
            0x0b => 0x04,                                 // VCR
            0x0c => self.state.borrow().rambr,            // RAMBR
            0x0d => 0, // unused address, returns zero (not open bus!)
            0x0e => self.state.borrow().cbr as u8, // CBR (lo)
            0x0f => (self.state.borrow().cbr >> 8) as u8, // CBR (hi)
            _ => unreachable!(),
        }
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        let data = self.peek_byte(addr);
        // According to fullsnes section "SNES Cart GSU-n General I/O Ports",
        // reading the SFR (hi byte, presumably?) clears the IRQ flag.  Indeed,
        // MesenCE clears this flag when reading the hi byte of SFR (but not
        // the lo byte).
        if addr.as_u8() & Self::ADDR_MASK == 0x01 {
            self.state.borrow_mut().sfr &= !SuperFxState::FLAG_IRQ;
        }
        data
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let addr = addr.as_u8() & Self::ADDR_MASK;
        match addr {
            0x00 => {
                let mut state = self.state.borrow_mut();
                state.sfr = (state.sfr & 0xff00) | u16::from(data);
                const_assert!(SuperFxState::FLAG_GO <= 0x80);
                // According to fullsnes section "SNES Cart GSU-n Code-Cache",
                // setting the GO flag to zero forces the CBR to zero and marks
                // all cache blocks as empty.
                if state.sfr & SuperFxState::FLAG_GO == 0 {
                    state.cbr = 0;
                    state.cache_flags = 0;
                }
            }
            0x01 => {
                let mut state = self.state.borrow_mut();
                state.sfr = (u16::from(data) << 8) | (state.sfr & 0x00ff);
            }
            0x02 => {} // unused address
            0x03 => {
                self.state.borrow_mut().bramr = data & SuperFxState::BRAMR_MASK
            }
            0x04 => self.state.borrow_mut().pbr = data,
            0x05 => {} // unused address
            0x06 => {} // ROMBR is read-only
            0x07 => {
                self.state.borrow_mut().cfgr = data & SuperFxState::CFGR_MASK
            }
            0x08 => self.state.borrow_mut().scbr = data,
            0x09 => {
                self.state.borrow_mut().clsr = data & SuperFxState::CLSR_MASK
            }
            0x0a => {
                self.state.borrow_mut().scmr = data & SuperFxState::SCMR_MASK
            }
            0x0b => {} // VCR is read-only
            0x0c => {} // RAMBR is read-only
            0x0d => {} // unused address
            0x0e => {} // CBR is read-only
            0x0f => {} // CBR is read-only
            _ => unreachable!(),
        }
    }
}

//===========================================================================//

/// Provides access to SuperFX instruction cache to the SNES CPU.
struct SuperFxCacheBus {
    state: Rc<RefCell<SuperFxState>>,
    watchpoints: BiHashMap<(u16, WatchKind), WatchId>,
}

impl SuperFxCacheBus {
    const BIT_WIDTH: BitWidth = BitWidth::new(9);
    const ADDR_MASK: u16 = 0x1ff;

    pub fn new(state: Rc<RefCell<SuperFxState>>) -> Self {
        Self { state, watchpoints: BiHashMap::new() }
    }
}

impl SimBus for SuperFxCacheBus {
    fn description(&self) -> String {
        "SuperFX instruction cache".to_string()
    }

    fn bit_width(&self) -> BitWidth {
        Self::BIT_WIDTH
    }

    fn label_at(&self, _addr: Addr) -> Option<String> {
        None
    }

    fn watchpoint_at(&self, addr: Addr, kind: WatchKind) -> Option<WatchId> {
        let addr = addr.as_u16() & Self::ADDR_MASK;
        self.watchpoints.get_by_left(&(addr, kind)).cloned()
    }

    fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        let addr = addr.as_u16() & Self::ADDR_MASK;
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
        _label: &str,
        _kind: WatchKind,
    ) -> Option<WatchId> {
        None
    }

    fn unwatch(&mut self, id: WatchId) {
        self.watchpoints.remove_by_right(&id);
    }

    fn peek_byte(&self, addr: Addr) -> u8 {
        let addr = addr.as_u16() & Self::ADDR_MASK;
        self.state.borrow().cache[addr as usize]
    }

    fn read_byte(&mut self, addr: Addr) -> u8 {
        self.peek_byte(addr)
    }

    fn write_byte(&mut self, addr: Addr, data: u8) {
        let addr = addr.as_u16() & Self::ADDR_MASK;
        let mut state = self.state.borrow_mut();
        state.cache[addr as usize] = data;
        // According to fullsnes section "SNES Cart GSU-n Code-Cache",
        // writing the last byte of a 16-byte cache line marks that line as
        // not-empty.
        if addr & 0x000f == 0x000f {
            let cache_line_number = addr >> 4;
            debug_assert!(cache_line_number < 32);
            let cache_line_bit = 1u32 << cache_line_number;
            state.cache_flags |= cache_line_bit;
        }
    }
}

//===========================================================================//
