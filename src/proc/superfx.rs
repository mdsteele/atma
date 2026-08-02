use super::util::watch;
use crate::addr::Addr;
use crate::bus::{SimBus, WatchKind};
use crate::dis::superfx::{Instruction, Operation, Reg};
use crate::proc::{SimBreak, SimProc};

//===========================================================================//

const SHIFT_Z: u8 = 1;
const SHIFT_CY: u8 = 2;
const SHIFT_S: u8 = 3;
const SHIFT_OV: u8 = 4;
const SHIFT_GO: u8 = 5;
const SHIFT_R: u8 = 6;
const SHIFT_ALT2: u8 = 8;
const SHIFT_ALT1: u8 = 9;
const SHIFT_IL: u8 = 10;
const SHIFT_IH: u8 = 11;
const SHIFT_B: u8 = 12;
const SHIFT_IRQ: u8 = 15;

// Flags for the Status Flags Register.  These are described in:
//   * https://sneslab.net/wiki/Super_FX#Status/Flag_Register_(SFR)
//   * fullsnes section "SNES Cart GSU-n General I/O Ports"
//   * SNES Book II page 2-4-4
const FLAG_Z: u16 = 1 << SHIFT_Z;
const FLAG_CY: u16 = 1 << SHIFT_CY;
const FLAG_S: u16 = 1 << SHIFT_S;
const FLAG_OV: u16 = 1 << SHIFT_OV;
const FLAG_GO: u16 = 1 << SHIFT_GO;
const FLAG_R: u16 = 1 << SHIFT_R;
const FLAG_ALT2: u16 = 1 << SHIFT_ALT2;
const FLAG_ALT1: u16 = 1 << SHIFT_ALT1;
const FLAG_IL: u16 = 1 << SHIFT_IL;
const FLAG_IH: u16 = 1 << SHIFT_IH;
const FLAG_B: u16 = 1 << SHIFT_B;
const FLAG_IRQ: u16 = 1 << SHIFT_IRQ;

const CBR_MASK: u16 = 0xfff0;
const RAMBR_MASK: u8 = 0x01;
const SFR_MASK: u16 = FLAG_Z
    | FLAG_CY
    | FLAG_S
    | FLAG_OV
    | FLAG_R
    | FLAG_ALT1
    | FLAG_ALT2
    | FLAG_IL
    | FLAG_IH
    | FLAG_B
    | FLAG_IRQ;

//===========================================================================//

#[derive(Clone, Copy)]
enum Microcode {
    DecodeOpcode,     // decode DATA as opcode, push new microcode
    FetchOpcode,      // DATA = next opcode (using prefetched if available)
    ReadByte,         // DATA = [ADDR], watch(ADDR, Read)
    ReadInst,         // ADDR = PBR:R15, push microcode to inc R15, ReadByte
    SetAddr(u32),     // ADDR = value
    SetPrefetched,    // prefetched_opcode = DATA
    SetReg(Reg, u16), // reg = value
    SetRegLo(Reg),    // reg = (reg & 0xff00) | DATA
    UpdatePc,         // PC16 = R15, push microcode to prefetch next opcode
    ZeroRegHi(Reg),   // reg = reg & 0x00ff
}

//===========================================================================//

/// A simulated SuperFX processor.
pub struct SuperFx {
    pc16: u16, // virtual lower bits of the 24-bit PC, copied from R15
    pbr: u8,   // Program Bank Register (upper 8 bits of the 24-bit PC)
    cbr: u16,  // Cache Base Register
    cache_flags: u32, // bitfield for which cache blocks are valid
    /// The RAM Bank Register, used to determine the upper 8 bits of cartridge
    /// RAM addresses used by certain SuperFX instructions.
    ///
    /// This is a 1-bit register: according to SNES Book II page 2-4-6, the
    /// upper 7 bits are always zero when the register is read.  However, a
    /// value of 0 actually corresponds to bank 0x70, and a value of 1
    /// corresponds to bank 0x71.
    rambr: u8, // Ram Bank Register (upper 8 bits of RAM address)
    /// The ROM Bank Register, used to determine the upper 8 bits of cartridge
    /// ROM addresses used by certain SuperFX instructions.
    ///
    /// This is an 8-bit register, according to SNES Book II page 2-4-5, even
    /// though page 2-3-4 indicates that only values from 0x00 through 0x5f are
    /// valid (which would seem to suggest that only seven bits are normally
    /// used in practice).
    rombr: u8, // Rom Bank Register (upper 8 bits of ROM address)
    sfr: u16,  // Status Flags Register
    regs: [u16; 16],
    src: Reg,
    dst: Reg,
    data: u8,
    addr: u32,
    prefetched_opcode: Option<u8>,
    microcode: Vec<Microcode>,
}

impl Default for SuperFx {
    fn default() -> SuperFx {
        SuperFx::new()
    }
}

impl SuperFx {
    /// Returns a new simulated SuperFX processor.
    pub fn new() -> SuperFx {
        SuperFx {
            // According to SNES Book II page 2-4-3, R15's initial value is
            // 0x0000, so we mirror that here.
            pc16: 0x0000,
            // According to SNES Book II page 2-4-5, PBR's initial value is
            // undefined.
            pbr: u8::MAX,
            // According to SNES Book II page 2-4-6, CBR's initial value is
            // 0x0000.
            cbr: 0x0000,
            cache_flags: 0,
            // According to SNES Book II page 2-9-101, RAMBR's initial value is
            // "invalid"; according to page 2-4-6, its default value is
            // "undefined".  fullsnes section "SNES Cart GSU-n General I/O
            // Ports" points out that existing games have at most 64 KiB of
            // cartridge RAM, so in practice this should always be set to zero.
            // Therefore, we initialize to its maximum, nonzero value as the
            // least "valid" value available.
            rambr: RAMBR_MASK,
            // According to SNES Book II page 2-9-104, ROMBR's initial value is
            // "invalid"; according to page 2-4-5, its default value is
            // "undefined".  Page 2-4-5 also indicates that ROMBR is eight bits
            // wide, but page 2-3-4 says that ROMBR may only be used to specify
            // banks 0x00 through 0x5f.  Therefore, we use 0xff as an "invalid"
            // initial value.
            rombr: u8::MAX,
            // According to SNES Book II page 2-4-4, SFR's initial value is
            // 0x0000.
            sfr: 0x0000,
            regs: [0u16; 16],
            src: Reg::R0,
            dst: Reg::R0,
            data: 0,
            addr: 0,
            prefetched_opcode: None,
            microcode: Vec::new(),
        }
    }

    fn alt(&self) -> u8 {
        (((self.sfr >> 9) & 0x1) | ((self.sfr >> 8) & 0x2)) as u8
    }

    fn clear_alt(&mut self) {
        self.sfr &= !(FLAG_ALT1 | FLAG_ALT2 | FLAG_B);
        self.src = Reg::R0;
        self.dst = Reg::R0;
    }

    fn execute_microcode(
        &mut self,
        bus: &mut dyn SimBus,
        microcode: Microcode,
    ) -> Result<(), SimBreak> {
        match microcode {
            Microcode::DecodeOpcode => self.exec_decode_opcode(),
            Microcode::FetchOpcode => self.exec_fetch_opcode(bus),
            Microcode::ReadByte => self.exec_read_byte(bus),
            Microcode::ReadInst => self.exec_read_inst(bus),
            Microcode::SetAddr(addr) => self.exec_set_addr(addr),
            Microcode::SetPrefetched => self.exec_set_prefetched(),
            Microcode::SetReg(reg, value) => self.exec_set_reg(reg, value),
            Microcode::SetRegLo(reg) => self.exec_set_reg_lo(reg),
            Microcode::UpdatePc => self.exec_update_pc(),
            Microcode::ZeroRegHi(reg) => self.exec_zero_reg_hi(reg),
        }
    }

    fn exec_decode_opcode(&mut self) -> Result<(), SimBreak> {
        let opcode = self.data;
        match Operation::from_opcode(opcode, self.alt()) {
            Operation::Alt1 => {
                self.sfr |= FLAG_ALT1;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::Alt2 => {
                self.sfr |= FLAG_ALT2;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::Alt3 => {
                self.sfr |= FLAG_ALT1 | FLAG_ALT2;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::And(reg) => {
                let value = self.get_reg(self.src) & self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Andi(imm) => {
                let value = self.get_reg(self.src) & u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Bic(reg) => {
                let value = self.get_reg(self.src) & !self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Bici(imm) => {
                let value = self.get_reg(self.src) & !u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Div2 => {
                let source = self.get_reg(self.src);
                self.set_flag(FLAG_CY, source & 0x0001 != 0);
                let value = ((source as i16) / 2) as u16;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::From(reg) => {
                // If the B flag is set (by a WITH prefix), then FROM acts as a
                // MOVES instruction (see
                // https://sneslab.net/wiki/MOVES_(Super_FX)).  Otherwise, it's
                // a prefix that sets the source register.
                if !self.get_flag(FLAG_B) {
                    self.src = reg;
                    self.microcode.push(Microcode::UpdatePc);
                    return Ok(()); // don't clear_alt()
                }
                let value = self.get_reg(reg);
                self.set_flag(FLAG_OV, value & 0x0080 != 0);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Hib => {
                let value = self.get_reg(self.src) >> 8;
                self.set_flag(FLAG_Z, value == 0);
                self.set_flag(FLAG_S, value >= 0x80);
                self.microcode.push(Microcode::SetReg(self.dst, value));
            }
            Operation::Ldb(reg) => {
                self.microcode.push(Microcode::ZeroRegHi(self.dst));
                self.microcode.push(Microcode::SetRegLo(self.dst));
                self.microcode.push(Microcode::ReadByte);
                let addr = (u32::from(0x70 | self.rambr) << 16)
                    | u32::from(self.get_reg(reg));
                self.microcode.push(Microcode::SetAddr(addr));
            }
            Operation::Link(offset) => {
                self.regs[11] = self.regs[15].wrapping_add(u16::from(offset));
            }
            Operation::Lob => {
                let value = self.get_reg(self.src) & 0x00ff;
                self.set_flag(FLAG_Z, value == 0);
                self.set_flag(FLAG_S, value >= 0x80);
                self.microcode.push(Microcode::SetReg(self.dst, value));
            }
            Operation::Lsr => {
                let source = self.get_reg(self.src);
                self.set_flag(FLAG_CY, source & 0x0001 != 0);
                let value = source >> 1;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Merge => {
                let value = (self.regs[7] & 0xff00) | (self.regs[8] >> 8);
                self.set_flag(FLAG_S, value & 0x8080 != 0);
                self.set_flag(FLAG_OV, value & 0xc0c0 != 0);
                self.set_flag(FLAG_CY, value & 0xe0e0 != 0);
                self.set_flag(FLAG_Z, value & 0xf0f0 != 0);
                self.microcode.push(Microcode::SetReg(self.dst, value));
            }
            Operation::Mult(reg) => {
                let lhs = (self.get_reg(self.src) as i8) as i16;
                let rhs = (self.get_reg(reg) as i8) as i16;
                let value = (lhs * rhs) as u16;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Multi(imm) => {
                let lhs = (self.get_reg(self.src) as i8) as i16;
                let rhs = i16::from(imm);
                let value = (lhs * rhs) as u16;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Nop => {}
            Operation::Not => {
                let value = !self.get_reg(self.src);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Or(reg) => {
                let value = self.get_reg(self.src) | self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Ori(imm) => {
                let value = self.get_reg(self.src) | u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Ramb => {
                // RAMBR is only 1 bit wide.  Accordingly, fullsnes section
                // "SNES Cart GSU-n CPU MOV Opcodes", the RAMB instruction sets
                // RAMBR to the lowest bit of the source register (note that
                // `RAMBR_MASK` is 0x01), ignoring all other bits.
                //
                // By constrast, SNES Book II page 2-9-101 is unclear on this
                // point, describing the RAMB instruction as setting the RAMBR
                // to the low *byte* of the source register, and giving an
                // example of setting it to 0x70 (which is the actual bank used
                // when RAMBR is zero).  That page also says that RAMBR's
                // initial value is invalid, suggesting that perhaps it is
                // important to use a valid bank byte here.  But the Star Fox
                // ROM apparently executes RAMB with the source register equal
                // to zero while starting up, so it looks like there's no need
                // to use a value of 0x70.
                self.rambr = (self.get_reg(self.src) as u8) & RAMBR_MASK;
            }
            Operation::Romb => {
                self.rombr = self.get_reg(self.src) as u8;
            }
            Operation::Sex => {
                let mut value = self.get_reg(self.src) & 0x00ff;
                if value >= 0x80 {
                    value |= 0xff00;
                }
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Stop => {
                self.sfr &= !FLAG_GO;
                self.sfr |= FLAG_IRQ;
                self.clear_alt();
                return Err(SimBreak::HaltOpcode("STOP", opcode));
            }
            Operation::Swap => {
                let value = self.get_reg(self.src).rotate_right(8);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::To(reg) => {
                // If B flag is set (by a WITH prefix), then TO acts as a MOVE
                // instruction (see https://sneslab.net/wiki/MOVE_(Super_FX)).
                // Otherwise, it's a prefix that sets the destination register.
                if !self.get_flag(FLAG_B) {
                    self.dst = reg;
                    self.microcode.push(Microcode::UpdatePc);
                    return Ok(()); // don't clear_alt()
                }
                let value = self.get_reg(self.src);
                // MOVE does not update the S or Z flags.
                self.microcode.push(Microcode::SetReg(reg, value));
            }
            Operation::Umult(reg) => {
                let lhs = self.get_reg(self.src) & 0x00ff;
                let rhs = self.get_reg(reg) & 0x00ff;
                let value = lhs * rhs;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Umulti(imm) => {
                let lhs = self.get_reg(self.src) & 0x00ff;
                let rhs = u16::from(imm);
                let value = lhs * rhs;
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::With(reg) => {
                self.src = reg;
                self.dst = reg;
                self.sfr |= FLAG_B;
                self.microcode.push(Microcode::UpdatePc);
                return Ok(()); // don't clear_alt()
            }
            Operation::Xor(reg) => {
                let value = self.get_reg(self.src) ^ self.get_reg(reg);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            Operation::Xori(imm) => {
                let value = self.get_reg(self.src) ^ u16::from(imm);
                self.update_sz_flags_and_push_set_dst_reg(value);
            }
            other => todo!("{other:?}"),
        }
        self.clear_alt();
        self.microcode.push(Microcode::UpdatePc);
        Ok(())
    }

    fn exec_fetch_opcode(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        if let Some(opcode) = self.prefetched_opcode.take() {
            self.data = opcode;
            Ok(())
        } else {
            self.exec_read_inst(bus)
        }
    }

    fn exec_read_byte(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let addr = Addr::from(self.addr);
        self.data = bus.read_byte(addr);
        watch(bus, addr, WatchKind::Read)
    }

    fn exec_read_inst(
        &mut self,
        bus: &mut dyn SimBus,
    ) -> Result<(), SimBreak> {
        let next = self.regs[15].wrapping_add(1);
        self.microcode.push(Microcode::SetReg(Reg::R15, next));
        self.addr = (u32::from(self.pbr) << 16) | u32::from(self.regs[15]);
        self.exec_read_byte(bus)
    }

    fn exec_set_addr(&mut self, addr: u32) -> Result<(), SimBreak> {
        self.addr = addr;
        Ok(())
    }

    fn exec_set_prefetched(&mut self) -> Result<(), SimBreak> {
        self.prefetched_opcode = Some(self.data);
        Ok(())
    }

    fn exec_set_reg(&mut self, reg: Reg, value: u16) -> Result<(), SimBreak> {
        self.set_reg(reg, value);
        Ok(())
    }

    fn exec_set_reg_lo(&mut self, reg: Reg) -> Result<(), SimBreak> {
        self.regs[reg.index()] &= 0xff00;
        self.regs[reg.index()] |= self.data as u16;
        Ok(())
    }

    fn exec_update_pc(&mut self) -> Result<(), SimBreak> {
        self.pc16 = self.regs[15];
        // TODO: optionally skip this part if pipelining is disabled
        if true {
            self.microcode.push(Microcode::SetPrefetched);
            self.microcode.push(Microcode::ReadInst);
        }
        Ok(())
    }

    fn exec_zero_reg_hi(&mut self, reg: Reg) -> Result<(), SimBreak> {
        self.regs[reg.index()] &= 0x00ff;
        Ok(())
    }

    fn get_flag(&self, flag: u16) -> bool {
        self.sfr & flag != 0
    }

    fn set_flag(&mut self, flag: u16, value: bool) {
        if value {
            self.sfr |= flag;
        } else {
            self.sfr &= !flag;
        }
    }

    fn update_sz_flags_16(&mut self, value: u16) {
        self.set_flag(FLAG_Z, value == 0);
        self.set_flag(FLAG_S, value >= 0x8000);
    }

    fn get_reg(&self, reg: Reg) -> u16 {
        self.regs[reg.index()]
    }

    fn set_reg(&mut self, reg: Reg, value: u16) {
        self.regs[reg.index()] = value;
    }

    fn update_sz_flags_and_push_set_dst_reg(&mut self, value: u16) {
        self.update_sz_flags_16(value);
        self.microcode.push(Microcode::SetReg(self.dst, value));
    }
}

impl SimProc for SuperFx {
    fn description(&self) -> String {
        "SuperFX".to_string()
    }

    fn disassemble(&self, bus: &dyn SimBus, pc: Addr) -> (u32, String) {
        let pc = pc.as_u32();
        let instruction = Instruction::decode(bus, pc, self.alt());
        (instruction.size(), instruction.format(bus, pc, self.rambr))
    }

    fn pc(&self) -> Addr {
        (Addr::from(self.pbr) << 16) | Addr::from(self.regs[15])
    }

    fn set_pc(&mut self, addr: Addr) {
        self.regs[15] = addr.as_u16();
        self.pbr = (addr >> 16).as_u8();
        self.prefetched_opcode = None;
        self.microcode.clear();
    }

    fn register_names(&self) -> &'static [&'static str] {
        &[
            "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
            "R11", "R12", "R13", "R14", "R15", "CBR", "RAMBR", "ROMBR", "SFR",
            "DATA",
        ]
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        match name {
            "R0" => Some(u32::from(self.regs[0])),
            "R1" => Some(u32::from(self.regs[1])),
            "R2" => Some(u32::from(self.regs[2])),
            "R3" => Some(u32::from(self.regs[3])),
            "R4" => Some(u32::from(self.regs[4])),
            "R5" => Some(u32::from(self.regs[5])),
            "R6" => Some(u32::from(self.regs[6])),
            "R7" => Some(u32::from(self.regs[7])),
            "R8" => Some(u32::from(self.regs[8])),
            "R9" => Some(u32::from(self.regs[9])),
            "R10" => Some(u32::from(self.regs[10])),
            "R11" => Some(u32::from(self.regs[11])),
            "R12" => Some(u32::from(self.regs[12])),
            "R13" => Some(u32::from(self.regs[13])),
            "R14" => Some(u32::from(self.regs[14])),
            "R15" => Some(u32::from(self.regs[15])),
            "CBR" => Some(u32::from(self.cbr)),
            "RAMBR" => Some(u32::from(self.rambr)),
            "ROMBR" => Some(u32::from(self.rombr)),
            "SFR" => Some(u32::from(self.sfr)),
            "DATA" => Some(u32::from(self.data)),
            _ => None,
        }
    }

    fn set_register(&mut self, name: &str, value: u32) {
        match name {
            "R0" => self.regs[0] = value as u16,
            "R1" => self.regs[1] = value as u16,
            "R2" => self.regs[2] = value as u16,
            "R3" => self.regs[3] = value as u16,
            "R4" => self.regs[4] = value as u16,
            "R5" => self.regs[5] = value as u16,
            "R6" => self.regs[6] = value as u16,
            "R7" => self.regs[7] = value as u16,
            "R8" => self.regs[8] = value as u16,
            "R9" => self.regs[9] = value as u16,
            "R10" => self.regs[10] = value as u16,
            "R11" => self.regs[11] = value as u16,
            "R12" => self.regs[12] = value as u16,
            "R13" => self.regs[13] = value as u16,
            "R14" => self.regs[14] = value as u16,
            "R15" => self.regs[15] = value as u16,
            "CBR" => self.cbr = (value as u16) & CBR_MASK,
            "RAMBR" => self.rambr = (value as u8) & RAMBR_MASK,
            "ROMBR" => self.rombr = value as u8,
            "SFR" => {
                self.sfr = (value as u16) & SFR_MASK;
                // According to fullsnes section "SNES Cart GSU-n Code-Cache",
                // setting the GO flag to zero forces the CBR to zero and marks
                // all cache blocks as empty.
                if !self.get_flag(FLAG_GO) {
                    self.cbr = 0;
                    self.cache_flags = 0;
                }
            }
            "DATA" => self.data = value as u8,
            _ => {}
        };
    }

    fn step(&mut self, bus: &mut dyn SimBus) -> Result<(), SimBreak> {
        if self.microcode.is_empty() {
            self.microcode.push(Microcode::DecodeOpcode);
            self.microcode.push(Microcode::FetchOpcode);
        }
        while let Some(microcode) = self.microcode.pop() {
            self.execute_microcode(bus, microcode)?;
        }
        watch(bus, self.pc(), WatchKind::Pc)
    }

    fn is_mid_instruction(&self) -> bool {
        !self.microcode.is_empty()
    }
}

//===========================================================================//
