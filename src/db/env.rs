use crate::bus::SimBus;
use crate::proc::{Breakpoint, SimBreak, SimProc};
use std::collections::{HashMap, HashSet};

//===========================================================================//

pub type ProcBus = (Box<dyn SimProc>, Box<dyn SimBus>);

//===========================================================================//

struct EnvProc {
    proc: Box<dyn SimProc>,
    bus: Box<dyn SimBus>,
    pc_breakpoints: HashSet<u32>,
}

impl EnvProc {
    fn new(proc: Box<dyn SimProc>, bus: Box<dyn SimBus>) -> EnvProc {
        EnvProc { proc, bus, pc_breakpoints: HashSet::new() }
    }

    fn description(&self) -> String {
        format!("{} with {}", self.proc.description(), self.bus.description())
    }

    fn pc(&self) -> u32 {
        self.proc.pc()
    }

    fn set_pc(&mut self, addr: u32) {
        self.proc.set_pc(addr);
    }

    fn register_names(&self) -> &'static [&'static str] {
        self.proc.register_names()
    }

    fn get_register(&self, name: &str) -> Option<u32> {
        self.proc.get_register(name)
    }

    fn set_register(&mut self, name: &str, value: u32) {
        self.proc.set_register(name, value)
    }

    fn write_byte(&mut self, addr: u32, data: u8) {
        self.bus.write_byte(addr, data);
    }

    fn add_pc_breakpoint(&mut self, addr: u32) {
        self.pc_breakpoints.insert(addr);
    }

    fn remove_pc_breakpoint(&mut self, addr: u32) {
        self.pc_breakpoints.remove(&addr);
    }

    fn disassemble(&self, addr: u32) -> (usize, String) {
        self.proc.disassemble(&*self.bus, addr)
    }

    fn step(&mut self) -> Result<(), SimBreak> {
        self.proc.step(&mut *self.bus)?;
        let pc = self.proc.pc();
        if self.pc_breakpoints.contains(&pc) {
            Err(SimBreak::Breakpoint(Breakpoint::Pc(pc)))
        } else {
            Ok(())
        }
    }
}

//===========================================================================//

/// A complete simulated environment, including one or more processors and
/// memory address spaces.
pub struct SimEnv {
    selected_processor: String,
    processors: HashMap<String, EnvProc>,
}

impl SimEnv {
    /// Constructs a new debugging environment with the given list of
    /// processors.  Panics if `processors` is empty.
    pub fn new(processors: Vec<(String, ProcBus)>) -> SimEnv {
        if processors.is_empty() {
            panic!("must provide non-empty list of processors to SimEnv::new");
        }
        let selected_processor = processors[0].0.clone();
        let processors = processors
            .into_iter()
            .map(|(name, (proc, bus))| (name, EnvProc::new(proc, bus)))
            .collect();
        SimEnv { selected_processor, processors }
    }

    #[cfg(test)]
    pub(crate) fn with_nop_cpu() -> SimEnv {
        let bus = crate::bus::new_null_bus();
        let cpu = crate::proc::NopProc::new();
        SimEnv::new(vec![("cpu".to_string(), (Box::new(cpu), bus))])
    }

    /// Returns a human-readable, multi-line description of this simulated
    /// environment.
    pub fn description(&self) -> String {
        self.processors
            .iter()
            .map(SimEnv::describe_processor)
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn describe_processor((name, proc): (&String, &EnvProc)) -> String {
        format!("{name}: {}, pc={:x}\n", proc.description(), proc.pc())
    }

    fn current_processor(&self) -> &EnvProc {
        self.processors.get(&self.selected_processor).unwrap()
    }

    fn current_processor_mut(&mut self) -> &mut EnvProc {
        self.processors.get_mut(&self.selected_processor).unwrap()
    }

    /// Returns the current address of the currently selected processor's
    /// program counter.
    pub fn pc(&self) -> u32 {
        self.current_processor().pc()
    }

    /// Sets the current address of the currently selected processor's program
    /// counter.
    pub fn set_pc(&mut self, addr: u32) {
        self.current_processor_mut().set_pc(addr);
    }

    /// Returns a list of the currently selected processor's register names and
    /// current values.
    pub fn register_names(&self) -> &'static [&'static str] {
        self.current_processor().register_names()
    }

    /// Returns the value of the specified register in the currently selected
    /// processor, or `None` if no such register exists in that processor.
    pub fn get_register(&self, name: &str) -> Option<u32> {
        self.current_processor().get_register(name)
    }

    /// Sets the value of the specified register in the currently selected
    /// processor.  Does nothing if no such register exists in that processor.
    pub fn set_register(&mut self, name: &str, value: u32) {
        self.current_processor_mut().set_register(name, value);
    }

    /// Writes a single byte to memory using the currently selected processor's
    /// memory bus.
    pub fn write_byte(&mut self, addr: u32, data: u8) {
        self.current_processor_mut().write_byte(addr, data);
    }

    /// Disassembles the instruction starting at the given address for the
    /// currently selected processor, returning the length of the instruction
    /// in bytes, and a human-readable string with the assembly code for that
    /// instruction.
    pub fn disassemble(&self, addr: u32) -> (usize, String) {
        self.current_processor().disassemble(addr)
    }

    /// Advances the currently selected processor by one instruction.
    pub fn step(&mut self) -> Result<(), SimBreak> {
        self.current_processor_mut().step()
    }

    /// Adds a new condition under which the simulation should be paused.
    pub fn add_breakpoint(&mut self, breakpoint: Breakpoint) {
        match breakpoint {
            Breakpoint::Pc(addr) => {
                self.current_processor_mut().add_pc_breakpoint(addr)
            }
            _ => panic!("{breakpoint:?} not supported yet"),
        }
    }

    /// Removes a condition under which the simulation should be paused.
    pub fn remove_breakpoint(&mut self, breakpoint: Breakpoint) {
        match breakpoint {
            Breakpoint::Pc(addr) => {
                self.current_processor_mut().remove_pc_breakpoint(addr)
            }
            _ => panic!("{breakpoint:?} not supported yet"),
        }
    }
}

//===========================================================================//
