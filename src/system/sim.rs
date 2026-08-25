use crate::addr::Addr;
use crate::bus::{SimBus, WatchId, WatchKind};
use crate::proc::{SimBreak, SimProc};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub type ProcBus = (Box<dyn SimProc>, Box<dyn SimBus>);

//===========================================================================//

struct EnvProc {
    proc: Box<dyn SimProc>,
    bus: Box<dyn SimBus>,
}

impl EnvProc {
    fn new(proc: Box<dyn SimProc>, bus: Box<dyn SimBus>) -> EnvProc {
        EnvProc { proc, bus }
    }

    fn description(&self) -> String {
        format!("{} core, {}", self.proc.description(), self.bus.description())
    }

    fn disassemble(&self, addr: Addr) -> (u32, String) {
        self.proc.disassemble(&*self.bus, addr)
    }

    fn step(&mut self) -> Result<(), SimBreak> {
        self.proc.step(&mut *self.bus)
    }
}

//===========================================================================//

/// A complete simulated system, including one or more processors and memory
/// address spaces.
pub struct SimSystem {
    selected_processor: Rc<str>,
    processors: HashMap<Rc<str>, EnvProc>,
}

impl SimSystem {
    /// Constructs a new simulated system with the given list of processors.
    /// Panics if `processors` is empty.
    pub fn new(processors: Vec<(Rc<str>, ProcBus)>) -> Self {
        if processors.is_empty() {
            panic!("must provide non-empty list of processors");
        }
        let selected_processor = processors[0].0.clone();
        let processors = processors
            .into_iter()
            .map(|(name, (proc, bus))| (name, EnvProc::new(proc, bus)))
            .collect();
        Self { selected_processor, processors }
    }

    /// Creates a new simulated system with a single `NopProc` CPU.
    pub fn with_nop_cpu() -> Self {
        let bus = crate::bus::new_open_bus(Addr::BITS);
        let cpu = crate::proc::NopProc::new();
        Self::new(vec![(Rc::from("cpu"), (Box::new(cpu), bus))])
    }

    /// Returns a human-readable, multi-line description of this simulated
    /// system.
    pub fn description(&self) -> String {
        self.processors
            .iter()
            .map(Self::describe_processor)
            .collect::<Vec<String>>()
            .join("")
    }

    fn describe_processor((name, proc): (&Rc<str>, &EnvProc)) -> String {
        format!("{name}: {}\n", proc.description())
    }

    /// Returns a sorted list of the names of all processors that exist in this
    /// simulated system.
    pub fn processor_names(&self) -> Vec<Rc<str>> {
        let mut vec =
            self.processors.keys().cloned().collect::<Vec<Rc<str>>>();
        vec.sort_unstable();
        vec
    }

    /// Returns true if this simulated system contains a processor with the
    /// given name.
    pub fn contains_processor(&self, proc_name: &str) -> bool {
        self.processors.contains_key(proc_name)
    }

    /// Returns a list of register names for the processor with the given name.
    ///
    /// Panics if no such processor exists in this simulated system.
    pub fn register_names(&self, proc_name: &str) -> &'static [&'static str] {
        let proc = self.processors.get(proc_name).unwrap();
        proc.proc.register_names()
    }

    fn current_processor(&self) -> &EnvProc {
        self.processors.get(&self.selected_processor).unwrap()
    }

    fn current_processor_mut(&mut self) -> &mut EnvProc {
        self.processors.get_mut(&self.selected_processor).unwrap()
    }

    /// Returns the name of the currently selected processor.
    pub fn selected_processor_name(&self) -> Rc<str> {
        self.selected_processor.clone()
    }

    /// Selects the processor with the given name as the currently selected
    /// processor.
    ///
    /// Panics if no such processor exists in this simulated system.
    pub fn select_processor(&mut self, proc_name: &str) {
        let (name, _) = self.processors.get_key_value(proc_name).unwrap();
        self.selected_processor = name.clone();
    }

    /// Returns the current address of the currently selected processor's
    /// program counter.
    pub fn pc(&self) -> Addr {
        self.current_processor().proc.pc()
    }

    /// Sets the current address of the currently selected processor's program
    /// counter.
    pub fn set_pc(&mut self, addr: Addr) {
        self.current_processor_mut().proc.set_pc(addr);
    }

    /// Returns the value of the specified register in the currently selected
    /// processor, or `None` if no such register exists in that processor.
    pub fn get_register(&self, name: &str) -> Option<u32> {
        self.current_processor().proc.get_register(name)
    }

    /// Sets the value of the specified register in the currently selected
    /// processor.  Does nothing if no such register exists in that processor.
    pub fn set_register(&mut self, name: &str, value: u32) {
        self.current_processor_mut().proc.set_register(name, value);
    }

    /// Writes a single byte to memory using the currently selected processor's
    /// memory bus.
    pub fn write_byte(&mut self, addr: Addr, data: u8) {
        self.current_processor_mut().bus.write_byte(addr, data);
    }

    /// Disassembles the instruction starting at the given address for the
    /// currently selected processor, returning the length of the instruction
    /// in bytes, and a human-readable string with the assembly code for that
    /// instruction.
    pub fn disassemble(&self, addr: Addr) -> (u32, String) {
        self.current_processor().disassemble(addr)
    }

    /// Advances the currently selected processor by one instruction.
    pub fn step(&mut self) -> Result<(), SimBreak> {
        self.current_processor_mut().step()
    }

    /// Returns true if the currently selected processor is currently paused in
    /// the middle of an instruction due to the last `step()` being interrupted
    /// by a breakpoint condition.  Returns false if the processor is between
    /// instructions.
    pub fn is_mid_instruction(&self) -> bool {
        self.current_processor().proc.is_mid_instruction()
    }

    /// Sets a watchpoint on the given address for the currently selected
    /// processor.
    pub fn watch_address(&mut self, addr: Addr, kind: WatchKind) -> WatchId {
        self.current_processor_mut().bus.watch_address(addr, kind)
    }

    /// Removes the specified watchpoint from the currently selected processor.
    pub fn unwatch(&mut self, id: WatchId) {
        self.current_processor_mut().bus.unwatch(id);
    }
}

//===========================================================================//
