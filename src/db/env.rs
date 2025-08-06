use crate::proc::{Breakpoint, SimBreak, SimProc};
use std::collections::{HashMap, HashSet};

//===========================================================================//

struct EnvProc {
    core: Box<dyn SimProc>,
    pc_breakpoints: HashSet<u32>,
}

impl EnvProc {
    fn new(core: Box<dyn SimProc>) -> EnvProc {
        EnvProc { core, pc_breakpoints: HashSet::new() }
    }

    fn pc(&self) -> u32 {
        self.core.pc()
    }

    pub fn registers(&self) -> Vec<(&'static str, u32)> {
        self.core.registers()
    }

    fn add_pc_breakpoint(&mut self, addr: u32) {
        self.pc_breakpoints.insert(addr);
    }

    fn disassemble(&self, addr: u32) -> (usize, String) {
        self.core.disassemble(addr)
    }

    fn step(&mut self) -> Result<(), SimBreak> {
        self.core.step()?;
        let pc = self.core.pc();
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
    pub fn new(processors: Vec<(String, Box<dyn SimProc>)>) -> SimEnv {
        if processors.is_empty() {
            panic!("must provide non-empty list of processors to SimEnv::new");
        }
        let selected_processor = processors[0].0.clone();
        let processors = processors
            .into_iter()
            .map(|(name, core)| (name, EnvProc::new(core)))
            .collect();
        SimEnv { selected_processor, processors }
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
        format!(
            "{}: {}, pc={:x}\n",
            name,
            proc.core.description(),
            proc.core.pc()
        )
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

    /// Returns a list of the currently selected processor's register names and
    /// current values.
    pub fn registers(&self) -> Vec<(&'static str, u32)> {
        self.current_processor().registers()
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
}

//===========================================================================//
