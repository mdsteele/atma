use super::proc::SimProc;
use std::collections::HashMap;

//===========================================================================//

/// A complete simulated environment, including one or more processors and
/// memory address spaces.
pub struct SimEnv {
    pub(crate) selected_processor: String,
    pub(crate) processors: HashMap<String, Box<dyn SimProc>>,
}

impl SimEnv {
    /// Returns a human-readable, multi-line description of this simulated
    /// environment.
    pub fn description(&self) -> String {
        self.processors
            .iter()
            .map(SimEnv::describe_processor)
            .collect::<Vec<String>>()
            .join("\n")
    }

    fn describe_processor(
        (name, proc): (&String, &Box<dyn SimProc>),
    ) -> String {
        format!("{}: {}, pc={:x}\n", name, proc.description(), proc.pc())
    }

    /// Advances the currently selected processor by one instruction.
    pub fn step(&mut self) {
        self.processors.get_mut(&self.selected_processor).unwrap().step()
    }
}

//===========================================================================//
