use super::inst::{AdsBreakpointKind, AdsInstruction};
use super::prog::AdsProgram;
use super::value::AdsValue;
use crate::db::SimEnv;
use crate::proc::{Breakpoint, SimBreak};
use num_bigint::BigInt;
use std::collections::HashMap;

//===========================================================================//

/// An error that can occur while executing an Atma Debugger Script program.
#[derive(Debug)]
pub struct AdsRuntimeError {}

//===========================================================================//

/// An in-progress execution of an [AdsProgram].
pub struct AdsEnvironment {
    sim: SimEnv,
    program: AdsProgram,
    pc: usize,
    variable_stack: Vec<AdsValue>,
    handler_stack: Vec<Breakpoint>,
    return_stack: Vec<usize>,
    handlers: HashMap<Breakpoint, Vec<usize>>,
}

impl AdsEnvironment {
    /// Creates a new execution of the given program.
    pub fn new(sim: SimEnv, program: AdsProgram) -> AdsEnvironment {
        AdsEnvironment {
            sim,
            program,
            pc: 0,
            variable_stack: Vec::new(),
            handler_stack: Vec::new(),
            return_stack: Vec::new(),
            handlers: HashMap::new(),
        }
    }

    /// Executes the next instruction in the program.
    pub fn step(&mut self) -> Result<bool, AdsRuntimeError> {
        match &self.program.instructions[self.pc] {
            AdsInstruction::BinOp(binop) => {
                debug_assert!(self.variable_stack.len() >= 2);
                let rhs = self.variable_stack.pop().unwrap();
                let lhs = self.variable_stack.pop().unwrap();
                self.variable_stack.push(binop.evaluate(lhs, rhs));
            }
            AdsInstruction::BranchUnless(offset) => {
                if !self.variable_stack.pop().unwrap().unwrap_bool() {
                    return self.jump(*offset);
                }
            }
            &AdsInstruction::CopyValue(index) => {
                let value = self.variable_stack[index].clone();
                self.variable_stack.push(value);
            }
            AdsInstruction::Exit => return Ok(true),
            AdsInstruction::Jump(offset) => {
                return self.jump(*offset);
            }
            AdsInstruction::ListIndex => {
                debug_assert!(self.variable_stack.len() >= 2);
                let rhs = self.variable_stack.pop().unwrap().unwrap_int();
                let lhs = self.variable_stack.pop().unwrap().unwrap_list();
                if rhs < BigInt::ZERO {
                    return Err(AdsRuntimeError {}); // TODO message
                }
                if rhs >= BigInt::from(lhs.len()) {
                    return Err(AdsRuntimeError {}); // TODO message
                }
                let item = lhs[usize::try_from(rhs).unwrap()].clone();
                self.variable_stack.push(item);
            }
            &AdsInstruction::MakeList(num_items) => {
                debug_assert!(self.variable_stack.len() >= num_items);
                let start = self.variable_stack.len() - num_items;
                let items = self.variable_stack.split_off(start);
                self.variable_stack.push(AdsValue::List(items));
            }
            &AdsInstruction::MakeTuple(num_items) => {
                debug_assert!(self.variable_stack.len() >= num_items);
                let start = self.variable_stack.len() - num_items;
                let items = self.variable_stack.split_off(start);
                self.variable_stack.push(AdsValue::Tuple(items));
            }
            AdsInstruction::PopHandler => {
                debug_assert!(!self.handler_stack.is_empty());
                let breakpoint = self.handler_stack.pop().unwrap();
                debug_assert!(self.handlers.contains_key(&breakpoint));
                let destinations = self.handlers.get_mut(&breakpoint).unwrap();
                debug_assert!(!destinations.is_empty());
                destinations.pop();
                if destinations.is_empty() {
                    self.handlers.remove(&breakpoint);
                    self.sim.remove_breakpoint(breakpoint);
                }
            }
            AdsInstruction::PopValue => {
                debug_assert!(!self.variable_stack.is_empty());
                self.variable_stack.pop();
            }
            AdsInstruction::Print => {
                let value = self.variable_stack.pop().unwrap();
                println!("{}", value);
            }
            &AdsInstruction::PushHandler(kind, offset) => {
                let breakpoint = self.create_breakpoint(kind);
                let destination = self.destination(offset);
                self.handlers.entry(breakpoint).or_default().push(destination);
                self.sim.add_breakpoint(breakpoint);
                self.handler_stack.push(breakpoint);
            }
            AdsInstruction::PushValue(value) => {
                self.variable_stack.push(value.clone());
            }
            &AdsInstruction::SetValue(index) => {
                let value = self.variable_stack.pop().unwrap();
                self.variable_stack[index] = value;
            }
            AdsInstruction::Step => {
                let pc = self.sim.pc();
                let instruction = self.sim.disassemble(self.sim.pc()).1;
                let result = self.sim.step();
                println!("${:04x} | {:16}", pc, instruction);
                match result {
                    Ok(()) => {}
                    Err(SimBreak::Breakpoint(breakpoint)) => {
                        println!("Breakpoint: {breakpoint:?}");
                        self.return_stack.push(self.pc);
                        debug_assert!(self.handlers.contains_key(&breakpoint));
                        let destinations =
                            self.handlers.get(&breakpoint).unwrap();
                        debug_assert!(!destinations.is_empty());
                        self.pc = *destinations.last().unwrap();
                        return Ok(false);
                    }
                    Err(SimBreak::HaltOpcode(mnemonic, opcode)) => {
                        println!("Halted by {mnemonic} opcode ${opcode:02x}");
                        return Ok(true);
                    }
                }
            }
            AdsInstruction::Return => {
                self.pc = self.return_stack.pop().unwrap();
            }
            &AdsInstruction::TupleItem(index) => {
                let items = self.variable_stack.pop().unwrap().unwrap_tuple();
                debug_assert!(index < items.len());
                self.variable_stack.push(items[index].clone());
            }
        }
        self.pc += 1;
        Ok(false)
    }

    fn jump(&mut self, offset: isize) -> Result<bool, AdsRuntimeError> {
        self.pc = self.destination(offset);
        Ok(false)
    }

    fn destination(&self, offset: isize) -> usize {
        let base = self.pc + 1;
        if offset < 0 {
            base - (-offset) as usize
        } else {
            base + offset as usize
        }
    }

    fn create_breakpoint(&mut self, kind: AdsBreakpointKind) -> Breakpoint {
        match kind {
            AdsBreakpointKind::Pc => {
                let addr = self.variable_stack.pop().unwrap().unwrap_int()
                    & BigInt::from(0xffffffffu32);
                Breakpoint::Pc(u32::try_from(addr).unwrap())
            }
        }
    }
}

//===========================================================================//
