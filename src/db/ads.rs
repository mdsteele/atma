use super::inst::{AdsBreakpointKind, AdsFrameRef, AdsInstruction};
use super::prog::AdsProgram;
use super::value::AdsValue;
use crate::db::SimEnv;
use crate::parse::ParseError;
use crate::proc::{Breakpoint, SimBreak};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::io::Write;

//===========================================================================//

/// An error that can occur while executing an Atma Debugger Script program.
#[derive(Debug)]
pub struct AdsRuntimeError {}

//===========================================================================//

struct CallFrame {
    /// The index into the call stack for the frame that lexically encloses
    /// this handler's declaration, or `None` if this call is to a handler that
    /// was declared in the global frame.
    parent_index: Option<usize>,
    /// The index into the value stack for the first local variable native to
    /// this call frame.
    frame_start: usize,
    /// The ADS program counter for the first instruction that should be
    /// executed after returning from this handler call.
    return_address: usize,
}

//===========================================================================//

struct HandlerData {
    /// The index into the call stack for the frame that lexically encloses
    /// this handler's declaration, or `None` if this handler was declared in
    /// the global frame.
    parent_index: Option<usize>,
    /// The ADS program counter for the first instruction that should be
    /// executed when the handler is invoked.
    destination_address: usize,
}

//===========================================================================//

/// An in-progress execution of an Atma Debugger Script program.
pub struct AdsEnvironment<W> {
    sim: SimEnv,
    program: AdsProgram,
    pc: usize,
    value_stack: Vec<AdsValue>,
    call_stack: Vec<CallFrame>,
    handler_stack: Vec<Breakpoint>,
    handlers: HashMap<Breakpoint, Vec<HandlerData>>,
    output: W,
}

impl<W: Write> AdsEnvironment<W> {
    /// Creates a new execution of the given program.
    pub fn create(
        source: &str,
        sim: SimEnv,
        output: W,
    ) -> Result<AdsEnvironment<W>, Vec<ParseError>> {
        let program = AdsProgram::compile_source(source, &sim)?;
        Ok(AdsEnvironment {
            sim,
            program,
            pc: 0,
            value_stack: Vec::new(),
            call_stack: Vec::new(),
            handler_stack: Vec::new(),
            handlers: HashMap::new(),
            output,
        })
    }

    /// Executes the next instruction in the program.
    pub fn step(&mut self) -> Result<bool, AdsRuntimeError> {
        match &self.program.instructions[self.pc] {
            AdsInstruction::BinOp(binop) => {
                debug_assert!(self.value_stack.len() >= 2);
                let rhs = self.value_stack.pop().unwrap();
                let lhs = self.value_stack.pop().unwrap();
                self.value_stack.push(binop.evaluate(lhs, rhs));
            }
            AdsInstruction::BranchUnless(offset) => {
                if !self.value_stack.pop().unwrap().unwrap_bool() {
                    return self.jump(*offset);
                }
            }
            AdsInstruction::Exit => return Ok(true),
            AdsInstruction::GetPc => {
                let value = self.sim.pc();
                self.value_stack.push(AdsValue::Integer(BigInt::from(value)));
            }
            AdsInstruction::GetRegister(name) => {
                let value = self.sim.get_register(name).unwrap();
                self.value_stack.push(AdsValue::Integer(BigInt::from(value)));
            }
            &AdsInstruction::GetValue(frame_ref, index) => {
                let start = self.frame_start(frame_ref);
                let value = self.value_stack[start + index].clone();
                self.value_stack.push(value);
            }
            AdsInstruction::Jump(offset) => {
                return self.jump(*offset);
            }
            AdsInstruction::ListIndex => {
                debug_assert!(self.value_stack.len() >= 2);
                let rhs = self.value_stack.pop().unwrap().unwrap_int();
                let lhs = self.value_stack.pop().unwrap().unwrap_list();
                if rhs < BigInt::ZERO {
                    return Err(AdsRuntimeError {}); // TODO message
                }
                if rhs >= BigInt::from(lhs.len()) {
                    return Err(AdsRuntimeError {}); // TODO message
                }
                let item = lhs[usize::try_from(rhs).unwrap()].clone();
                self.value_stack.push(item);
            }
            &AdsInstruction::MakeList(num_items) => {
                debug_assert!(self.value_stack.len() >= num_items);
                let start = self.value_stack.len() - num_items;
                let items = self.value_stack.split_off(start);
                self.value_stack.push(AdsValue::List(items));
            }
            &AdsInstruction::MakeTuple(num_items) => {
                debug_assert!(self.value_stack.len() >= num_items);
                let start = self.value_stack.len() - num_items;
                let items = self.value_stack.split_off(start);
                self.value_stack.push(AdsValue::Tuple(items));
            }
            AdsInstruction::PopHandler => {
                debug_assert!(!self.handler_stack.is_empty());
                let breakpoint = self.handler_stack.pop().unwrap();
                debug_assert!(self.handlers.contains_key(&breakpoint));
                let handlers = self.handlers.get_mut(&breakpoint).unwrap();
                debug_assert!(!handlers.is_empty());
                handlers.pop();
                if handlers.is_empty() {
                    self.handlers.remove(&breakpoint);
                    self.sim.remove_breakpoint(breakpoint);
                }
            }
            AdsInstruction::PopValue => {
                debug_assert!(!self.value_stack.is_empty());
                self.value_stack.pop();
            }
            AdsInstruction::Print => {
                let value = self.value_stack.pop().unwrap();
                // TODO: Report IO error via AdsRuntimeError.
                writeln!(self.output, "{}", value)
                    .map_err(|_| AdsRuntimeError {})?;
            }
            &AdsInstruction::PushHandler(kind, offset) => {
                let parent_index = if self.call_stack.is_empty() {
                    None
                } else {
                    Some(self.call_stack.len() - 1)
                };
                let destination_address = self.destination(offset);
                let data = HandlerData { parent_index, destination_address };
                let breakpoint = self.create_breakpoint(kind);
                self.handlers.entry(breakpoint).or_default().push(data);
                self.sim.add_breakpoint(breakpoint);
                self.handler_stack.push(breakpoint);
            }
            AdsInstruction::PushValue(value) => {
                self.value_stack.push(value.clone());
            }
            &AdsInstruction::SetRegister(name) => {
                let value = self.pop_address_value();
                self.sim.set_register(name, value);
            }
            AdsInstruction::SetMemory => {
                let addr = self.pop_address_value();
                let data = self.value_stack.pop().unwrap().unwrap_int();
                let data = u8::try_from(data & BigInt::from(0xff)).unwrap();
                self.sim.write_byte(addr, data);
            }
            AdsInstruction::SetPc => {
                let addr = self.pop_address_value();
                self.sim.set_pc(addr);
            }
            &AdsInstruction::SetValue(frame_ref, index) => {
                let start = self.frame_start(frame_ref);
                let value = self.value_stack.pop().unwrap();
                self.value_stack[start + index] = value;
            }
            AdsInstruction::Step => {
                let pc = self.sim.pc();
                let instruction = self.sim.disassemble(self.sim.pc()).1;
                let result = self.sim.step();
                eprintln!("${:04x} | {:16}", pc, instruction);
                match result {
                    Ok(()) => {}
                    Err(SimBreak::Breakpoint(breakpoint)) => {
                        eprintln!("Breakpoint: {breakpoint:?}");
                        debug_assert!(self.handlers.contains_key(&breakpoint));
                        let handlers = self.handlers.get(&breakpoint).unwrap();
                        debug_assert!(!handlers.is_empty());
                        let data = handlers.last().unwrap();
                        self.call_stack.push(CallFrame {
                            parent_index: data.parent_index,
                            frame_start: self.value_stack.len(),
                            return_address: self.pc + 1,
                        });
                        self.pc = data.destination_address;
                        return Ok(false);
                    }
                    Err(SimBreak::HaltOpcode(mnemonic, opcode)) => {
                        eprintln!("Halted by {mnemonic} opcode ${opcode:02x}");
                        return Ok(true);
                    }
                }
            }
            AdsInstruction::Return => {
                self.pc = self.call_stack.pop().unwrap().return_address;
                return Ok(false);
            }
            &AdsInstruction::TupleItem(index) => {
                let items = self.value_stack.pop().unwrap().unwrap_tuple();
                debug_assert!(index < items.len());
                self.value_stack.push(items[index].clone());
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

    fn pop_address_value(&mut self) -> u32 {
        let int_value = self.value_stack.pop().unwrap().unwrap_int();
        u32::try_from(int_value & BigInt::from(0xffffffffu32)).unwrap()
    }

    fn create_breakpoint(&mut self, kind: AdsBreakpointKind) -> Breakpoint {
        match kind {
            AdsBreakpointKind::Pc => Breakpoint::Pc(self.pop_address_value()),
        }
    }

    fn frame_start(&self, frame_ref: AdsFrameRef) -> usize {
        match frame_ref {
            AdsFrameRef::Global => 0,
            AdsFrameRef::Local(mut depth) => {
                debug_assert!(!self.call_stack.is_empty());
                let mut frame_index = self.call_stack.len() - 1;
                while depth > 0 {
                    frame_index =
                        self.call_stack[frame_index].parent_index.unwrap();
                    depth -= 1;
                }
                self.call_stack[frame_index].frame_start
            }
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsEnvironment, SimEnv};
    use std::io::Write;

    fn make_env<'a>(
        source: &str,
        output: &'a mut Vec<u8>,
    ) -> AdsEnvironment<&'a mut Vec<u8>> {
        let sim = SimEnv::with_nop_cpu();
        AdsEnvironment::create(source, sim, output).unwrap()
    }

    fn run_to_completion<W: Write>(env: &mut AdsEnvironment<W>) {
        loop {
            match env.step() {
                Ok(true) => return,
                Ok(false) => {}
                Err(err) => panic!("{err:?}"),
            }
        }
    }

    #[test]
    fn empty_program_finishes_in_one_step() {
        let mut output = Vec::<u8>::new();
        let mut ads = make_env("", &mut output);
        assert!(matches!(ads.step(), Ok(true)));
        assert_eq!(String::from_utf8(output).unwrap(), "");
    }

    #[test]
    fn print_statement() {
        let mut output = Vec::<u8>::new();
        let mut ads = make_env("print 42\n", &mut output);
        run_to_completion(&mut ads);
        assert_eq!(String::from_utf8(output).unwrap(), "42\n");
    }

    #[test]
    fn when_handler() {
        let source = "\
          when at $0001 {\n\
            print 2\n\
          }\n\
          print 1\n\
          step\n\
          print 3\n";
        let mut output = Vec::<u8>::new();
        let mut ads = make_env(source, &mut output);
        run_to_completion(&mut ads);
        assert_eq!(String::from_utf8(output).unwrap(), "1\n2\n3\n");
    }

    #[test]
    fn when_handler_with_local_variable() {
        let source = "\
          var x = 1
          when at $0001 {\n\
            var y = 2
            print x\n\
            print y\n\
          }\n\
          var z = 3\n\
          step\n\
          print z\n";
        let mut output = Vec::<u8>::new();
        let mut ads = make_env(source, &mut output);
        run_to_completion(&mut ads);
        assert_eq!(String::from_utf8(output).unwrap(), "1\n2\n3\n");
    }

    #[test]
    fn run_until_statement() {
        let source = "\
          var x = 1
          when at $0010 {\n\
            set x = 2
          }\n\
          run until at $0020
          print x\n";
        let mut output = Vec::<u8>::new();
        let mut ads = make_env(source, &mut output);
        run_to_completion(&mut ads);
        assert_eq!(String::from_utf8(output).unwrap(), "2\n");
    }

    #[test]
    fn nested_handlers() {
        let source = "\
          when at $0010 {\n\
            print 1
            var x = 2\n
            when at $0020 {\n\
              print x
              set x = 3\n
            }\n
            run until at $0030
            print x
          }\n\
          run until at $0040
          print 4\n";
        let mut output = Vec::<u8>::new();
        let mut ads = make_env(source, &mut output);
        run_to_completion(&mut ads);
        assert_eq!(String::from_utf8(output).unwrap(), "1\n2\n3\n4\n");
    }

    #[test]
    fn get_and_set_pc() {
        // Unlike user-defined variables, "PC" is case-insensitive.
        let source = "\
          run until at $0010\n\
          print pc\n\
          set Pc = $0020\n\
          print pC\n";
        let mut output = Vec::<u8>::new();
        let mut ads = make_env(source, &mut output);
        run_to_completion(&mut ads);
        assert_eq!(String::from_utf8(output).unwrap(), "16\n32\n");
    }
}

//===========================================================================//
