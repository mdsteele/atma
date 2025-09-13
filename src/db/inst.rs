use super::binop::AdsBinOp;
use super::value::AdsValue;
use crate::bus::WatchKind;

//===========================================================================//

#[derive(Debug, Eq, PartialEq)]
pub enum AdsInstruction {
    /// Pops the top two values from the value stack, evaluates the specified
    /// binary operation using the second-from-the-top value as the left-hand
    /// side and the topmost value as the right-hand side, then pushes the
    /// result onto the value stack.
    BinOp(AdsBinOp),
    /// Pops the top value from the value stack (which must be a boolean).  If
    /// the value is false, adds the given offset to the ADS program counter.
    BranchUnless(isize),
    /// Exits the program.
    Exit,
    /// Pops the top value from the value stack (which must be a tuple), and
    /// pushes its elements onto the stack in order, such that the last element
    /// of the tuple ends up as the top element on the stack.
    ExpandTuple,
    /// Pushes the integer value of the simulated processsor's program counter
    /// onto the stack.
    GetPc,
    /// Pushes the integer value of the specified register in the simulated
    /// processsor onto the stack.
    GetRegister(&'static str),
    /// Copies the value at the specified offset from the start of the
    /// specified frame in the call stack, and pushes the copied value onto the
    /// stack.
    GetValue(AdsFrameRef, usize),
    /// Pops the top two values from the value stack, and uses the topmost
    /// value (which must be an integer) as an index into the
    /// second-from-the-top value (which must be a list), then pushes that list
    /// element back onto the stack. If the index value is out of range, an ADS
    /// runtime error will occur.
    ListIndex,
    /// Adds the given offset to the ADS program counter.
    Jump(isize),
    /// Pops the specified number of values from the value stack (which must
    /// all have the same type), packs them into a list (with the topmost value
    /// last), then pushes that list onto the value stack.
    MakeList(usize),
    /// Pops the specified number of values from the value stack, packs them
    /// into a tuple (with the topmost value last), then pushes that tuple onto
    /// the value stack.
    MakeTuple(usize),
    /// Pops a handler from the handler stack, and removes its breakpoint from
    /// the simulated processsor if it was the last handler for that
    /// breakpoint.
    PopHandler,
    /// Pops a value from the value stack and discards it.
    PopValue,
    /// Pops the top value from the value stack and prints it to stdout.
    Print,
    /// Pops the top value from the value stack and uses it as the argument for
    /// the specified breakpoint kind to set a breakpoint for the simulated
    /// processor, and pushes a new handler onto the handler stack for that
    /// breakpoint that will jumps to the ADS instruction relative to this one
    /// when the breakpoint is reached.
    PushHandler(WatchKind, isize),
    /// Pushes a value onto the value stack.
    PushValue(AdsValue),
    /// Returns from the current breakpoint handler.
    Return,
    /// Pops the top two values from the value stack (which must both be
    /// integers), then writes a byte to the simulated processor's memory bus,
    /// using the topmost value as the address and the second-from-the-top
    /// value as the data to write.
    SetMemory,
    /// Pops a value from the value stack (which must be an integer), and sets
    /// the simulated processsor's program counter to that value.
    SetPc,
    /// Pops a value from the value stack (which must be an integer), and sets
    /// the value of the specified register in the simulated processsor to the
    /// popped value.
    SetRegister(&'static str),
    /// Pops a value from the value stack, then sets the value at the specified
    /// offset from the start of the specified call frame to the popped value.
    SetValue(AdsFrameRef, usize),
    /// Advances the simulated processor by one instruction.
    Step,
    /// Pops the top value from the value stack (which must be a tuple), gets
    /// the specified item from that tuple, then pushes that item onto the
    /// value stack.
    TupleItem(usize),
}

//===========================================================================//

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AdsFrameRef {
    Global,       // outermost frame
    Local(usize), // 0 for innermost frame, 1 for next most inner, etc.
}

//===========================================================================//
