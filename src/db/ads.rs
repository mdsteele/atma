use super::breakpoint::AdsBreakpoint;
use super::expr::{AdsDeclarationKind, AdsExpr, AdsExprOp, AdsTypeEnv};
use super::value::{AdsType, AdsValue};
use crate::db::SimEnv;
use crate::parse::{
    AdsModuleAst, AdsStmtAst, BreakpointAst, DeclareAst, ExprAst,
    IdentifierAst, ParseError,
};
use crate::proc::{Breakpoint, SimBreak};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::io::Read;

//===========================================================================//

enum AdsInstruction {
    /// Evaluates the boolean expression, and adds the given offset to the ADS
    /// program counter if the result is false.
    BranchUnless(AdsExpr, isize),
    /// Exit the program.
    Exit,
    /// Adds the given offset to the ADS program counter.
    Jump(isize),
    /// Pops a handler from the handler stack and removes its breakpoint.
    PopHandler,
    /// Pops a variable from the variable stack.
    PopVar,
    /// Evaluates the expression and prints the result to stdout.
    Print(AdsExpr),
    /// Pushes a new handler onto the handler stack, and sets a breakpoint for
    /// the simulated processor and jumps to the ADS instruction relative to
    /// this one whenever that breakpoint is reached.
    PushHandler(AdsBreakpoint, isize),
    /// Pushes a new variable onto the variable stack.
    PushVar(AdsExpr),
    /// Returns from the current breakpoint handler.
    Return,
    /// Updates an existing variable on the variable stack.
    SetVar(usize, AdsExpr),
    /// Advances the simulated processor by one instruction.
    Step,
}

//===========================================================================//

struct AdsCompiler {
    env: AdsTypeEnv,
    errors: Vec<ParseError>,
}

impl AdsCompiler {
    fn new() -> AdsCompiler {
        AdsCompiler { env: AdsTypeEnv::empty(), errors: Vec::new() }
    }

    fn into_errors(self) -> Vec<ParseError> {
        self.errors
    }

    fn typecheck_statements(
        &mut self,
        statements: Vec<AdsStmtAst>,
        instructions_out: &mut Vec<AdsInstruction>,
    ) {
        for statement in statements {
            self.typecheck_statement(statement, instructions_out);
        }
    }

    fn typecheck_statement(
        &mut self,
        statement: AdsStmtAst,
        instructions_out: &mut Vec<AdsInstruction>,
    ) {
        match statement {
            AdsStmtAst::Relax => {}
            AdsStmtAst::Exit => instructions_out.push(AdsInstruction::Exit),
            AdsStmtAst::Step => instructions_out.push(AdsInstruction::Step),
            AdsStmtAst::Print(expr) => {
                if let Some((expr, _)) = self.typecheck_expr(expr) {
                    instructions_out.push(AdsInstruction::Print(expr));
                }
            }
            AdsStmtAst::Set(id, expr_ast) => {
                self.typecheck_set_statement(id, expr_ast, instructions_out);
            }
            AdsStmtAst::Declare(kind, id, expr) => {
                let (expr_type, static_value) =
                    if let Some((expr, ty)) = self.typecheck_expr(expr) {
                        let static_value = expr.static_value().cloned();
                        instructions_out.push(AdsInstruction::PushVar(expr));
                        (ty, static_value)
                    } else {
                        (AdsType::Bottom, None)
                    };
                let kind = match kind {
                    DeclareAst::Let => {
                        AdsDeclarationKind::Constant(static_value)
                    }
                    DeclareAst::Var => AdsDeclarationKind::Variable,
                };
                self.env.add_declaration(kind, id, expr_type);
            }
            AdsStmtAst::If(pred_ast, then_ast, else_ast) => {
                let pred_span = pred_ast.span;
                let pred_expr = match self.typecheck_expr(pred_ast) {
                    Some((expr, AdsType::Boolean)) => expr,
                    Some((_, ty)) => {
                        let message = format!(
                            "predicate must be of type bool, not {ty}"
                        );
                        self.errors.push(
                            ParseError::new(pred_span, message).with_label(
                                pred_span,
                                format!("this expression has type {ty}"),
                            ),
                        );
                        AdsExpr::constant(false)
                    }
                    None => AdsExpr::constant(false),
                };
                self.push_scope();
                let mut then_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(then_ast, &mut then_stmts);
                self.pop_scope(&mut then_stmts);
                self.push_scope();
                let mut else_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(else_ast, &mut else_stmts);
                self.pop_scope(&mut else_stmts);
                if !else_stmts.is_empty() {
                    then_stmts
                        .push(AdsInstruction::Jump(else_stmts.len() as isize));
                }
                instructions_out.push(AdsInstruction::BranchUnless(
                    pred_expr,
                    then_stmts.len() as isize,
                ));
                instructions_out.append(&mut then_stmts);
                instructions_out.append(&mut else_stmts);
            }
            AdsStmtAst::RunUntil(breakpoint_ast) => {
                let breakpoint = self.typecheck_breakpoint(breakpoint_ast);
                let index = self.env.variable_stack_size();
                instructions_out
                    .push(AdsInstruction::PushVar(AdsExpr::constant(false)));
                instructions_out
                    .push(AdsInstruction::PushHandler(breakpoint, 1));
                instructions_out.push(AdsInstruction::Jump(2));
                instructions_out.push(AdsInstruction::SetVar(
                    index,
                    AdsExpr::constant(true),
                ));
                instructions_out.push(AdsInstruction::Return);
                instructions_out.push(AdsInstruction::Step);
                instructions_out.push(AdsInstruction::BranchUnless(
                    AdsExpr { ops: vec![AdsExprOp::Variable(index)] },
                    -2,
                ));
                instructions_out.push(AdsInstruction::PopHandler);
                instructions_out.push(AdsInstruction::PopVar);
            }
            AdsStmtAst::When(breakpoint_ast, do_ast) => {
                let breakpoint = self.typecheck_breakpoint(breakpoint_ast);
                self.push_scope();
                let mut do_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(do_ast, &mut do_stmts);
                self.pop_scope(&mut do_stmts);
                do_stmts.push(AdsInstruction::Return);
                self.env.add_handler();
                instructions_out
                    .push(AdsInstruction::PushHandler(breakpoint, 1));
                instructions_out
                    .push(AdsInstruction::Jump(do_stmts.len() as isize));
                instructions_out.append(&mut do_stmts);
            }
        }
    }

    fn typecheck_set_statement(
        &mut self,
        id: IdentifierAst,
        expr_ast: ExprAst,
        instructions_out: &mut Vec<AdsInstruction>,
    ) {
        let expr_span = expr_ast.span;
        let opt_expr = self.typecheck_expr(expr_ast);
        if let Some(decl) = self.env.get_declaration(&id.name) {
            match decl.kind {
                AdsDeclarationKind::Variable => {
                    if let Some((expr, expr_type)) = opt_expr {
                        if expr_type == decl.var_type {
                            instructions_out.push(AdsInstruction::SetVar(
                                decl.stack_index,
                                expr,
                            ));
                        } else {
                            let message = format!(
                                "cannot assign {expr_type} value to {} \
                                 variable `{}`",
                                decl.var_type, id.name
                            );
                            let label1 = format!(
                                "this expression has type {expr_type}"
                            );
                            let label2 = format!(
                                "`{}` was declared as {} here",
                                id.name, decl.var_type
                            );
                            self.errors.push(
                                ParseError::new(id.span, message)
                                    .with_label(expr_span, label1)
                                    .with_label(decl.id_span, label2),
                            );
                        }
                    }
                }
                AdsDeclarationKind::Constant(_) => {
                    let message = format!(
                        "cannot change value of constant `{}`",
                        id.name
                    );
                    let label1 = format!("cannot set value of `{}`", id.name);
                    let label2 = format!(
                        "`{}` was declared as a constant here",
                        id.name
                    );
                    self.errors.push(
                        ParseError::new(id.span, message)
                            .with_label(id.span, label1)
                            .with_label(decl.id_span, label2),
                    );
                }
            }
        } else {
            let message = format!("no such variable: `{}`", id.name);
            let label = "this was never declared".to_string();
            self.errors.push(
                ParseError::new(id.span, message).with_label(id.span, label),
            );
        }
    }

    fn typecheck_expr(&mut self, ast: ExprAst) -> Option<(AdsExpr, AdsType)> {
        match AdsExpr::typecheck(&ast, &self.env) {
            Ok((expr, ty)) => Some((expr, ty)),
            Err(mut errors) => {
                self.errors.append(&mut errors);
                None
            }
        }
    }

    fn typecheck_breakpoint(&mut self, ast: BreakpointAst) -> AdsBreakpoint {
        match AdsBreakpoint::typecheck(&ast, &self.env) {
            Ok(breakpoint) => breakpoint,
            Err(mut errors) => {
                self.errors.append(&mut errors);
                AdsBreakpoint::Pc(AdsExpr::constant(false))
            }
        }
    }

    fn push_scope(&mut self) {
        self.env.push_scope();
    }

    fn pop_scope(&mut self, instructions_out: &mut Vec<AdsInstruction>) {
        let (num_handlers, num_variables) = self.env.pop_scope();
        for _ in 0..num_handlers {
            instructions_out.push(AdsInstruction::PopHandler);
        }
        for _ in 0..num_variables {
            instructions_out.push(AdsInstruction::PopVar);
        }
    }
}

//===========================================================================//

/// An executable Atma Debugger Script program.
pub struct AdsProgram {
    instructions: Vec<AdsInstruction>,
}

impl AdsProgram {
    /// Reads an Atma Debugger Script program from a file.
    pub fn read_from<R: Read>(
        reader: R,
    ) -> Result<AdsProgram, Vec<ParseError>> {
        AdsProgram::typecheck(AdsModuleAst::read_from(reader)?)
    }

    /// Distills the abstract syntax tree for an Atma Debugger Script module
    /// into a program that can be executed.
    pub fn typecheck(
        module: AdsModuleAst,
    ) -> Result<AdsProgram, Vec<ParseError>> {
        let mut compiler = AdsCompiler::new();
        let mut instructions = Vec::<AdsInstruction>::new();
        compiler.typecheck_statements(module.statements, &mut instructions);
        let errors = compiler.into_errors();
        if errors.is_empty() {
            Ok(AdsProgram { instructions })
        } else {
            Err(errors)
        }
    }
}

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
            AdsInstruction::BranchUnless(predicate, offset) => {
                if !self.evaluate(predicate)?.unwrap_bool() {
                    return self.jump(*offset);
                }
            }
            AdsInstruction::Exit => return Ok(true),
            AdsInstruction::Jump(offset) => {
                return self.jump(*offset);
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
            AdsInstruction::PopVar => {
                debug_assert!(!self.variable_stack.is_empty());
                self.variable_stack.pop();
            }
            AdsInstruction::Print(expr) => {
                println!("{}", self.evaluate(expr)?);
            }
            AdsInstruction::PushHandler(breakpoint, offset) => {
                let breakpoint = self.eval_breakpoint(breakpoint)?;
                let destination = self.destination(*offset);
                self.handlers.entry(breakpoint).or_default().push(destination);
                self.sim.add_breakpoint(breakpoint);
                self.handler_stack.push(breakpoint);
            }
            AdsInstruction::PushVar(expr) => {
                self.variable_stack.push(self.evaluate(expr)?);
            }
            AdsInstruction::SetVar(index, expr) => {
                self.variable_stack[*index] = self.evaluate(expr)?;
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

    fn evaluate(&self, expr: &AdsExpr) -> Result<AdsValue, AdsRuntimeError> {
        let mut stack = Vec::<AdsValue>::new();
        for op in &expr.ops {
            match op {
                AdsExprOp::BinOp(binop) => {
                    debug_assert!(stack.len() >= 2);
                    let rhs = stack.pop().unwrap();
                    let lhs = stack.pop().unwrap();
                    stack.push(binop.evaluate(lhs, rhs));
                }
                AdsExprOp::ListIndex => {
                    debug_assert!(stack.len() >= 2);
                    let rhs = stack.pop().unwrap().unwrap_int();
                    let lhs = stack.pop().unwrap().unwrap_list();
                    if rhs < BigInt::ZERO {
                        return Err(AdsRuntimeError {}); // TODO message
                    }
                    if rhs >= BigInt::from(lhs.len()) {
                        return Err(AdsRuntimeError {}); // TODO message
                    }
                    stack.push(lhs[usize::try_from(rhs).unwrap()].clone());
                }
                AdsExprOp::Literal(value) => stack.push(value.clone()),
                &AdsExprOp::MakeList(num_items) => {
                    debug_assert!(stack.len() >= num_items);
                    let items = stack.split_off(stack.len() - num_items);
                    stack.push(AdsValue::List(items));
                }
                &AdsExprOp::MakeTuple(num_items) => {
                    debug_assert!(stack.len() >= num_items);
                    let items = stack.split_off(stack.len() - num_items);
                    stack.push(AdsValue::Tuple(items));
                }
                &AdsExprOp::TupleItem(index) => {
                    debug_assert!(!stack.is_empty());
                    let items = stack.pop().unwrap().unwrap_tuple();
                    debug_assert!(index < items.len());
                    stack.push(items[index].clone());
                }
                AdsExprOp::Variable(index) => {
                    stack.push(self.variable_stack[*index].clone());
                }
            }
        }
        assert_eq!(stack.len(), 1);
        Ok(stack.pop().unwrap())
    }

    fn eval_breakpoint(
        &self,
        breakpoint: &AdsBreakpoint,
    ) -> Result<Breakpoint, AdsRuntimeError> {
        match breakpoint {
            AdsBreakpoint::Pc(expr) => {
                let addr = self.evaluate(expr)?.unwrap_int()
                    & BigInt::from(0xffffffffu32);
                Ok(Breakpoint::Pc(addr.try_into().unwrap()))
            }
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{
        AdsDeclarationKind, AdsExpr, AdsExprOp, AdsType, AdsTypeEnv, AdsValue,
    };
    use crate::parse::{ExprAst, ExprAstNode, IdentifierAst, SrcSpan};
    use num_bigint::BigInt;
    use std::ops::Range;

    fn expr(ops: Vec<AdsExprOp>) -> AdsExpr {
        AdsExpr { ops }
    }

    fn id_ast(name: &str, range: Range<usize>) -> ExprAst {
        ExprAst {
            span: SrcSpan::from_byte_range(range),
            node: ExprAstNode::Identifier(name.to_string()),
        }
    }

    fn int_ast(value: i32, range: Range<usize>) -> ExprAst {
        ExprAst {
            span: SrcSpan::from_byte_range(range),
            node: ExprAstNode::IntLiteral(BigInt::from(value)),
        }
    }

    fn int_value(value: i32) -> AdsValue {
        AdsValue::Integer(BigInt::from(value))
    }

    #[test]
    fn typecheck_identifier_expr() {
        let mut env = AdsTypeEnv::empty();
        env.add_declaration(
            AdsDeclarationKind::Constant(None),
            IdentifierAst {
                span: SrcSpan::from_byte_range(1..4),
                name: "foo".to_string(),
            },
            AdsType::Boolean,
        );
        assert_eq!(
            AdsExpr::typecheck(&id_ast("foo", 10..13), &env),
            Ok((expr(vec![AdsExprOp::Variable(0)]), AdsType::Boolean))
        );
    }

    #[test]
    fn typecheck_int_literal_expr() {
        let env = AdsTypeEnv::empty();
        assert_eq!(
            AdsExpr::typecheck(&int_ast(42, 0..2), &env),
            Ok((
                expr(vec![AdsExprOp::Literal(int_value(42))]),
                AdsType::Integer,
            ))
        );
    }
}

//===========================================================================//
