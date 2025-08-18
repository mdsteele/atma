use super::breakpoint::AdsBreakpoint;
use super::expr::{AdsExpr, AdsExprOp, AdsTypeEnv};
use super::value::{AdsType, AdsValue};
use crate::db::SimEnv;
use crate::parse::{
    AdsModuleAst, AdsStmtAst, BreakpointAst, DeclareAst, ExprAst, ParseError,
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
    /// Declares a new variable in the current scope (possibly shadowing an
    /// existing variable).
    Declare(String, AdsExpr),
    /// Exit the program.
    Exit,
    /// Adds the given offset to the ADS program counter.
    Jump(isize),
    /// Evaluates the expression and prints the result to stdout.
    Print(AdsExpr),
    /// Returns from the current breakpoint handler.
    Return,
    /// Updates an existing variable, in the current scope or an enclosing one.
    Set(String, AdsExpr),
    /// Advances the simulated processor by one instruction.
    Step,
    /// Sets a breakpoint for the simulated processor and jumps to the ADS
    /// instruction relative to this one whenever that breakpoint is reached.
    When(AdsBreakpoint, isize),
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
                let expr_span = expr_ast.span;
                let opt_expr = self.typecheck_expr(expr_ast);
                match self.env.get_declaration(&id.name) {
                    Some((DeclareAst::Var, decl_span, var_type)) => {
                        if let Some((expr, expr_type)) = opt_expr {
                            if expr_type == *var_type {
                                instructions_out
                                    .push(AdsInstruction::Set(id.name, expr));
                            } else {
                                let message = format!(
                                    "cannot assign {expr_type} value to \
                                     {var_type} variable `{}`",
                                    id.name
                                );
                                let label1 = format!(
                                    "this expression has type {expr_type}"
                                );
                                let label2 = format!(
                                    "`{}` was declared as {var_type} here",
                                    id.name
                                );
                                self.errors.push(
                                    ParseError::new(id.span, message)
                                        .with_label(expr_span, label1)
                                        .with_label(decl_span, label2),
                                );
                            }
                        }
                    }
                    Some((DeclareAst::Let, decl_span, _)) => {
                        let message = format!(
                            "cannot change value of constant `{}`",
                            id.name
                        );
                        let label1 =
                            format!("cannot set value of `{}`", id.name);
                        let label2 = format!(
                            "`{}` was declared as a constant here",
                            id.name
                        );
                        self.errors.push(
                            ParseError::new(id.span, message)
                                .with_label(id.span, label1)
                                .with_label(decl_span, label2),
                        );
                    }
                    None => {
                        let message = format!("no such variable: {}", id.name);
                        let label =
                            "this variable was never declared".to_string();
                        self.errors.push(
                            ParseError::new(id.span, message)
                                .with_label(id.span, label),
                        );
                    }
                }
            }
            AdsStmtAst::Declare(kind, id, expr) => {
                let ty = if let Some((expr, ty)) = self.typecheck_expr(expr) {
                    instructions_out
                        .push(AdsInstruction::Declare(id.name.clone(), expr));
                    ty
                } else {
                    AdsType::Bottom
                };
                self.env.declare(kind, id, ty);
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
                let mut then_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(then_ast, &mut then_stmts);
                let mut else_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(else_ast, &mut else_stmts);
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
            AdsStmtAst::When(breakpoint_ast, do_ast) => {
                let breakpoint = self.typecheck_breakpoint(breakpoint_ast);
                let mut do_stmts = Vec::<AdsInstruction>::new();
                self.typecheck_statements(do_ast, &mut do_stmts);
                do_stmts.push(AdsInstruction::Return);
                instructions_out.push(AdsInstruction::When(breakpoint, 1));
                instructions_out
                    .push(AdsInstruction::Jump(do_stmts.len() as isize));
                instructions_out.append(&mut do_stmts);
            }
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
    stack: Vec<usize>,
    scope: AdsScope,
    handlers: HashMap<Breakpoint, usize>,
}

impl AdsEnvironment {
    /// Creates a new execution of the given program.
    pub fn new(sim: SimEnv, program: AdsProgram) -> AdsEnvironment {
        AdsEnvironment {
            sim,
            program,
            pc: 0,
            scope: AdsScope::empty(),
            stack: Vec::new(),
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
            AdsInstruction::Declare(name, expr) => {
                self.scope.declare_value(
                    name.clone(),
                    self.evaluate(expr)?,
                    &self.sim,
                );
            }
            AdsInstruction::Print(expr) => {
                println!("{}", self.evaluate(expr)?);
            }
            AdsInstruction::Set(name, expr) => {
                self.scope.update_value(
                    name.clone(),
                    self.evaluate(expr)?,
                    &self.sim,
                );
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
                        self.stack.push(self.pc);
                        self.pc = *self.handlers.get(&breakpoint).unwrap();
                        return Ok(false);
                    }
                    Err(SimBreak::HaltOpcode(mnemonic, opcode)) => {
                        println!("Halted by {mnemonic} opcode ${opcode:02x}");
                        return Ok(true);
                    }
                }
            }
            AdsInstruction::Return => {
                self.pc = self.stack.pop().unwrap();
            }
            AdsInstruction::When(breakpoint, offset) => {
                let breakpoint = self.eval_breakpoint(breakpoint)?;
                self.handlers.insert(breakpoint, self.destination(*offset));
                self.sim.add_breakpoint(breakpoint);
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
                    assert!(stack.len() >= 2);
                    let rhs = stack.pop().unwrap();
                    let lhs = stack.pop().unwrap();
                    stack.push(binop.evaluate(lhs, rhs));
                }
                AdsExprOp::Id(name) => {
                    stack.push(self.scope.get_value(name, &self.sim));
                }
                AdsExprOp::Literal(value) => stack.push(value.clone()),
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

struct AdsScope {
    locals: HashMap<String, AdsValue>,
}

impl AdsScope {
    pub fn empty() -> AdsScope {
        AdsScope { locals: HashMap::new() }
    }

    pub fn get_value(&self, id: &str, _: &SimEnv) -> AdsValue {
        self.locals.get(id).unwrap().clone()
    }

    pub fn declare_value(&mut self, id: String, value: AdsValue, _: &SimEnv) {
        self.locals.insert(id, value);
    }

    pub fn update_value(&mut self, id: String, value: AdsValue, _: &SimEnv) {
        // TODO: if the variable doesn't exist in this scope, check enclosing
        // scopes.
        self.locals.insert(id, value);
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsExpr, AdsExprOp, AdsType, AdsTypeEnv, AdsValue};
    use crate::parse::{
        DeclareAst, ExprAst, ExprAstNode, IdentifierAst, SrcSpan,
    };
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
        env.declare(
            DeclareAst::Let,
            IdentifierAst {
                span: SrcSpan::from_byte_range(1..4),
                name: "foo".to_string(),
            },
            AdsType::Boolean,
        );
        assert_eq!(
            AdsExpr::typecheck(&id_ast("foo", 10..13), &env),
            Ok((
                expr(vec![AdsExprOp::Id("foo".to_string())]),
                AdsType::Boolean,
            ))
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
