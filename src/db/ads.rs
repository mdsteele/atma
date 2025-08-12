use super::binop::AdsBinOp;
use super::value::{AdsType, AdsValue};
use crate::db::SimEnv;
use crate::parse::{
    AdsModuleAst, AdsStmtAst, ExprAst, ExprAstNode, ParseError,
};
use std::collections::HashMap;
use std::io::Read;

//===========================================================================//

#[derive(Clone, Debug, Eq, PartialEq)]
enum AdsExprOp {
    BinOp(AdsBinOp),
    Id(String),
    Literal(AdsValue),
}

//===========================================================================//

/// An expression in an [AdsProgram].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AdsExpr {
    ops: Vec<AdsExprOp>,
}

impl AdsExpr {
    pub fn constant<T: Into<AdsValue>>(value: T) -> AdsExpr {
        AdsExpr { ops: vec![AdsExprOp::Literal(value.into())] }
    }

    pub fn typecheck(
        expr: &ExprAst,
        env: &AdsTypeEnv,
    ) -> Result<(AdsExpr, AdsType), Vec<ParseError>> {
        let subexpressions: Vec<&ExprAst> = {
            let mut stack = vec![expr];
            let mut subexprs = Vec::<&ExprAst>::new();
            while let Some(subexpr) = stack.pop() {
                subexprs.push(subexpr);
                match &subexpr.node {
                    ExprAstNode::BoolLiteral(_) => {}
                    ExprAstNode::Identifier(_) => {}
                    ExprAstNode::IntLiteral(_) => {}
                    ExprAstNode::BinOp(_, lhs, rhs) => {
                        stack.push(lhs);
                        stack.push(rhs);
                    }
                }
            }
            subexprs
        };
        let mut types = Vec::<AdsType>::new();
        let mut ops = Vec::<AdsExprOp>::new();
        let mut errors = Vec::<ParseError>::new();
        for subexpr in subexpressions.into_iter().rev() {
            match &subexpr.node {
                &ExprAstNode::BoolLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::Boolean(value)));
                    types.push(AdsType::Boolean);
                }
                ExprAstNode::Identifier(id) => {
                    if let Some(ty) = env.get_type(id) {
                        ops.push(AdsExprOp::Id(id.clone()));
                        types.push(ty.clone());
                    } else {
                        let span = subexpr.span;
                        let message = format!("No such identifier: {}", id);
                        errors.push(ParseError::new(span, message));
                        types.push(AdsType::Bottom);
                    }
                }
                ExprAstNode::IntLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::Integer(
                        value.clone(),
                    )));
                    types.push(AdsType::Integer);
                }
                ExprAstNode::BinOp(binop_ast, lhs_ast, rhs_ast) => {
                    debug_assert!(types.len() >= 2);
                    let rhs_type = types.pop().unwrap();
                    let lhs_type = types.pop().unwrap();
                    match AdsBinOp::typecheck(
                        *binop_ast,
                        lhs_ast.span,
                        lhs_type,
                        rhs_ast.span,
                        rhs_type,
                    ) {
                        Ok((binop, ty)) => {
                            ops.push(AdsExprOp::BinOp(binop));
                            types.push(ty);
                        }
                        Err(mut errs) => {
                            errors.append(&mut errs);
                            types.push(AdsType::Bottom);
                        }
                    }
                }
            }
        }
        debug_assert_eq!(types.len(), 1);
        if errors.is_empty() {
            Ok((AdsExpr { ops }, types.pop().unwrap()))
        } else {
            Err(errors)
        }
    }
}

//===========================================================================//

pub struct AdsTypeEnv {
    variables: HashMap<String, AdsType>,
}

impl AdsTypeEnv {
    fn empty() -> AdsTypeEnv {
        AdsTypeEnv { variables: HashMap::new() }
    }

    fn bind(&mut self, id: String, ty: AdsType) {
        self.variables.insert(id, ty);
    }

    fn get_type(&self, id: &str) -> Option<&AdsType> {
        self.variables.get(id)
    }
}

//===========================================================================//

enum AdsInstruction {
    BranchUnless(AdsExpr, isize),
    Exit,
    Let(String, AdsExpr),
    Jump(isize),
    Print(AdsExpr),
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
            AdsStmtAst::Print(expr) => {
                if let Some((expr, _)) = self.typecheck_expr(expr) {
                    instructions_out.push(AdsInstruction::Print(expr));
                }
            }
            AdsStmtAst::Let(id, expr) => {
                if let Some((expr, ty)) = self.typecheck_expr(expr) {
                    self.env.bind(id.name.clone(), ty);
                    instructions_out.push(AdsInstruction::Let(id.name, expr));
                } else {
                    self.env.bind(id.name.clone(), AdsType::Bottom);
                }
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
        }
    }

    fn typecheck_expr(&mut self, expr: ExprAst) -> Option<(AdsExpr, AdsType)> {
        match AdsExpr::typecheck(&expr, &self.env) {
            Ok((expr, ty)) => Some((expr, ty)),
            Err(mut errors) => {
                self.errors.append(&mut errors);
                None
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
    scope: AdsScope,
}

impl AdsEnvironment {
    /// Creates a new execution of the given program.
    pub fn new(sim: SimEnv, program: AdsProgram) -> AdsEnvironment {
        AdsEnvironment { sim, program, pc: 0, scope: AdsScope::empty() }
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
            AdsInstruction::Let(name, expr) => {
                self.scope.set_value(
                    name.clone(),
                    self.evaluate(expr)?,
                    &self.sim,
                );
            }
            AdsInstruction::Print(expr) => {
                println!("{}", self.evaluate(expr)?);
            }
        }
        self.pc += 1;
        Ok(false)
    }

    fn jump(&mut self, mut offset: isize) -> Result<bool, AdsRuntimeError> {
        offset += 1;
        if offset < 0 {
            let offset = (-offset) as usize;
            assert!(offset <= self.pc);
            self.pc -= offset;
        } else {
            self.pc += offset as usize;
        }
        Ok(false)
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

    pub fn set_value(&mut self, id: String, value: AdsValue, _: &SimEnv) {
        self.locals.insert(id, value);
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsExpr, AdsExprOp, AdsType, AdsTypeEnv, AdsValue};
    use crate::parse::{ExprAst, ExprAstNode, SrcSpan};
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
        env.bind("foo".to_string(), AdsType::Boolean);
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
