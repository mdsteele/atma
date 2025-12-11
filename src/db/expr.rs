use super::env::SimEnv;
use super::inst::{AdsFrameRef, AdsInstruction};
use crate::expr::{ExprCompiler, ExprEnv, ExprType, ExprValue};
use crate::parse::{ExprAst, IdentifierAst, ParseError, ParseResult, SrcSpan};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

#[derive(Clone)]
pub(crate) enum AdsDeclKind {
    Constant(Option<ExprValue>),
    Variable,
}

impl AdsDeclKind {
    fn static_value(&self) -> Option<ExprValue> {
        match self {
            AdsDeclKind::Constant(static_value) => static_value.clone(),
            AdsDeclKind::Variable => None,
        }
    }
}

//===========================================================================//

pub struct AdsDecl {
    pub kind: AdsDeclKind,
    pub id_span: SrcSpan,
    pub var_type: ExprType,
    pub stack_index: usize,
}

//===========================================================================//

struct AdsScope {
    /// The stack index, relative to the start of the call frame that this
    /// scope appears in, for the first variable in this scope.
    frame_start: usize,
    /// The variables currently declared in this scope.
    variables: HashMap<Rc<str>, AdsDecl>,
    /// The total number of handlers declared in this scope.
    num_handlers: usize,
    /// The total number of variables declared in this scope.  Note that this
    /// may be greater than `self.variables.len()`, due to shadowing.
    num_variables: usize,
}

impl AdsScope {
    fn with_start(frame_start: usize) -> AdsScope {
        AdsScope {
            frame_start,
            variables: HashMap::new(),
            num_handlers: 0,
            num_variables: 0,
        }
    }

    fn frame_end(&self) -> usize {
        self.frame_start + self.num_variables
    }

    fn get_declaration(&self, id: &str) -> Option<&AdsDecl> {
        self.variables.get(id)
    }

    fn add_declaration(
        &mut self,
        kind: AdsDeclKind,
        id: IdentifierAst,
        var_type: ExprType,
    ) {
        let id_span = id.span;
        let stack_index = self.frame_end();
        let decl = AdsDecl { kind, id_span, var_type, stack_index };
        self.variables.insert(id.name, decl);
        self.num_variables += 1;
    }

    fn add_handler(&mut self) {
        self.num_handlers += 1;
    }

    fn close(self, out: &mut Vec<AdsInstruction>) {
        for _ in 0..self.num_handlers {
            out.push(AdsInstruction::PopHandler);
        }
        for _ in 0..self.num_variables {
            out.push(AdsInstruction::PopValue);
        }
    }
}

//===========================================================================//

pub struct AdsTypeEnv<'a> {
    sim_env: &'a SimEnv,
    frames: Vec<Vec<AdsScope>>,
}

impl<'a> AdsTypeEnv<'a> {
    pub fn new(sim_env: &'a SimEnv) -> AdsTypeEnv<'a> {
        AdsTypeEnv { sim_env, frames: vec![vec![AdsScope::with_start(0)]] }
    }

    pub fn in_global_frame(&self) -> bool {
        debug_assert!(!self.frames.is_empty());
        self.frames.len() == 1
    }

    pub fn frame_end(&self) -> usize {
        debug_assert!(!self.frames.is_empty());
        let frame = self.frames.last().unwrap();
        debug_assert!(!frame.is_empty());
        frame.last().unwrap().frame_end()
    }

    pub fn push_frame(&mut self) {
        self.frames.push(vec![AdsScope::with_start(0)]);
    }

    pub fn pop_frame(&mut self, out: &mut Vec<AdsInstruction>) {
        debug_assert!(self.frames.len() >= 2);
        let mut frame = self.frames.pop().unwrap();
        debug_assert_eq!(frame.len(), 1);
        let scope = frame.pop().unwrap();
        scope.close(out);
    }

    pub fn push_scope(&mut self) {
        debug_assert!(!self.frames.is_empty());
        let frame = self.frames.last_mut().unwrap();
        debug_assert!(!frame.is_empty());
        let start = frame.last().unwrap().frame_end();
        frame.push(AdsScope::with_start(start));
    }

    /// Returns the number of handlers and variables in the popped scope.
    pub fn pop_scope(&mut self, out: &mut Vec<AdsInstruction>) {
        assert!(!self.frames.is_empty());
        let frame = self.frames.last_mut().unwrap();
        debug_assert!(frame.len() >= 2);
        let scope = frame.pop().unwrap();
        scope.close(out);
    }

    pub fn add_handler(&mut self) {
        assert!(!self.frames.is_empty());
        let frame = self.frames.last_mut().unwrap();
        debug_assert!(!frame.is_empty());
        let scope = frame.last_mut().unwrap();
        scope.add_handler();
    }

    pub fn add_declaration(
        &mut self,
        kind: AdsDeclKind,
        id: IdentifierAst,
        ty: ExprType,
    ) {
        assert!(!self.frames.is_empty());
        let frame = self.frames.last_mut().unwrap();
        debug_assert!(!frame.is_empty());
        let scope = frame.last_mut().unwrap();
        scope.add_declaration(kind, id, ty);
    }

    pub fn get_declaration(
        &self,
        id: &str,
    ) -> Option<(AdsFrameRef, &AdsDecl)> {
        let num_frames = self.frames.len();
        for (frame_index, frame) in self.frames.iter().rev().enumerate() {
            for scope in frame.iter().rev() {
                if let Some(decl) = scope.get_declaration(id) {
                    let frame_ref = if frame_index + 1 == num_frames {
                        AdsFrameRef::Global
                    } else {
                        AdsFrameRef::Local(frame_index)
                    };
                    return Some((frame_ref, decl));
                }
            }
        }
        None
    }

    pub fn register_names(&self) -> &'static [&'static str] {
        self.sim_env.register_names()
    }

    pub fn typecheck_expression(
        &self,
        expr: ExprAst,
    ) -> ParseResult<(Vec<AdsInstruction>, ExprType)> {
        let (ops, expr_type, _static_value) =
            ExprCompiler::new(self).typecheck(&expr)?;
        Ok((ops, expr_type))
    }
}

impl<'a> ExprEnv for AdsTypeEnv<'a> {
    type Op = AdsInstruction;

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        id: &str,
    ) -> ParseResult<(Self::Op, ExprType, Option<ExprValue>)> {
        if let Some((frame_ref, decl)) = self.get_declaration(id) {
            let op = AdsInstruction::GetValue(frame_ref, decl.stack_index);
            let expr_type = decl.var_type.clone();
            let static_value = decl.kind.static_value();
            return Ok((op, expr_type, static_value));
        }
        for &register in self.register_names() {
            if id.eq_ignore_ascii_case(register) {
                let op = AdsInstruction::GetRegister(register);
                return Ok((op, ExprType::Integer, None));
            }
        }
        if id.eq_ignore_ascii_case("PC") {
            return Ok((AdsInstruction::GetPc, ExprType::Integer, None));
        }
        let message = format!("No such identifier: `{id}`");
        let label = "this was never declared".to_string();
        Err(vec![ParseError::new(span, message).with_label(span, label)])
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{
        AdsDeclKind, AdsFrameRef, AdsInstruction, AdsTypeEnv, ExprType,
        ExprValue, SimEnv,
    };
    use crate::parse::{ExprAst, ExprAstNode, IdentifierAst, SrcSpan};
    use num_bigint::BigInt;
    use std::ops::Range;
    use std::rc::Rc;

    fn id_ast(name: &str, range: Range<usize>) -> ExprAst {
        ExprAst {
            span: SrcSpan::from_byte_range(range),
            node: ExprAstNode::Identifier(Rc::from(name)),
        }
    }

    fn int_ast(value: i32, range: Range<usize>) -> ExprAst {
        ExprAst {
            span: SrcSpan::from_byte_range(range),
            node: ExprAstNode::IntLiteral(BigInt::from(value)),
        }
    }

    fn int_value(value: i32) -> ExprValue {
        ExprValue::Integer(BigInt::from(value))
    }

    #[test]
    fn typecheck_identifier_expr() {
        let sim_env = SimEnv::with_nop_cpu();
        let mut env = AdsTypeEnv::new(&sim_env);
        env.add_declaration(
            AdsDeclKind::Constant(None),
            IdentifierAst {
                span: SrcSpan::from_byte_range(1..4),
                name: Rc::from("foo"),
            },
            ExprType::Boolean,
        );
        assert_eq!(
            env.typecheck_expression(id_ast("foo", 10..13)),
            Ok((
                vec![AdsInstruction::GetValue(AdsFrameRef::Global, 0)],
                ExprType::Boolean
            ))
        );
    }

    #[test]
    fn typecheck_int_literal_expr() {
        let sim_env = SimEnv::with_nop_cpu();
        let env = AdsTypeEnv::new(&sim_env);
        assert_eq!(
            env.typecheck_expression(int_ast(42, 0..2)),
            Ok((
                vec![AdsInstruction::PushValue(int_value(42))],
                ExprType::Integer,
            ))
        );
    }
}

//===========================================================================//
