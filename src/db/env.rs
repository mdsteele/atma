use super::error::{AdsError, AdsResult, AdsSrcContext};
use super::inst::{AdsFrameRef, AdsInstruction};
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprNotStaticReason, ExprStatic,
    ExprType, ExprTypeError, ExprTypeResult, ExprUnOp, ExprValue,
    make_global_builtin_values,
};
use crate::parse::AdsModuleAst;
use crate::parse::{ExprAst, IdentifierAst};
use crate::system::SimSystem;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) enum AdsDeclKind {
    Constant(ExprStatic),
    Variable,
}

//===========================================================================//

pub(super) struct AdsDecl {
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
    /// The name of the currently-active processor for this scope.
    proc_name: Rc<str>,
    /// The variables currently declared in this scope.
    variables: HashMap<Rc<str>, AdsDecl>,
    /// The total number of handlers declared in this scope.
    num_handlers: usize,
    /// The total number of variables declared in this scope.  Note that this
    /// may be greater than `self.variables.len()`, due to shadowed variables
    /// and/or internal variables.
    num_variables: usize,
}

impl AdsScope {
    fn new(frame_start: usize, proc_name: Rc<str>) -> AdsScope {
        AdsScope {
            frame_start,
            proc_name,
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

    fn add_internal_variable(&mut self) -> usize {
        let index = self.frame_end();
        self.num_variables += 1;
        index
    }

    fn add_handler(&mut self) {
        self.num_handlers += 1;
    }
}

//===========================================================================//

pub(super) struct AdsTypeEnv<'a> {
    system: &'a SimSystem,
    builtins: HashMap<Rc<str>, (ExprValue, ExprType)>,
    context_stack: Vec<Rc<AdsSrcContext>>,
    frame_stack: Vec<Vec<AdsScope>>,
}

impl<'a> AdsTypeEnv<'a> {
    pub fn new(system: &'a SimSystem, root_path: Rc<str>) -> AdsTypeEnv<'a> {
        let builtins = make_global_builtin_values();
        let root_context = Rc::new(AdsSrcContext::root(root_path));
        let default_proc_name = system.selected_processor_name();
        AdsTypeEnv {
            system,
            builtins,
            context_stack: vec![root_context],
            frame_stack: vec![vec![AdsScope::new(0, default_proc_name)]],
        }
    }

    fn current_scope(&self) -> &AdsScope {
        self.frame_stack.last().unwrap().last().unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut AdsScope {
        self.frame_stack.last_mut().unwrap().last_mut().unwrap()
    }

    pub fn current_src_context(&self) -> Rc<AdsSrcContext> {
        self.context_stack.last().unwrap().clone()
    }

    pub fn push_src_context(&mut self, context: Rc<AdsSrcContext>) {
        self.context_stack.push(context);
    }

    pub fn pop_src_context(&mut self) {
        debug_assert!(self.context_stack.len() >= 2);
        self.context_stack.pop();
    }

    pub fn parse_source(&self, source_code: &str) -> AdsResult<AdsModuleAst> {
        AdsModuleAst::parse_source(source_code).map_err(|errs| {
            let context = self.current_src_context();
            errs.map(|error| AdsError::ParseError {
                context: context.clone(),
                error,
            })
        })
    }

    /// Begins a new handler frame.
    ///
    /// The new frame will automatically include its own root scope, which will
    /// automatically be closed by the corresponding call to `pop_frame`.
    pub fn push_frame(&mut self) {
        let enclosing_frame = self.frame_stack.last_mut().unwrap();
        let enclosing_scope = enclosing_frame.last().unwrap();
        let proc_name = enclosing_scope.proc_name.clone();
        self.frame_stack.push(vec![AdsScope::new(0, proc_name)]);
    }

    /// Closes the current handler frame, adding any instructions necessary to
    /// close out its root scope.
    ///
    /// This method should always be called in conjunction with `push_frame`.
    /// It is an error to pop the global frame.
    pub fn pop_frame(&mut self, out: &mut Vec<AdsInstruction>) {
        debug_assert!(self.frame_stack.len() >= 2);
        let mut frame = self.frame_stack.pop().unwrap();
        debug_assert_eq!(frame.len(), 1);
        let scope = frame.pop().unwrap();
        self.close_scope(scope, out);
    }

    /// Begins a new scope in the current handler frame (or global frame).
    ///
    /// The new scope will inherit the current processor of the enclosing
    /// scope.
    pub fn push_scope(&mut self) {
        let enclosing_frame = self.frame_stack.last_mut().unwrap();
        let enclosing_scope = enclosing_frame.last().unwrap();
        let proc_name = enclosing_scope.proc_name.clone();
        let start = enclosing_scope.frame_end();
        enclosing_frame.push(AdsScope::new(start, proc_name));
    }

    /// Closes the current scope, adding any instructions necessary to close
    /// out any handlers/variables declared in this scope, and/or to restore
    /// the current processor from the enclosing scope.
    ///
    /// This method should always be called in conjunction with `push_scope`.
    /// It is an error to pop the root scope of a frame; use `pop_frame` for
    /// that instead.
    pub fn pop_scope(&mut self, out: &mut Vec<AdsInstruction>) {
        let frame = self.frame_stack.last_mut().unwrap();
        debug_assert!(frame.len() >= 2);
        let scope = frame.pop().unwrap();
        self.close_scope(scope, out);
    }

    /// Private helper method to close out the given scope after it has been
    /// popped from the scope stack.
    fn close_scope(&self, scope: AdsScope, out: &mut Vec<AdsInstruction>) {
        for _ in 0..scope.num_handlers {
            out.push(AdsInstruction::PopHandler);
        }
        for _ in 0..scope.num_variables {
            out.push(AdsInstruction::PopValue);
        }
        let prev_proc_name = &self.current_scope().proc_name;
        if *prev_proc_name != scope.proc_name {
            out.push(AdsInstruction::SetProc(prev_proc_name.clone()));
        }
    }

    /// Sets the current processor for the current scope.
    pub fn set_proc(
        &mut self,
        proc_name: Rc<str>,
        out: &mut Vec<AdsInstruction>,
    ) {
        debug_assert!(self.contains_processor(&proc_name));
        let scope = self.current_scope_mut();
        if scope.proc_name != proc_name {
            out.push(AdsInstruction::SetProc(proc_name.clone()));
            scope.proc_name = proc_name;
        }
    }

    pub fn add_handler(&mut self) {
        self.current_scope_mut().add_handler();
    }

    /// Adds a variable to the current scope that is internal to the compiler
    /// (i.e. it has no associated declaration), and returns the frame
    /// reference and frame-relative stack index for the new variable.
    pub fn add_internal_variable(&mut self) -> usize {
        self.current_scope_mut().add_internal_variable()
    }

    pub fn add_declaration(
        &mut self,
        kind: AdsDeclKind,
        id: IdentifierAst,
        expr_type: ExprType,
    ) {
        self.current_scope_mut().add_declaration(kind, id, expr_type);
    }

    pub fn get_declaration(
        &self,
        id: &str,
    ) -> Option<(AdsFrameRef, &AdsDecl)> {
        for (frame_index, frame) in self.frame_stack.iter().rev().enumerate() {
            for scope in frame.iter().rev() {
                if let Some(decl) = scope.get_declaration(id) {
                    let frame_ref = AdsFrameRef(frame_index);
                    return Some((frame_ref, decl));
                }
            }
        }
        None
    }

    /// Returns a sorted list of all processor names in the simulated system.
    pub fn processor_names(&self) -> Vec<Rc<str>> {
        self.system.processor_names()
    }

    /// Returns true if the simulated system contains a processor with the
    /// given name.
    pub fn contains_processor(&self, proc_name: &str) -> bool {
        self.system.contains_processor(proc_name)
    }

    /// Returns a list of register names for the current processor in the
    /// current scope.
    pub fn get_register_names(&self) -> &'static [&'static str] {
        self.system.register_names(&self.current_scope().proc_name)
    }

    pub fn typecheck_expression(
        &self,
        expr: ExprAst,
    ) -> AdsResult<(Vec<AdsInstruction>, ExprType, ExprStatic)> {
        let context = self.current_src_context();
        ExprCompiler::new(self).typecheck(expr).map_err(|errs| {
            errs.map(|error| AdsError::ExprTypeError {
                context: context.clone(),
                error,
            })
        })
    }
}

impl<'a> ExprEnv for AdsTypeEnv<'a> {
    type Op = AdsInstruction;

    fn typecheck_here_label(
        &self,
        span: SrcSpan,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)> {
        Err(Errs::one(ExprTypeError::RelativeLabelInDebuggerScript { span }))
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, ExprStatic)> {
        if let Some((frame_ref, decl)) = self.get_declaration(name) {
            let op = AdsInstruction::GetValue(frame_ref, decl.stack_index);
            let expr_type = decl.var_type.clone();
            let expr_static = match &decl.kind {
                AdsDeclKind::Constant(expr_static) => expr_static.clone(),
                AdsDeclKind::Variable => Err(ExprNotStaticReason::Variable {
                    span,
                    name: name.clone(),
                }),
            };
            return Ok((op, expr_type, expr_static));
        }
        for &register in self.get_register_names() {
            if name.eq_ignore_ascii_case(register) {
                let op = AdsInstruction::GetRegister(register);
                let expr_static = Err(ExprNotStaticReason::Variable {
                    span,
                    name: name.clone(),
                });
                return Ok((op, ExprType::Integer, expr_static));
            }
        }
        if name.eq_ignore_ascii_case("PC") {
            let expr_static = Err(ExprNotStaticReason::Variable {
                span,
                name: name.clone(),
            });
            return Ok((
                AdsInstruction::GetPc,
                ExprType::Integer,
                expr_static,
            ));
        }
        if let Some((value, expr_type)) = self.builtins.get(name) {
            let op = AdsInstruction::PushValue(value.clone());
            return Ok((op, expr_type.clone(), Ok(value.clone())));
        }
        Err(Errs::one(ExprTypeError::UnknownIdentifier {
            span,
            name: name.clone(),
        }))
    }

    fn apply_function_op(
        &self,
        _func_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> Self::Op {
        AdsInstruction::Apply { context: self.current_src_context(), arg_span }
    }

    fn binary_operation_op(
        &self,
        binop: ExprBinOp,
        op_span: SrcSpan,
        lhs_span: SrcSpan,
        rhs_span: SrcSpan,
    ) -> Self::Op {
        AdsInstruction::BinOp {
            context: self.current_src_context(),
            binop,
            op_span,
            lhs_span,
            rhs_span,
        }
    }

    fn list_index_op(
        &self,
        list_span: SrcSpan,
        index_span: SrcSpan,
    ) -> Self::Op {
        AdsInstruction::ListIndex {
            context: self.current_src_context(),
            list_span,
            index_span,
        }
    }

    fn unary_operation_op(
        &self,
        unop: ExprUnOp,
        op_span: SrcSpan,
        arg_span: SrcSpan,
    ) -> Self::Op {
        AdsInstruction::UnOp {
            context: self.current_src_context(),
            unop,
            op_span,
            arg_span,
        }
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{
        AdsDeclKind, AdsFrameRef, AdsInstruction, AdsTypeEnv, ExprType,
        ExprValue, SimSystem,
    };
    use crate::error::SrcSpan;
    use crate::expr::ExprNotStaticReason;
    use crate::parse::{ExprAst, ExprAstNode, IdentifierAst, IdentifierKind};
    use num_bigint::BigInt;
    use std::assert_matches;
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
        let system = SimSystem::with_nop_cpu();
        let mut env = AdsTypeEnv::new(&system, Rc::from("input"));
        env.add_declaration(
            AdsDeclKind::Constant(Err(ExprNotStaticReason::Phantom)),
            IdentifierAst {
                span: SrcSpan::from_byte_range(1..4),
                name: Rc::from("foo"),
                kind: IdentifierKind::Standard,
            },
            ExprType::Boolean,
        );
        let (instructions, expr_type, expr_static) =
            env.typecheck_expression(id_ast("foo", 10..13)).unwrap();
        assert_eq!(expr_type, ExprType::Boolean);
        assert_matches!(expr_static, Err(ExprNotStaticReason::Phantom));
        assert_matches!(
            instructions.as_slice(),
            [AdsInstruction::GetValue(AdsFrameRef(0), 0)]
        );
    }

    #[test]
    fn typecheck_int_literal_expr() {
        let system = SimSystem::with_nop_cpu();
        let env = AdsTypeEnv::new(&system, Rc::from("input"));
        let (instructions, expr_type, expr_static) =
            env.typecheck_expression(int_ast(42, 0..2)).unwrap();
        assert_eq!(expr_type, ExprType::Integer);
        assert_matches!(expr_static, Ok(value) if value == int_value(42));
        assert_matches!(instructions.as_slice(), [
            AdsInstruction::PushValue(value)
        ] if *value == int_value(42));
    }
}

//===========================================================================//
