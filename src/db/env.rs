use super::error::{AdsError, AdsResult, AdsSrcContext, AdsSrcLoc};
use super::inst::{AdsFrameRef, AdsInstruction};
use crate::error::{Errs, SrcSpan};
use crate::expr::{
    ExprBinOp, ExprCompiler, ExprEnv, ExprLabel, ExprNotStaticReason,
    ExprStatic, ExprSymbolProvenance, ExprType, ExprTypeError, ExprTypeResult,
    ExprUnOp, ExprValue, make_global_builtin_values,
};
use crate::parse::AdsModuleAst;
use crate::parse::{ExprAst, HereLabelKind, IdentifierAst, SymbolTable};
use crate::system::SimSystem;
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

pub(super) enum AdsDeclKind {
    Immutable(ExprStatic),
    Mutable,
}

//===========================================================================//

struct AdsDecl {
    pub kind: AdsDeclKind,
    pub id_loc: AdsSrcLoc,
    pub var_type: ExprType,
    pub stack_index: usize,
}

//===========================================================================//

pub(super) enum AdsVariableImmutableReason {
    /// The variable is a builtin (which cannot be mutated).
    Builtin,
    /// The variable is a local that was declared immutable with `let`.
    DeclaredImmutable(AdsSrcLoc),
    /// The variable is a symbol table entry (which cannot be mutated).
    Symbol,
}

//===========================================================================//

pub(super) struct AdsVariable<'a>(AdsVarKind<'a>);

enum AdsVarKind<'a> {
    Builtin(&'a (ExprValue, ExprType)),
    Declared(AdsFrameRef, &'a AdsDecl),
    Pc,
    Register(&'static str),
    Symbol(&'a ExprLabel),
}

impl<'a> AdsVariable<'a> {
    pub fn getter_instruction(&self) -> AdsInstruction {
        match self.0 {
            AdsVarKind::Builtin((value, _)) => {
                AdsInstruction::PushValue(value.clone())
            }
            AdsVarKind::Declared(frame_ref, decl) => {
                AdsInstruction::GetValue(frame_ref, decl.stack_index)
            }
            AdsVarKind::Pc => AdsInstruction::GetPc,
            AdsVarKind::Register(reg) => AdsInstruction::GetRegister(reg),
            AdsVarKind::Symbol(label) => {
                AdsInstruction::PushValue(ExprValue::Label(label.clone()))
            }
        }
    }

    pub fn taker_instruction(&self) -> AdsInstruction {
        match self.0 {
            AdsVarKind::Builtin((value, _)) => {
                AdsInstruction::PushValue(value.clone())
            }
            AdsVarKind::Declared(frame_ref, decl) => {
                AdsInstruction::TakeValue(frame_ref, decl.stack_index)
            }
            AdsVarKind::Pc => AdsInstruction::GetPc,
            AdsVarKind::Register(reg) => AdsInstruction::GetRegister(reg),
            AdsVarKind::Symbol(label) => {
                AdsInstruction::PushValue(ExprValue::Label(label.clone()))
            }
        }
    }

    pub fn setter_instruction(
        &self,
    ) -> Result<AdsInstruction, AdsVariableImmutableReason> {
        match self.0 {
            AdsVarKind::Builtin(_) => Err(AdsVariableImmutableReason::Builtin),
            AdsVarKind::Declared(frame_ref, decl) => match &decl.kind {
                AdsDeclKind::Immutable(_) => {
                    Err(AdsVariableImmutableReason::DeclaredImmutable(
                        decl.id_loc.clone(),
                    ))
                }
                AdsDeclKind::Mutable => {
                    Ok(AdsInstruction::SetValue(frame_ref, decl.stack_index))
                }
            },
            AdsVarKind::Pc => Ok(AdsInstruction::SetPc),
            AdsVarKind::Register(reg) => Ok(AdsInstruction::SetRegister(reg)),
            AdsVarKind::Symbol(_) => Err(AdsVariableImmutableReason::Symbol),
        }
    }

    pub fn expr_type(&self) -> ExprType {
        match self.0 {
            AdsVarKind::Builtin((_, expr_type)) => expr_type.clone(),
            AdsVarKind::Declared(_, decl) => decl.var_type.clone(),
            AdsVarKind::Pc | AdsVarKind::Register(_) => ExprType::Integer,
            AdsVarKind::Symbol(_) => ExprType::Label,
        }
    }

    pub fn expr_static(&self, span: SrcSpan, name: &Rc<str>) -> ExprStatic {
        match self.0 {
            AdsVarKind::Builtin((value, _)) => Ok(value.clone()),
            AdsVarKind::Declared(_, decl)
                if let AdsDeclKind::Immutable(expr_static) = &decl.kind =>
            {
                expr_static.clone()
            }
            AdsVarKind::Symbol(label) => Ok(ExprValue::Label(label.clone())),
            _ => {
                Err(ExprNotStaticReason::Variable { span, name: name.clone() })
            }
        }
    }
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
        context: Rc<AdsSrcContext>,
        kind: AdsDeclKind,
        id: IdentifierAst,
        var_type: ExprType,
    ) {
        let id_loc = AdsSrcLoc { context, span: id.span };
        let stack_index = self.frame_end();
        let decl = AdsDecl { kind, id_loc, var_type, stack_index };
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
    symbols: HashMap<Rc<str>, ExprLabel>,
    context_stack: Vec<Rc<AdsSrcContext>>,
    frame_stack: Vec<Vec<AdsScope>>,
}

impl<'a> AdsTypeEnv<'a> {
    pub fn new(
        root_path: Rc<str>,
        system: &'a SimSystem,
        symbols: &SymbolTable,
    ) -> AdsTypeEnv<'a> {
        let root_context = Rc::new(AdsSrcContext::root(root_path));
        let default_proc_name = system.selected_processor_name();
        let root_scope = AdsScope::new(0, default_proc_name);
        AdsTypeEnv {
            system,
            builtins: make_global_builtin_values(),
            symbols: collect_symbols(symbols),
            context_stack: vec![root_context],
            frame_stack: vec![vec![root_scope]],
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
    /// (i.e. it has no associated declaration), and returns the frame-relative
    /// stack index for the new variable.
    pub fn add_internal_variable(&mut self) -> usize {
        self.current_scope_mut().add_internal_variable()
    }

    pub fn add_declaration(
        &mut self,
        kind: AdsDeclKind,
        id: IdentifierAst,
        expr_type: ExprType,
    ) {
        let context = self.current_src_context();
        self.current_scope_mut().add_declaration(context, kind, id, expr_type);
    }

    fn get_declaration(&self, id: &str) -> Option<(AdsFrameRef, &AdsDecl)> {
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

    pub fn get_variable(&self, name: &Rc<str>) -> Option<AdsVariable<'_>> {
        if let Some((frame_ref, decl)) = self.get_declaration(name) {
            return Some(AdsVariable(AdsVarKind::Declared(frame_ref, decl)));
        }
        for &register in self.get_register_names() {
            if name.eq_ignore_ascii_case(register) {
                return Some(AdsVariable(AdsVarKind::Register(register)));
            }
        }
        if name.eq_ignore_ascii_case("PC") {
            return Some(AdsVariable(AdsVarKind::Pc));
        }
        if let Some(label) = self.symbols.get(name) {
            return Some(AdsVariable(AdsVarKind::Symbol(label)));
        }
        if let Some(builtin) = self.builtins.get(name) {
            return Some(AdsVariable(AdsVarKind::Builtin(builtin)));
        }
        None
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
        _span: SrcSpan,
        _kind: HereLabelKind,
    ) -> ExprTypeResult<(Self::Op, ExprStatic)> {
        unreachable!() // "here" labels don't parse in debugger scripts
    }

    fn typecheck_identifier(
        &self,
        span: SrcSpan,
        name: &Rc<str>,
    ) -> ExprTypeResult<(Self::Op, ExprType, ExprStatic)> {
        if let Some(variable) = self.get_variable(name) {
            let op = variable.getter_instruction();
            let expr_type = variable.expr_type();
            let expr_static = variable.expr_static(span, name);
            Ok((op, expr_type, expr_static))
        } else {
            Err(Errs::one(ExprTypeError::UnknownIdentifier {
                span,
                name: name.clone(),
            }))
        }
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
        AdsInstruction::GetListIndex {
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

    fn with_addr_op(&self, _op_span: SrcSpan) -> ExprTypeResult<Self::Op> {
        todo!("use address space of current proc")
    }

    fn with_addr_static(
        &self,
        _op_span: SrcSpan,
        _address: BigInt,
    ) -> ExprTypeResult<ExprLabel> {
        todo!("use address space of current proc")
    }
}

//===========================================================================//

fn collect_symbols(table: &SymbolTable) -> HashMap<Rc<str>, ExprLabel> {
    let mut map = HashMap::<Rc<str>, ExprLabel>::new();
    for symbol in &table.entries {
        // If two or more symbols have the same name, prefer the external one
        // if any, otherwise prefer the first one in the table.
        if symbol.is_external || !map.contains_key(&symbol.name) {
            let label = ExprLabel::AddrAbsolute {
                space: symbol.space.clone(),
                address: BigInt::from(symbol.address),
                provenance: Some(ExprSymbolProvenance {
                    symbol_name: symbol.name.clone(),
                    offset: BigInt::ZERO,
                }),
            };
            map.insert(symbol.name.clone(), label);
        }
    }
    map
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
    use crate::parse::{
        CompoundIdAst, ExprAst, ExprAstNode, IdentifierAst, IdentifierKind,
        SymbolTable,
    };
    use num_bigint::BigInt;
    use std::assert_matches;
    use std::ops::Range;
    use std::rc::Rc;

    fn id_ast(name: &str, range: Range<usize>) -> ExprAst {
        let span = SrcSpan::from_byte_range(range);
        let name = Rc::<str>::from(name);
        ExprAst {
            span,
            node: ExprAstNode::Identifier(CompoundIdAst {
                span,
                ids: vec![IdentifierAst {
                    span,
                    name: name.clone(),
                    kind: IdentifierKind::Standard,
                }],
                name,
            }),
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
        let symbols = SymbolTable::default();
        let mut env = AdsTypeEnv::new(Rc::from("input"), &system, &symbols);
        env.add_declaration(
            AdsDeclKind::Immutable(Err(ExprNotStaticReason::Phantom)),
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
        let symbols = SymbolTable::default();
        let env = AdsTypeEnv::new(Rc::from("input"), &system, &symbols);
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
