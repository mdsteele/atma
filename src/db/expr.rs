use super::binop::AdsBinOp;
use super::inst::AdsInstruction;
use super::value::{AdsType, AdsValue};
use crate::parse::{
    BinOpAst, ExprAst, ExprAstNode, IdentifierAst, ParseError, SrcSpan,
};
use num_bigint::BigInt;
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

struct AdsExprCompiler<'a> {
    env: &'a AdsTypeEnv,
    // Invariant: If the top N entries of `types` all hold `is_static = true`,
    // then the top N entries of `ops` are all safe for
    // `AdsExprCompiler::unwrap_static()`.
    types: Vec<(AdsType, bool)>, // (type, is_static)
    ops: Vec<AdsInstruction>,
    errors: Vec<ParseError>,
}

impl<'a> AdsExprCompiler<'a> {
    pub fn new(env: &'a AdsTypeEnv) -> AdsExprCompiler<'a> {
        AdsExprCompiler {
            env,
            types: Vec::new(),
            ops: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn typecheck(
        mut self,
        expr: &ExprAst,
    ) -> Result<(Vec<AdsInstruction>, AdsType), Vec<ParseError>> {
        let mut stack = vec![expr];
        let mut subexprs = Vec::<&ExprAst>::new();
        while let Some(subexpr) = stack.pop() {
            subexprs.push(subexpr);
            match &subexpr.node {
                ExprAstNode::BinOp(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::BoolLiteral(_) => {}
                ExprAstNode::Identifier(_) => {}
                ExprAstNode::Index(_, lhs, rhs) => {
                    stack.push(lhs);
                    stack.push(rhs);
                }
                ExprAstNode::IntLiteral(_) => {}
                ExprAstNode::ListLiteral(items) => stack.extend(items),
                ExprAstNode::StrLiteral(_) => {}
                ExprAstNode::TupleLiteral(items) => stack.extend(items),
            }
        }
        for subexpr in subexprs.into_iter().rev() {
            self.typecheck_subexpr(subexpr);
        }
        debug_assert_eq!(self.types.len(), 1);
        if self.errors.is_empty() {
            Ok((self.ops, self.types.pop().unwrap().0))
        } else {
            Err(self.errors)
        }
    }

    fn typecheck_subexpr(&mut self, subexpr: &ExprAst) {
        match &subexpr.node {
            ExprAstNode::BinOp(binop_ast, lhs_ast, rhs_ast) => {
                self.typecheck_binop_node(*binop_ast, lhs_ast, rhs_ast);
            }
            &ExprAstNode::BoolLiteral(value) => {
                self.types.push((AdsType::Boolean, true));
                self.ops
                    .push(AdsInstruction::PushValue(AdsValue::Boolean(value)));
            }
            ExprAstNode::Identifier(id) => {
                self.typecheck_identifier(subexpr.span, id);
            }
            ExprAstNode::Index(index_span, lhs_ast, rhs_ast) => {
                self.typecheck_index(*index_span, lhs_ast, rhs_ast);
            }
            ExprAstNode::IntLiteral(value) => {
                self.ops.push(AdsInstruction::PushValue(AdsValue::Integer(
                    value.clone(),
                )));
                self.types.push((AdsType::Integer, true));
            }
            ExprAstNode::ListLiteral(item_asts) => {
                self.typecheck_list_literal(item_asts);
            }
            ExprAstNode::StrLiteral(value) => {
                self.ops.push(AdsInstruction::PushValue(AdsValue::String(
                    value.clone(),
                )));
                self.types.push((AdsType::String, true));
            }
            ExprAstNode::TupleLiteral(item_asts) => {
                self.typecheck_tuple_literal(item_asts);
            }
        }
    }

    fn typecheck_binop_node(
        &mut self,
        binop_ast: (SrcSpan, BinOpAst),
        lhs_ast: &ExprAst,
        rhs_ast: &ExprAst,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        if lhs_type == AdsType::Bottom || rhs_type == AdsType::Bottom {
            self.types.push((AdsType::Bottom, false));
            return;
        }
        match AdsBinOp::typecheck(
            binop_ast,
            lhs_ast.span,
            lhs_type,
            rhs_ast.span,
            rhs_type,
        ) {
            Ok((binop, result_type)) => {
                if lhs_static && rhs_static {
                    let rhs_value = self.pop_static();
                    let lhs_value = self.pop_static();
                    let result_value = binop.evaluate(lhs_value, rhs_value);
                    self.ops.push(AdsInstruction::PushValue(result_value));
                    self.types.push((result_type, true));
                } else {
                    self.ops.push(AdsInstruction::BinOp(binop));
                    self.types.push((result_type, false));
                }
            }
            Err(mut errs) => {
                self.errors.append(&mut errs);
                self.types.push((AdsType::Bottom, false));
            }
        }
    }

    fn typecheck_identifier(&mut self, span: SrcSpan, id: &str) {
        if let Some(decl) = self.env.get_declaration(id) {
            self.ops.push(AdsInstruction::CopyValue(decl.stack_index));
            self.types.push((decl.var_type.clone(), decl.kind.is_static()));
        } else {
            let message = format!("No such identifier: `{id}`");
            let label = "this was never declared".to_string();
            self.errors
                .push(ParseError::new(span, message).with_label(span, label));
            self.types.push((AdsType::Bottom, false));
        }
    }

    fn typecheck_index(
        &mut self,
        index_span: SrcSpan,
        lhs_ast: &ExprAst,
        rhs_ast: &ExprAst,
    ) {
        debug_assert!(self.types.len() >= 2);
        let (rhs_type, rhs_static) = self.types.pop().unwrap();
        let (lhs_type, lhs_static) = self.types.pop().unwrap();
        match (lhs_type, rhs_type) {
            (_, AdsType::Bottom) | (AdsType::Bottom, _) => {
                self.types.push((AdsType::Bottom, false));
            }
            (AdsType::List(item_type), AdsType::Integer) => {
                let item_type = Rc::unwrap_or_clone(item_type);
                if !lhs_static || !rhs_static {
                    self.ops.push(AdsInstruction::ListIndex);
                    self.types.push((item_type, false));
                    return;
                }
                let index = self.pop_static().unwrap_int();
                let list_values = self.pop_static().unwrap_list();
                if index < BigInt::ZERO
                    || index >= BigInt::from(list_values.len())
                {
                    let message =
                        "list index is statically out of range".to_string();
                    let label1 =
                        format!("this list has length {}", list_values.len());
                    let label2 =
                        format!("the value of this expression is {index}");
                    self.errors.push(
                        ParseError::new(rhs_ast.span, message)
                            .with_label(lhs_ast.span, label1)
                            .with_label(rhs_ast.span, label2),
                    );
                    self.types.push((AdsType::Bottom, false));
                    return;
                }
                let result_value =
                    list_values[usize::try_from(index).unwrap()].clone();
                self.ops.push(AdsInstruction::PushValue(result_value));
                self.types.push((item_type, true));
            }
            (AdsType::List(_), rhs_type) => {
                let message = format!("cannot use {rhs_type} as a list index");
                let label = format!("this expression has type {rhs_type}");
                self.errors.push(
                    ParseError::new(rhs_ast.span, message)
                        .with_label(rhs_ast.span, label),
                );
                self.types.push((AdsType::Bottom, false));
            }
            (AdsType::Tuple(item_types), AdsType::Integer) => {
                if !rhs_static {
                    let message = "tuple index must be static".to_string();
                    let label = "this expression isn't static".to_string();
                    self.errors.push(
                        ParseError::new(rhs_ast.span, message)
                            .with_label(rhs_ast.span, label),
                    );
                    self.types.push((AdsType::Bottom, false));
                    return;
                }
                let index = self.pop_static().unwrap_int();
                if index < BigInt::ZERO
                    || index >= BigInt::from(item_types.len())
                {
                    let message = "tuple index out of bounds".to_string();
                    let label1 = format!(
                        "this expression has type {}",
                        AdsType::Tuple(item_types)
                    );
                    let label2 =
                        format!("the value of this expression is {index}");
                    self.errors.push(
                        ParseError::new(rhs_ast.span, message)
                            .with_label(lhs_ast.span, label1)
                            .with_label(rhs_ast.span, label2),
                    );
                    self.types.push((AdsType::Bottom, false));
                    return;
                }
                let index = usize::try_from(index).unwrap();
                let item_type = item_types[index].clone();
                if lhs_static {
                    let tuple_values = self.pop_static().unwrap_tuple();
                    self.ops.push(AdsInstruction::PushValue(
                        tuple_values[index].clone(),
                    ));
                    self.types.push((item_type, true));
                } else {
                    self.ops.push(AdsInstruction::TupleItem(index));
                    self.types.push((item_type, false));
                }
            }
            (AdsType::Tuple(_), rhs_type) => {
                let message =
                    format!("cannot use {rhs_type} as a tuple index");
                let label = format!("this expression has type {rhs_type}");
                self.errors.push(
                    ParseError::new(rhs_ast.span, message)
                        .with_label(rhs_ast.span, label),
                );
                self.types.push((AdsType::Bottom, false));
            }
            (lhs_type, _) => {
                let message =
                    format!("cannot index into value of type {lhs_type}");
                let label = format!("this expression has type {lhs_type}");
                self.errors.push(
                    ParseError::new(index_span, message)
                        .with_label(lhs_ast.span, label),
                );
                self.types.push((AdsType::Bottom, false));
            }
        }
    }

    fn typecheck_list_literal(&mut self, item_asts: &[ExprAst]) {
        let num_items = item_asts.len();
        let (item_types, is_static) = self.pop_types(num_items);
        let item_type = item_types.first().cloned().unwrap_or(AdsType::Bottom);
        for (ty, ast) in item_types.into_iter().zip(item_asts) {
            if ty != item_type {
                let message =
                    "all items in a list must have the same type".to_string();
                let label1 = format!("this item has type {item_type}");
                let label2 = format!("this item has type {ty}");
                self.errors.push(
                    ParseError::new(ast.span, message)
                        .with_label(item_asts[0].span, label1)
                        .with_label(ast.span, label2),
                );
                break;
            }
        }
        if is_static {
            let item_values = self.pop_statics(num_items);
            self.ops
                .push(AdsInstruction::PushValue(AdsValue::List(item_values)));
        } else {
            self.ops.push(AdsInstruction::MakeList(num_items));
        }
        self.types.push((AdsType::List(Rc::new(item_type)), is_static));
    }

    fn typecheck_tuple_literal(&mut self, item_asts: &[ExprAst]) {
        let num_items = item_asts.len();
        let (item_types, is_static) = self.pop_types(num_items);
        if is_static {
            let item_values = self.pop_statics(num_items);
            self.ops
                .push(AdsInstruction::PushValue(AdsValue::Tuple(item_values)));
        } else {
            self.ops.push(AdsInstruction::MakeTuple(num_items));
        }
        self.types.push((AdsType::Tuple(Rc::new(item_types)), is_static));
    }

    fn unwrap_static(&self, op: AdsInstruction) -> AdsValue {
        match op {
            AdsInstruction::CopyValue(index) => {
                let decl = self.env.declaration_for_index(index).unwrap();
                decl.kind.clone().unwrap_static()
            }
            AdsInstruction::PushValue(value) => value,
            op => panic!("pop_static on {op:?}"),
        }
    }

    fn pop_static(&mut self) -> AdsValue {
        let op = self.ops.pop().unwrap();
        self.unwrap_static(op)
    }

    fn pop_statics(&mut self, num_items: usize) -> Vec<AdsValue> {
        debug_assert!(num_items <= self.ops.len());
        let ops = self.ops.split_off(self.ops.len() - num_items);
        ops.into_iter().map(|op| self.unwrap_static(op)).collect::<Vec<_>>()
    }

    fn pop_types(&mut self, num_items: usize) -> (Vec<AdsType>, bool) {
        debug_assert!(num_items <= self.types.len());
        let (item_types, are_static): (Vec<_>, Vec<_>) =
            self.types.drain((self.types.len() - num_items)..).unzip();
        let is_static = are_static.into_iter().all(|s| s);
        (item_types, is_static)
    }
}

//===========================================================================//

#[derive(Clone)]
pub enum AdsDeclKind {
    Constant(Option<AdsValue>),
    Variable,
}

impl AdsDeclKind {
    pub fn is_static(&self) -> bool {
        matches!(self, AdsDeclKind::Constant(Some(_)))
    }

    pub fn unwrap_static(self) -> AdsValue {
        match self {
            AdsDeclKind::Constant(Some(value)) => value,
            AdsDeclKind::Constant(None) => {
                panic!("AdsDeclKind::unwrap_static on non-static constant")
            }
            AdsDeclKind::Variable => {
                panic!("AdsDeclKind::unwrap_static on variable")
            }
        }
    }
}

//===========================================================================//

pub struct AdsDecl {
    pub kind: AdsDeclKind,
    pub id_span: SrcSpan,
    pub var_type: AdsType,
    pub stack_index: usize,
}

//===========================================================================//

struct AdsScope {
    variables: HashMap<String, AdsDecl>,
    num_handlers: usize,
    stack_start: usize,
    frame_size: usize,
}

impl AdsScope {
    fn with_start(stack_start: usize) -> AdsScope {
        AdsScope {
            variables: HashMap::new(),
            num_handlers: 0,
            stack_start,
            frame_size: 0,
        }
    }

    fn variable_stack_size(&self) -> usize {
        self.stack_start + self.frame_size
    }

    fn get_declaration(&self, id: &str) -> Option<&AdsDecl> {
        self.variables.get(id)
    }

    pub fn declaration_for_index(
        &self,
        stack_index: usize,
    ) -> Option<&AdsDecl> {
        self.variables.values().find(|decl| decl.stack_index == stack_index)
    }

    fn add_declaration(
        &mut self,
        kind: AdsDeclKind,
        id: IdentifierAst,
        var_type: AdsType,
    ) {
        self.variables.insert(
            id.name,
            AdsDecl {
                kind,
                id_span: id.span,
                var_type,
                stack_index: self.variable_stack_size(),
            },
        );
        self.frame_size += 1;
    }

    fn add_handler(&mut self) {
        self.num_handlers += 1;
    }
}

//===========================================================================//

pub struct AdsTypeEnv {
    scopes: Vec<AdsScope>,
}

impl AdsTypeEnv {
    pub fn empty() -> AdsTypeEnv {
        AdsTypeEnv { scopes: vec![AdsScope::with_start(0)] }
    }

    pub fn variable_stack_size(&self) -> usize {
        debug_assert!(!self.scopes.is_empty());
        self.scopes.last().unwrap().variable_stack_size()
    }

    pub fn push_scope(&mut self) {
        let start = self.variable_stack_size();
        self.scopes.push(AdsScope::with_start(start));
    }

    /// Returns the number of handlers and variables in the popped scope.
    pub fn pop_scope(&mut self) -> (usize, usize) {
        debug_assert!(self.scopes.len() >= 2);
        let scope = self.scopes.pop().unwrap();
        (scope.num_handlers, scope.frame_size)
    }

    pub fn add_handler(&mut self) {
        debug_assert!(!self.scopes.is_empty());
        self.scopes.last_mut().unwrap().add_handler();
    }

    pub fn add_declaration(
        &mut self,
        kind: AdsDeclKind,
        id: IdentifierAst,
        ty: AdsType,
    ) {
        debug_assert!(!self.scopes.is_empty());
        self.scopes.last_mut().unwrap().add_declaration(kind, id, ty);
    }

    pub fn get_declaration(&self, id: &str) -> Option<&AdsDecl> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.get_declaration(id) {
                return Some(decl);
            }
        }
        None
    }

    pub fn declaration_for_index(
        &self,
        stack_index: usize,
    ) -> Option<&AdsDecl> {
        for scope in self.scopes.iter().rev() {
            if scope.stack_start <= stack_index {
                return scope.declaration_for_index(stack_index);
            }
        }
        None
    }

    pub fn typecheck_expression(
        &self,
        expr: ExprAst,
    ) -> Result<(Vec<AdsInstruction>, AdsType), Vec<ParseError>> {
        AdsExprCompiler::new(self).typecheck(&expr)
    }
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::{AdsDeclKind, AdsInstruction, AdsType, AdsTypeEnv, AdsValue};
    use crate::parse::{ExprAst, ExprAstNode, IdentifierAst, SrcSpan};
    use num_bigint::BigInt;
    use std::ops::Range;

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
            AdsDeclKind::Constant(None),
            IdentifierAst {
                span: SrcSpan::from_byte_range(1..4),
                name: "foo".to_string(),
            },
            AdsType::Boolean,
        );
        assert_eq!(
            env.typecheck_expression(id_ast("foo", 10..13)),
            Ok((vec![AdsInstruction::CopyValue(0)], AdsType::Boolean))
        );
    }

    #[test]
    fn typecheck_int_literal_expr() {
        let env = AdsTypeEnv::empty();
        assert_eq!(
            env.typecheck_expression(int_ast(42, 0..2)),
            Ok((
                vec![AdsInstruction::PushValue(int_value(42))],
                AdsType::Integer,
            ))
        );
    }
}

//===========================================================================//
