use super::binop::AdsBinOp;
use super::value::{AdsType, AdsValue};
use crate::parse::{
    DeclareAst, ExprAst, ExprAstNode, IdentifierAst, ParseError, SrcSpan,
};
use std::collections::HashMap;
use std::rc::Rc;

//===========================================================================//

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AdsExprOp {
    BinOp(AdsBinOp),
    Literal(AdsValue),
    MakeList(usize),
    MakeTuple(usize),
    Variable(usize),
}

//===========================================================================//

/// An expression in an [AdsProgram].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AdsExpr {
    pub(crate) ops: Vec<AdsExprOp>,
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
                    ExprAstNode::BinOp(_, lhs, rhs) => {
                        stack.push(lhs);
                        stack.push(rhs);
                    }
                    ExprAstNode::BoolLiteral(_) => {}
                    ExprAstNode::Identifier(_) => {}
                    ExprAstNode::IntLiteral(_) => {}
                    ExprAstNode::ListLiteral(items) => stack.extend(items),
                    ExprAstNode::StrLiteral(_) => {}
                    ExprAstNode::TupleLiteral(items) => stack.extend(items),
                }
            }
            subexprs
        };
        let mut types = Vec::<AdsType>::new();
        let mut ops = Vec::<AdsExprOp>::new();
        let mut errors = Vec::<ParseError>::new();
        for subexpr in subexpressions.into_iter().rev() {
            match &subexpr.node {
                ExprAstNode::BinOp(binop_ast, lhs_ast, rhs_ast) => {
                    debug_assert!(types.len() >= 2);
                    let rhs_type = types.pop().unwrap();
                    let lhs_type = types.pop().unwrap();
                    if lhs_type == AdsType::Bottom
                        || rhs_type == AdsType::Bottom
                    {
                        types.push(AdsType::Bottom);
                    } else {
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
                &ExprAstNode::BoolLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::Boolean(value)));
                    types.push(AdsType::Boolean);
                }
                ExprAstNode::Identifier(id) => {
                    if let Some(decl) = env.get_declaration(id) {
                        ops.push(AdsExprOp::Variable(decl.stack_index));
                        types.push(decl.var_type.clone());
                    } else {
                        let span = subexpr.span;
                        let message = format!("No such identifier: `{}`", id);
                        let label = "this was never declared".to_string();
                        errors.push(
                            ParseError::new(span, message)
                                .with_label(subexpr.span, label),
                        );
                        types.push(AdsType::Bottom);
                    }
                }
                ExprAstNode::IntLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::Integer(
                        value.clone(),
                    )));
                    types.push(AdsType::Integer);
                }
                ExprAstNode::ListLiteral(item_asts) => {
                    let num_items = item_asts.len();
                    debug_assert!(types.len() >= num_items);
                    let item_types = types.split_off(types.len() - num_items);
                    let item_type = item_types
                        .first()
                        .cloned()
                        .unwrap_or(AdsType::Integer);
                    for (ty, ast) in item_types.into_iter().zip(item_asts) {
                        if ty != item_type {
                            let message =
                                "all items in a list must have the same type"
                                    .to_string();
                            let label1 =
                                format!("this item has type {item_type}");
                            let label2 = format!("this item has type {ty}");
                            errors.push(
                                ParseError::new(ast.span, message)
                                    .with_label(item_asts[0].span, label1)
                                    .with_label(ast.span, label2),
                            );
                            break;
                        }
                    }
                    ops.push(AdsExprOp::MakeList(num_items));
                    types.push(AdsType::List(Rc::new(item_type)));
                }
                ExprAstNode::StrLiteral(value) => {
                    ops.push(AdsExprOp::Literal(AdsValue::String(
                        value.clone(),
                    )));
                    types.push(AdsType::String);
                }
                ExprAstNode::TupleLiteral(item_asts) => {
                    let num_items = item_asts.len();
                    debug_assert!(types.len() >= num_items);
                    let item_types = types.split_off(types.len() - num_items);
                    ops.push(AdsExprOp::MakeTuple(num_items));
                    types.push(AdsType::Tuple(Rc::new(item_types)));
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

pub struct AdsDeclaration {
    pub kind: DeclareAst,
    pub id_span: SrcSpan,
    pub var_type: AdsType,
    pub stack_index: usize,
}

//===========================================================================//

struct AdsScope {
    variables: HashMap<String, AdsDeclaration>,
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

    fn get_declaration(&self, id: &str) -> Option<&AdsDeclaration> {
        self.variables.get(id)
    }

    fn add_declaration(
        &mut self,
        kind: DeclareAst,
        id: IdentifierAst,
        var_type: AdsType,
    ) {
        self.variables.insert(
            id.name,
            AdsDeclaration {
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
        kind: DeclareAst,
        id: IdentifierAst,
        ty: AdsType,
    ) {
        debug_assert!(!self.scopes.is_empty());
        self.scopes.last_mut().unwrap().add_declaration(kind, id, ty);
    }

    pub fn get_declaration(&self, id: &str) -> Option<&AdsDeclaration> {
        for scope in self.scopes.iter().rev() {
            if let Some(decl) = scope.get_declaration(id) {
                return Some(decl);
            }
        }
        None
    }
}

//===========================================================================//
