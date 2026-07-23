//! Facilities for parsing expressions.

use super::atom::{Context, Extra, parse_tokens, symbol};
use super::error::ParseResult;
use super::lex::{Token, TokenValue};
use crate::error::SrcSpan;
use chumsky::{self, ConfigParser, IterParser, Parser, pratt};
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

// The binding power of various classes of operators:
const BIND_LOGICAL_OR: u16 = 1;
const BIND_LOGICAL_AND: u16 = 2;
const BIND_COMPARISON: u16 = 3;
const BIND_BIT_OR: u16 = 4;
const BIND_BIT_XOR: u16 = 5;
const BIND_BIT_AND: u16 = 6;
const BIND_BIT_SHIFT: u16 = 7;
const BIND_ADDITIVE: u16 = 8;
const BIND_MULTIPLICATIVE: u16 = 9;
const BIND_UNARY_PREFIX: u16 = 10;
const BIND_EXPONENTIATE: u16 = 11;

//===========================================================================//

/// A unary operation on an expression in an abstract syntax tree.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnOpAst {
    /// Bitwise NOT.
    BitNot,
    /// Logical NOT.
    LogNot,
    /// Negation.
    Neg,
}

impl UnOpAst {
    /// Specifies the verb to use when describing how this operator acts on a
    /// subexpression.
    pub(crate) fn verb(self) -> &'static str {
        match self {
            UnOpAst::BitNot => "bitwise NOT",
            UnOpAst::LogNot => "logical NOT",
            UnOpAst::Neg => "negate",
        }
    }
}

//===========================================================================//

/// A binary operation between two expressions in an abstract syntax tree.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinOpAst {
    /// Addition.
    Add,
    /// Bitwise AND.
    BitAnd,
    /// Bitwise OR.
    BitOr,
    /// Bitwise XOR.
    BitXor,
    /// "Equals" comparison.
    CmpEq,
    /// "Greater-than-or-equal-to" comparison.
    CmpGe,
    /// "Greater-than" comparison.
    CmpGt,
    /// "Less-than-or-equal-to" comparison.
    CmpLe,
    /// "Less-than" comparison.
    CmpLt,
    /// "Not-equals" comparison.
    CmpNe,
    /// Division.
    Div,
    /// Exponentiation.
    Pow,
    /// Logical (short-circuiting) AND.
    LogAnd,
    /// Logical (short-circuiting) OR.
    LogOr,
    /// Modulo.
    Mod,
    /// Multiplication.
    Mul,
    /// Bit-shift left.
    Shl,
    /// Bit-shift right.
    Shr,
    /// Subtraction.
    Sub,
}

impl BinOpAst {
    /// Specifies the formatting to use when describing how this operator
    /// relates two subexpressions, returning a `(verb, conj, rev)` triple.  If
    /// the returned `rev` boolean is false, then this specifies the phrasing
    /// "verb LHS conj RHS", e.g. "divide LHS by RHS".  If the returned `rev`
    /// boolean is true, then this instead specifies the phrasing "verb RHS
    /// conj LHS", e.g. "subtract RHS from LHS".
    pub(crate) fn verb_conj_rev(self) -> (&'static str, &'static str, bool) {
        match self {
            BinOpAst::Add => ("add", "to", true),
            BinOpAst::BitAnd => ("bitwise AND", "with", false),
            BinOpAst::BitOr => ("bitwise OR", "with", false),
            BinOpAst::BitXor => ("bitwise XOR", "with", false),
            BinOpAst::CmpEq | BinOpAst::CmpNe => ("equate", "with", false),
            BinOpAst::CmpGe
            | BinOpAst::CmpGt
            | BinOpAst::CmpLe
            | BinOpAst::CmpLt => ("order", "against", false),
            BinOpAst::Div => ("divide", "by", false),
            BinOpAst::LogAnd => ("logical AND", "with", false),
            BinOpAst::LogOr => ("logical OR", "with", false),
            BinOpAst::Mod => ("modulo", "by", false),
            BinOpAst::Mul => ("multiply", "by", false),
            BinOpAst::Pow => ("exponentiate", "by", false),
            BinOpAst::Shl | BinOpAst::Shr => ("shift", "by", false),
            BinOpAst::Sub => ("subtract", "from", true),
        }
    }
}

//===========================================================================//

/// Kinds of identifiers that can appear in an abstract syntax tree.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IdentifierKind {
    /// A standard identifier (e.g. "foo").
    Standard,
    /// A built-in name (e.g. "%foo").
    Builtin,
    /// A macro placeholder (e.g. "%FOO").
    Placeholder,
}

//===========================================================================//

/// An identifier in an expression or lvalue.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IdentifierAst {
    /// The location in the source code where this instance of the identifier
    /// appears.
    pub span: SrcSpan,
    /// The name of the identifier.
    pub name: Rc<str>,
    /// What kind of identifier this is.
    pub kind: IdentifierKind,
}

impl IdentifierAst {
    /// Parses a sequence of tokens into an identifier abstract syntax tree.
    pub fn parse(tokens: &[Token]) -> ParseResult<IdentifierAst> {
        parse_tokens(IdentifierAst::parser(), tokens)
    }

    pub(super) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], IdentifierAst, Extra<'a>> + Clone {
        let identifier_token = chumsky::prelude::any()
            .try_map(|token: Token, span| {
                if let TokenValue::Identifier(name) = token.value {
                    Ok(IdentifierAst {
                        name,
                        span: token.span,
                        kind: IdentifierKind::Standard,
                    })
                } else {
                    Err(chumsky::error::Rich::custom(span, ""))
                }
            })
            .labelled("identifier");
        let builtin_token = chumsky::prelude::any()
            .try_map(|token: Token, span| {
                if let TokenValue::Builtin(name) = token.value {
                    Ok(IdentifierAst {
                        name,
                        span: token.span,
                        kind: IdentifierKind::Builtin,
                    })
                } else {
                    Err(chumsky::error::Rich::custom(span, ""))
                }
            })
            .labelled("builtin");
        let placeholder_token = chumsky::prelude::any()
            .try_map(|token: Token, span| {
                if let TokenValue::Placeholder(name) = token.value {
                    Ok(IdentifierAst {
                        name,
                        span: token.span,
                        kind: IdentifierKind::Placeholder,
                    })
                } else {
                    Err(chumsky::error::Rich::custom(span, ""))
                }
            })
            .labelled("placeholder");
        identifier_token.or(builtin_token).or(placeholder_token
            .contextual()
            .configure(|_, ctx: &Context| ctx.allow_placeholder_as_identifier))
    }
}

//===========================================================================//

/// The abstract syntax tree for an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprAst {
    /// The location in the source code where this expression appears.
    pub span: SrcSpan,
    /// The contents of this expression.
    pub node: ExprAstNode,
}

impl ExprAst {
    /// Parses a sequence of tokens into an expression abstract syntax tree.
    pub fn parse(tokens: &[Token]) -> ParseResult<ExprAst> {
        parse_tokens(ExprAst::parser(), tokens)
    }

    pub(super) fn parser<'a>()
    -> impl Parser<'a, &'a [Token], ExprAst, Extra<'a>> + Clone {
        chumsky::prelude::recursive(|expr| {
            let parenthesized_expr = chumsky::prelude::group((
                symbol(TokenValue::ParenOpen),
                expr.clone()
                    .separated_by(symbol(TokenValue::Comma))
                    .collect::<Vec<_>>(),
                symbol(TokenValue::ParenClose),
            ))
            .map(
                |(open, mut asts, close): (Token, Vec<ExprAst>, Token)| {
                    ExprAst {
                        span: open.span.merged_with(close.span),
                        node: if asts.len() == 1 {
                            asts.pop().unwrap().node
                        } else {
                            ExprAstNode::TupleLiteral(asts)
                        },
                    }
                },
            );
            let list_literal = chumsky::prelude::group((
                symbol(TokenValue::BraceOpen),
                expr.clone()
                    .separated_by(symbol(TokenValue::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
                symbol(TokenValue::BraceClose),
            ))
            .map(
                |(open, asts, close): (Token, Vec<ExprAst>, Token)| ExprAst {
                    span: open.span.merged_with(close.span),
                    node: ExprAstNode::ListLiteral(asts),
                },
            );
            let here_label = symbol(TokenValue::DollarLeft).map(|token| {
                ExprAst { span: token.span, node: ExprAstNode::HereLabel }
            });
            let identifier = IdentifierAst::parser().map(|id| ExprAst {
                span: id.span,
                node: match id.kind {
                    IdentifierKind::Standard | IdentifierKind::Builtin => {
                        ExprAstNode::Identifier(id.name)
                    }
                    IdentifierKind::Placeholder => {
                        ExprAstNode::Placeholder(id.name)
                    }
                },
            });

            let expr_atom = chumsky::prelude::choice((
                parenthesized_expr.clone(),
                here_label,
                identifier,
                list_literal,
                bool_literal(),
                int_literal(),
                str_literal(),
            ))
            .labelled("subexpression");

            enum ExprAtomSuffix {
                Call(ExprAst),
                Index(SrcSpan, ExprAst),
            }
            let call_suffix = parenthesized_expr.map(ExprAtomSuffix::Call);
            let index_suffix = chumsky::prelude::group((
                symbol(TokenValue::BracketOpen),
                expr,
                symbol(TokenValue::BracketClose),
            ))
            .map(|(open, index, close)| {
                ExprAtomSuffix::Index(open.span.merged_with(close.span), index)
            });
            let any_suffix =
                chumsky::prelude::choice((call_suffix, index_suffix));
            let suffixed_expr =
                expr_atom.foldl(any_suffix.repeated(), |base, suffix| {
                    match suffix {
                        ExprAtomSuffix::Call(arg) => ExprAst {
                            span: base.span.merged_with(arg.span),
                            node: ExprAstNode::Apply(
                                Box::new(base),
                                Box::new(arg),
                            ),
                        },
                        ExprAtomSuffix::Index(index_span, index) => ExprAst {
                            span: base.span.merged_with(index_span),
                            node: ExprAstNode::Index(
                                index_span,
                                Box::new(base),
                                Box::new(index),
                            ),
                        },
                    }
                });

            let pratt_expr = suffixed_expr.pratt((
                pratt::infix(
                    pratt::left(BIND_EXPONENTIATE),
                    symbol(TokenValue::StarStar),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Pow, l, o, r),
                ),
                pratt::prefix(
                    BIND_UNARY_PREFIX,
                    symbol(TokenValue::Tilde),
                    |o, s, _| ExprAst::unop(UnOpAst::BitNot, o, s),
                ),
                pratt::prefix(
                    BIND_UNARY_PREFIX,
                    symbol(TokenValue::Bang),
                    |o, s, _| ExprAst::unop(UnOpAst::LogNot, o, s),
                ),
                pratt::prefix(
                    BIND_UNARY_PREFIX,
                    symbol(TokenValue::Minus),
                    |o, s, _| ExprAst::unop(UnOpAst::Neg, o, s),
                ),
                pratt::infix(
                    pratt::left(BIND_MULTIPLICATIVE),
                    symbol(TokenValue::Star),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Mul, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_MULTIPLICATIVE),
                    symbol(TokenValue::Slash),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Div, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_MULTIPLICATIVE),
                    symbol(TokenValue::Percent),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Mod, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_ADDITIVE),
                    symbol(TokenValue::Plus),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Add, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_ADDITIVE),
                    symbol(TokenValue::Minus),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Sub, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_BIT_SHIFT),
                    symbol(TokenValue::LessLess),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Shl, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_BIT_SHIFT),
                    symbol(TokenValue::GreaterGreater),
                    |l, o, r, _| ExprAst::binop(BinOpAst::Shr, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_BIT_AND),
                    symbol(TokenValue::And),
                    |l, o, r, _| ExprAst::binop(BinOpAst::BitAnd, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_BIT_XOR),
                    symbol(TokenValue::Caret),
                    |l, o, r, _| ExprAst::binop(BinOpAst::BitXor, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_BIT_OR),
                    symbol(TokenValue::Or),
                    |l, o, r, _| ExprAst::binop(BinOpAst::BitOr, l, o, r),
                ),
                pratt::infix(
                    pratt::none(BIND_COMPARISON),
                    symbol(TokenValue::EqualsEquals),
                    |l, o, r, _| ExprAst::binop(BinOpAst::CmpEq, l, o, r),
                ),
                pratt::infix(
                    pratt::none(BIND_COMPARISON),
                    symbol(TokenValue::LessThan),
                    |l, o, r, _| ExprAst::binop(BinOpAst::CmpLt, l, o, r),
                ),
                pratt::infix(
                    pratt::none(BIND_COMPARISON),
                    symbol(TokenValue::LessEquals),
                    |l, o, r, _| ExprAst::binop(BinOpAst::CmpLe, l, o, r),
                ),
                pratt::infix(
                    pratt::none(BIND_COMPARISON),
                    symbol(TokenValue::GreaterThan),
                    |l, o, r, _| ExprAst::binop(BinOpAst::CmpGt, l, o, r),
                ),
                pratt::infix(
                    pratt::none(BIND_COMPARISON),
                    symbol(TokenValue::GreaterEquals),
                    |l, o, r, _| ExprAst::binop(BinOpAst::CmpGe, l, o, r),
                ),
                pratt::infix(
                    pratt::none(BIND_COMPARISON),
                    symbol(TokenValue::BangEquals),
                    |l, o, r, _| ExprAst::binop(BinOpAst::CmpNe, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_LOGICAL_AND),
                    symbol(TokenValue::AndAnd),
                    |l, o, r, _| ExprAst::binop(BinOpAst::LogAnd, l, o, r),
                ),
                pratt::infix(
                    pratt::left(BIND_LOGICAL_OR),
                    symbol(TokenValue::OrOr),
                    |l, o, r, _| ExprAst::binop(BinOpAst::LogOr, l, o, r),
                ),
            ));

            let conditional_expr = pratt_expr
                .clone()
                .then_ignore(symbol(TokenValue::Question))
                .then(pratt_expr.clone())
                .then_ignore(symbol(TokenValue::Colon))
                .repeated()
                .foldr(pratt_expr, |(pred, lhs), rhs| ExprAst {
                    span: pred.span.merged_with(rhs.span),
                    node: ExprAstNode::Conditional(
                        Box::new(pred),
                        Box::new(lhs),
                        Box::new(rhs),
                    ),
                });
            conditional_expr.labelled("expression")
        })
    }

    fn binop(
        binop: BinOpAst,
        lhs: ExprAst,
        op: Token,
        rhs: ExprAst,
    ) -> ExprAst {
        ExprAst {
            span: lhs.span.merged_with(rhs.span),
            node: ExprAstNode::BinOp(
                (op.span, binop),
                Box::new(lhs),
                Box::new(rhs),
            ),
        }
    }

    fn unop(unop: UnOpAst, op: Token, subexpr: ExprAst) -> ExprAst {
        ExprAst {
            span: op.span.merged_with(subexpr.span),
            node: ExprAstNode::UnOp((op.span, unop), Box::new(subexpr)),
        }
    }
}

//===========================================================================//

/// One node in the abstract syntax tree for an expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExprAstNode {
    /// A function application.
    Apply(Box<ExprAst>, Box<ExprAst>),
    /// A binary operation between two subexpressions.
    BinOp((SrcSpan, BinOpAst), Box<ExprAst>, Box<ExprAst>),
    /// An boolean literal.
    BoolLiteral(bool),
    /// A ternary conditional.
    Conditional(Box<ExprAst>, Box<ExprAst>, Box<ExprAst>),
    /// A "here" label.
    HereLabel,
    /// An identifier.
    Identifier(Rc<str>),
    /// An indexing operation (e.g. into a list).
    Index(SrcSpan, Box<ExprAst>, Box<ExprAst>),
    /// An integer literal.
    IntLiteral(BigInt),
    /// A list literal.
    ListLiteral(Vec<ExprAst>),
    /// A macro placeholder.
    Placeholder(Rc<str>),
    /// A string literal.
    StrLiteral(Rc<str>),
    /// A tuple literal.
    TupleLiteral(Vec<ExprAst>),
    /// A unary operation on a subexpression.
    UnOp((SrcSpan, UnOpAst), Box<ExprAst>),
}

//===========================================================================//

fn bool_literal<'a>()
-> impl Parser<'a, &'a [Token], ExprAst, Extra<'a>> + Clone {
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::BoolLiteral(boolean) = token.value {
                Ok(ExprAst {
                    span: token.span,
                    node: ExprAstNode::BoolLiteral(boolean),
                })
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("boolean literal")
}

fn int_literal<'a>() -> impl Parser<'a, &'a [Token], ExprAst, Extra<'a>> + Clone
{
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::IntLiteral(int) = token.value {
                Ok(ExprAst {
                    span: token.span,
                    node: ExprAstNode::IntLiteral(int),
                })
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("integer literal")
}

fn str_literal<'a>() -> impl Parser<'a, &'a [Token], ExprAst, Extra<'a>> + Clone
{
    chumsky::prelude::any()
        .try_map(|token: Token, span| {
            if let TokenValue::StrLiteral(string) = token.value {
                Ok(ExprAst {
                    span: token.span,
                    node: ExprAstNode::StrLiteral(string),
                })
            } else {
                Err(chumsky::error::Rich::custom(span, ""))
            }
        })
        .labelled("string literal")
}

//===========================================================================//

#[cfg(test)]
mod tests {
    use super::super::atom::tokenize;
    use super::super::error::ParseResult;
    use super::{BinOpAst, ExprAst, ExprAstNode};
    use crate::error::SrcSpan;
    use num_bigint::BigInt;
    use std::ops::Range;
    use std::rc::Rc;

    fn parse(input: &str) -> ParseResult<ExprAst> {
        ExprAst::parse(&tokenize(input)?)
    }

    fn int_node(value: i32) -> ExprAstNode {
        ExprAstNode::IntLiteral(BigInt::from(value))
    }

    fn int_ast(range: Range<usize>, value: i32) -> ExprAst {
        ExprAst {
            span: SrcSpan::from_byte_range(range),
            node: int_node(value),
        }
    }

    #[test]
    fn identifier() {
        assert_eq!(
            parse("foo"),
            Ok(ExprAst {
                span: SrcSpan::from_byte_range(0..3),
                node: ExprAstNode::Identifier(Rc::from("foo")),
            })
        );
    }

    #[test]
    fn int_literal() {
        assert_eq!(parse("123"), Ok(int_ast(0..3, 123)));
    }

    #[test]
    fn addition() {
        assert_eq!(
            parse("1 + 2 + (3 + 4)"),
            Ok(ExprAst {
                span: SrcSpan::from_byte_range(0..15),
                node: ExprAstNode::BinOp(
                    (SrcSpan::from_byte_range(6..7), BinOpAst::Add),
                    Box::new(ExprAst {
                        span: SrcSpan::from_byte_range(0..5),
                        node: ExprAstNode::BinOp(
                            (SrcSpan::from_byte_range(2..3), BinOpAst::Add),
                            Box::new(int_ast(0..1, 1)),
                            Box::new(int_ast(4..5, 2)),
                        ),
                    }),
                    Box::new(ExprAst {
                        span: SrcSpan::from_byte_range(8..15),
                        node: ExprAstNode::BinOp(
                            (SrcSpan::from_byte_range(11..12), BinOpAst::Add),
                            Box::new(int_ast(9..10, 3)),
                            Box::new(int_ast(13..14, 4)),
                        ),
                    }),
                ),
            })
        );
    }
}

//===========================================================================//
