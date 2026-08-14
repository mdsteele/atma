//! Facilities for parsing identifiers and declarations.

use super::atom::{Context, Extra, parse_tokens};
use super::error::ParseResult;
use super::lex::{Token, TokenValue};
use crate::error::SrcSpan;
use chumsky::{self, ConfigParser, Parser};
use std::rc::Rc;

//===========================================================================//

/// The kind of variable declaration.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DeclarationKind {
    /// Declares a new constant.
    Let,
    /// Declares a new variable.
    Var,
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

#[cfg(test)]
mod tests {
    use super::super::atom::tokenize;
    use super::super::error::ParseResult;
    use super::super::expr::{ExprAst, ExprAstNode};
    use crate::error::SrcSpan;
    use std::rc::Rc;

    fn parse(input: &str) -> ParseResult<ExprAst> {
        ExprAst::parse(&tokenize(input)?)
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
}

//===========================================================================//
