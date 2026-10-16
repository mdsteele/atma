use super::atom::{AtmaParser, symbol};
use super::expr::ExprAst;
use super::id::IdentifierAst;
use crate::lex::TokenValue;
use chumsky::Parser;

//===========================================================================//

/// An attribute on e.g. an assembly chunk or linker config entry.
#[derive(Debug)]
pub struct AttributeAst {
    /// The attribute identifier.
    pub id: IdentifierAst,
    /// The attribute value expression.
    pub expr: ExprAst,
}

impl AttributeAst {
    pub(super) fn parser<'a>() -> impl AtmaParser<'a, Self> {
        IdentifierAst::parser()
            .then_ignore(symbol(TokenValue::Equals))
            .then(ExprAst::parser())
            .map(|(id, expr)| Self { id, expr })
    }
}

//===========================================================================//
