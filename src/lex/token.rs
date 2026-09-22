use crate::error::SrcSpan;
use num_bigint::BigInt;
use std::rc::Rc;

//===========================================================================//

/// The contents of a single lexical token.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenValue {
    /// A "`&`" symbol.
    And,
    /// A "`&&`" symbol.
    AndAnd,
    /// A "`<-`" symbol.
    ArrowLeft,
    /// A "`!`" symbol.
    Bang,
    /// A "`!=`" symbol.
    BangEquals,
    /// An boolean literal.
    BoolLiteral(bool),
    /// A "`}`" symbol.
    BraceClose,
    /// A "`{`" symbol.
    BraceOpen,
    /// A "`]`" symbol.
    BracketClose,
    /// A "`[`" symbol.
    BracketOpen,
    /// A built-in constant.
    Builtin(Rc<str>),
    /// A "`^`" symbol.
    Caret,
    /// A "`:`" symbol.
    Colon,
    /// A "`::`" symbol.
    ColonColon,
    /// A "`,`" symbol.
    Comma,
    /// An assembler directive.
    Directive(Rc<str>),
    /// A "`$v`" symbol.
    DollarDown,
    /// A "`$<`" symbol.
    DollarLeft,
    /// A "`$>`" symbol.
    DollarRight,
    /// A "`$^`" symbol.
    DollarUp,
    /// A "`==`" symbol.
    EqualsEquals,
    /// A "`=`" symbol.
    Equals,
    /// A "`>=`" symbol.
    GreaterEquals,
    /// A "`>>`" symbol.
    GreaterGreater,
    /// A "`>`" symbol.
    GreaterThan,
    /// An identifier or keyword.
    Identifier(Rc<str>),
    /// An integer literal.
    IntLiteral(BigInt),
    /// A "`<=`" symbol.
    LessEquals,
    /// A "`<<`" symbol.
    LessLess,
    /// A "`<`" symbol.
    LessThan,
    /// A linebreak (that wasn't suppressed, e.g. by a backslash).
    Linebreak,
    /// A "`-`" symbol.
    Minus,
    /// A "`|`" symbol.
    Or,
    /// A "`||`" symbol.
    OrOr,
    /// A "`)`" symbol.
    ParenClose,
    /// A "`(`" symbol.
    ParenOpen,
    /// A "`%`" symbol.
    Percent,
    /// A "`%%`" symbol.
    PercentPercent,
    /// A placeholder in a macro definition.
    Placeholder(Rc<str>),
    /// A "`+`" symbol.
    Plus,
    /// A "`++`" symbol.
    PlusPlus,
    /// A "`#`" symbol.
    Pound,
    /// A "`?`" symbol.
    Question,
    /// A "`/`" symbol.
    Slash,
    /// A "`*`" symbol.
    Star,
    /// A "`**`" symbol.
    StarStar,
    /// A string literal.
    StrLiteral(Rc<str>),
    /// A "`~`" symbol.
    Tilde,
    /// A "`_`" symbol.
    Underscore,
}

impl TokenValue {
    /// Returns the human-readable name for this kind of token.
    pub fn name(&self) -> &'static str {
        match &self {
            TokenValue::And => "`&`",
            TokenValue::AndAnd => "`&&`",
            TokenValue::ArrowLeft => "`<-`",
            TokenValue::Bang => "`!`",
            TokenValue::BangEquals => "`!=`",
            TokenValue::BoolLiteral(_) => "boolean literal",
            TokenValue::BraceClose => "`}`",
            TokenValue::BraceOpen => "`{`",
            TokenValue::BracketClose => "`]`",
            TokenValue::BracketOpen => "`[`",
            TokenValue::Builtin(_) => "builtin",
            TokenValue::Caret => "`^`",
            TokenValue::Colon => "`:`",
            TokenValue::ColonColon => "`::`",
            TokenValue::Comma => "`,`",
            TokenValue::Directive(_) => "directive",
            TokenValue::DollarDown => "`$v`",
            TokenValue::DollarLeft => "`$<`",
            TokenValue::DollarRight => "`$>`",
            TokenValue::DollarUp => "`$^`",
            TokenValue::EqualsEquals => "`==`",
            TokenValue::Equals => "`=`",
            TokenValue::GreaterEquals => "`>=`",
            TokenValue::GreaterGreater => "`>>`",
            TokenValue::GreaterThan => "`>`",
            TokenValue::Identifier(_) => "identifier",
            TokenValue::IntLiteral(_) => "integer literal",
            TokenValue::LessEquals => "`<=`",
            TokenValue::LessLess => "`<<`",
            TokenValue::LessThan => "`<`",
            TokenValue::Linebreak => "linebreak",
            TokenValue::Minus => "`-`",
            TokenValue::Or => "`|`",
            TokenValue::OrOr => "`||`",
            TokenValue::ParenClose => "`)`",
            TokenValue::ParenOpen => "`(`",
            TokenValue::Percent => "`%`",
            TokenValue::PercentPercent => "`%%`",
            TokenValue::Placeholder(_) => "placeholder",
            TokenValue::Plus => "`+`",
            TokenValue::PlusPlus => "`++`",
            TokenValue::Pound => "`#`",
            TokenValue::Question => "`?`",
            TokenValue::Slash => "`/`",
            TokenValue::Star => "`*`",
            TokenValue::StarStar => "`**`",
            TokenValue::StrLiteral(_) => "string literal",
            TokenValue::Tilde => "`~`",
            TokenValue::Underscore => "`_`",
        }
    }
}

//===========================================================================//

/// A single lexical token, including location information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    /// The locaiton in the file of the start of the token.
    pub span: SrcSpan,
    /// The contents of the token.
    pub value: TokenValue,
}

//===========================================================================//
