//! Facilities for tokenizing source code.

mod error;
mod lexer;
mod string;
mod token;

pub use error::LexerError;
pub use lexer::TokenLexer;
pub use token::{Token, TokenValue};

//===========================================================================//
