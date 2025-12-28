//! GML Lexer - Tokenizer for GameMaker Language
//!
//! This crate provides a fast, hand-written lexer for GML that produces
//! a stream of tokens suitable for parsing.

mod token;
mod lexer;

pub use token::{Token, TokenKind, Span};
pub use lexer::Lexer;
