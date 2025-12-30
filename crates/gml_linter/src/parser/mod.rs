//! GML Parser - AST builder for GameMaker Language
//!
//! A hand-written recursive descent parser that produces an AST from tokens.

mod ast;
mod parse;

pub use ast::*;
pub use parse::Parser;
