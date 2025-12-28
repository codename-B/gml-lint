//! GML Parser - AST builder for GameMaker Language
//!
//! A hand-written recursive descent parser that produces an AST from tokens.

mod ast;
mod parser;

pub use ast::*;
pub use parser::Parser;
