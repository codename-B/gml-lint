//! GML Linter - Core linting logic and rules
//!
//! This crate provides the linting infrastructure and built-in rules.
//!
//! Architecture (optimized single-pass):
//! 1. Parse source into AST
//! 2. Run unified Checker which does EVERYTHING in one pass:
//!    - Builds SemanticModel (bindings/references)
//!    - Runs all check_stmt/check_expr rules
//!    - Validates control flow (break/continue/return)
//!    - Runs semantic rules (unused variables)

pub mod lexer;
pub mod parser;
pub mod diagnostics;
pub mod cache;
pub mod project;
pub mod semantic;
pub mod walker;

mod checker;
mod context;
pub mod keywords;
mod rule;
pub mod rules;
pub mod semantic_model;
#[cfg(test)]
pub mod test_utils;
pub mod builtins;



pub use context::{LintContext, SymbolProvider, DefaultSymbolProvider};
pub use rule::{Rule, RuleCode};
pub use semantic_model::SemanticModel;
pub use diagnostics::{Diagnostic, Severity, Category, Location};
pub use parser::Parser;

use std::sync::atomic::{AtomicU64, Ordering};

// Global statistics for profiling
pub static CUMULATIVE_LEX_TIME_NS: AtomicU64 = AtomicU64::new(0);
pub static CUMULATIVE_PARSE_TIME_NS: AtomicU64 = AtomicU64::new(0);
pub static CUMULATIVE_LINT_TIME_NS: AtomicU64 = AtomicU64::new(0);

/// Get the current profiling statistics in milliseconds
pub fn get_statistics_ms() -> (f64, f64, f64) {
    let lex = CUMULATIVE_LEX_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
    let parse = CUMULATIVE_PARSE_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
    let lint = CUMULATIVE_LINT_TIME_NS.load(Ordering::Relaxed) as f64 / 1_000_000.0;
    (lex, parse, lint)
}

/// Reset the profiling statistics
pub fn reset_statistics() {
    CUMULATIVE_LEX_TIME_NS.store(0, Ordering::Relaxed);
    CUMULATIVE_PARSE_TIME_NS.store(0, Ordering::Relaxed);
    CUMULATIVE_LINT_TIME_NS.store(0, Ordering::Relaxed);
}

/// Convert byte offset to (line, column) - 1-indexed
fn offset_to_line_col(source: &str, offset: u32) -> (u32, u32) {
    let offset = offset as usize;
    let mut line = 1;
    let mut col = 1;
    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Lint a source file using unified single-pass checker
pub fn lint_source(source: &str, file_path: &str) -> Vec<Diagnostic> {
    let all_rules = rules::all_rules();
    let provider = DefaultSymbolProvider;
    lint_source_with_rules(source, file_path, &all_rules, &provider)
}

/// Lint a source file with a pre-allocated set of rules
pub fn lint_source_with_rules(source: &str, file_path: &str, rules: &[Box<dyn Rule>], symbol_provider: &dyn SymbolProvider) -> Vec<Diagnostic> {
    // Lexing
    let start_lex = std::time::Instant::now();
    let parser = Parser::new(source);
    let lex_time = start_lex.elapsed();

    // Parsing
    let start_parse = std::time::Instant::now();
    let program = match parser.parse() {
        Ok(program) => program,
        Err(errors) => {
            return errors
                .into_iter()
                .map(|e| {
                    // Compute line/col from span offset
                    let (line, col) = offset_to_line_col(source, e.span.start);
                    Diagnostic::error(
                        "E001",
                        e.message,
                        Location::new(file_path, line, col, e.span.start, e.span.end),
                    )
                })
                .collect();
        }
    };
    let parse_time = start_parse.elapsed();

    // Create lint context
    let start_lint = std::time::Instant::now();
    let db = semantic::db::Database::new();
    let ctx = LintContext::new(source, file_path, &program, symbol_provider, &db);
    
    // Run unified single-pass checker
    let diags = checker::check_program(&ctx, &program, rules);
    let lint_time = start_lint.elapsed();
    
    // Update global statistics
    CUMULATIVE_LEX_TIME_NS.fetch_add(lex_time.as_nanos() as u64, Ordering::Relaxed);
    CUMULATIVE_PARSE_TIME_NS.fetch_add(parse_time.as_nanos() as u64, Ordering::Relaxed);
    CUMULATIVE_LINT_TIME_NS.fetch_add(lint_time.as_nanos() as u64, Ordering::Relaxed);
    
    diags
}





/// Lint a file from disk
pub fn lint_file(path: &str) -> Result<Vec<Diagnostic>, std::io::Error> {
    let source = std::fs::read_to_string(path)?;
    Ok(lint_source(&source, path))
}
