//! GML004: Unreachable code
//!
//! Detects code that can never be executed.
//!
//! This rule uses the single-pass visitor pattern. When it sees a block-containing
//! statement, it checks if any code follows a terminator within that block.

use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::{Block, Stmt};
use crate::{LintContext, Rule, RuleCode, SemanticModel};


pub struct UnreachableCode;

impl Rule for UnreachableCode {
    fn code(&self) -> RuleCode {
        RuleCode("GML004")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "unreachable-code"
    }

    fn description(&self) -> &'static str {
        "Code after return, break, continue, or exit is unreachable."
    }

    // Note: check() is not overridden - we use check_stmt() for single-pass traversal
    
    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {
        // When we encounter a block-containing statement, check its block(s) for unreachable code
        match stmt {
            Stmt::FunctionDecl { body, .. } => {
                check_block_for_unreachable(ctx, body, diagnostics);
            }
            
            Stmt::If { then_branch, .. } => {
                check_block_for_unreachable(ctx, then_branch, diagnostics);
                // else_branch is handled as a separate statement by the visitor
            }
            
            Stmt::For { body, .. }
            | Stmt::While { body, .. }
            | Stmt::DoUntil { body, .. }
            | Stmt::Repeat { body, .. }
            | Stmt::With { body, .. } => {
                check_block_for_unreachable(ctx, body, diagnostics);
            }
            
            Stmt::Switch { cases, .. } => {
                for case in cases {
                    check_stmt_list_for_unreachable(ctx, &case.body, diagnostics);
                }
            }
            
            Stmt::TryCatch { try_block, catch_block, finally_block, .. } => {
                check_block_for_unreachable(ctx, try_block, diagnostics);
                if let Some(catch) = catch_block {
                    check_block_for_unreachable(ctx, catch, diagnostics);
                }
                if let Some(finally) = finally_block {
                    check_block_for_unreachable(ctx, finally, diagnostics);
                }
            }
            
            Stmt::Block(block) => {
                check_block_for_unreachable(ctx, block, diagnostics);
            }
            
            _ => {}
        }
    }
}

/// Check a block for unreachable code (code after a terminator)
fn check_block_for_unreachable(ctx: &LintContext, block: &Block, diagnostics: &mut Vec<Diagnostic>) {
    check_stmt_list_for_unreachable(ctx, &block.statements, diagnostics);
}

/// Check a statement list for unreachable code
fn check_stmt_list_for_unreachable(ctx: &LintContext, statements: &[Stmt], diagnostics: &mut Vec<Diagnostic>) {
    let mut found_terminator = false;
    let mut terminator_name = "";
    
    for stmt in statements {
        if found_terminator {
            // Skip empty statements (preprocessor directives, stray semicolons)
            if matches!(stmt, Stmt::Empty { .. }) {
                continue;
            }
            
            // Skip break statements (often used redundantly after return in switches)
            if matches!(stmt, Stmt::Break { .. }) {
                continue;
            }

            // This statement is unreachable
            let span = get_stmt_span(stmt);
            if let Some(span) = span {
                let (line, col) = ctx.offset_to_line_col(span.0);
                diagnostics.push(Diagnostic::warning(
                    "GML004",
                    format!("Unreachable code after '{}'", terminator_name),
                    Location::new(ctx.file_path(), line, col, span.0, span.1),
                ));
            }
            // Only report once per unreachable section
            break;
        }
        
        // Check if this statement terminates control flow
        if is_terminator(stmt) {
            found_terminator = true;
            terminator_name = get_terminator_name(stmt);
        }
    }
}

fn is_terminator(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Return { .. } | Stmt::Exit { .. } | Stmt::Break { .. } | Stmt::Continue { .. } | Stmt::Throw { .. }
    )
}

fn get_terminator_name(stmt: &Stmt) -> &'static str {
    match stmt {
        Stmt::Return { .. } => "return",
        Stmt::Exit { .. } => "exit",
        Stmt::Break { .. } => "break",
        Stmt::Continue { .. } => "continue",
        Stmt::Throw { .. } => "throw",
        _ => "unknown",
    }
}

fn get_stmt_span(stmt: &Stmt) -> Option<(u32, u32)> {
    match stmt {
        Stmt::VarDecl { span, .. } => Some((span.start, span.end)),
        Stmt::GlobalVarDecl { span, .. } => Some((span.start, span.end)),
        Stmt::FunctionDecl { span, .. } => Some((span.start, span.end)),
        Stmt::If { span, .. } => Some((span.start, span.end)),
        Stmt::For { span, .. } => Some((span.start, span.end)),
        Stmt::While { span, .. } => Some((span.start, span.end)),
        Stmt::DoUntil { span, .. } => Some((span.start, span.end)),
        Stmt::Repeat { span, .. } => Some((span.start, span.end)),
        Stmt::With { span, .. } => Some((span.start, span.end)),
        Stmt::Switch { span, .. } => Some((span.start, span.end)),
        Stmt::Return { span, .. } => Some((span.start, span.end)),
        Stmt::Exit { span, .. } => Some((span.start, span.end)),
        Stmt::Break { span, .. } => Some((span.start, span.end)),
        Stmt::Continue { span, .. } => Some((span.start, span.end)),
        Stmt::Throw { span, .. } => Some((span.start, span.end)),
        Stmt::TryCatch { span, .. } => Some((span.start, span.end)),
        Stmt::Enum { span, .. } => Some((span.start, span.end)),
        Stmt::Block(block) => Some((block.span.start, block.span.end)),
        Stmt::Expr { span, .. } => Some((span.start, span.end)),
        Stmt::Empty { span, .. } => Some((span.start, span.end)),
    }
}
