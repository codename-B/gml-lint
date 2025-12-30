//! GML009: Semicolon before branch
//!
//! Detects semicolons between a condition and its body.
//! This is almost always a bug.

use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::Stmt;
use crate::{LintContext, Rule, RuleCode, SemanticModel};


pub struct SemicolonBeforeBranch;

impl Rule for SemicolonBeforeBranch {
    fn code(&self) -> RuleCode {
        RuleCode("GML009")
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn name(&self) -> &'static str {
        "semicolon-before-branch"
    }

    fn description(&self) -> &'static str {
        "Detects semicolons between a condition and its body"
    }

    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {

        match stmt {
            Stmt::If { then_branch, .. } => {
                check_semicolon(ctx, then_branch, diagnostics);
            }
            Stmt::While { body, .. } => {
                check_semicolon(ctx, body, diagnostics);
            }
            Stmt::Repeat { body, .. } => {
                check_semicolon(ctx, body, diagnostics);
            }
            Stmt::With { body, .. } => {
                check_semicolon(ctx, body, diagnostics);
            }
             Stmt::For { body, .. } => {
                check_semicolon(ctx, body, diagnostics);
            }
            _ => {}
        }
    }
}

fn check_semicolon(ctx: &LintContext, body: &crate::parser::Block, diagnostics: &mut Vec<Diagnostic>) {
    // Only flag if the body has EXACTLY one statement
    if body.statements.len() == 1 {
        let first = &body.statements[0];
        if matches!(first, Stmt::Empty { .. }) {
            // Only flag if it looks like a semicolon (length 1)
            // Directives like #region are also Stmt::Empty but have longer spans
            if first.span().end - first.span().start == 1 {
                // Double check it's not a 1-char macro or something (unlikely)
                // But generally ; is the only 1-char Stmt::Empty
                report(ctx, first.span().start, first.span().end, diagnostics);
            }
        }
    }
}

fn report(ctx: &LintContext, start: u32, end: u32, diagnostics: &mut Vec<Diagnostic>) {
    let (line, col) = ctx.offset_to_line_col(start);
    diagnostics.push(Diagnostic::error(
        "GML009",
        "Semicolon before branch - this will cause the branch to execute unconditionally".to_string(),
        Location::new(ctx.file_path(), line, col, start, end),
    ));
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_lint;

    #[test]
    fn test_semicolon_before_if() {
        assert_lint(
            Box::new(SemicolonBeforeBranch),
            "if (x == 5); { do_thing(); }",
            "GML009",
        );
    }
}
