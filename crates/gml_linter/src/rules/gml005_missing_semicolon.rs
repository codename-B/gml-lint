//! GML005: Missing semicolon
//!
//! While semicolons are optional in GML, they are recommended for consistency.
//! 
//! This rule uses the single-pass visitor pattern for efficient checking.

use gml_diagnostics::{Category, Diagnostic, Edit, Fix, Location};
use gml_parser::Stmt;

use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct MissingSemicolon;

impl Rule for MissingSemicolon {
    fn code(&self) -> RuleCode {
        RuleCode("GML005")
    }

    fn category(&self) -> Category {
        Category::Style
    }

    fn name(&self) -> &'static str {
        "missing-semicolon"
    }

    fn description(&self) -> &'static str {
        "Statements should end with a semicolon."
    }

    // Note: check() is not overridden - we use check_stmt() for single-pass traversal
    
    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {

        let source = ctx.source();
        
        match stmt {
            // Statements that should end with semicolons
            Stmt::VarDecl { span, .. }
            | Stmt::GlobalVarDecl { span, .. }
            | Stmt::Return { span, .. }
            | Stmt::Exit { span, .. }
            | Stmt::Break { span, .. }
            | Stmt::Continue { span, .. }
            | Stmt::Throw { span, .. }
            | Stmt::Expr { span, .. } => {
                check_semicolon(ctx, source, *span, "Statement is missing a semicolon", diagnostics);
            }
            
            // Do-until should also end with semicolon
            Stmt::DoUntil { span, .. } => {
                check_semicolon(ctx, source, *span, "do-until statement is missing a semicolon", diagnostics);
            }
            
            // No semicolon check needed for other statements (visitor handles recursion)
            _ => {}
        }
    }
}

/// Helper to check if a span ends with semicolon and add diagnostic if not
fn check_semicolon<'a>(
    ctx: &LintContext<'a>,

    source: &str,
    span: gml_lexer::Span,
    message: &str,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let end_byte = span.end as usize;
    if end_byte > 0 && end_byte <= source.len() {
        let last_char = source.as_bytes().get(end_byte.saturating_sub(1));
        if last_char != Some(&b';') {
            let (line, col) = ctx.offset_to_line_col(span.end);
            let mut diag = Diagnostic::style(
                "GML005",
                message,
                Location::new(ctx.file_path(), line, col, span.end, span.end),
            );
            
            // Add auto-fix
            diag = diag.with_fix(Fix {
                message: "Add semicolon".to_string(),
                edits: vec![Edit::insert(span.end, ";")],
            });
            
            diagnostics.push(diag);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_lint_fix;

    #[test]
    fn test_missing_semicolon_basic() {
        assert_lint_fix(Box::new(MissingSemicolon), "var x = 42", "var x = 42;");
    }

    #[test]
    fn test_missing_semicolon_with_comment() {
        // Should insert semicolon BEFORE the comment
        assert_lint_fix(Box::new(MissingSemicolon), "x = 42 // comment", "x = 42; // comment");
    }

    #[test]
    fn test_missing_semicolon_with_trailing_space() {
        assert_lint_fix(Box::new(MissingSemicolon), "return true  ", "return true;  ");
    }

    #[test]
    fn test_semicolon_already_exists() {
        assert_lint_fix(Box::new(MissingSemicolon), "var x = 42;", "var x = 42;");
    }

    #[test]
    fn test_block_does_not_get_semicolon() {
        assert_lint_fix(Box::new(MissingSemicolon), "{ x = 1; }", "{ x = 1; }");
    }

    #[test]
    fn test_if_stmt_no_semicolon() {
        assert_lint_fix(Box::new(MissingSemicolon), "if (true) { x = 1; }", "if (true) { x = 1; }");
    }

    #[test]
    fn test_do_until_semicolon() {
        assert_lint_fix(Box::new(MissingSemicolon), "do { x++; } until (x > 10)", "do { x++; } until (x > 10);");
    }
}
