//! GML007: Assignment in condition (no-single-equals)
//!
//! Warns when a single `=` is used in a context where `==` was likely intended.
//! This catches common bugs like `if (x = 5)` instead of `if (x == 5)`.
//!
//! This rule uses the single-pass visitor pattern.

use crate::diagnostics::{Category, Diagnostic, Edit, Fix, Location};
use crate::parser::{AssignOp, BinaryOp, Expr, Stmt};
use crate::{LintContext, Rule, RuleCode, SemanticModel};


pub struct NoSingleEquals;

impl Rule for NoSingleEquals {
    fn code(&self) -> RuleCode {
        RuleCode("GML007")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "no-single-equals"
    }

    fn description(&self) -> &'static str {
        "Warns when assignment `=` might be confused with comparison `==`"
    }

    // Note: check() is not overridden - we use check_stmt() for single-pass traversal
    
    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {

        // Check conditions in control flow statements
        match stmt {
            Stmt::If { condition, .. } => {
                check_condition_for_assignment(ctx, condition, diagnostics);
            }
            Stmt::While { condition, .. } => {
                check_condition_for_assignment(ctx, condition, diagnostics);
            }
            Stmt::DoUntil { condition, .. } => {
                check_condition_for_assignment(ctx, condition, diagnostics);
            }
            Stmt::For { condition: Some(condition), .. } => {
                check_condition_for_assignment(ctx, condition, diagnostics);
            }
            _ => {}
        }
    }
    
    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        // Check for assignment using ==
        // Check for assignment using ==
        // (Removed invalid check for AssignOp::Equal)

        // Check ternary conditions
        if let Expr::Ternary { condition, .. } = expr {
            check_condition_for_assignment(ctx, condition, diagnostics);
        }
    }
}

/// Check if an expression contains an assignment that might be intended as comparison
fn check_condition_for_assignment(ctx: &LintContext, expr: &Expr, diagnostics: &mut Vec<Diagnostic>) {

    match expr {
        // Direct assignment in condition: if (x = 5)
        Expr::Assignment { span, op, .. } => {
            // Only warn for simple `=`, not for `+=`, `-=`, etc.
            if *op == AssignOp::Assign {
                let (line, col) = ctx.offset_to_line_col(span.start);
                
                // Find the position of the `=` sign for the fix
                let eq_start = find_equals_position(ctx.source(), span.start, span.end);
                
                let mut diag = Diagnostic::warning(
                    "GML007",
                    "Assignment `=` in condition - did you mean `==`?".to_string(),
                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                );
                
                // Add auto-fix to change = to ==
                if let Some(eq_pos) = eq_start {
                    diag = diag.with_fix(Fix {
                        message: "Change to comparison `==`".to_string(),
                        edits: vec![Edit::new(eq_pos, eq_pos + 1, "==")],
                    });
                }
                
                diagnostics.push(diag);
            }
        }
        
        // Check nested expressions - assignments inside other expressions
        Expr::Grouping { expr: inner, .. } => {
            check_condition_for_assignment(ctx, inner, diagnostics);
        }
        
        // Check logical operators - if (a = 1 && b = 2)
        Expr::Binary { left, right, op, .. } => {
            if matches!(op, BinaryOp::And | BinaryOp::Or) {
                check_condition_for_assignment(ctx, left, diagnostics);
                check_condition_for_assignment(ctx, right, diagnostics);
            }
        }
        
        _ => {}
    }
}


/// Find the position of the `=` sign in an assignment expression
fn find_equals_position(source: &str, start: u32, end: u32) -> Option<u32> {
    let range_start = start as usize;
    let range_end = (end as usize).min(source.len());
    
    if range_start >= source.len() || range_start >= range_end {
        return None;
    }
    
    let slice = &source[range_start..range_end];

    
    // Find the first `=` that is not part of `==`, `!=`, `<=`, `>=`, etc.
    let bytes = slice.as_bytes();
    for (i, &ch) in bytes.iter().enumerate() {
        if ch == b'=' {
            // Check it's not part of ==, !=, <=, >=
            let prev = if i > 0 { bytes.get(i - 1).copied() } else { None };
            let next = bytes.get(i + 1).copied();
            
            // Skip if it's ==, !=, <=, >=
            if next == Some(b'=') {
                continue;
            }
            // Skip if it's +=, -=, *=, /=, etc.
            if matches!(prev, Some(b'+') | Some(b'-') | Some(b'*') | Some(b'/') | Some(b'%') | Some(b'&') | Some(b'|') | Some(b'^') | Some(b'!') | Some(b'<') | Some(b'>')) {
                continue;
            }
            
            let pos = Some(start + i as u32);
            return pos;

        }
    }
    
    None

}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_lint_fix;
    
    #[test]
    fn test_assignment_in_if_condition() {
        assert_lint_fix(
            Box::new(NoSingleEquals),
            "if (x = 5) { }",
            "if (x == 5) { }",
        );
    }
    
    #[test]
    fn test_comparison_is_fine() {
        assert_lint_fix(
            Box::new(NoSingleEquals),
            "if (x == 5) { }",
            "if (x == 5) { }",  // No change expected
        );
    }
}
