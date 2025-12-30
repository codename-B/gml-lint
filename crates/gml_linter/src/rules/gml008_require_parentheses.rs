//! GML008: Require parentheses around conditions
//!
//! Warns when a condition is not wrapped in parentheses.
//! This promotes better style and consistency.

use crate::diagnostics::{Category, Diagnostic, Location, Fix, Edit};
use crate::parser::{Expr, Stmt};
use crate::{LintContext, Rule, RuleCode, SemanticModel};


pub struct RequireParentheses;

impl Rule for RequireParentheses {
    fn code(&self) -> RuleCode {
        RuleCode("GML008")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "require-parentheses"
    }

    fn description(&self) -> &'static str {
        "Control flow statements should use parentheses around conditions."
    }

    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {

        match stmt {
            Stmt::If { condition, has_parentheses, .. } => {
                self.check_condition(ctx, *has_parentheses, condition, "if", diagnostics);
            }
            Stmt::While { condition, has_parentheses, .. } => {
                self.check_condition(ctx, *has_parentheses, condition, "while", diagnostics);
            }
            Stmt::DoUntil { condition, has_parentheses, .. } => {
                self.check_condition(ctx, *has_parentheses, condition, "until", diagnostics);
            }
            Stmt::Repeat { count, has_parentheses, .. } => {
                self.check_condition(ctx, *has_parentheses, count, "repeat", diagnostics);
            }
            Stmt::With { target, has_parentheses, .. } => {
                self.check_condition(ctx, *has_parentheses, target, "with", diagnostics);
            }
            Stmt::Switch { value, has_parentheses, .. } => {
                self.check_condition(ctx, *has_parentheses, value, "switch", diagnostics);
            }
            _ => {}
        }
    }
}

impl RequireParentheses {
    fn check_condition<'a>(&self, ctx: &LintContext<'a>, has_parentheses: bool, condition: &Expr<'a>, keyword: &str, diagnostics: &mut Vec<Diagnostic>) {
        // Also check Expr::Grouping as a fallback (though parser should handle it)
        if !has_parentheses && !matches!(condition, Expr::Grouping { .. }) {
            let span = condition.span();
            let (line, col) = ctx.offset_to_line_col(span.start);
            
            let mut diag = Diagnostic::hint(
                self.code().0,
                format!("Condition for '{}' should be wrapped in parentheses", keyword),
                Location::new(ctx.file_path(), line, col, span.start, span.end),
            );

            diag = diag.with_fix(Fix {
                message: "Wrap condition in parentheses".to_string(),
                edits: vec![
                    Edit::insert(span.start, "("),
                    Edit::insert(span.end, ")"),
                ],
            });
            
            diagnostics.push(diag);
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_lint;

    #[test]
    fn test_missing_parentheses() {
        assert_lint(
            Box::new(RequireParentheses),
            "if x == 5 { }",
            "GML008",
        );
    }

    #[test]
    fn test_has_parentheses() {
        assert_lint(
            Box::new(RequireParentheses),
            "if (x == 5) { }",
            "",
        );
    }
}
