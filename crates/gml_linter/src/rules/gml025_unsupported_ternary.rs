//! GML025: Unparenthesized nested ternary operator
//!
//! Detects nested ternary operators that are not wrapped in parentheses.
//! Example: `x = cond1 ? a : cond2 ? b : c;` is invalid
//! But:     `x = cond1 ? a : (cond2 ? b : c);` is valid
//!
//! Auto-fix: Wraps the nested ternary in parentheses

use crate::diagnostics::{Category, Diagnostic, Edit, Fix, Location};
use crate::parser::Expr;
use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct UnsupportedTernary;

impl Rule for UnsupportedTernary {
    fn code(&self) -> RuleCode {
        RuleCode("GML025")
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn name(&self) -> &'static str {
        "unparenthesized-nested-ternary"
    }

    fn description(&self) -> &'static str {
        "Detects nested ternary operators that are not wrapped in parentheses"
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Expr::Ternary { then_expr, else_expr, span, .. } = expr {
            // Check then_expr - a raw ternary without parentheses is invalid
            if let Expr::Ternary { span: inner_span, .. } = then_expr.as_ref() {
                let (line, col) = ctx.offset_to_line_col(span.start);
                let inner_text = &ctx.source()[inner_span.start as usize..inner_span.end as usize];
                diagnostics.push(Diagnostic::error(
                    "GML025",
                    "Nested ternary must be wrapped in parentheses",
                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                ).with_fix(Fix {
                    message: "Wrap nested ternary in parentheses".to_string(),
                    edits: vec![Edit {
                        start: inner_span.start,
                        end: inner_span.end,
                        replacement: format!("({})", inner_text),
                    }],
                }));
                return;
            }

            // Check else_expr - a raw ternary without parentheses is invalid
            if let Expr::Ternary { span: inner_span, .. } = else_expr.as_ref() {
                let (line, col) = ctx.offset_to_line_col(span.start);
                let inner_text = &ctx.source()[inner_span.start as usize..inner_span.end as usize];
                diagnostics.push(Diagnostic::error(
                    "GML025",
                    "Nested ternary must be wrapped in parentheses",
                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                ).with_fix(Fix {
                    message: "Wrap nested ternary in parentheses".to_string(),
                    edits: vec![Edit {
                        start: inner_span.start,
                        end: inner_span.end,
                        replacement: format!("({})", inner_text),
                    }],
                }));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::UnsupportedTernary;
    use crate::test_utils::*;

    #[test]
    fn test_unparenthesized_nested_ternary_detected() {
        assert_lint(
            Box::new(UnsupportedTernary),
            "var x = a ? b : c ? d : e;",
            "GML025"
        );
    }

    #[test]
    fn test_parenthesized_nested_ternary_allowed() {
        assert_lint(
            Box::new(UnsupportedTernary),
            "var x = a ? b : (c ? d : e);",
            "" // No error
        );
    }

    #[test]
    fn test_complex_parenthesized_allowed() {
        assert_lint(
            Box::new(UnsupportedTernary),
            "draw_get_valign() == fa_bottom ? _y : (draw_get_valign() == fa_top ? _y - 10 : _y + 5);",
            "" // No error
        );
    }

    #[test]
    fn test_simple_ternary_allowed() {
        assert_lint(
            Box::new(UnsupportedTernary),
            "var x = condition ? true : false;",
            "" // No error
        );
    }

    #[test]
    fn test_autofix_wraps_in_parentheses() {
        assert_lint_fix(
            Box::new(UnsupportedTernary),
            "var x = a ? b : c ? d : e;",
            "var x = a ? b : (c ? d : e);"
        );
    }
}
