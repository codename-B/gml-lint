//! GML027: Redundant boolean comparison
//!
//! Checks for comparisons with boolean literals, like `x == true` or `y != false`.
//! These should be simplified to `x` or `!y`.

use crate::parser::{BinaryOp, Expr, Literal};
use crate::Rule;
use crate::diagnostics::{Category, Diagnostic, Location, Fix, Edit};
use crate::LintContext;
use crate::RuleCode;
use crate::SemanticModel;

#[derive(Debug)]
pub struct RedundantBooleanComparison;

impl Rule for RedundantBooleanComparison {
    fn code(&self) -> RuleCode {
        RuleCode("GML027")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "RedundantBooleanComparison"
    }

    fn description(&self) -> &'static str {
        "Redundant comparison with a boolean literal."
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Expr::Binary { left, op, right, span } = expr {
            match op {
                BinaryOp::Equal | BinaryOp::NotEqual => {
                    // Check which side is a boolean literal and extract the value
                    let left_bool = match left.as_ref() {
                        Expr::Literal { value: Literal::Boolean(b), .. } => Some(*b),
                        _ => None,
                    };
                    let right_bool = match right.as_ref() {
                        Expr::Literal { value: Literal::Boolean(b), .. } => Some(*b),
                        _ => None,
                    };

                    // Get the non-literal expression and the boolean value
                    let (target_expr, bool_val) = match (left_bool, right_bool) {
                        (Some(b), None) => (right.as_ref(), b),
                        (None, Some(b)) => (left.as_ref(), b),
                        _ => return, // Either no bool literal or both are literals
                    };

                    // Get the source text of the target expression
                    let target_text = &ctx.source()[target_expr.span().start as usize..target_expr.span().end as usize];

                    // Check if the target expression needs parentheses when negated
                    let needs_parens = !matches!(target_expr, 
                        Expr::Identifier { .. } | 
                        Expr::Literal { .. } | 
                        Expr::Call { .. } | 
                        Expr::Member { .. } | 
                        Expr::Index { .. } | 
                        Expr::Grouping { .. }
                    );

                    // Determine the simplified expression:
                    // x == true  -> x
                    // x == false -> !x
                    // x != true  -> !x
                    // x != false -> x
                    let needs_negation = match (op, bool_val) {
                        (BinaryOp::Equal, true) => false,   // x == true -> x
                        (BinaryOp::Equal, false) => true,   // x == false -> !x
                        (BinaryOp::NotEqual, true) => true, // x != true -> !x
                        (BinaryOp::NotEqual, false) => false, // x != false -> x
                        _ => unreachable!(),
                    };

                    let replacement = if needs_negation {
                        if needs_parens {
                            format!("!({})", target_text)
                        } else {
                            format!("!{}", target_text)
                        }
                    } else {
                        target_text.to_string()
                    };

                    let suggestion = if needs_negation {
                        format!("Simplify to '!{}'", target_text)
                    } else {
                        format!("Simplify to '{}'", target_text)
                    };

                    let (line, col) = ctx.offset_to_line_col(span.start);
                    diagnostics.push(Diagnostic::warning(
                        self.code().as_str(),
                        "Redundant comparison with boolean literal. Simplify the expression.",
                        Location::new(ctx.file_path(), line, col, span.start, span.end),
                    ).with_fix(Fix {
                        message: suggestion,
                        edits: vec![Edit::new(span.start, span.end, replacement)],
                    }));
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{assert_lint, assert_lint_fix};

    #[test]
    fn test_redundant_comparison() {
        assert_lint(Box::new(RedundantBooleanComparison), "x == true", "GML027");
        assert_lint(Box::new(RedundantBooleanComparison), "true == x", "GML027");
        assert_lint(Box::new(RedundantBooleanComparison), "x != false", "GML027");
        assert_lint(Box::new(RedundantBooleanComparison), "false != x", "GML027");
        assert_lint(Box::new(RedundantBooleanComparison), "x == false", "GML027");
    }

    #[test]
    fn test_autofix_equal_true() {
        assert_lint_fix(Box::new(RedundantBooleanComparison), "x == true", "x");
        assert_lint_fix(Box::new(RedundantBooleanComparison), "true == x", "x");
    }

    #[test]
    fn test_autofix_equal_false() {
        assert_lint_fix(Box::new(RedundantBooleanComparison), "x == false", "!x");
        assert_lint_fix(Box::new(RedundantBooleanComparison), "false == x", "!x");
    }

    #[test]
    fn test_autofix_not_equal_true() {
        assert_lint_fix(Box::new(RedundantBooleanComparison), "x != true", "!x");
        assert_lint_fix(Box::new(RedundantBooleanComparison), "true != x", "!x");
    }

    #[test]
    fn test_autofix_not_equal_false() {
        assert_lint_fix(Box::new(RedundantBooleanComparison), "x != false", "x");
        assert_lint_fix(Box::new(RedundantBooleanComparison), "false != x", "x");
    }

    #[test]
    fn test_autofix_complex_expression() {
        assert_lint_fix(Box::new(RedundantBooleanComparison), "foo() == true", "foo()");
        assert_lint_fix(Box::new(RedundantBooleanComparison), "a.b == false", "!a.b");
        assert_lint_fix(Box::new(RedundantBooleanComparison), "arr[0] != true", "!arr[0]");
    }
}
