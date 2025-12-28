//! GML011: Check argument counts
//!
//! Validates that function calls match the number of parameters in their declarations.

use gml_diagnostics::{Category, Diagnostic, Location};
use gml_parser::Expr;

use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct CheckArgumentCounts;

impl Rule for CheckArgumentCounts {
    fn code(&self) -> RuleCode {
        RuleCode("GML011")
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn name(&self) -> &'static str {
        "check-argument-counts"
    }

    fn description(&self) -> &'static str {
        "Checks that function calls have the correct number of arguments."
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        let (callee, args, span) = match expr {
            Expr::Call { callee, args, span } => (callee, args, span),
            Expr::New { callee, args, span } => (callee, args, span),
            _ => return,
        };

        if let Expr::Identifier { name, .. } = callee.as_ref() {
            if let Some(binding_id) = model.resolve_name(name) {
                if let Some(binding) = model.get_binding(binding_id) {
                    // Get min and max argument counts
                    let min_args = binding.min_parameter_count;
                    let max_args = binding.parameter_count;
                    
                    // Only check if we have parameter info
                    if let (Some(min), Some(max)) = (min_args, max_args) {
                        let argc = args.len();
                        if argc < min {
                            let (line, col) = ctx.offset_to_line_col(span.start);
                            if min == max {
                                diagnostics.push(Diagnostic::error(
                                    self.code().0,
                                    format!("Function '{}' expects {} arguments, but {} were provided", name, min, argc),
                                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                                ));
                            } else {
                                diagnostics.push(Diagnostic::error(
                                    self.code().0,
                                    format!("Function '{}' expects {}-{} arguments, but {} were provided", name, min, max, argc),
                                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                                ));
                            }
                        } else if argc > max && !binding.is_variadic {
                            let (line, col) = ctx.offset_to_line_col(span.start);
                            if min == max {
                                diagnostics.push(Diagnostic::error(
                                    self.code().0,
                                    format!("Function '{}' expects {} arguments, but {} were provided", name, max, argc),
                                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                                ));
                            } else {
                                diagnostics.push(Diagnostic::error(
                                    self.code().0,
                                    format!("Function '{}' expects {}-{} arguments, but {} were provided", name, min, max, argc),
                                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                                ));
                            }
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_lint;

    #[test]
    fn test_wrong_argument_count() {
        assert_lint(
            Box::new(CheckArgumentCounts),
            "function foo(a, b) { } foo(1);",
            "GML011",
        );
    }

    #[test]
    fn test_correct_argument_count() {
        assert_lint(
            Box::new(CheckArgumentCounts),
            "function foo(a, b) { } foo(1, 2);",
            "",
        );
    }

    #[test]
    fn test_variadic_argument_count() {
        assert_lint(
            Box::new(CheckArgumentCounts),
            "function foo() { var c = argument_count; } foo(1, 2, 3);",
            "",
        );
    }
}
