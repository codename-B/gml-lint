//! GML006: Keyword shadowing
//!
//! Detects when reserved keywords are used as variable or function names.
//!
//! This rule uses the single-pass visitor pattern for efficient checking.

use gml_diagnostics::{Category, Diagnostic, Location};
use gml_parser::Stmt;

use crate::{LintContext, Rule, RuleCode, SemanticModel};
use crate::keywords;

pub struct KeywordShadowing;

impl Rule for KeywordShadowing {
    fn code(&self) -> RuleCode {
        RuleCode("GML006")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "keyword-shadowing"
    }

    fn description(&self) -> &'static str {
        "Variable name shadows a keyword."
    }

    // Note: check() is not overridden - we use check_stmt() for single-pass traversal
    
    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {

        match stmt {
            Stmt::VarDecl { declarations, .. } => {
                for decl in declarations {
                    // Check if variable name is a keyword (not readonly - that's GML002)
                    if keywords::is_keyword(decl.name) && !keywords::is_readonly(decl.name) {
                        let (line, col) = ctx.offset_to_line_col(decl.span.start);
                        diagnostics.push(Diagnostic::warning(
                            "GML006",
                            format!("Variable name '{}' shadows a reserved keyword", decl.name),
                            Location::new(ctx.file_path(), line, col, decl.span.start, decl.span.end),
                        ));
                    }
                    // Check if variable name is a reserved built-in (score, lives, etc.)
                    if keywords::is_reserved_builtin(decl.name) {
                        let (line, col) = ctx.offset_to_line_col(decl.span.start);
                        diagnostics.push(Diagnostic::error(
                            "GML006",
                            format!("Identifier '{}' is reserved and cannot be used as a variable name", decl.name),
                            Location::new(ctx.file_path(), line, col, decl.span.start, decl.span.end),
                        ));
                    }
                }
            }
            Stmt::FunctionDecl { name, span, .. } => {
                // Check if function name is a keyword
                if keywords::is_keyword(name) {
                    let (line, col) = ctx.offset_to_line_col(span.start);
                    diagnostics.push(Diagnostic::warning(
                        "GML006",
                        format!("Function name '{}' shadows a reserved keyword", name),
                        Location::new(ctx.file_path(), line, col, span.start, span.end),
                    ));
                }
            }
            // No check needed for other statements (visitor handles recursion)
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::KeywordShadowing;
    use crate::test_utils::*;

    // Note: Using keywords like 'if' or 'while' as variable/function names
    // causes parser errors, not lint errors, so we can't test them here.

    #[test]
    fn test_var_uses_reserved_builtin_score() {
        // 'score' is a reserved built-in variable
        assert_lint(
            Box::new(KeywordShadowing),
            "var score = 0;",
            "GML006"
        );
    }

    #[test]
    fn test_var_uses_reserved_builtin_lives() {
        // 'lives' is a reserved built-in variable
        assert_lint(
            Box::new(KeywordShadowing),
            "var lives = 3;",
            "GML006"
        );
    }

    #[test]
    fn test_var_normal_name_is_valid() {
        // Normal variable names should not trigger warnings
        assert_lint(
            Box::new(KeywordShadowing),
            "var myScore = 0;",
            "" // No error
        );
    }

    // Note: Using keywords like 'while' as function names
    // causes parser errors, not lint errors.
}
