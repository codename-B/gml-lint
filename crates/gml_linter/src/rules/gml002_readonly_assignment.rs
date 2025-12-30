//! GML002: Assignment to readonly constant
//!
//! Detects assignments to built-in readonly values like `true`, `false`, `self`, `other`, etc.
//!
//! This rule uses the single-pass visitor pattern for efficient checking.

use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::{Expr, Stmt};
use crate::{LintContext, Rule, RuleCode, SemanticModel};
use crate::keywords;


pub struct ReadonlyAssignment;

impl Rule for ReadonlyAssignment {
    fn code(&self) -> RuleCode {
        RuleCode("GML002")
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn name(&self) -> &'static str {
        "readonly-assignment"
    }

    fn description(&self) -> &'static str {
        "Detects assignments to built-in readonly constants"
    }

    // Note: check() is not overridden - we use check_stmt()/check_expr() for single-pass traversal
    
    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {

        if let Stmt::VarDecl { declarations, .. } = stmt {
            for decl in declarations {
                // Check if variable name is a readonly constant
                if keywords::is_readonly(decl.name) {
                    let (line, col) = ctx.offset_to_line_col(decl.span.start);
                    diagnostics.push(Diagnostic::error(
                        "GML002",
                        format!("Cannot redeclare readonly constant '{}'", decl.name),
                        Location::new(ctx.file_path(), line, col, decl.span.start, decl.span.end),
                    ));
                }
            }
        }
    }
    
    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {

        use crate::parser::Literal;

        if let Expr::Assignment { target, span, .. } = expr {
            match target.as_ref() {
                // Direct assignment to readonly constant: `noone = 5`
                Expr::Identifier { name, .. } => {
                    if keywords::is_readonly(name) {
                        let (line, col) = ctx.offset_to_line_col(span.start);
                        diagnostics.push(Diagnostic::error(
                            "GML002",
                            format!("Cannot assign to readonly constant '{}'", name),
                            Location::new(ctx.file_path(), line, col, span.start, span.end),
                        ));
                    }
                }
                // Assignment to literals: `undefined = 5`, `true = 5`
                Expr::Literal { value, .. } => {
                    let name = match value {
                        Literal::Undefined => Some("undefined"),
                        Literal::Boolean(true) => Some("true"),
                        Literal::Boolean(false) => Some("false"),
                        _ => None,
                    };
                    if let Some(name) = name {
                        let (line, col) = ctx.offset_to_line_col(span.start);
                        diagnostics.push(Diagnostic::error(
                            "GML002",
                            format!("Cannot assign to readonly constant '{}'", name),
                            Location::new(ctx.file_path(), line, col, span.start, span.end),
                        ));
                    }
                }
                // Member assignment to readonly instance var: `snd.id = 5`
                Expr::Member { field, .. } => {
                    if keywords::is_readonly_instance_var(field) {
                        let (line, col) = ctx.offset_to_line_col(span.start);
                        diagnostics.push(Diagnostic::error(
                            "GML002",
                            format!("Cannot assign to readonly instance variable '{}'", field),
                            Location::new(ctx.file_path(), line, col, span.start, span.end),
                        ));
                    }
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::ReadonlyAssignment;
    use crate::test_utils::*;

    #[test]
    fn test_assign_to_readonly_constant_noone() {
        // Cannot assign to readonly constants
        assert_lint(
            Box::new(ReadonlyAssignment),
            "noone = 5;",
            "GML002"
        );
    }

    #[test]
    fn test_assign_to_self() {
        assert_lint(
            Box::new(ReadonlyAssignment),
            "self = other;",
            "GML002"
        );
    }

    #[test]
    fn test_member_assign_to_readonly_id() {
        // Cannot assign to readonly instance var via member access: snd.id = 5
        assert_lint(
            Box::new(ReadonlyAssignment),
            "snd.id = 5;",
            "GML002"
        );
    }

    #[test]
    fn test_member_assign_to_writable_var() {
        // Assigning to writable vars is allowed: obj.x = 5
        assert_lint(
            Box::new(ReadonlyAssignment),
            "obj.x = 5;",
            "" // No error
        );
    }

    // Note: var declarations with readonly names like 'var true = 5' are parser errors,
    // not lint errors, so we can't test them here.

    #[test]
    fn test_assign_to_undefined() {
        // undefined is parsed as a literal, not identifier
        assert_lint(
            Box::new(ReadonlyAssignment),
            "undefined = 5;",
            "GML002"
        );
    }

    #[test]
    fn test_assign_to_true() {
        // true is parsed as a literal, not identifier
        assert_lint(
            Box::new(ReadonlyAssignment),
            "true = 5;",
            "GML002"
        );
    }
}
