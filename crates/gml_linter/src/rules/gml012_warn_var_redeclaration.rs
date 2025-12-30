use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::Stmt;
use crate::{LintContext, Rule, RuleCode, SemanticModel};
use rustc_hash::FxHashSet;

pub struct WarnVarRedeclaration;

impl Rule for WarnVarRedeclaration {
    fn code(&self) -> RuleCode {
        RuleCode("GML012")
    }

    fn name(&self) -> &'static str {
        "warn-var-redeclaration"
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn description(&self) -> &'static str {
        "Warns when a variable is redeclared in the same scope."
    }

    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Stmt::VarDecl { declarations, span: _ } = stmt {
            let mut seen_in_stmt = FxHashSet::default();
            
            for decl in declarations {
                let name = decl.name;
                
                // Only check for duplicates in the same statement (var a, a;)
                // We do NOT warn about redeclaration across statements because:
                // - `var` is function-scoped in GML (not block-scoped)
                // - Redeclaring `var x` in the same function is valid GML
                // - GMEdit's default behavior (blockScopedVar=false) also doesn't warn
                if !seen_in_stmt.insert(name) {
                    let (line, col) = ctx.offset_to_line_col(decl.span.start);
                    diagnostics.push(Diagnostic::warning(
                        self.code().0,
                        format!("Variable '{}' is declared multiple times in the same statement", name),
                        Location::new(ctx.file_path(), line, col, decl.span.start, decl.span.end)
                    ));
                }
            }
        }
    }
}

impl WarnVarRedeclaration {
}

#[cfg(test)]
mod tests {
    use crate::rules::WarnVarRedeclaration;
    use crate::test_utils::*;

    #[test]
    fn test_duplicate_in_same_statement() {
        // var a, a; - duplicate in same statement
        assert_lint(
            Box::new(WarnVarRedeclaration),
            "var a, a;",
            "GML012"
        );
    }

    #[test]
    fn test_redeclaration_same_scope_allowed() {
        // Redeclaration in same scope is ALLOWED for function-scoped `var`
        // (GMEdit's default blockScopedVar=false also doesn't warn)
        assert_lint(
            Box::new(WarnVarRedeclaration),
            "var a; var a;",
            ""  // No warning - redeclaration is valid for var
        );
    }
    
    #[test]
    fn test_shadowing_in_nested_scope_allowed() {
        // Shadowing in child block is ALLOWED for function-scoped `var`
        assert_lint(
            Box::new(WarnVarRedeclaration),
            "var a; if (true) { var a; }",
            ""  // No warning - shadowing is valid for var
        );
    }
    
    #[test]
    fn test_no_warning_different_variables() {
        // No warning for different variables
        assert_lint(
            Box::new(WarnVarRedeclaration),
            "var a; var b;",
            ""
        );
    }
}
