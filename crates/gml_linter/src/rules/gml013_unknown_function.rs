use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::Expr;
use crate::{LintContext, Rule, RuleCode, SemanticModel};
use crate::builtins::get_builtins;

pub struct UnknownFunction;

impl Rule for UnknownFunction {
    fn code(&self) -> RuleCode {
        RuleCode("GML013")
    }

    fn name(&self) -> &'static str {
        "unknown-function"
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn description(&self) -> &'static str {
        "Function or variable reference is not defined."
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Expr::Call { callee, .. } = expr {
            if let Expr::Identifier { name, span } = &**callee {
                // Check if built-in
                if get_builtins().contains(name) {
                    return;
                }
                
                // Check if defined in project (SemanticModel or SymbolProvider)
                if model.resolve_name(name).is_some() || ctx.symbol_provider().has_function(name) || ctx.symbol_provider().has_variable(name) {
                    return;
                }
                
                // Not found
                let (line, col) = ctx.offset_to_line_col(span.start);
                diagnostics.push(Diagnostic::warning(
                    self.code().0,
                    format!("Unknown function '{}'", name),
                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::rules::UnknownFunction;
    use crate::test_utils::*;
    use crate::SymbolProvider;

    /// Test symbol provider for cross-file function resolution tests
    struct MockSymbolProvider {
        functions: Vec<String>,
    }
    
    impl MockSymbolProvider {
        fn new(functions: &[&str]) -> Self {
            Self { functions: functions.iter().map(|s| s.to_string()).collect() }
        }
    }
    
    impl SymbolProvider for MockSymbolProvider {
        fn has_function(&self, name: &str) -> bool {
            self.functions.iter().any(|f| f == name)
        }
        fn has_variable(&self, _name: &str) -> bool { false }
        fn has_global(&self, _name: &str) -> bool { false }
    }

    #[test]
    fn test_unknown_function_builtin() {
        // Built-in function should not warn
        assert_lint(
            Box::new(UnknownFunction),
            "is_real(0);",
            ""
        );
    }

    #[test]
    fn test_unknown_function_unknown() {
        // Unknown function should warn
        assert_lint(
            Box::new(UnknownFunction),
            "some_random_function();",
            "GML013"
        );
    }
    
    #[test]
    fn test_unknown_function_same_file() {
        // Function defined in same file should not warn
        assert_lint(
            Box::new(UnknownFunction),
            "function foo() {} foo();",
            ""
        );
    }

    #[test]
    fn test_cross_file_function_resolution() {
        // Function provided by symbol provider (simulating cross-file)
        let provider = MockSymbolProvider::new(&["external_script_function"]);
        assert_lint_with_provider(
            Box::new(UnknownFunction),
            "external_script_function();",
            &provider,
            ""  // No warning expected
        );
    }

    #[test]
    fn test_cross_file_macro_resolution() {
        // Macro provided by symbol provider  
        let provider = MockSymbolProvider::new(&["MY_MACRO"]);
        assert_lint_with_provider(
            Box::new(UnknownFunction),
            "MY_MACRO();",
            &provider,
            ""  // No warning expected
        );
    }

    #[test]
    fn test_cross_file_missing_function() {
        // Function NOT in symbol provider should still warn
        let provider = MockSymbolProvider::new(&["other_function"]);
        assert_lint_with_provider(
            Box::new(UnknownFunction),
            "missing_function();",
            &provider,
            "GML013"
        );
    }
}
