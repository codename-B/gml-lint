use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::Expr;
use crate::{LintContext, Rule, RuleCode, SemanticModel};
use crate::builtins::get_builtins;

pub struct UnknownIdentifier;

impl Rule for UnknownIdentifier {
    fn code(&self) -> RuleCode {
        RuleCode("GML022")
    }

    fn name(&self) -> &'static str {
        "unknown-identifier"
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn description(&self) -> &'static str {
        "Identifier is not defined."
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Expr::Identifier { name, span } = expr {
            // IGNORE function calls - those are handled by GML013 UnknownFunction
            if get_builtins().contains(name) {
                return;
            }

            // 2. Check bindings (locals, args, function name)
            if model.resolve_name(name).is_some() {
                return;
            }

            // 3. Check SymbolProvider (resources, macros, scripts)
            if ctx.symbol_provider().has_variable(name) || ctx.symbol_provider().has_function(name) {
                return;
            }

            // 4. Special GML "argument[0-9]+" check
            if let Some(remainder) = name.strip_prefix("argument") {
                 if remainder.chars().all(|c| c.is_ascii_digit()) {
                     return;
                 }
            }

            // Not found!
            let (line, col) = ctx.offset_to_line_col(span.start);
            diagnostics.push(Diagnostic::error(
                self.code().0,
                format!("Unknown identifier '{}'", name),
                Location::new(ctx.file_path(), line, col, span.start, span.end),
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::rules::UnknownIdentifier;
    use crate::test_utils::*;
    use crate::SymbolProvider;

    struct MockSymbolProvider {
        vars: Vec<String>,
    }
    
    impl SymbolProvider for MockSymbolProvider {
        fn has_function(&self, _name: &str) -> bool { false }
        fn has_variable(&self, name: &str) -> bool { self.vars.iter().any(|s| s == name) }
        fn has_global(&self, _name: &str) -> bool { false }
    }

    #[test]
    fn test_unknown_identifier_var() {
        assert_lint(
            Box::new(UnknownIdentifier),
            "var x = 1; var z = my_unknown_var;", // unknown read
            "GML022"
        );
    }

    #[test]
    fn test_known_identifier_local() {
        assert_lint(
            Box::new(UnknownIdentifier),
            "var x = 1; var y = x;", 
            ""
        );
    }
    
    #[test]
    fn test_known_identifier_builtin() {
        assert_lint(
            Box::new(UnknownIdentifier),
            "x = room_speed;", 
            ""
        );
    }

    #[test]
    fn test_known_macro() {
        let provider = MockSymbolProvider { vars: vec!["MY_MACRO".to_string()] };
        
        // This should pass because MY_MACRO is in the provider
        assert_lint_with_provider(
            Box::new(UnknownIdentifier),
            "x = MY_MACRO;", 
            &provider,
            ""
        );
    }

    #[test]
    fn test_unknown_identifier_in_constructor() {
        // This reproduces the user's issue
        // new UISlider(..., slider_bar, ...);
        // slider_bar is unknown
        assert_lint(
            Box::new(UnknownIdentifier),
            "var s = new UISlider(10, 20, slider_bar);", 
            "GML022"
        );
    }
    
    #[test]
    fn test_argument_special_vars() {
        assert_lint(
            Box::new(UnknownIdentifier),
            "return argument0;", 
            ""
        );
    }
}
