//! GML017/018: Constructor validation
//!
//! GML017: Warn when 'new' is used on non-constructor functions.
//! GML018: Warn when a constructor is called as a regular function.

use gml_diagnostics::{Category, Diagnostic, Location};
use gml_parser::Expr;

use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct ConstructorValidation;

impl Rule for ConstructorValidation {
    fn code(&self) -> RuleCode {
        RuleCode("GML017")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "constructor-validation"
    }

    fn description(&self) -> &'static str {
        "Checks for constructor usage errors (GML017: missing new, GML018: invalid constructor call)."
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        match expr {
            Expr::New { callee, span, .. } => {
                if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if let Some(id) = model.resolve_name(name) {
                        if let Some(binding) = model.get_binding(id) {
                            // We need to track is_constructor in Binding
                            // For now we check if it's a function
                            if binding.kind == crate::semantic::BindingKind::Function && !binding.is_constructor {
                                let (line, col) = ctx.offset_to_line_col(span.start);
                                diagnostics.push(Diagnostic::warning(
                                    "GML017",
                                    format!("Using 'new' on non-constructor function '{}'", name),
                                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                                ));
                            }
                        }
                    }
                }
            }
            Expr::Call { callee, span, .. } => {
                 if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if let Some(id) = model.resolve_name(name) {
                        if let Some(binding) = model.get_binding(id) {
                            if binding.is_constructor {
                                let (line, col) = ctx.offset_to_line_col(span.start);
                                diagnostics.push(Diagnostic::warning(
                                    "GML018",
                                    format!("Calling constructor '{}' without 'new'", name),
                                    Location::new(ctx.file_path(), line, col, span.start, span.end),
                                ));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

