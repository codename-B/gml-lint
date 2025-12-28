//! GML003: Unused variable
//!
//! Detects variables that are declared but never used.

use gml_diagnostics::{Category, Diagnostic, Edit, Fix, Location};
use crate::{LintContext, Rule, RuleCode, SemanticModel};


pub struct UnusedVariable;

impl Rule for UnusedVariable {
    fn code(&self) -> RuleCode {
        RuleCode("GML003")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "unused-variable"
    }

    fn description(&self) -> &'static str {
        "Detects variables that are declared but never used"
    }

    fn check_post<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, diagnostics: &mut Vec<Diagnostic>) {
        for binding in model.unused_bindings() {
            let (line, col) = ctx.offset_to_line_col(binding.name_span.start);
            let new_name = format!("_{}", binding.name);
            
            let mut diag = Diagnostic::warning(
                self.code().0,
                format!("Variable '{}' is declared but never used", binding.name),
                Location::new(
                    ctx.file_path(),
                    line,
                    col,
                    binding.name_span.start,
                    binding.name_span.end,
                ),
            );
            
            diag = diag.with_fix(Fix {
                message: format!("Rename to '{}'", new_name),
                edits: vec![Edit::new(
                    binding.name_span.start,
                    binding.name_span.end,
                    &new_name,
                )],
            });
            
            diagnostics.push(diag);
        }
    }
}
