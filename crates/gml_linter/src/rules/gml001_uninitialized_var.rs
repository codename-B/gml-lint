//! GML001: Potentially uninitialized variable
//!
//! Detects variables that may be used before being assigned a value.

use gml_diagnostics::{Category, Diagnostic, Location};
use crate::{LintContext, Rule, RuleCode, SemanticModel};
use gml_parser::Stmt;


pub struct UninitializedVariable;

impl Rule for UninitializedVariable {
    fn code(&self) -> RuleCode {
        RuleCode("GML001")
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn name(&self) -> &'static str {
        "uninitialized-variable"
    }

    fn description(&self) -> &'static str {
        "Variables must be initialized before use."
    }

    fn check_stmt<'a>(&self, _ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, _stmt: &Stmt<'a>, _diagnostics: &mut Vec<Diagnostic>) {
        // We actually check this via bindings in check_expr or post-process?
        // Actually GML001 is often done by checking assignments or references.
        // Let's check implementation.
        // The provided snippet for check_stmt body was incomplete and contained code from check_post.
        // Keeping only the comments as the actual logic for check_stmt is not provided.
    }

    fn check_post<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, diagnostics: &mut Vec<Diagnostic>) {
        for binding in model.uninitialized_usages() {
            let (line, col) = ctx.offset_to_line_col(binding.name_span.start);
            diagnostics.push(Diagnostic::error(
                self.code().0,
                format!("Variable '{}' may be used before being initialized", binding.name),
                Location::new(ctx.file_path(), line, col, binding.name_span.start, binding.name_span.end),
            ));
        }
    }
}
