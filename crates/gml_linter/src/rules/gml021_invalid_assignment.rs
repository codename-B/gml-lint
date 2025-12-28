use gml_parser::{Expr, AssignOp};
use gml_semantic::{types::Type, infer, Db};
use crate::{LintContext, Rule, RuleCode};
use gml_diagnostics::{Category, Diagnostic, Location};
use crate::semantic::SemanticModel;
use gml_semantic::scope::TypeEnv;

pub struct InvalidAssignment;

impl Rule for InvalidAssignment {
    fn code(&self) -> RuleCode {
        RuleCode("GML021")
    }

    fn name(&self) -> &'static str {
        "invalid-assignment"
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn description(&self) -> &'static str {
        "Invalid assignment between incompatible types."
    }

    fn check_expr(
        &self,
        ctx: &LintContext,
        _model: &SemanticModel,
        env: &TypeEnv,
        expr: &Expr,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        if let Expr::Assignment { target, value, op, span } = expr {
            // Only strictly check standard assignment "="
            if *op != AssignOp::Assign {
                return;
            }

            // Unwrap grouping
            let mut current = target.as_ref();
            while let Expr::Grouping { expr, .. } = current {
                current = expr.as_ref();
            }

            if let Expr::Identifier { name, .. } = current {
                if let Some(old_type) = env.lookup(name) {
                    // Skip if the variable was originally Any (e.g. parameter)
                    // and we are currently in a narrowed scope.
                    if env.is_narrowed_from_any(name) {
                        return;
                    }

                    let new_type = infer::infer_expression(ctx.db(), env, value);
                    if !is_assignable(&old_type, &new_type) {
                        let (line, col) = ctx.offset_to_line_col(span.start);
                        diagnostics.push(Diagnostic::error(
                            self.code().0,
                            format!("Invalid assignment: cannot assign type '{:?}' to variable '{}' of type '{:?}'", new_type, name, old_type),
                            Location::new(ctx.file_path(), line, col, span.start, span.end),
                        ));
                    }
                }
            } else if let Expr::Member { object, field, .. } = current {
                let obj_ty = infer::infer_expression(ctx.db(), env, object);
                if let Type::Struct(id) = obj_ty {
                    if let Some(old_type) = ctx.db().lookup_struct_field(id, field) {
                        let new_type = infer::infer_expression(ctx.db(), env, value);
                        if !is_assignable(&old_type, &new_type) {
                            let (line, col) = ctx.offset_to_line_col(span.start);
                            diagnostics.push(Diagnostic::error(
                                self.code().0,
                                format!("Invalid assignment: cannot assign type '{:?}' to property '{}' of type '{:?}'", new_type, field, old_type),
                                Location::new(ctx.file_path(), line, col, span.start, span.end),
                            ));
                        }
                    }
                }
            }
        }
    }
}

fn is_assignable(target: &Type, source: &Type) -> bool {
    // If either is Any, we can't be sure, so allow it (gradual typing)
    if matches!(target, Type::Any) || matches!(source, Type::Any) { return true; }
    
    // If either is Unknown/Never? (We have Any, Never)
    if matches!(target, Type::Never) || matches!(source, Type::Never) { return true; }

    // If target is Undefined, it means the variable was declared but not initialized.
    // If source is Undefined, it means we are clearing the variable (common in GML).
    if matches!(target, Type::Undefined) || matches!(source, Type::Undefined) {
        return true;
    }

    // Exact match
    if target == source { return true; }

    // Relaxed matching for Structs, Functions, and Arrays (v0.1.0)
    if matches!(target, Type::Struct(_)) && matches!(source, Type::Struct(_)) { return true; }
    if matches!(target, Type::Function(_)) && matches!(source, Type::Function(_)) { return true; }
    if matches!(target, Type::Array(_)) && matches!(source, Type::Array(_)) { return true; }
    
    false
}

#[cfg(test)]
mod tests {
    use crate::rules::InvalidAssignment;
    use crate::test_utils::*;

    #[test]
    fn test_valid_assignment() {
        assert_lint(
            Box::new(InvalidAssignment),
            "var x = 1; x = 2;",
            ""
        );
    }

    #[test]
    fn test_invalid_assignment() {
        assert_lint(
            Box::new(InvalidAssignment),
            "var x = 1; x = \"hello\";",
            "GML021"
        );
    }

    #[test]
    fn test_initialization() {
        assert_lint(
            Box::new(InvalidAssignment),
            "var x; x = 1;",
            "" // Should pass (Undefined -> Real)
        );
    }

    #[test]
    fn test_assign_undefined() {
        assert_lint(
            Box::new(InvalidAssignment),
            "var x = 1; x = undefined;",
            "" // Should pass (clearing the variable)
        );
    }

    #[test]
    fn test_narrowing_assignment() {
        assert_lint(
            Box::new(InvalidAssignment),
            "var x = undefined; if (is_string(x)) { x = 10; }",
            "GML021" // x is narrowed to String, so x = 10 is invalid
        );
    }

    #[test]
    fn test_struct_member_assignment() {
        assert_lint(
            Box::new(InvalidAssignment),
            "var s = { x: 1 }; s.x = \"hello\";",
            "GML021"
        );
    }

    #[test]
    fn test_function_return_assignment() {
        assert_lint(
            Box::new(InvalidAssignment),
            "function get_str() { return \"s\"; } var x = get_str(); x = 10;",
            "GML021"
        );
    }

    #[test]
    fn test_normalization_pattern() {
        // x starts as Any (parameter-like), is narrowed to String, then reassigned to Real
        assert_lint(
            Box::new(InvalidAssignment),
            "var f = function(x) { if (is_string(x)) x = 10; };",
            "" 
        );
    }

    #[test]
    fn test_any_assignment() {
        // Assume some function returns Any?
        // infer.rs returns Any for unknown identifiers/calls basically.
        assert_lint(
            Box::new(InvalidAssignment),
            "var x = 1; x = some_unknown_func();",
            "" // Should pass
        );
    }
}
