//! GML026: Uncaptured closure variable
//!
//! Detects anonymous functions that reference variables from their enclosing scope
//! without using method() to explicitly bind them.
//!
//! In GML, anonymous functions do not have access to local variables from their
//! parent scope. To access outer variables, you must use:
//!   method({ var_name: var_name }, function() { /* can use var_name */ })
//!
//! Example:
//!   var x = 10;
//!   array_sort(arr, function(a, b) {
//!       return a.dist - b.dist + x;  // ERROR: x is not accessible
//!   });
//!
//! Fix:
//!   var x = 10;
//!   array_sort(arr, method({ x: x }, function(a, b) {
//!       return a.dist - b.dist + x;  // OK: x is bound via method()
//!   }));

use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::Expr;
use crate::{LintContext, Rule, RuleCode, SemanticModel};
use rustc_hash::FxHashSet;

pub struct UncapturedClosureVar;

impl Rule for UncapturedClosureVar {
    fn code(&self) -> RuleCode {
        RuleCode("GML026")
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn name(&self) -> &'static str {
        "uncaptured-closure-variable"
    }

    fn description(&self) -> &'static str {
        "Detects anonymous functions that reference outer scope variables without method() binding"
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        // We look for Call expressions where:
        // 1. The callee is NOT "method"
        // 2. One of the arguments is a FunctionExpr
        // 3. That FunctionExpr uses identifiers that are local variables in the outer scope
        
        if let Expr::Call { callee, args, .. } = expr {
            // Check if this is a method() call - if so, skip (it's handled correctly)
            if let Expr::Identifier { name, .. } = callee.as_ref() {
                if *name == "method" {
                    return;
                }
            }
            
            // Check each argument for anonymous functions
            for arg in args {
                if let Expr::FunctionExpr { params, body, span, .. } = arg {
                    // Collect identifiers used in the function body
                    let mut used_identifiers = FxHashSet::default();
                    collect_identifiers_in_body(body, &mut used_identifiers);
                    
                    // Remove function parameters (they're locally scoped)
                    for param in params {
                        used_identifiers.remove(param.name);
                    }
                    
                    // Remove variables declared inside the function
                    let mut local_vars = FxHashSet::default();
                    collect_local_declarations(body, &mut local_vars);
                    for local in &local_vars {
                        used_identifiers.remove(local.as_str());
                    }
                    
                    // Check remaining identifiers - are they local variables from outer scope?
                    for ident in &used_identifiers {
                        // Skip builtins, globals, and self/other/global keywords
                        if is_builtin_or_keyword(ident) {
                            continue;
                        }
                        
                        // Check if this is a local variable in the outer scope
                        if let Some(binding_id) = model.resolve_name(ident) {
                            let binding = model.get_binding(binding_id);
                            if let Some(b) = binding {
                                use crate::semantic_model::BindingKind;
                                // Only flag if it's a local variable (not a function, global, or parameter)
                                if matches!(b.kind, BindingKind::Variable | BindingKind::UninitializedVariable) {
                                    let (line, col) = ctx.offset_to_line_col(span.start);
                                    diagnostics.push(Diagnostic::error(
                                        "GML026",
                                        format!(
                                            "Variable '{}' from outer scope is not accessible in anonymous function. Use method({{ {}: {} }}, function() {{ ... }}) to capture it",
                                            ident, ident, ident
                                        ),
                                        Location::new(ctx.file_path(), line, col, span.start, span.end),
                                    ));
                                    // Only report first uncaptured variable per function
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Collect all identifiers used in the function body
fn collect_identifiers_in_body<'a>(body: &crate::parser::Block<'a>, identifiers: &mut FxHashSet<&'a str>) {
    for stmt in &body.statements {
        collect_identifiers_in_stmt(stmt, identifiers);
    }
}

fn collect_identifiers_in_stmt<'a>(stmt: &crate::parser::Stmt<'a>, identifiers: &mut FxHashSet<&'a str>) {
    use crate::parser::Stmt;
    
    match stmt {
        Stmt::VarDecl { declarations, .. } => {
            for decl in declarations {
                if let Some(init) = &decl.init {
                    collect_identifiers_in_expr(init, identifiers);
                }
            }
        }
        Stmt::Expr { expr, .. } => collect_identifiers_in_expr(expr, identifiers),
        Stmt::If { condition, then_branch, else_branch, .. } => {
            collect_identifiers_in_expr(condition, identifiers);
            collect_identifiers_in_body(then_branch, identifiers);
            if let Some(else_stmt) = else_branch {
                collect_identifiers_in_stmt(else_stmt, identifiers);
            }
        }
        Stmt::For { init, condition, update, body, .. } => {
            if let Some(init) = init {
                collect_identifiers_in_stmt(init, identifiers);
            }
            if let Some(cond) = condition {
                collect_identifiers_in_expr(cond, identifiers);
            }
            if let Some(upd) = update {
                collect_identifiers_in_expr(upd, identifiers);
            }
            collect_identifiers_in_body(body, identifiers);
        }
        Stmt::While { condition, body, .. } => {
            collect_identifiers_in_expr(condition, identifiers);
            collect_identifiers_in_body(body, identifiers);
        }
        Stmt::DoUntil { body, condition, .. } => {
            collect_identifiers_in_body(body, identifiers);
            collect_identifiers_in_expr(condition, identifiers);
        }
        Stmt::Repeat { count, body, .. } => {
            collect_identifiers_in_expr(count, identifiers);
            collect_identifiers_in_body(body, identifiers);
        }
        Stmt::With { target, body, .. } => {
            collect_identifiers_in_expr(target, identifiers);
            collect_identifiers_in_body(body, identifiers);
        }
        Stmt::Switch { value, cases, .. } => {
            collect_identifiers_in_expr(value, identifiers);
            for case in cases {
                if let Some(val) = &case.value {
                    collect_identifiers_in_expr(val, identifiers);
                }
                for inner in &case.body {
                    collect_identifiers_in_stmt(inner, identifiers);
                }
            }
        }
        Stmt::Return { value: Some(val), .. } => {
            collect_identifiers_in_expr(val, identifiers);
        }
        Stmt::Throw { value, .. } => collect_identifiers_in_expr(value, identifiers),
        Stmt::Block(block) => collect_identifiers_in_body(block, identifiers),
        Stmt::TryCatch { try_block, catch_block, finally_block, .. } => {
            collect_identifiers_in_body(try_block, identifiers);
            if let Some(catch) = catch_block {
                collect_identifiers_in_body(catch, identifiers);
            }
            if let Some(fin) = finally_block {
                collect_identifiers_in_body(fin, identifiers);
            }
        }
        _ => {}
    }
}

fn collect_identifiers_in_expr<'a>(expr: &Expr<'a>, identifiers: &mut FxHashSet<&'a str>) {
    match expr {
        Expr::Identifier { name, .. } => {
            identifiers.insert(name);
        }
        Expr::Binary { left, right, .. } => {
            collect_identifiers_in_expr(left, identifiers);
            collect_identifiers_in_expr(right, identifiers);
        }
        Expr::Unary { operand, .. } | Expr::Update { operand, .. } | Expr::Grouping { expr: operand, .. } => {
            collect_identifiers_in_expr(operand, identifiers);
        }
        Expr::Call { callee, args, .. } | Expr::New { callee, args, .. } => {
            collect_identifiers_in_expr(callee, identifiers);
            for arg in args {
                collect_identifiers_in_expr(arg, identifiers);
            }
        }
        Expr::Index { object, index, .. } => {
            collect_identifiers_in_expr(object, identifiers);
            collect_identifiers_in_expr(index, identifiers);
        }
        Expr::Index2D { object, index1, index2, .. } => {
            collect_identifiers_in_expr(object, identifiers);
            collect_identifiers_in_expr(index1, identifiers);
            collect_identifiers_in_expr(index2, identifiers);
        }
        Expr::Member { object, .. } => {
            collect_identifiers_in_expr(object, identifiers);
        }
        Expr::Ternary { condition, then_expr, else_expr, .. } => {
            collect_identifiers_in_expr(condition, identifiers);
            collect_identifiers_in_expr(then_expr, identifiers);
            collect_identifiers_in_expr(else_expr, identifiers);
        }
        Expr::NullCoalesce { left, right, .. } => {
            collect_identifiers_in_expr(left, identifiers);
            collect_identifiers_in_expr(right, identifiers);
        }
        Expr::Array { elements, .. } => {
            for elem in elements {
                collect_identifiers_in_expr(elem, identifiers);
            }
        }
        Expr::Struct { fields, .. } => {
            for (_, value) in fields {
                collect_identifiers_in_expr(value, identifiers);
            }
        }
        Expr::Assignment { target, value, .. } => {
            collect_identifiers_in_expr(target, identifiers);
            collect_identifiers_in_expr(value, identifiers);
        }
        // Don't recurse into nested function expressions
        Expr::FunctionExpr { .. } => {}
        Expr::Literal { .. } => {}
    }
}

/// Collect local variable declarations inside the function body
fn collect_local_declarations<'a>(body: &crate::parser::Block<'a>, locals: &mut FxHashSet<String>) {
    for stmt in &body.statements {
        collect_local_declarations_in_stmt(stmt, locals);
    }
}

fn collect_local_declarations_in_stmt<'a>(stmt: &crate::parser::Stmt<'a>, locals: &mut FxHashSet<String>) {
    use crate::parser::Stmt;
    
    match stmt {
        Stmt::VarDecl { declarations, .. } => {
            for decl in declarations {
                locals.insert(decl.name.to_string());
            }
        }
        Stmt::For { init, body, .. } => {
            if let Some(init) = init {
                collect_local_declarations_in_stmt(init, locals);
            }
            collect_local_declarations(body, locals);
        }
        Stmt::If { then_branch, else_branch, .. } => {
            collect_local_declarations(then_branch, locals);
            if let Some(else_stmt) = else_branch {
                collect_local_declarations_in_stmt(else_stmt, locals);
            }
        }
        Stmt::While { body, .. } | Stmt::DoUntil { body, .. } | Stmt::Repeat { body, .. } | Stmt::With { body, .. } => {
            collect_local_declarations(body, locals);
        }
        Stmt::Switch { cases, .. } => {
            for case in cases {
                for inner in &case.body {
                    collect_local_declarations_in_stmt(inner, locals);
                }
            }
        }
        Stmt::Block(block) => collect_local_declarations(block, locals),
        Stmt::TryCatch { try_block, catch_block, finally_block, .. } => {
            collect_local_declarations(try_block, locals);
            if let Some(catch) = catch_block {
                collect_local_declarations(catch, locals);
            }
            if let Some(fin) = finally_block {
                collect_local_declarations(fin, locals);
            }
        }
        _ => {}
    }
}

/// Check if an identifier is a builtin/keyword that doesn't need capturing
fn is_builtin_or_keyword(name: &str) -> bool {
    // Common GML keywords and builtins that are always accessible
    let lower = name.to_lowercase();
    matches!(lower.as_str(),
        "self" | "other" | "all" | "noone" | "global" | "undefined" | "true" | "false" |
        "infinity" | "nan" | "pi" |
        // Common instance variables
        "x" | "y" | "id" | "visible" | "depth" | "persistent" | "solid" | "sprite_index" |
        "image_index" | "image_speed" | "image_xscale" | "image_yscale" | "image_angle" |
        "image_alpha" | "image_blend" | "mask_index" | "direction" | "speed" | "hspeed" |
        "vspeed" | "gravity" | "gravity_direction" | "friction" | "path_index" |
        "path_position" | "path_positionprevious" | "path_speed" | "path_scale" |
        "path_orientation" | "path_endaction" | "object_index" | "alarm" | "timeline_index" |
        "timeline_position" | "timeline_speed" | "timeline_running" | "timeline_loop" |
        "in_sequence" | "sequence_instance" | "bbox_left" | "bbox_right" | "bbox_top" |
        "bbox_bottom" | "sprite_width" | "sprite_height" | "sprite_xoffset" | "sprite_yoffset" |
        "image_number" | "image_width" | "image_height" | "image_xoffset" | "image_yoffset" |
        "layer" | "xstart" | "ystart" | "xprevious" | "yprevious"
    ) || 
    // Use the keywords module for additional checks
    crate::keywords::is_keyword(name) ||
    crate::keywords::is_readonly(name) ||
    crate::keywords::is_instance_var(name) ||
    crate::keywords::is_reserved_builtin(name)
}

#[cfg(test)]
mod tests {
    use super::UncapturedClosureVar;
    use crate::test_utils::*;

    #[test]
    fn test_uncaptured_variable_in_callback() {
        assert_lint(
            Box::new(UncapturedClosureVar),
            r#"
            function test() {
                var start_col = 5;
                array_sort(arr, function(a, b) {
                    return point_distance(start_col, 0, a.col, 0);
                });
            }
            "#,
            "GML026"
        );
    }

    #[test]
    fn test_method_binding_is_allowed() {
        assert_lint(
            Box::new(UncapturedClosureVar),
            r#"
            function test() {
                var start_col = 5;
                array_sort(arr, method({ sc: start_col }, function(a, b) {
                    return point_distance(sc, 0, a.col, 0);
                }));
            }
            "#,
            "" // No error - using method() binding
        );
    }

    #[test]
    fn test_parameter_usage_is_allowed() {
        assert_lint(
            Box::new(UncapturedClosureVar),
            r#"
            function test() {
                array_sort(arr, function(a, b) {
                    return a.dist - b.dist;
                });
            }
            "#,
            "" // No error - a and b are parameters
        );
    }

    #[test]
    fn test_local_var_in_callback_allowed() {
        assert_lint(
            Box::new(UncapturedClosureVar),
            r#"
            function test() {
                array_sort(arr, function(a, b) {
                    var diff = a.dist - b.dist;
                    return diff;
                });
            }
            "#,
            "" // No error - diff is declared locally
        );
    }

    #[test]
    fn test_builtin_function_allowed() {
        assert_lint(
            Box::new(UncapturedClosureVar),
            r#"
            function test() {
                array_sort(arr, function(a, b) {
                    return point_distance(0, 0, a.col, a.row);
                });
            }
            "#,
            "" // No error - point_distance is a builtin
        );
    }
}
