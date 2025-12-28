//! Unified single-pass checker - combines semantic model building and rule checking
//!
//! This is the key optimization from ruff: instead of multiple AST traversals,
//! we do everything in ONE pass:
//! 1. Build SemanticModel (bindings/references)
//! 2. Run all lint rules
//! 3. Collect diagnostics
//!
//! This eliminates 3-4 separate traversals, providing ~3-4x speedup.

use gml_diagnostics::Diagnostic;
use gml_lexer::Span;
use gml_parser::{Program, Stmt, Expr, Param, Block};
use crate::semantic::{BindingKind, ScopeKind, SemanticModel};
use crate::{LintContext, Rule};
use gml_semantic::Db;

/// Context flags for control flow validation
#[derive(Clone, Copy)]
struct ControlFlowContext {
    in_loop: bool,
    in_switch: bool,
    in_function: bool,
}

impl Default for ControlFlowContext {
    fn default() -> Self {
        Self {
            in_loop: false,
            in_switch: false,
            // In GML, scripts and events are callable contexts where
            // return/exit is valid, so we start with in_function = true
            in_function: true,
        }
    }
}


use gml_semantic::scope::TypeEnv;

/// Unified checker that does everything in one AST pass
pub struct Checker<'a> {
    ctx: &'a LintContext<'a>,
    rules: &'a [Box<dyn Rule>],
    model: SemanticModel<'a>,
    type_env: TypeEnv,

    diagnostics: Vec<Diagnostic>,
    cf_ctx: ControlFlowContext,
    return_stack: Vec<Vec<gml_semantic::types::Type>>,
}

impl<'a> Checker<'a> {
    pub fn new(ctx: &'a LintContext<'a>, rules: &'a [Box<dyn Rule>]) -> Self {
        Self {
            ctx,
            rules,
            model: SemanticModel::new(),
            type_env: TypeEnv::new(),
            diagnostics: Vec::with_capacity(16),
            cf_ctx: ControlFlowContext::default(),
            return_stack: Vec::new(),
        }
    }

    /// Run the unified check on the program
    pub fn check(mut self, program: &Program<'a>) -> (SemanticModel<'a>, Vec<Diagnostic>) {

        for stmt in &program.statements {
            self.visit_stmt(stmt);
        }

        
        // Run semantic-based rules after traversal (unused variables, etc.)
        self.run_semantic_rules();
        
        (self.model, self.diagnostics)
    }

    /// Visit a statement - builds semantic model AND runs rules
    fn visit_stmt(&mut self, stmt: &Stmt<'a>) {

        // Run all rules on this statement (visitor pattern)
        for rule in self.rules {
            rule.check_stmt(self.ctx, &self.model, &self.type_env, stmt, &mut self.diagnostics);
        }

        
        // Build semantic model AND recurse into children
        match stmt {
            Stmt::VarDecl { declarations, .. } => {
                for decl in declarations {
                    let name_span = Span {
                        start: decl.span.start,
                        end: decl.span.start + decl.name.len() as u32,
                    };
                    
                    let kind = if decl.init.is_some() {
                        BindingKind::Variable
                    } else {
                        BindingKind::UninitializedVariable
                    };
                    
                    self.model.add_binding(decl.name, kind, name_span, decl.span, None, None, false);
                    
                    if let Some(init) = &decl.init {
                        // Infer type from initializer
                        let ty = gml_semantic::infer::infer_expression(self.ctx.db(), &self.type_env, init);
                        self.type_env.insert(decl.name.to_string(), ty);
                        
                        self.visit_expr(init);
                    } else {
                        // Uninitialized vars are typically Any or Undefined until assigned
                        self.type_env.insert(decl.name.to_string(), gml_semantic::types::Type::Undefined);
                    }
                }
            }
            
            Stmt::GlobalVarDecl { name, span, .. } => {
                let name_span = Span {
                    start: span.start,
                    end: span.start + name.len() as u32,
                };
                self.model.add_binding(name, BindingKind::GlobalVar, name_span, *span, None, None, false);
            }
            
            Stmt::FunctionDecl { name, params, body, span, is_constructor, .. } => {
                // Add function binding
                let name_span = Span {
                    start: span.start,
                    end: span.start + name.len() as u32,
                };
                // Count required params (those without default values)
                let required_params = params.iter().filter(|p| p.default.is_none()).count();
                let total_params = params.len();
                let binding_id = self.model.add_binding(name, BindingKind::Function, name_span, *span, Some(required_params), Some(total_params), *is_constructor);

                // Enter function scope
                let scope_id = self.model.push_scope(ScopeKind::Function);
                self.model.set_scope_function_binding(scope_id, binding_id);
                
                let old_cf = self.cf_ctx;
                let old_env = self.type_env.clone(); // Clone for restoring
                self.type_env = self.type_env.fork(); // Fork for new scope
                
                self.cf_ctx.in_function = true;
                self.cf_ctx.in_loop = false;
                
                self.return_stack.push(Vec::new());

                // Add parameters
                for param in params {
                    let param_span = Span {
                        start: param.span.start,
                        end: param.span.start + param.name.len() as u32,
                    };
                    self.model.add_binding(param.name, BindingKind::Parameter, param_span, param.span, None, None, false);
                    
                    // Parameters are Any by default (unless we have JSDoc)
                    self.type_env.insert(param.name.to_string(), gml_semantic::types::Type::Any);

                    if let Some(default) = &param.default {
                        self.visit_expr(default);
                    }
                }
                
                // Visit body
                for inner in &body.statements {
                    self.visit_stmt(inner);
                }
                
                // Finalize return type
                let returns = self.return_stack.pop().unwrap();
                let return_ty = unify_types(&returns);
                
                // Store function signature if it's a named function
                let fn_id = self.ctx.db().define_function();
                self.ctx.db().set_return_type(fn_id, return_ty.clone());
                
                self.cf_ctx = old_cf;
                self.type_env = old_env;
                
                // Update the parent environment with the function type
                self.type_env.insert(name.to_string(), gml_semantic::types::Type::Function(fn_id));
                
                self.model.pop_scope();
            }
            
            Stmt::If { condition, then_branch, else_branch, .. } => {
                self.visit_expr(condition);
                
                self.model.push_scope(ScopeKind::Block);
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                // Apply positive type narrowing
                let narrowing = gml_semantic::narrow::infer_narrowing(condition, true);
                for (name, ty) in narrowing {
                    self.type_env.insert(name, ty);
                }
                
                for inner in &then_branch.statements {
                    self.visit_stmt(inner);
                }
                self.model.pop_scope();
                self.type_env = old_env.clone(); // Restore to before If
                
                if let Some(else_stmt) = else_branch {
                     // Apply negative type narrowing for else branch
                     let narrowing = gml_semantic::narrow::infer_narrowing(condition, false);
                     if !narrowing.is_empty() {
                         let old_else_env = self.type_env.clone();
                         for (name, ty) in narrowing {
                             self.type_env.insert(name, ty);
                         }
                         self.visit_stmt(else_stmt);
                         self.type_env = old_else_env;
                     } else {
                         self.visit_stmt(else_stmt);
                     }
                }
            }
            
            Stmt::For { init, condition, update, body, .. } => {
                self.model.push_scope(ScopeKind::Block);
                let old_cf = self.cf_ctx;
                
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                self.cf_ctx.in_loop = true;
                
                if let Some(init) = init {
                    self.visit_stmt(init);
                }
                if let Some(cond) = condition {
                    self.visit_expr(cond);
                }
                if let Some(upd) = update {
                    self.visit_expr(upd);
                }
                
                for inner in &body.statements {
                    self.visit_stmt(inner);
                }
                
                self.cf_ctx = old_cf;
                self.type_env = old_env;
                self.model.pop_scope();
            }
            
            Stmt::While { condition, body, .. } => {
                self.visit_expr(condition);
                
                self.model.push_scope(ScopeKind::Block);
                let old_cf = self.cf_ctx;
                
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                self.cf_ctx.in_loop = true;
                
                for inner in &body.statements {
                    self.visit_stmt(inner);
                }
                
                self.cf_ctx = old_cf;
                self.type_env = old_env;
                self.model.pop_scope();
            }
            
            Stmt::DoUntil { body, condition, .. } => {
                self.model.push_scope(ScopeKind::Block);
                let old_cf = self.cf_ctx;
                
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                self.cf_ctx.in_loop = true;
                
                for inner in &body.statements {
                    self.visit_stmt(inner);
                }
                
                self.cf_ctx = old_cf;
                self.type_env = old_env;
                self.model.pop_scope();
                
                self.visit_expr(condition);
            }
            
            Stmt::Repeat { count, body, .. } => {
                self.visit_expr(count);
                
                self.model.push_scope(ScopeKind::Block);
                let old_cf = self.cf_ctx;
                
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                self.cf_ctx.in_loop = true;
                
                for inner in &body.statements {
                    self.visit_stmt(inner);
                }
                
                self.cf_ctx = old_cf;
                self.type_env = old_env;
                self.model.pop_scope();
            }
            
            Stmt::With { target, body, .. } => {
                self.visit_expr(target);
                
                self.model.push_scope(ScopeKind::Block);
                let old_cf = self.cf_ctx;
                
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                self.cf_ctx.in_loop = true; // with allows break/continue
                
                for inner in &body.statements {
                    self.visit_stmt(inner);
                }
                
                self.cf_ctx = old_cf;
                self.type_env = old_env;
                self.model.pop_scope();
            }
            
            Stmt::Switch { value, cases, .. } => {
                self.visit_expr(value);
                
                let old_cf = self.cf_ctx;
                self.cf_ctx.in_switch = true; // switch allows break but NOT continue
                
                for case in cases {
                    if let Some(val) = &case.value {
                        self.visit_expr(val);
                    }
                    
                    self.model.push_scope(ScopeKind::Block);
                    
                    let old_env = self.type_env.clone();
                    self.type_env = self.type_env.fork();
                    
                    for inner in &case.body {
                        self.visit_stmt(inner);
                    }
                    self.type_env = old_env;
                    self.model.pop_scope();
                }
                
                self.cf_ctx = old_cf;
            }
            
            Stmt::TryCatch { try_block, catch_var, catch_block, finally_block, .. } => {
                self.model.push_scope(ScopeKind::Block);
                for inner in &try_block.statements {
                    self.visit_stmt(inner);
                }
                self.model.pop_scope();
                
                if let Some(catch) = catch_block {
                    self.model.push_scope(ScopeKind::Block);
                    
                    if let Some(var_name) = catch_var {
                        let span = Span { start: 0, end: var_name.len() as u32 };
                        self.model.add_binding(var_name, BindingKind::CatchVar, span, span, None, None, false);
                    }
                    
                    for inner in &catch.statements {
                        self.visit_stmt(inner);
                    }
                    self.model.pop_scope();
                }
                
                if let Some(finally) = finally_block {
                    self.model.push_scope(ScopeKind::Block);
                    for inner in &finally.statements {
                        self.visit_stmt(inner);
                    }
                    self.model.pop_scope();
                }
            }
            
            Stmt::Block(block) => {
                self.model.push_scope(ScopeKind::Block);
                let old_env = self.type_env.clone();
                self.type_env = self.type_env.fork();
                
                for inner in &block.statements {
                    self.visit_stmt(inner);
                }
                self.type_env = old_env;
                self.model.pop_scope();
            }
            
            Stmt::Return { value, span, .. } => {
                // Check if return is outside a function
                if !self.cf_ctx.in_function {
                    let (line, col) = self.ctx.offset_to_line_col(span.start);
                    self.diagnostics.push(Diagnostic::error(
                        "GML016",
                        "`return` statement outside of function".to_string(),
                        gml_diagnostics::Location::new(self.ctx.file_path(), line, col, span.start, span.end),
                    ));
                }
                
                let ty = if let Some(val) = value {
                    gml_semantic::infer::infer_expression(self.ctx.db(), &self.type_env, val)
                } else {
                    gml_semantic::types::Type::Undefined
                };
                if let Some(stack) = self.return_stack.last_mut() {
                    stack.push(ty);
                }

                if let Some(val) = value {
                    self.visit_expr(val);
                }
            }
            
            Stmt::Exit { .. } => {}
            
            Stmt::Break { span, .. } => {
                // break is valid in loops OR switch statements
                if !self.cf_ctx.in_loop && !self.cf_ctx.in_switch {
                    let (line, col) = self.ctx.offset_to_line_col(span.start);
                    self.diagnostics.push(Diagnostic::error(
                        "GML014",
                        "`break` statement outside of loop or switch".to_string(),
                        gml_diagnostics::Location::new(self.ctx.file_path(), line, col, span.start, span.end),
                    ));
                }
            }
            
            Stmt::Continue { span, .. } => {
                if !self.cf_ctx.in_loop {
                    let (line, col) = self.ctx.offset_to_line_col(span.start);
                    self.diagnostics.push(Diagnostic::error(
                        "GML015",
                        "`continue` statement outside of loop".to_string(),
                        gml_diagnostics::Location::new(self.ctx.file_path(), line, col, span.start, span.end),
                    ));
                }
            }
            
            Stmt::Throw { value, .. } => {
                self.visit_expr(value);
            }
            
            Stmt::Expr { expr, .. } => {
                self.visit_expr(expr);
            }
            
            Stmt::Enum { members, .. } => {
                for member in members {
                    if let Some(val) = &member.value {
                        self.visit_expr(val);
                    }
                }
            }
            
            Stmt::Empty { .. } => {}
        }
    }

    /// Visit an expression - builds semantic model AND runs rules
    fn visit_expr(&mut self, expr: &Expr<'a>) {

        // Run all rules on this expression
        for rule in self.rules {
            rule.check_expr(self.ctx, &self.model, &self.type_env, expr, &mut self.diagnostics);
        }

        
        // Build semantic model (track references) AND recurse
        match expr {
            Expr::Identifier { name, .. } => {
                // Check for variadic usage (case-insensitive)
                let lower_name = name.to_lowercase();
                if lower_name == "argument_count" || lower_name == "argument" || 
                   (lower_name.starts_with("argument") && lower_name.len() > 8 && lower_name[8..].chars().all(|c| c.is_ascii_digit())) 
                {
                    if let Some(fn_id) = self.model.current_function_binding() {
                        self.model.mark_variadic(fn_id);
                    }
                }

                self.model.add_reference(name);
            }
            
            Expr::Assignment { target, value, .. } => {
                // Check if assigning to uninitialized variable
                if let Expr::Identifier { name, .. } = target.as_ref() {
                    if let Some(id) = self.model.resolve_name(name) {
                        self.model.mark_initialized(id);
                    }
                    
                    // Update TypeEnv
                    let ty = gml_semantic::infer::infer_expression(self.ctx.db(), &self.type_env, value);
                    self.type_env.insert(name.to_string(), ty);
                } else if let Expr::Member { object, field, .. } = target.as_ref() {
                    // Update struct field in DB if object's type is a known struct
                    let obj_ty = gml_semantic::infer::infer_expression(self.ctx.db(), &self.type_env, object);
                    if let gml_semantic::types::Type::Struct(id) = obj_ty {
                        let val_ty = gml_semantic::infer::infer_expression(self.ctx.db(), &self.type_env, value);
                        self.ctx.db().add_struct_field(id, field.to_string(), val_ty);
                    }
                }
                
                self.visit_lvalue(target);
                self.visit_expr(value);
            }
            
            Expr::Binary { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            
            Expr::Unary { operand, .. } | Expr::Update { operand, .. } => {
                self.visit_expr(operand);
            }
            
            Expr::Call { callee, args, .. } => {
                let mut method_context_keys = Vec::new();

                // Check for method({ ... }, function() {}) pattern
                if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if *name == "method" && args.len() >= 2 {
                        // Arg 0: Struct literal?
                        if let Expr::Struct { fields, .. } = &args[0] {
                            for (key, _) in fields {
                                method_context_keys.push(*key);
                            }
                        }
                    }
                }

                self.visit_expr(callee);
                for (i, arg) in args.iter().enumerate() {
                    // If this is the second argument to method() and we found keys, inject them
                    if i == 1 && !method_context_keys.is_empty() {
                        if let Expr::FunctionExpr { params, body, .. } = arg {
                            self.visit_function_expr_with_context(params, body, &method_context_keys);
                            continue;
                        }
                    }
                    self.visit_expr(arg);
                }
            }
            
            Expr::New { callee, args, .. } => {
                self.visit_expr(callee);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            
            Expr::Index { object, index, .. } => {
                self.visit_expr(object);
                self.visit_expr(index);
            }
            
            Expr::Index2D { object, index1, index2, .. } => {
                self.visit_expr(object);
                self.visit_expr(index1);
                self.visit_expr(index2);
            }
            
            Expr::Member { object, .. } => {
                self.visit_expr(object);
            }
            
            Expr::Ternary { condition, then_expr, else_expr, .. } => {
                self.visit_expr(condition);
                self.visit_expr(then_expr);
                self.visit_expr(else_expr);
            }
            
            Expr::NullCoalesce { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            
            Expr::Array { elements, .. } => {
                for elem in elements {
                    self.visit_expr(elem);
                }
            }
            
            Expr::Struct { fields, .. } => {
                for (_, value) in fields {
                    self.visit_expr(value);
                }
            }
            
            Expr::Grouping { expr, .. } => {
                self.visit_expr(expr);
            }
            
            Expr::FunctionExpr { params, body, .. } => {
                self.visit_function_expr_with_context(params, body, &[]);
            }
            
            Expr::Literal { .. } => {}
        }
    }

    fn visit_function_expr_with_context(&mut self, params: &[Param<'a>], body: &Block<'a>, context_keys: &[&'a str]) {
        // Count required params (those without default values)
        let required_params = params.iter().filter(|p| p.default.is_none()).count();
        let total_params = params.len();

        // Function expressions are anonymous, but we create a binding for them in the model
        // so we can track their variadic status and parameter counts.
        let binding_id = self.model.add_binding("<anonymous>", BindingKind::Function, Span::new(0, 0), Span::new(0, 0), Some(required_params), Some(total_params), false);

        let scope_id = self.model.push_scope(ScopeKind::Function);
        self.model.set_scope_function_binding(scope_id, binding_id);
        
        // Inject context keys as bindings (simulated local variables)
        for key in context_keys {
            // We use a dummy span for these injected variables
            let dummy_span = Span::new(0, 0);
            let id = self.model.add_binding(key, BindingKind::Variable, dummy_span, dummy_span, None, None, true);
            self.model.mark_initialized(id); 
        }

        let old_cf = self.cf_ctx;
        let old_env = self.type_env.clone();
        self.type_env = self.type_env.fork();

        self.cf_ctx.in_function = true;
        self.cf_ctx.in_loop = false;
        
        self.return_stack.push(Vec::new());

        for param in params {
            let param_span = Span {
                start: param.span.start,
                end: param.span.start + param.name.len() as u32,
            };
            self.model.add_binding(param.name, BindingKind::Parameter, param_span, param.span, None, None, false);
            
            // Parameters are Any by default
            self.type_env.insert(param.name.to_string(), gml_semantic::types::Type::Any);
        }

        for stmt in &body.statements {
            self.visit_stmt(stmt);
        }
        
        // Finalize return type for anonymous function
        let _returns = self.return_stack.pop().unwrap();
        // (We don't currently store this anywhere for anonymous functions,
        // but it's popped to keep the stack consistent)

        self.cf_ctx = old_cf;
        self.type_env = old_env;
        self.model.pop_scope();
    }

    /// Visit an expression in L-value context (assignment target)
    /// This does NOT run check_expr rules on the target itself (to avoid "Unknown Identifier" errors),
    /// but recurses into sub-expressions that are read (like array indices or object refs).
    fn visit_lvalue(&mut self, expr: &Expr<'a>) {
        match expr {
            Expr::Identifier { .. } => {
                // Definition/Write - do NOT add_reference, do NOT run checks
            }
            
            Expr::Member { object, .. } => {
                // object.prop = val
                // object IS read
                self.visit_expr(object);
                // prop is written, do nothing
            }
            
            Expr::Index { object, index, .. } => {
                // arr[i] = val
                // arr IS read (we need to find the array to write to it)
                self.visit_expr(object);
                self.visit_expr(index);
            }
            
            Expr::Index2D { object, index1, index2, .. } => {
                self.visit_expr(object);
                self.visit_expr(index1);
                self.visit_expr(index2);
            }
            
            Expr::Array { elements, .. } => {
                // [a, b] = [1, 2] destructuring?
                // GML doesn't really support this standardly in all versions, but if it did:
                for elem in elements {
                    self.visit_lvalue(elem);
                }
            }
            
            // For complex L-values or invalid ones, fall back to visit_expr to be safe/catch errors
            _ => self.visit_expr(expr),
        }
    }

    /// Run semantic-based rules after traversal
    fn run_semantic_rules(&mut self) {
        // Run post-processing for all rules
        for rule in self.rules {
            rule.check_post(self.ctx, &self.model, &mut self.diagnostics);
        }
    }
}


/// Run unified check on a program
pub fn check_program<'a>(
    ctx: &'a LintContext<'a>,
    program: &Program<'a>,
    rules: &'a [Box<dyn Rule>],
) -> Vec<Diagnostic> {

    let checker = Checker::new(ctx, rules);
    let (_, diagnostics) = checker.check(program);
    diagnostics
}

fn unify_types(types: &[gml_semantic::types::Type]) -> gml_semantic::types::Type {
    if types.is_empty() { return gml_semantic::types::Type::Undefined; }
    let first = &types[0];
    if types.iter().all(|t| t == first) {
        return first.clone();
    }
    gml_semantic::types::Type::Any
}

#[cfg(test)]
mod tests {
    use crate::test_utils::*;
    use crate::rules::TypeMismatch;

    // =========================================================================
    // GML016: return/exit at top-level should NOT be an error (scripts are functions)
    // =========================================================================

    #[test]
    fn test_return_at_top_level_is_valid() {
        // In GML, scripts are callable functions, so return is valid at top level
        // We use TypeMismatch as a no-op rule just to trigger the checker
        assert_lint(
            Box::new(TypeMismatch),
            "return 42;",
            "" // No GML016 error expected
        );
    }

    #[test]
    fn test_exit_at_top_level_is_valid() {
        assert_lint(
            Box::new(TypeMismatch),
            "exit;",
            "" // No GML016 error expected
        );
    }

    #[test]
    fn test_return_inside_if_at_top_level_is_valid() {
        assert_lint(
            Box::new(TypeMismatch),
            "if (x > 0) { return x; }",
            "" // No error expected
        );
    }

    #[test]
    fn test_return_inside_inline_function_is_valid() {
        assert_lint(
            Box::new(TypeMismatch),
            "var fn = function() { return 1; };",
            "" // No error expected
        );
    }

    // =========================================================================
    // GML014: break outside loop/switch IS an error
    // =========================================================================

    #[test]
    fn test_break_outside_loop_is_error() {
        // Use a helper that checks for specific diagnostics
        let source = "break;";
        let parser = gml_parser::Parser::new(source);
        let program = parser.parse().unwrap();
        let db = gml_semantic::db::Database::new();
        let provider = crate::DefaultSymbolProvider;
        let ctx = crate::LintContext::new(source, "test.gml", &program, &provider, &db);
        let rules: Vec<Box<dyn crate::Rule>> = vec![];
        let checker = super::Checker::new(&ctx, &rules);
        let (_, diagnostics) = checker.check(&program);
        assert!(diagnostics.iter().any(|d| d.code == "GML014"), "Expected GML014 for break outside loop");
    }

    #[test]
    fn test_break_inside_for_loop_is_valid() {
        let source = "for (var i = 0; i < 10; i++) { break; }";
        let parser = gml_parser::Parser::new(source);
        let program = parser.parse().unwrap();
        let db = gml_semantic::db::Database::new();
        let provider = crate::DefaultSymbolProvider;
        let ctx = crate::LintContext::new(source, "test.gml", &program, &provider, &db);
        let rules: Vec<Box<dyn crate::Rule>> = vec![];
        let checker = super::Checker::new(&ctx, &rules);
        let (_, diagnostics) = checker.check(&program);
        assert!(!diagnostics.iter().any(|d| d.code == "GML014"), "Should NOT get GML014 for break inside loop");
    }

    #[test]
    fn test_break_inside_switch_is_valid() {
        let source = "switch (x) { case 1: break; }";
        let parser = gml_parser::Parser::new(source);
        let program = parser.parse().unwrap();
        let db = gml_semantic::db::Database::new();
        let provider = crate::DefaultSymbolProvider;
        let ctx = crate::LintContext::new(source, "test.gml", &program, &provider, &db);
        let rules: Vec<Box<dyn crate::Rule>> = vec![];
        let checker = super::Checker::new(&ctx, &rules);
        let (_, diagnostics) = checker.check(&program);
        assert!(!diagnostics.iter().any(|d| d.code == "GML014"), "Should NOT get GML014 for break inside switch");
    }

    // =========================================================================
    // GML015: continue outside loop IS an error
    // =========================================================================

    #[test]
    fn test_continue_outside_loop_is_error() {
        let source = "continue;";
        let parser = gml_parser::Parser::new(source);
        let program = parser.parse().unwrap();
        let db = gml_semantic::db::Database::new();
        let provider = crate::DefaultSymbolProvider;
        let ctx = crate::LintContext::new(source, "test.gml", &program, &provider, &db);
        let rules: Vec<Box<dyn crate::Rule>> = vec![];
        let checker = super::Checker::new(&ctx, &rules);
        let (_, diagnostics) = checker.check(&program);
        assert!(diagnostics.iter().any(|d| d.code == "GML015"), "Expected GML015 for continue outside loop");
    }

    #[test]
    fn test_continue_inside_while_loop_is_valid() {
        let source = "while (true) { continue; }";
        let parser = gml_parser::Parser::new(source);
        let program = parser.parse().unwrap();
        let db = gml_semantic::db::Database::new();
        let provider = crate::DefaultSymbolProvider;
        let ctx = crate::LintContext::new(source, "test.gml", &program, &provider, &db);
        let rules: Vec<Box<dyn crate::Rule>> = vec![];
        let checker = super::Checker::new(&ctx, &rules);
        let (_, diagnostics) = checker.check(&program);
        assert!(!diagnostics.iter().any(|d| d.code == "GML015"), "Should NOT get GML015 for continue inside loop");
    }

    #[test]
    fn test_continue_in_switch_without_loop_is_error() {
        // continue is NOT valid inside a switch unless also in a loop
        let source = "switch (x) { case 1: continue; }";
        let parser = gml_parser::Parser::new(source);
        let program = parser.parse().unwrap();
        let db = gml_semantic::db::Database::new();
        let provider = crate::DefaultSymbolProvider;
        let ctx = crate::LintContext::new(source, "test.gml", &program, &provider, &db);
        let rules: Vec<Box<dyn crate::Rule>> = vec![];
        let checker = super::Checker::new(&ctx, &rules);
        let (_, diagnostics) = checker.check(&program);
        assert!(diagnostics.iter().any(|d| d.code == "GML015"), "Expected GML015 for continue in switch without loop");
    }
}

