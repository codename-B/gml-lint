//! Semantic visitor - builds the SemanticModel during AST traversal
//!
//! This visitor collects bindings (declarations) and references (usages) during
//! a single pass through the AST, building a SemanticModel that rules can query.

use crate::lexer::Span;
use crate::parser::{Expr, Stmt};

use crate::semantic_model::{BindingKind, ScopeKind, SemanticModel};

/// Build a SemanticModel from a program by traversing the AST once
pub fn build_semantic_model(program: &crate::parser::Program) -> SemanticModel {
    let mut model = SemanticModel::new();
    
    for stmt in &program.statements {
        visit_stmt(&mut model, stmt);
    }
    
    model
}

fn visit_stmt(model: &mut SemanticModel, stmt: &Stmt) {
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
                
                model.add_binding(decl.name.clone(), kind, name_span, decl.span, None, None, false);
                
                // Visit initializer expression (may reference other variables)
                if let Some(init) = &decl.init {
                    visit_expr(model, init);
                }
            }
        }
        
        Stmt::GlobalVarDecl { name, span, .. } => {
            let name_span = Span {
                start: span.start,
                end: span.start + name.len() as u32,
            };
            model.add_binding(name.clone(), BindingKind::GlobalVar, name_span, *span, None, None, false);
        }
        
        Stmt::FunctionDecl { name, params, body, span, .. } => {
            // Add the function itself as a binding
            let name_span = Span {
                start: span.start,
                end: span.start + name.len() as u32,
            };
            // Count required params (those without default values)
            let required_params = params.iter().filter(|p| p.default.is_none()).count();
            let total_params = params.len();
            let binding_id = model.add_binding(name.clone(), BindingKind::Function, name_span, *span, Some(required_params), Some(total_params), false);
            
            // Enter function scope
            let scope_id = model.push_scope(ScopeKind::Function);
            model.set_scope_function_binding(scope_id, binding_id);
            
            // Add parameters as bindings
            for param in params {
                let param_name_span = Span {
                    start: param.span.start,
                    end: param.span.start + param.name.len() as u32,
                };
                model.add_binding(param.name.clone(), BindingKind::Parameter, param_name_span, param.span, None, None, false);
                
                // Visit default value if any
                if let Some(default) = &param.default {
                    visit_expr(model, default);
                }
            }
            
            // Visit function body
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            
            model.pop_scope();
        }
        
        Stmt::If { condition, then_branch, else_branch, .. } => {
            visit_expr(model, condition);
            
            model.push_scope(ScopeKind::Block);
            for stmt in &then_branch.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
            
            if let Some(else_stmt) = else_branch {
                visit_stmt(model, else_stmt);
            }
        }
        
        Stmt::For { init, condition, update, body, .. } => {
            model.push_scope(ScopeKind::Block);
            
            if let Some(init_stmt) = init {
                visit_stmt(model, init_stmt);
            }
            if let Some(cond) = condition {
                visit_expr(model, cond);
            }
            if let Some(upd) = update {
                visit_expr(model, upd);
            }
            
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            
            model.pop_scope();
        }
        
        Stmt::While { condition, body, .. } => {
            visit_expr(model, condition);
            
            model.push_scope(ScopeKind::Block);
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
        }
        
        Stmt::DoUntil { body, condition, .. } => {
            model.push_scope(ScopeKind::Block);
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
            
            visit_expr(model, condition);
        }
        
        Stmt::Repeat { count, body, .. } => {
            visit_expr(model, count);
            
            model.push_scope(ScopeKind::Block);
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
        }
        
        Stmt::With { target, body, .. } => {
            visit_expr(model, target);
            
            model.push_scope(ScopeKind::Block);
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
        }
        
        Stmt::Switch { value, cases, .. } => {
            visit_expr(model, value);
            
            for case in cases {
                if let Some(val) = &case.value {
                    visit_expr(model, val);
                }
                
                model.push_scope(ScopeKind::Block);
                for stmt in &case.body {
                    visit_stmt(model, stmt);
                }
                model.pop_scope();
            }
        }
        
        Stmt::TryCatch { try_block, catch_var, catch_block, finally_block, .. } => {
            model.push_scope(ScopeKind::Block);
            for stmt in &try_block.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
            
            if let Some(catch) = catch_block {
                model.push_scope(ScopeKind::Block);
                
                // Add catch variable as binding
                if let Some(var_name) = catch_var {
                    let span = Span { start: 0, end: var_name.len() as u32 }; // Approximate
                    model.add_binding(var_name.clone(), BindingKind::CatchVar, span, span, None, None, false);
                }
                
                for stmt in &catch.statements {
                    visit_stmt(model, stmt);
                }
                model.pop_scope();
            }
            
            if let Some(finally) = finally_block {
                model.push_scope(ScopeKind::Block);
                for stmt in &finally.statements {
                    visit_stmt(model, stmt);
                }
                model.pop_scope();
            }
        }
        
        Stmt::Block(block) => {
            model.push_scope(ScopeKind::Block);
            for stmt in &block.statements {
                visit_stmt(model, stmt);
            }
            model.pop_scope();
        }
        
        Stmt::Return { value, .. } => {
            if let Some(val) = value {
                visit_expr(model, val);
            }
        }
        
        Stmt::Throw { value, .. } => {
            visit_expr(model, value);
        }
        
        Stmt::Expr { expr, .. } => {
            visit_expr(model, expr);
        }
        
        Stmt::Enum { members, .. } => {
            for member in members {
                if let Some(val) = &member.value {
                    visit_expr(model, val);
                }
            }
        }
        
        // No bindings or references in these
        Stmt::Exit { .. } 
        | Stmt::Break { .. } 
        | Stmt::Continue { .. }
        | Stmt::Empty { .. } => {}
    }
}

fn visit_expr(model: &mut SemanticModel, expr: &Expr) {
    match expr {
        Expr::Identifier { name, .. } => {
            // Check for variadic usage (case-insensitive)
            let lower_name = name.to_lowercase();
            if lower_name == "argument_count" || lower_name == "argument" || 
               (lower_name.starts_with("argument") && lower_name.len() > 8 && lower_name[8..].chars().all(|c| c.is_ascii_digit())) 
            {
                if let Some(fn_id) = model.current_function_binding() {
                    model.mark_variadic(fn_id);
                }
            }

            // This is a reference to a variable
            model.add_reference(name);
        }
        
        Expr::Assignment { target, value, .. } => {
            // Check if assigning to an uninitialized variable
            if let Expr::Identifier { name, .. } = target.as_ref() {
                if let Some(id) = model.resolve_name(name) {
                    model.mark_initialized(id);
                }
            }
            
            // Visit target and value
            visit_expr(model, target);
            visit_expr(model, value);
        }
        
        Expr::Binary { left, right, .. } => {
            visit_expr(model, left);
            visit_expr(model, right);
        }
        
        Expr::Unary { operand, .. } | Expr::Update { operand, .. } => {
            visit_expr(model, operand);
        }
        
        Expr::Call { callee, args, .. } | Expr::New { callee, args, .. } => {
            visit_expr(model, callee);
            for arg in args {
                visit_expr(model, arg);
            }
        }
        
        Expr::Index { object, index, .. } => {
            visit_expr(model, object);
            visit_expr(model, index);
        }
        
        Expr::Index2D { object, index1, index2, .. } => {
            visit_expr(model, object);
            visit_expr(model, index1);
            visit_expr(model, index2);
        }
        
        Expr::Member { object, .. } => {
            visit_expr(model, object);
        }
        
        Expr::Ternary { condition, then_expr, else_expr, .. } => {
            visit_expr(model, condition);
            visit_expr(model, then_expr);
            visit_expr(model, else_expr);
        }
        
        Expr::NullCoalesce { left, right, .. } => {
            visit_expr(model, left);
            visit_expr(model, right);
        }
        
        Expr::Array { elements, .. } => {
            for elem in elements {
                visit_expr(model, elem);
            }
        }
        
        Expr::Struct { fields, .. } => {
            for (_, value) in fields {
                visit_expr(model, value);
            }
        }
        
        Expr::Grouping { expr, .. } => {
            visit_expr(model, expr);
        }
        
        Expr::FunctionExpr { params, body, .. } => {
            // Count required params (those without default values)
            let required_params = params.iter().filter(|p| p.default.is_none()).count();
            let total_params = params.len();

            // Function expressions are anonymous, but we create a binding for them in the model
            // so we can track their variadic status and parameter counts.
            let binding_id = model.add_binding("<anonymous>", BindingKind::Function, Span::new(0, 0), Span::new(0, 0), Some(required_params), Some(total_params), false);

            let scope_id = model.push_scope(ScopeKind::Function);
            model.set_scope_function_binding(scope_id, binding_id);
            
            for param in params {
                let param_name_span = Span {
                    start: param.span.start,
                    end: param.span.start + param.name.len() as u32,
                };
                model.add_binding(param.name.clone(), BindingKind::Parameter, param_name_span, param.span, None, None, false);
            }
            
            for stmt in &body.statements {
                visit_stmt(model, stmt);
            }
            
            model.pop_scope();
        }
        
        // Literals have no references
        Expr::Literal { .. } => {}

    }
}
