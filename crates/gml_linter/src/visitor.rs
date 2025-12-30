//! Single-pass AST visitor for running all lint rules efficiently
//!
//! Instead of each rule traversing the AST separately, we traverse once
//! and call all rules at each node. This is the key optimization from ruff.

use crate::diagnostics::Diagnostic;
use crate::parser::{Expr, Stmt};
use crate::{LintContext, Rule};

/// Visit the entire program with all rules in a single pass
pub fn visit_with_rules(
    ctx: &LintContext,
    rules: &[Box<dyn Rule>],
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::with_capacity(16); // Pre-allocate
    
    for stmt in &ctx.program().statements {
        visit_stmt(ctx, stmt, rules, &mut diagnostics);
    }
    
    diagnostics
}

/// Visit a statement and all its children, running rules at each node
fn visit_stmt(
    ctx: &LintContext,
    stmt: &Stmt,
    rules: &[Box<dyn Rule>],
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Run all rules on this statement
    for rule in rules {
        rule.check_stmt(ctx, stmt, diagnostics);
    }
    
    // Visit child expressions and recurse into nested statements
    match stmt {
        Stmt::VarDecl { declarations, .. } => {
            for decl in declarations {
                if let Some(init) = &decl.init {
                    visit_expr(ctx, init, rules, diagnostics);
                }
            }
        }
        
        Stmt::GlobalVarDecl { .. } => {}
        
        Stmt::FunctionDecl { body, .. } => {
            for inner in &body.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
        }
        
        Stmt::If { condition, then_branch, else_branch, .. } => {
            visit_expr(ctx, condition, rules, diagnostics);
            for inner in &then_branch.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
            if let Some(else_stmt) = else_branch {
                visit_stmt(ctx, else_stmt, rules, diagnostics);
            }
        }
        
        Stmt::For { init, condition, update, body, .. } => {
            if let Some(init) = init {
                visit_stmt(ctx, init, rules, diagnostics);
            }
            if let Some(cond) = condition {
                visit_expr(ctx, cond, rules, diagnostics);
            }
            if let Some(upd) = update {
                visit_expr(ctx, upd, rules, diagnostics);
            }
            for inner in &body.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
        }
        
        Stmt::While { condition, body, .. } => {
            visit_expr(ctx, condition, rules, diagnostics);
            for inner in &body.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
        }
        
        Stmt::DoUntil { body, condition, .. } => {
            for inner in &body.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
            visit_expr(ctx, condition, rules, diagnostics);
        }
        
        Stmt::Repeat { count, body, .. } => {
            visit_expr(ctx, count, rules, diagnostics);
            for inner in &body.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
        }
        
        Stmt::With { target, body, .. } => {
            visit_expr(ctx, target, rules, diagnostics);
            for inner in &body.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
        }
        
        Stmt::Switch { value, cases, .. } => {
            visit_expr(ctx, value, rules, diagnostics);
            for case in cases {
                if let Some(val) = &case.value {
                    visit_expr(ctx, val, rules, diagnostics);
                }
                for inner in &case.body {
                    visit_stmt(ctx, inner, rules, diagnostics);
                }
            }
        }
        
        Stmt::TryCatch { try_block, catch_block, finally_block, .. } => {
            for inner in &try_block.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
            if let Some(catch) = catch_block {
                for inner in &catch.statements {
                    visit_stmt(ctx, inner, rules, diagnostics);
                }
            }
            if let Some(finally) = finally_block {
                for inner in &finally.statements {
                    visit_stmt(ctx, inner, rules, diagnostics);
                }
            }
        }
        
        Stmt::Block(block) => {
            for inner in &block.statements {
                visit_stmt(ctx, inner, rules, diagnostics);
            }
        }
        
        Stmt::Return { value, .. } => {
            if let Some(val) = value {
                visit_expr(ctx, val, rules, diagnostics);
            }
        }
        
        Stmt::Throw { value, .. } => {
            visit_expr(ctx, value, rules, diagnostics);
        }
        
        Stmt::Expr { expr, .. } => {
            visit_expr(ctx, expr, rules, diagnostics);
        }
        
        Stmt::Enum { members, .. } => {
            for member in members {
                if let Some(val) = &member.value {
                    visit_expr(ctx, val, rules, diagnostics);
                }
            }
        }
        
        // Statements with no children to visit
        Stmt::Exit { .. } 
        | Stmt::Break { .. } 
        | Stmt::Continue { .. }
        | Stmt::Empty { .. } => {}
    }
}

/// Visit an expression and all its children
fn visit_expr(
    ctx: &LintContext,
    expr: &Expr,
    rules: &[Box<dyn Rule>],
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Run all rules on this expression
    for rule in rules {
        rule.check_expr(ctx, expr, diagnostics);
    }
    
    // Recurse into child expressions
    match expr {
        Expr::Binary { left, right, .. } => {
            visit_expr(ctx, left, rules, diagnostics);
            visit_expr(ctx, right, rules, diagnostics);
        }
        
        Expr::Unary { operand, .. } => {
            visit_expr(ctx, operand, rules, diagnostics);
        }
        
        Expr::Update { operand, .. } => {
            visit_expr(ctx, operand, rules, diagnostics);
        }
        
        Expr::Call { callee, args, .. } => {
            visit_expr(ctx, callee, rules, diagnostics);
            for arg in args {
                visit_expr(ctx, arg, rules, diagnostics);
            }
        }
        
        Expr::New { callee, args, .. } => {
            visit_expr(ctx, callee, rules, diagnostics);
            for arg in args {
                visit_expr(ctx, arg, rules, diagnostics);
            }
        }
        
        Expr::Index { object, index, .. } => {
            visit_expr(ctx, object, rules, diagnostics);
            visit_expr(ctx, index, rules, diagnostics);
        }
        
        Expr::Index2D { object, index1, index2, .. } => {
            visit_expr(ctx, object, rules, diagnostics);
            visit_expr(ctx, index1, rules, diagnostics);
            visit_expr(ctx, index2, rules, diagnostics);
        }
        
        Expr::Member { object, .. } => {
            visit_expr(ctx, object, rules, diagnostics);
        }
        
        Expr::Ternary { condition, then_expr, else_expr, .. } => {
            visit_expr(ctx, condition, rules, diagnostics);
            visit_expr(ctx, then_expr, rules, diagnostics);
            visit_expr(ctx, else_expr, rules, diagnostics);
        }
        
        Expr::NullCoalesce { left, right, .. } => {
            visit_expr(ctx, left, rules, diagnostics);
            visit_expr(ctx, right, rules, diagnostics);
        }
        
        Expr::Array { elements, .. } => {
            for elem in elements {
                visit_expr(ctx, elem, rules, diagnostics);
            }
        }
        
        Expr::Struct { fields, .. } => {
            for (_, value) in fields {
                visit_expr(ctx, value, rules, diagnostics);
            }
        }
        
        Expr::Grouping { expr, .. } => {
            visit_expr(ctx, expr, rules, diagnostics);
        }
        
        Expr::Assignment { target, value, .. } => {
            visit_expr(ctx, target, rules, diagnostics);
            visit_expr(ctx, value, rules, diagnostics);
        }
        
        Expr::FunctionExpr { body, .. } => {
            for stmt in &body.statements {
                visit_stmt(ctx, stmt, rules, diagnostics);
            }
        }
        
        // Leaf expressions with no children
        Expr::Identifier { .. }
        Expr::Literal { .. } => {}

    }
}
