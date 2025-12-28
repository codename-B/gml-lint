//! GML010: Function has no return (check-has-return)
//!
//! Warns when a function is inconsistent about returning values.
//! If some paths return a value, all paths should return a value.

use gml_diagnostics::{Category, Diagnostic, Location, Fix, Edit};
use gml_parser::{Stmt, Expr, Block};
use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct CheckHasReturn;

impl Rule for CheckHasReturn {
    fn code(&self) -> RuleCode {
        RuleCode("GML010")
    }

    fn category(&self) -> Category {
        Category::Warning
    }

    fn name(&self) -> &'static str {
        "check-has-return"
    }

    fn description(&self) -> &'static str {
        "Functions should consistently return values or not."
    }

    fn check_stmt<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, stmt: &Stmt<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Stmt::FunctionDecl { body, name: _, span, .. } = stmt {
            self.check_function_body(ctx, body, *span, diagnostics);
        }
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &gml_semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Expr::FunctionExpr { body, span, .. } = expr {
            // For anonymous functions, use the function keyword span or start of span
            self.check_function_body(ctx, body, *span, diagnostics);
        }
    }
}

impl CheckHasReturn {
    fn check_function_body<'a>(
        &self,
        ctx: &LintContext<'a>,
        body: &Block<'a>,
        report_span: gml_lexer::Span,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let mut has_val_return = false;
        let mut has_void_return = false;
        let mut returns = Vec::new();

        // Collect all return statements in this function (shallow search)
        self.collect_returns(body, &mut returns);

        for (ret_val, _) in &returns {
            if ret_val.is_some() {
                has_val_return = true;
            } else {
                has_void_return = true;
            }
        }

        // If we have a mix of return values and no-values
        if has_val_return && has_void_return {
            let (line, col) = ctx.offset_to_line_col(report_span.start);
            let mut diag = Diagnostic::info(
                "GML010",
                "Function has mixed `return <value>` and `return;` statements.".to_string(),
                Location::new(ctx.file_path(), line, col, report_span.start, report_span.end),
            );

            // Create fix to correct all void returns
            let mut edits = Vec::new();
            for (ret_val, span) in &returns {
                if ret_val.is_none() {
                    // Replace implicit return with explicit return undefined
                    edits.push(Edit::new(span.start, span.end, "return undefined;"));
                }
            }
            
            if !edits.is_empty() {
                diag = diag.with_fix(Fix {
                    message: "Replace implicit returns with `return undefined;`".to_string(),
                    edits,
                });
            }

            diagnostics.push(diag);
            // Don't return early, still check for fall-off-the-end if we have value returns
        }

        // If we have some value returns, check if we can fall off the end
        if has_val_return && !self.all_paths_return(body) {
            let (line, col) = ctx.offset_to_line_col(report_span.start);
            let mut diag = Diagnostic::warning(
                "GML010",
                "Not all code paths return a value.".to_string(),
                Location::new(ctx.file_path(), line, col, report_span.start, report_span.end),
            );
            
            // Fix: Append "return undefined;" at end of function body
            // body.span includes braces { ... }
            // We insert before the closing brace.
            let insert_pos = body.span.end.saturating_sub(1);
            diag = diag.with_fix(Fix {
                message: "Append `return undefined;` to end of function".to_string(),
                edits: vec![Edit::insert(insert_pos, "\n    return undefined;")],
            });
            
            diagnostics.push(diag);
        }
    }

    fn collect_returns<'a, 'b>(&self, body: &'b Block<'a>, returns: &mut Vec<(Option<&'b Expr<'a>>, gml_lexer::Span)>) {
        for stmt in &body.statements {
            self.collect_returns_in_stmt(stmt, returns);
        }
    }

    fn collect_returns_in_stmt<'a, 'b>(&self, stmt: &'b Stmt<'a>, returns: &mut Vec<(Option<&'b Expr<'a>>, gml_lexer::Span)>) {
        match stmt {
            Stmt::Return { value, span } => {
                returns.push((value.as_ref(), *span));
            }
            Stmt::If { then_branch, else_branch, .. } => {
                self.collect_returns(then_branch, returns);
                if let Some(else_stmt) = else_branch {
                    self.collect_returns_in_stmt(else_stmt, returns);
                }
            }
            Stmt::For { body, .. } | Stmt::While { body, .. } | Stmt::DoUntil { body, .. } | Stmt::Repeat { body, .. } | Stmt::With { body, .. } => {
                self.collect_returns(body, returns);
            }
            Stmt::Switch { cases, .. } => {
                for case in cases {
                    for inner in &case.body {
                        self.collect_returns_in_stmt(inner, returns);
                    }
                }
            }
            Stmt::TryCatch { try_block, catch_block, finally_block, .. } => {
                self.collect_returns(try_block, returns);
                if let Some(catch) = catch_block {
                    self.collect_returns(catch, returns);
                }
                if let Some(finally) = finally_block {
                    self.collect_returns(finally, returns);
                }
            }
            Stmt::Block(block) => {
                self.collect_returns(block, returns);
            }
            // Nested functions are handled by their own visit
            Stmt::FunctionDecl { .. } => {}
            _ => {}
        }
    }

    fn all_paths_return<'a>(&self, body: &Block<'a>) -> bool {
        self.stmt_all_paths_return(&Stmt::Block(body.clone()))
    }

    fn stmt_all_paths_return<'a>(&self, stmt: &Stmt<'a>) -> bool {
        match stmt {
            Stmt::Return { .. } => true,
            Stmt::Throw { .. } => true,
            Stmt::Exit { .. } => true, // Exit is a terminal point in GML
            Stmt::Block(block) => {
                for inner in &block.statements {
                    if self.stmt_all_paths_return(inner) {
                        return true; // Path ends here
                    }
                }
                false
            }
            Stmt::If { then_branch, else_branch, .. } => {
                let then_returns = self.all_paths_return(then_branch);
                if let Some(else_stmt) = else_branch {
                    then_returns && self.stmt_all_paths_return(else_stmt)
                } else {
                    false // No else branch, so the path through no-if is open
                }
            }
            Stmt::Switch { cases, .. } => {
                if cases.is_empty() { return false; }
                let mut has_default = false;
                for case in cases {
                    if case.value.is_none() { has_default = true; }
                }
                if !has_default { return false; }
                
                // If all cases return, then switch returns
                for case in cases {
                    let mut case_returns = false;
                    for inner in &case.body {
                        if self.stmt_all_paths_return(inner) {
                            case_returns = true;
                            break;
                        }
                    }
                    if !case_returns { return false; }
                }
                true
            }
            Stmt::TryCatch { try_block, catch_block, .. } => {
                // If both try and catch return, then the whole thing returns
                let try_returns = self.all_paths_return(try_block);
                if let Some(catch) = catch_block {
                    try_returns && self.all_paths_return(catch)
                } else {
                    try_returns
                }
            }
            // Loops might run 0 times (except do-until), so they don't guarantee return
            _ => false,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::assert_lint;

    #[test]
    fn test_mixed_returns() {
        assert_lint(
            Box::new(CheckHasReturn),
            "function foo() { if (true) return 5; return; }",
            "GML010",
        );
    }

    #[test]
    fn test_fall_through() {
        assert_lint(
            Box::new(CheckHasReturn),
            "function foo() { if (true) return 5; }",
            "GML010",
        );
    }

    #[test]
    fn test_all_paths_return() {
        assert_lint(
            Box::new(CheckHasReturn),
            "function foo() { if (true) return 5; else return 10; }",
            "",
        );
    }
}
