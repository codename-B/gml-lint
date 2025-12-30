//! Rule trait and utilities

use crate::diagnostics::{Category, Diagnostic};
use crate::parser::{Expr, Stmt};
use crate::{LintContext, SemanticModel};

/// A rule code identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RuleCode(pub &'static str);

impl RuleCode {
    pub fn as_str(&self) -> &'static str {
        self.0
    }
}

impl std::fmt::Display for RuleCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A lint rule
/// 
/// Rules can implement checks in two ways:
/// 1. Override `check_stmt()`/`check_expr()` for per-node analysis (called during single-pass traversal)
/// 2. Override `check_post()` for post-processing after the full traversal (e.g., unused variables)
/// 
/// The single-pass approach is more efficient for most rules.
pub trait Rule: Send + Sync {
    /// The rule code (e.g., "GML001")
    fn code(&self) -> RuleCode;
    
    /// The rule category
    fn category(&self) -> Category;
    
    /// Human-readable name
    fn name(&self) -> &'static str;
    
    /// Description of what this rule checks
    fn description(&self) -> &'static str;
    
    /// Check a single statement during single-pass traversal.
    /// This is called once per statement in the AST.
    /// Override this for statement-level rules.
    #[allow(unused_variables)]
    fn check_stmt<'a>(&self, _ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, _stmt: &Stmt<'a>, _diagnostics: &mut Vec<Diagnostic>) {}

    
    /// Check a single expression during single-pass traversal.
    /// This is called once per expression in the AST.
    /// Override this for expression-level rules.
    #[allow(unused_variables)]
    fn check_expr<'a>(&self, _ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _env: &crate::semantic::scope::TypeEnv, _expr: &Expr<'a>, _diagnostics: &mut Vec<Diagnostic>) {}

    /// Run post-processing rules after the full AST traversal and semantic model building.
    #[allow(unused_variables)]
    fn check_post<'a>(&self, _ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _diagnostics: &mut Vec<Diagnostic>) {}
}

