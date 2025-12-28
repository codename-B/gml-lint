use crate::rule::{Rule, RuleCode};
use crate::context::LintContext;
use crate::semantic::SemanticModel;
use gml_parser::{Expr, BinaryOp};
use gml_diagnostics::{Diagnostic, Location, Category};
use gml_semantic::types::Type;
use gml_semantic::infer::infer_expression;

pub struct TypeMismatch;

impl Rule for TypeMismatch {
    fn code(&self) -> RuleCode {
        RuleCode("GML020")
    }

    fn name(&self) -> &'static str {
        "Type Mismatch"
    }
    
    fn category(&self) -> Category {
        Category::Error
    }

    fn description(&self) -> &'static str {
        "Checks for type mismatches in binary operations."
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, env: &gml_semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        if let Expr::Binary { left, op, right, span } = expr {
             let left_ty = infer_expression(ctx.db(), env, left);
             let right_ty = infer_expression(ctx.db(), env, right);
             
             if *op == BinaryOp::Add {
                 // GML allows Real+Real and String+String.
                 // Real+String or String+Real is generally invalid (unless GML allows implicit stringification? GML logic says usually no).
                 
                 // If either is Any or Undefined, we skip (too noisy).
                 // If either is Array, we might skip or error (arrays generally don't add).
                 
                 match (left_ty, right_ty) {
                     (Type::Real, Type::String) | (Type::String, Type::Real) => {
                         let (line, col) = ctx.offset_to_line_col(span.start);
                         diagnostics.push(Diagnostic::error(
                             self.code().as_str(),
                             "Type mismatch: cannot add Real (number) and String.".to_string(),
                             Location::new(ctx.file_path(), line, col, span.start, span.end),
                         ));
                     },
                     _ => {} // Everything else is potentially valid or covered by 'Any'
                 }
             }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::rules::TypeMismatch;
    use crate::test_utils::*;

    #[test]
    fn test_type_mismatch_numeric_ops() {
        assert_lint(
            Box::new(TypeMismatch),
            "var a = 1 + \"string\";",
            "GML020"
        );
        assert_lint(
            Box::new(TypeMismatch),
            "var a = \"s\" + 1;",
            "GML020"
        );
    }

    #[test]
    fn test_type_match_numeric_ops() {
        assert_lint(
            Box::new(TypeMismatch),
            "var a = 1 + 2;",
            ""
        );
        assert_lint(
            Box::new(TypeMismatch),
            "var a = 1.5 + 3;",
            ""
        );
    }

    #[test]
    fn test_type_match_string_concat() {
        assert_lint(
            Box::new(TypeMismatch),
            "var a = \"hello\" + \" world\";",
            ""
        );
    }

    #[test]
    fn test_type_match_vars() {
        assert_lint(
            Box::new(TypeMismatch),
            "var a = 1; var b = 2; var c = a + b;",
            ""
        );
        assert_lint(
            Box::new(TypeMismatch),
            "var s = \"hello\"; var t = \"world\"; var u = s + t;",
            ""
        );
    }

    #[test]
    fn test_type_mismatch_vars() {
        assert_lint(
            Box::new(TypeMismatch),
            "var a = 1; var s = \"string\"; var c = a + s;",
            "GML020"
        );
    }
}
