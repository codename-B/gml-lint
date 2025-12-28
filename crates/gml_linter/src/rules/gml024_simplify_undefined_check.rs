use gml_diagnostics::{Category, Diagnostic, Location, Fix, Edit};
use gml_parser::{Expr, BinaryOp, Literal};
use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct SimplifyUndefinedCheck;

impl Rule for SimplifyUndefinedCheck {
    fn code(&self) -> RuleCode {
        RuleCode("GML024")
    }

    fn name(&self) -> &'static str {
        "SimplifyUndefinedCheck"
    }

    fn category(&self) -> Category {
        Category::Style
    }

    fn description(&self) -> &'static str {
        "Use implicit boolean check instead of explicit 'undefined' check"
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _type_env: &gml_semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        let Expr::Binary { left, op, right, span } = expr else {
            return;
        };

        // Determine which side (if any) is 'undefined'
        let left_is_undef = matches!(**left, Expr::Literal { value: Literal::Undefined, .. });
        let right_is_undef = matches!(**right, Expr::Literal { value: Literal::Undefined, .. });

        if !left_is_undef && !right_is_undef {
            return;
        }

        // We only care about == and !=
        if !matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
            return;
        }

        let target_expr = if left_is_undef { right } else { left };
        
        let (message, replacement) = match op {
            BinaryOp::Equal => {
                // x == undefined -> !x
                // Get the text of the non-undefined operand
                let target_text = &ctx.source()[target_expr.span().start as usize..target_expr.span().end as usize];
                
                // If the target expression has lower precedence than unary '!', wrap in parens
                // For safety, we can check basic types. Identifiers/Literals don't need parens.
                let needs_parens = match **target_expr {
                    Expr::Identifier { .. } | Expr::Literal { .. } | Expr::Call { .. } | Expr::Member { .. } | Expr::Index { .. } | Expr::Grouping { .. } => false,
                    _ => true,
                };

                let new_code = if needs_parens {
                    format!("!({})", target_text)
                } else {
                    format!("!{}", target_text)
                };

                ("Use '!variable' instead of 'variable == undefined'", new_code)
            }
            BinaryOp::NotEqual => {
                // x != undefined -> x
                let target_text = &ctx.source()[target_expr.span().start as usize..target_expr.span().end as usize];
                
                // Note: If this is used in a boolean context (if, while), just 'x' is fine.
                // If it's used as a value `var b = (x != undefined)`, then `var b = x` relies on x being truthy.
                // In GML, undefined is falsy (sort of, keeps changing). But `!undefined` is true. `!!undefined` is false.
                // The user explicitly asked for this rule, so they likely follow the "undefined is falsey" convention.
                
                ("Use 'variable' instead of 'variable != undefined'", target_text.to_string())
            }
            _ => unreachable!(), 
        };

        let (line, col) = ctx.offset_to_line_col(span.start);
        
        diagnostics.push(Diagnostic::hint(
            self.code().0,
            message,
            Location::new(ctx.file_path(), line, col, span.start, span.end)
        ).with_fix(Fix {
            message: "Simplify undefined check".to_string(),
            edits: vec![Edit::new(span.start, span.end, replacement)],
        }));
    }
}
