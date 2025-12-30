use crate::diagnostics::{Category, Diagnostic, Location};
use crate::parser::Expr;
use crate::{LintContext, Rule, RuleCode, SemanticModel};

pub struct UninitializedGlobal;

impl Rule for UninitializedGlobal {
    fn code(&self) -> RuleCode {
        RuleCode("GML023")
    }

    fn name(&self) -> &'static str {
        "UninitializedGlobal"
    }

    fn category(&self) -> Category {
        Category::Error
    }

    fn description(&self) -> &'static str {
        "Global variable read but never assigned anywhere in project"
    }

    fn check_expr<'a>(&self, ctx: &LintContext<'a>, _model: &SemanticModel<'a>, _type_env: &crate::semantic::scope::TypeEnv, expr: &Expr<'a>, diagnostics: &mut Vec<Diagnostic>) {
        // We are looking for global.variable reads
        let Expr::Member { object, field, span } = expr else {
            return;
        };

        // Check if object is "global" identifier
        let Expr::Identifier { name: obj_name, .. } = &**object else {
            return;
        };
        
        if *obj_name != "global" {
            return;
        }

        // Check against symbol provider
        if !ctx.symbol_provider().has_global(field) {
            let (line, col) = ctx.offset_to_line_col(span.start);
            diagnostics.push(Diagnostic::error(
                self.code().0,
                format!("Global variable 'global.{}' is read but never assigned", field),
                Location::new(ctx.file_path(), line, col, span.start, span.end)
            ));
        }
    }
}
