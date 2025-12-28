//! Built-in lint rules

mod gml001_uninitialized_var;
mod gml002_readonly_assignment;
mod gml003_unused_var;
mod gml004_unreachable_code;
mod gml005_missing_semicolon;
mod gml006_keyword_shadowing;
mod gml007_no_single_equals;
mod gml008_require_parentheses;
mod gml009_semicolon_before_branch;
mod gml010_check_has_return;
mod gml011_check_argument_counts;
mod gml012_warn_var_redeclaration;
mod gml013_unknown_function;
mod gml014_016_control_flow;
mod gml017_018_constructors;
mod gml020_type_mismatch;
mod gml021_invalid_assignment;
mod gml022_unknown_identifier;
mod gml023_uninitialized_global;
mod gml024_simplify_undefined_check;

use crate::Rule;

pub use gml001_uninitialized_var::UninitializedVariable;
pub use gml002_readonly_assignment::ReadonlyAssignment;
pub use gml003_unused_var::UnusedVariable;
pub use gml004_unreachable_code::UnreachableCode;
pub use gml005_missing_semicolon::MissingSemicolon;
pub use gml006_keyword_shadowing::KeywordShadowing;
pub use gml007_no_single_equals::NoSingleEquals;
pub use gml008_require_parentheses::RequireParentheses;
pub use gml009_semicolon_before_branch::SemicolonBeforeBranch;
pub use gml010_check_has_return::CheckHasReturn;
pub use gml011_check_argument_counts::CheckArgumentCounts;
pub use gml012_warn_var_redeclaration::WarnVarRedeclaration;
pub use gml013_unknown_function::UnknownFunction;
pub use gml017_018_constructors::ConstructorValidation;
pub use gml020_type_mismatch::TypeMismatch;
pub use gml021_invalid_assignment::InvalidAssignment;
pub use gml022_unknown_identifier::UnknownIdentifier;
pub use gml023_uninitialized_global::UninitializedGlobal;
pub use gml024_simplify_undefined_check::SimplifyUndefinedCheck;

/// Get all built-in rules
/// Note: GML003 (UnusedVariable) is handled by semantic rules in lib.rs, not here
pub fn all_rules() -> Vec<Box<dyn Rule>> {
    vec![
        Box::new(UninitializedVariable),
        Box::new(ReadonlyAssignment),
        // GML003 is now handled by semantic rules
        Box::new(UnreachableCode),
        Box::new(MissingSemicolon),
        Box::new(KeywordShadowing),
        Box::new(NoSingleEquals),
        Box::new(RequireParentheses),
        Box::new(SemicolonBeforeBranch),
        Box::new(CheckHasReturn),
        Box::new(CheckArgumentCounts),
        Box::new(WarnVarRedeclaration),
        Box::new(UnknownFunction),
        Box::new(ConstructorValidation),
        Box::new(TypeMismatch),
        Box::new(InvalidAssignment),
        Box::new(UnknownIdentifier),
        Box::new(UninitializedGlobal),
        Box::new(SimplifyUndefinedCheck),
    ]
}
