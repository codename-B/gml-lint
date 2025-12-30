use crate::parser::Expr;
use super::types::Type;
use rustc_hash::FxHashMap;

/// Result of type narrowing: variable name -> narrowed type
pub type Narrowing = FxHashMap<String, Type>;

/// Analyze an expression to find type guards.
/// truthy: if true, we assume the expression evaluated to true.
pub fn infer_narrowing(expr: &Expr, truthy: bool) -> Narrowing {
    let mut narrowing = FxHashMap::default();
    collect_narrowing(expr, truthy, &mut narrowing);
    narrowing
}

fn collect_narrowing(expr: &Expr, truthy: bool, results: &mut Narrowing) {
    match expr {
        // Handle !expr
        Expr::Unary { op: crate::parser::UnaryOp::Not, operand, .. } => {
            collect_narrowing(operand, !truthy, results);
        }

        // Handle is_string(x), etc.
        Expr::Call { callee, args, .. } => {
            if let Expr::Identifier { name, .. } = &**callee {
                if args.len() == 1 {
                    if let Expr::Identifier { name: var_name, .. } = &args[0] {
                        if let Some(ty) = func_to_type(name) {
                            if truthy {
                                results.insert(var_name.to_string(), ty);
                            }
                        }
                    }
                }
            }
        }

        // Handle typeof(x) == "string"
        Expr::Binary { left, op, right, .. } => {
            match op {
                crate::parser::BinaryOp::Equal => {
                    handle_comparison(left, right, truthy, results);
                }
                crate::parser::BinaryOp::NotEqual => {
                    handle_comparison(left, right, !truthy, results);
                }
                crate::parser::BinaryOp::And if truthy => {
                    collect_narrowing(left, true, results);
                    collect_narrowing(right, true, results);
                }
                crate::parser::BinaryOp::Or if !truthy => {
                    collect_narrowing(left, false, results);
                    collect_narrowing(right, false, results);
                }
                _ => {}
            }
        }
        
        // Handle (condition)
        Expr::Grouping { expr, .. } => {
            collect_narrowing(expr, truthy, results);
        }

        _ => {}
    }
}

fn handle_comparison(left: &Expr, right: &Expr, matches: bool, results: &mut Narrowing) {
    // Handle typeof(x) == "string"
    if let Expr::Call { callee, args, .. } = left {
        if let Expr::Identifier { name, .. } = &**callee {
            if *name == "typeof" && args.len() == 1 {
                if let Expr::Identifier { name: var_name, .. } = &args[0] {
                    if let Expr::Literal { value: crate::parser::Literal::String(s), .. } = right {
                        if matches {
                            if let Some(ty) = string_to_type(s) {
                                results.insert(var_name.to_string(), ty);
                            }
                        }
                    }
                }
            }
        }
    }

    // Handle x != undefined (matches=false if op was != and truthy=true)
    if let Expr::Identifier { name, .. } = left {
        if let Expr::Literal { value: crate::parser::Literal::Undefined, .. } = right {
            if matches {
                results.insert(name.to_string(), Type::Undefined);
            }
        }
    }
}

fn func_to_type(name: &str) -> Option<Type> {
    match name {
        "is_string" => Some(Type::String),
        "is_real" | "is_numeric" => Some(Type::Real), // GMS calls numbers 'real'
        "is_bool" => Some(Type::Bool),
        "is_array" => Some(Type::Array(super::db::TypeId(0))), // Dummy TypeId for now
        "is_pure_undefined" | "is_undefined" => Some(Type::Undefined),
        _ => None,
    }
}

fn string_to_type(s: &str) -> Option<Type> {
    match s {
        "string" => Some(Type::String),
        "number" => Some(Type::Real),
        "bool" => Some(Type::Bool),
        "array" => Some(Type::Array(super::db::TypeId(0))),
        "undefined" => Some(Type::Undefined),
        _ => None,
    }
}
