use crate::db::Db;
use crate::types::Type;
use crate::scope::TypeEnv;
use gml_parser::{Expr, Literal, BinaryOp, UnaryOp};

#[allow(clippy::only_used_in_recursion)]
pub fn infer_expression(db: &dyn Db, env: &TypeEnv, expr: &Expr) -> Type {
    match expr {
        Expr::Literal { value, .. } => {
            match value {
                Literal::Integer(_) => Type::Real,
                Literal::Float(_) => Type::Real,
                Literal::String(_) => Type::String,
                Literal::Boolean(_) => Type::Bool,
                Literal::Undefined => Type::Undefined,
            }
        },
        Expr::Grouping { expr, .. } => infer_expression(db, env, expr),
        Expr::Unary { op, operand, .. } => {
            let ty = infer_expression(db, env, operand);
            match op {
                UnaryOp::Not => Type::Bool,
                UnaryOp::Pos | UnaryOp::Neg | UnaryOp::BitNot => {
                    if ty == Type::Real { Type::Real } else { Type::Any }
                },
            }
        },
        Expr::Binary { left, op, right, .. } => {
            let left_ty = infer_expression(db, env, left);
            let right_ty = infer_expression(db, env, right);
            
            match op {
                // Arithmetic
                BinaryOp::Add => {
                    if left_ty == Type::Real && right_ty == Type::Real { Type::Real }
                    else if left_ty == Type::String && right_ty == Type::String { Type::String }
                    else { Type::Any }
                },
                BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod | BinaryOp::IDiv => {
                    Type::Real 
                },
                // Comparison
                BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => {
                    Type::Bool
                },
                // Boolean
                BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                    Type::Bool
                },
                // Bitwise
                BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::ShiftLeft | BinaryOp::ShiftRight => {
                    Type::Real
                }
            }
        },
        Expr::Identifier { name, .. } => {
            env.lookup(name).unwrap_or(Type::Any)
        },
        Expr::Assignment { value, .. } => {
            infer_expression(db, env, value)
        },
        Expr::Struct { fields, .. } => {
            let id = db.define_struct();
            for (name, val) in fields {
                let ty = infer_expression(db, env, val);
                db.add_struct_field(id, name.to_string(), ty);
            }
            Type::Struct(id)
        },
        Expr::Member { object, field, .. } => {
            let obj_ty = infer_expression(db, env, object);
            if let Type::Struct(id) = obj_ty {
                db.lookup_struct_field(id, field).unwrap_or(Type::Any)
            } else {
                Type::Any
            }
        },
        Expr::Call { callee, .. } => {
            let callee_ty = infer_expression(db, env, callee);
            if let Type::Function(id) = callee_ty {
                db.lookup_return_type(id)
            } else {
                Type::Any
            }
        },
        _ => Type::Any,
    }
}
