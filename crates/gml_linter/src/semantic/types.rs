use super::db::TypeId;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Real,
    String,
    Bool,
    Undefined,
    Array(TypeId),
    Struct(TypeId), // Point to a struct definition in the DB
    Function(TypeId), // Point to a function signature in the DB
    Any,
    Never,
}
