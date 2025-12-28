//! AST node definitions for GML

use gml_lexer::Span;

/// A GML program (file)
#[derive(Debug, Clone)]
pub struct Program<'a> {
    pub statements: Vec<Stmt<'a>>,
    pub span: Span,
}

/// A statement in GML
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    /// Variable declaration: `var x = 5;` or `var a, b = 1, c;`
    VarDecl {
        declarations: Vec<VarDeclItem<'a>>,
        span: Span,
    },
    /// Global variable declaration: `globalvar g_score;`
    GlobalVarDecl {
        name: &'a str,
        span: Span,
    },
    /// Function declaration: `function foo(a, b) { ... }`
    FunctionDecl {
        name: &'a str,
        params: Vec<Param<'a>>,
        body: Block<'a>,
        is_constructor: bool,
        span: Span,
    },
    /// If statement
    If {
        condition: Expr<'a>,
        then_branch: Block<'a>,
        else_branch: Option<Box<Stmt<'a>>>,
        has_parentheses: bool,
        span: Span,
    },
    /// For loop
    For {
        init: Option<Box<Stmt<'a>>>,
        condition: Option<Expr<'a>>,
        update: Option<Expr<'a>>,
        body: Block<'a>,
        span: Span,
    },
    /// While loop
    While {
        condition: Expr<'a>,
        body: Block<'a>,
        has_parentheses: bool,
        span: Span,
    },
    /// Do-until loop
    DoUntil {
        body: Block<'a>,
        condition: Expr<'a>,
        has_parentheses: bool,
        span: Span,
    },
    /// Repeat loop
    Repeat {
        count: Expr<'a>,
        body: Block<'a>,
        has_parentheses: bool,
        span: Span,
    },
    /// With statement
    With {
        target: Expr<'a>,
        body: Block<'a>,
        has_parentheses: bool,
        span: Span,
    },
    /// Switch statement
    Switch {
        value: Expr<'a>,
        cases: Vec<Case<'a>>,
        has_parentheses: bool,
        span: Span,
    },
    /// Return statement
    Return {
        value: Option<Expr<'a>>,
        span: Span,
    },
    /// Exit statement
    Exit {
        span: Span,
    },
    /// Break statement
    Break {
        span: Span,
    },
    /// Continue statement
    Continue {
        span: Span,
    },
    /// Throw statement
    Throw {
        value: Expr<'a>,
        span: Span,
    },
    /// Try-catch-finally
    TryCatch {
        try_block: Block<'a>,
        catch_var: Option<&'a str>,
        catch_block: Option<Block<'a>>,
        finally_block: Option<Block<'a>>,
        span: Span,
    },
    /// Enum declaration
    Enum {
        name: &'a str,
        members: Vec<EnumMember<'a>>,
        span: Span,
    },
    /// Block of statements
    Block(Block<'a>),
    /// Expression statement
    Expr {
        expr: Expr<'a>,
        span: Span,
    },
    /// Empty statement
    Empty {
        span: Span,
    },
}

impl<'a> Stmt<'a> {
    pub fn span(&self) -> Span {
        match self {
            Stmt::VarDecl { span, .. } => *span,
            Stmt::GlobalVarDecl { span, .. } => *span,
            Stmt::FunctionDecl { span, .. } => *span,
            Stmt::If { span, .. } => *span,
            Stmt::For { span, .. } => *span,
            Stmt::While { span, .. } => *span,
            Stmt::DoUntil { span, .. } => *span,
            Stmt::Repeat { span, .. } => *span,
            Stmt::With { span, .. } => *span,
            Stmt::Switch { span, .. } => *span,
            Stmt::Return { span, .. } => *span,
            Stmt::Exit { span, .. } => *span,
            Stmt::Break { span, .. } => *span,
            Stmt::Continue { span, .. } => *span,
            Stmt::Throw { span, .. } => *span,
            Stmt::TryCatch { span, .. } => *span,
            Stmt::Enum { span, .. } => *span,
            Stmt::Block(block) => block.span,
            Stmt::Expr { span, .. } => *span,
            Stmt::Empty { span, .. } => *span,
        }
    }
}


/// A block of statements
#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub statements: Vec<Stmt<'a>>,
    pub span: Span,
}

impl<'a> Block<'a> {
    pub fn new(statements: Vec<Stmt<'a>>, span: Span) -> Self {
        Self { statements, span }
    }
}

/// A function parameter
#[derive(Debug, Clone)]
pub struct Param<'a> {
    pub name: &'a str,
    pub default: Option<Expr<'a>>,
    pub span: Span,
}

/// A switch case
#[derive(Debug, Clone)]
pub struct Case<'a> {
    pub value: Option<Expr<'a>>, // None = default case
    pub body: Vec<Stmt<'a>>,
    pub span: Span,
}

/// An enum member
#[derive(Debug, Clone)]
pub struct EnumMember<'a> {
    pub name: &'a str,
    pub value: Option<Expr<'a>>,
    pub span: Span,
}

/// A single variable declaration item
#[derive(Debug, Clone)]
pub struct VarDeclItem<'a> {
    pub name: &'a str,
    pub init: Option<Expr<'a>>,
    pub span: Span,
}

/// An expression in GML
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    /// Literal value
    Literal {
        value: Literal<'a>,
        span: Span,
    },

    /// Identifier
    Identifier {
        name: &'a str,
        span: Span,
    },
    /// Binary operation: `a + b`
    Binary {
        left: Box<Expr<'a>>,
        op: BinaryOp,
        right: Box<Expr<'a>>,
        span: Span,
    },
    /// Unary operation: `-x`, `!x`
    Unary {
        op: UnaryOp,
        operand: Box<Expr<'a>>,
        span: Span,
    },
    /// Function call: `foo(a, b)`
    Call {
        callee: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
        span: Span,
    },
    /// Member access: `obj.x`
    Member {
        object: Box<Expr<'a>>,
        field: &'a str,
        span: Span,
    },
    /// Index access: `arr[0]`
    Index {
        object: Box<Expr<'a>>,
        index: Box<Expr<'a>>,
        span: Span,
    },
    /// 2D Index access: `arr[x, y]`, `grid[# x, y]`
    Index2D {
        object: Box<Expr<'a>>,
        index1: Box<Expr<'a>>,
        index2: Box<Expr<'a>>,
        span: Span,
    },
    /// Array literal: `[1, 2, 3]`
    Array {
        elements: Vec<Expr<'a>>,
        span: Span,
    },
    /// Struct literal: `{ x: 1, y: 2 }`
    Struct {
        fields: Vec<(&'a str, Expr<'a>)>,
        span: Span,
    },
    /// Assignment: `x = 5`
    Assignment {
        target: Box<Expr<'a>>,
        op: AssignOp,
        value: Box<Expr<'a>>,
        span: Span,
    },
    /// Ternary: `cond ? a : b`
    Ternary {
        condition: Box<Expr<'a>>,
        then_expr: Box<Expr<'a>>,
        else_expr: Box<Expr<'a>>,
        span: Span,
    },
    /// Null coalescing: `a ?? b`
    NullCoalesce {
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
        span: Span,
    },
    /// Pre/post increment/decrement
    Update {
        operand: Box<Expr<'a>>,
        op: UpdateOp,
        prefix: bool,
        span: Span,
    },
    /// New expression: `new Foo()`
    New {
        callee: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
        span: Span,
    },
    /// Grouping (parentheses)
    Grouping {
        expr: Box<Expr<'a>>,
        span: Span,
    },
    /// Anonymous function expression: `function(a, b) { ... }`
    FunctionExpr {
        params: Vec<Param<'a>>,
        body: Block<'a>,
        is_constructor: bool,
        span: Span,
    },
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal { span, .. } => *span,

            Expr::Identifier { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Member { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Index2D { span, .. } => *span,
            Expr::Array { span, .. } => *span,
            Expr::Struct { span, .. } => *span,
            Expr::Assignment { span, .. } => *span,
            Expr::Ternary { span, .. } => *span,
            Expr::NullCoalesce { span, .. } => *span,
            Expr::Update { span, .. } => *span,
            Expr::New { span, .. } => *span,
            Expr::Grouping { span, .. } => *span,
            Expr::FunctionExpr { span, .. } => *span,
        }
    }
}

/// A literal value
#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Integer(i64),
    Float(f64),
    String(&'a str),
    Boolean(bool),
    Undefined,
}

impl<'a> Literal<'a> {
}


/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod, IDiv,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    And, Or, Xor,
    BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Pos, Neg, Not, BitNot,
}

/// Assignment operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    Add, Sub, Mul, Div, Mod,
    BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight,
    NullCoalesce,
}

/// Increment/decrement operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateOp {
    Increment,
    Decrement,
}

/// Accessor types for GML
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessorKind {
    List, Map, Grid, Struct, ArrayDirect
}
