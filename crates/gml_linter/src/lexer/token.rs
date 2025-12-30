//! Token definitions for GML

/// A span in the source code (byte offsets)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

/// A token produced by the lexer
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Macro definition content
#[derive(Debug, Clone, PartialEq)]
pub struct MacroDefinition<'a> {
    pub name: &'a str,
    pub body: Option<&'a str>,
}

/// All possible token types in GML (aligned with GMEdit's GmlLinterKind)
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'a> {
    // Literals
    Integer(i64),
    Float(f64),
    String, // Unit variant - use span to get text
    True,
    False,
    Undefined,

    // Identifiers and Keywords
    Identifier, // Unit variant - use span to get text

    // Keywords
    If,
    Then,           // GMEdit has LKThen
    Else,
    For,
    While,
    Do,
    Until,
    Repeat,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Return,
    Exit,
    With,
    Var,
    Globalvar,
    Function,
    Constructor,
    New,
    Delete,
    Static,
    Enum,
    Try,
    Catch,
    Finally,
    Throw,

    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Div,            // div (integer division)
    Mod,            // mod

    // Comparison
    Equal,          // ==
    NotEqual,       // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=

    // Logical
    And,            // && or and
    Or,             // || or or
    Not,            // ! or not
    Xor,            // ^^ or xor

    // Bitwise
    BitAnd,         // &
    BitOr,          // |
    BitXor,         // ^
    BitNot,         // ~
    ShiftLeft,      // <<
    ShiftRight,     // >>

    // Assignment
    Assign,         // =
    PlusAssign,     // +=
    MinusAssign,    // -=
    StarAssign,     // *=
    SlashAssign,    // /=
    PercentAssign,  // %=
    BitAndAssign,   // &=
    BitOrAssign,    // |=
    BitXorAssign,   // ^=
    ShiftLeftAssign,  // <<=
    ShiftRightAssign, // >>=

    // Null-coalescing
    NullCoalesce,   // ??
    NullCoalesceAssign, // ??=
    
    // Null-safe operators (GMEdit: LKNullDot, LKNullSqb)
    NullDot,        // ?.
    NullBracket,    // ?[

    // Increment/Decrement
    Increment,      // ++
    Decrement,      // --

    // Ternary
    Question,       // ?
    Colon,          // :

    // Arrows (GMEdit: LKArrow, LKArrowFunc)
    Arrow,          // ->
    ArrowFunc,      // =>

    // Punctuation
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]
    Comma,          // ,
    Semicolon,      // ;
    Dot,            // .
    DotDot,         // .. (for array ranges)
    At,             // @ (for string templates)
    Dollar,         // $ (for hex colors and struct access)
    Hash,           // # (for macros/regions)

    // Accessor operators
    ArrayAccessor,  // [@  (direct array write)
    ListAccessor,   // [|  (list accessor)
    MapAccessor,    // [?  (map accessor)
    GridAccessor,   // [#  (grid accessor)
    StructAccessor, // [$  (struct accessor)

    // Comments (we track these for potential doc extraction)
    LineComment,  // Unit variant - use span
    BlockComment, // Unit variant - use span
    DocComment,   // Unit variant - use span

    // Preprocessor
    Macro(Box<MacroDefinition<'a>>),     // #macro <name> [value]
    Region,             // #region <name> - use span
    EndRegion,          // #endregion
    Define,             // #define <name> - use span

    // Special
    Eof,
    Error(Box<String>), // Box<String> is 8 bytes, whereas Box<str> is 16 bytes
    Newline,
}


impl<'a> TokenKind<'a> {
    /// Check if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::If
                | TokenKind::Else
                | TokenKind::For
                | TokenKind::While
                | TokenKind::Do
                | TokenKind::Until
                | TokenKind::Repeat
                | TokenKind::Switch
                | TokenKind::Case
                | TokenKind::Default
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::Exit
                | TokenKind::With
                | TokenKind::Var
                | TokenKind::Globalvar
                | TokenKind::Function
                | TokenKind::Constructor
                | TokenKind::New
                | TokenKind::Delete
                | TokenKind::Static
                | TokenKind::Enum
                | TokenKind::Try
                | TokenKind::Catch
                | TokenKind::Finally
                | TokenKind::Throw
        )
    }

    /// Check if this token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Integer(_)
                | TokenKind::Float(_)
                | TokenKind::String
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Undefined
        )
    }
}
