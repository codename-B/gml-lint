//! GML Diagnostics - Rule-independent diagnostic types
//!
//! This crate provides the core diagnostic infrastructure used by the linter.

/// Severity level of a diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// Category of a lint rule
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Category {
    /// Likely bugs or errors
    Error,
    /// Potential issues or bad practices
    Warning,
    /// Style and formatting issues
    Style,
    /// Performance improvements
    Performance,
    /// Deprecated feature usage
    Base,
    /// Information or unknown state (fallback)
    Info,
}

/// A location in source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    /// File path
    pub file: String,
    /// 1-indexed line number
    pub line: u32,
    /// 1-indexed column number
    pub column: u32,
    /// Byte offset start
    pub start: u32,
    /// Byte offset end
    pub end: u32,
}

impl Location {
    pub fn new(file: impl Into<String>, line: u32, column: u32, start: u32, end: u32) -> Self {
        Self {
            file: file.into(),
            line,
            column,
            start,
            end,
        }
    }
}

/// A suggested fix for a diagnostic
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fix {
    /// Description of what this fix does
    pub message: String,
    /// Edits to apply
    pub edits: Vec<Edit>,
}

/// A text edit
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edit {
    /// Byte offset start
    pub start: u32,
    /// Byte offset end
    pub end: u32,
    /// Replacement text
    pub replacement: String,
}

impl Edit {
    pub fn new(start: u32, end: u32, replacement: impl Into<String>) -> Self {
        Self {
            start,
            end,
            replacement: replacement.into(),
        }
    }

    /// Create an insertion at a position
    pub fn insert(pos: u32, text: impl Into<String>) -> Self {
        Self::new(pos, pos, text)
    }

    /// Create a deletion of a range
    pub fn delete(start: u32, end: u32) -> Self {
        Self::new(start, end, "")
    }
}

/// A diagnostic message from the linter
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Rule code (e.g., "GML001")
    pub code: String,
    /// Human-readable message
    pub message: String,
    /// Severity level
    pub severity: Severity,
    /// Category
    pub category: Category,
    /// Location in source
    pub location: Location,
    /// Optional fix
    pub fix: Option<Fix>,
}

impl Diagnostic {
    pub fn new(
        code: impl Into<String>,
        message: impl Into<String>,
        severity: Severity,
        category: Category,
        location: Location,
    ) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            severity,
            category,
            location,
            fix: None,
        }
    }

    /// Add a fix to this diagnostic
    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.fix = Some(fix);
        self
    }

    /// Create an error diagnostic
    pub fn error(
        code: impl Into<String>,
        message: impl Into<String>,
        location: Location,
    ) -> Self {
        Self::new(code, message, Severity::Error, Category::Error, location)
    }

    /// Create a warning diagnostic
    pub fn warning(
        code: impl Into<String>,
        message: impl Into<String>,
        location: Location,
    ) -> Self {
        Self::new(code, message, Severity::Warning, Category::Warning, location)
    }

    /// Create a style diagnostic
    pub fn style(
        code: impl Into<String>,
        message: impl Into<String>,
        location: Location,
    ) -> Self {
        Self::new(code, message, Severity::Info, Category::Style, location)
    }

    /// Create an info diagnostic
    pub fn info(
        code: impl Into<String>,
        message: impl Into<String>,
        location: Location,
    ) -> Self {
        Self::new(code, message, Severity::Info, Category::Info, location)
    }

    /// Create a hint diagnostic
    pub fn hint(
        code: impl Into<String>,
        message: impl Into<String>,
        location: Location,
    ) -> Self {
        Self::new(code, message, Severity::Hint, Category::Style, location)
    }
}
