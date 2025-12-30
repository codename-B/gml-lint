//! Lint context - provides information to rules during checking

use crate::diagnostics::Diagnostic;
use crate::parser::Program;

/// Trait for providing project-wide symbol information
pub trait SymbolProvider {
    fn has_function(&self, name: &str) -> bool;
    fn has_variable(&self, name: &str) -> bool;
    /// Check if a global variable has been assigned anywhere in the project
    fn has_global(&self, name: &str) -> bool;
}

pub struct DefaultSymbolProvider;
impl SymbolProvider for DefaultSymbolProvider {
    fn has_function(&self, _name: &str) -> bool { false }
    fn has_variable(&self, _name: &str) -> bool { false }
    fn has_global(&self, _name: &str) -> bool { true } // Default: assume globals exist
}

/// Context provided to rules during linting
pub struct LintContext<'a> {
    /// The source code being linted
    source: &'a str,
    /// The file path
    file_path: &'a str,
    /// The parsed program
    program: &'a Program<'a>,

    /// Line starts (byte offsets)
    line_starts: Vec<usize>,
    /// Collected diagnostics
    diagnostics: Vec<Diagnostic>,
    /// Symbol provider (project context)
    symbol_provider: &'a dyn SymbolProvider,
    /// Semantic database (type inference)
    db: &'a crate::semantic::db::Database,
}

impl<'a> LintContext<'a> {
    /// Create a new lint context
    pub fn new(source: &'a str, file_path: &'a str, program: &'a Program<'a>, symbol_provider: &'a dyn SymbolProvider, db: &'a crate::semantic::db::Database) -> Self {

        // Pre-compute line starts for offset -> line/column conversion
        // Estimate: ~40 chars per line on average
        let mut line_starts = Vec::with_capacity(source.len() / 40 + 1);
        line_starts.push(0);
        for (i, c) in source.bytes().enumerate() {
            if c == b'\n' {
                line_starts.push(i + 1);
            }
        }

        Self {
            source,
            file_path,
            program,
            line_starts,
            diagnostics: Vec::new(),
            symbol_provider,
            db,
        }
    }

    /// Get the source code
    pub fn source(&self) -> &str {
        self.source
    }

    /// Get the file path
    pub fn file_path(&self) -> &str {
        self.file_path
    }

    /// Get the parsed program
    pub fn program(&self) -> &Program<'a> {
        self.program
    }


    /// Get the symbol provider
    pub fn symbol_provider(&self) -> &dyn SymbolProvider {
        self.symbol_provider
    }

    /// Get the semantic database
    pub fn db(&self) -> &crate::semantic::db::Database {
        self.db
    }

    /// Convert a byte offset to line and column (1-indexed)
    pub fn offset_to_line_col(&self, offset: u32) -> (u32, u32) {
        let offset = offset as usize;
        
        // Binary search for the line
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        };
        
        let line_start = self.line_starts[line];
        let column = offset - line_start + 1;
        
        ((line + 1) as u32, column as u32)
    }

    /// Add a diagnostic
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Add multiple diagnostics
    pub fn add_diagnostics(&mut self, diagnostics: Vec<Diagnostic>) {
        self.diagnostics.extend(diagnostics);
    }

    /// Consume the context and return all diagnostics
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}
