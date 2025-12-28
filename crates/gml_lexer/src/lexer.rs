//! GML Lexer implementation
//!
//! A hand-written lexer for GML, optimized for speed similar to Ruff's approach.

use crate::token::{Span, Token, TokenKind};

/// The GML lexer
pub struct Lexer<'a> {
    /// Source code being lexed
    source: &'a str,
    /// Source as bytes for faster access
    bytes: &'a [u8],
    /// Current position in bytes
    pos: usize,
    /// Start of current token
    token_start: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            token_start: 0,
        }
    }

    /// Tokenize the entire source, returning all tokens
    pub fn tokenize(mut self) -> Vec<Token<'a>> {
        // Pre-allocate based on estimate: ~1 token per 5 characters on average
        let mut tokens = Vec::with_capacity(self.source.len() / 5);
        loop {
            let token = self.next_token();
            let is_eof = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.token_start = self.pos;

        if self.is_at_end() {
            return self.make_token(TokenKind::Eof);
        }

        let c = self.advance();

        // Handle different token types
        match c {
            // Single-character tokens
            b'(' => self.make_token(TokenKind::LeftParen),
            b')' => self.make_token(TokenKind::RightParen),
            b'{' => self.make_token(TokenKind::LeftBrace),
            b'}' => self.make_token(TokenKind::RightBrace),
            b'[' => self.handle_bracket(),
            b']' => self.make_token(TokenKind::RightBracket),
            b',' => self.make_token(TokenKind::Comma),
            b';' => self.make_token(TokenKind::Semicolon),
            b'~' => self.make_token(TokenKind::BitNot),
            b'@' => self.make_token(TokenKind::At),

            // Dot (could be .. for ranges)
            b'.' => {
                if self.check(b'.') {
                    self.advance();
                    self.make_token(TokenKind::DotDot)
                } else {
                    self.make_token(TokenKind::Dot)
                }
            }

            // Operators that could be compound
            b'+' => {
                if self.check(b'+') {
                    self.advance();
                    self.make_token(TokenKind::Increment)
                } else if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::PlusAssign)
                } else {
                    self.make_token(TokenKind::Plus)
                }
            }
            b'-' => {
                if self.check(b'-') {
                    self.advance();
                    self.make_token(TokenKind::Decrement)
                } else if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::MinusAssign)
                } else if self.check(b'>') {
                    self.advance();
                    self.make_token(TokenKind::Arrow)
                } else {
                    self.make_token(TokenKind::Minus)
                }
            }
            b'*' => {
                if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::StarAssign)
                } else {
                    self.make_token(TokenKind::Star)
                }
            }
            b'/' => self.handle_slash(),
            b'%' => {
                if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::PercentAssign)
                } else {
                    self.make_token(TokenKind::Percent)
                }
            }

            // Comparison operators
            b'=' => {
                if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::Equal)
                } else if self.check(b'>') {
                    self.advance();
                    self.make_token(TokenKind::ArrowFunc)
                } else {
                    self.make_token(TokenKind::Assign)
                }
            }
            b'!' => {
                if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::NotEqual)
                } else {
                    self.make_token(TokenKind::Not)
                }
            }
            b'<' => {
                if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::LessEqual)
                } else if self.check(b'<') {
                    self.advance();
                    if self.check(b'=') {
                        self.advance();
                        self.make_token(TokenKind::ShiftLeftAssign)
                    } else {
                        self.make_token(TokenKind::ShiftLeft)
                    }
                } else if self.check(b'>') {
                    self.advance();
                    self.make_token(TokenKind::NotEqual) // <> is also not equal in GML
                } else {
                    self.make_token(TokenKind::Less)
                }
            }
            b'>' => {
                if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::GreaterEqual)
                } else if self.check(b'>') {
                    self.advance();
                    if self.check(b'=') {
                        self.advance();
                        self.make_token(TokenKind::ShiftRightAssign)
                    } else {
                        self.make_token(TokenKind::ShiftRight)
                    }
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }

            // Logical operators
            b'&' => {
                if self.check(b'&') {
                    self.advance();
                    self.make_token(TokenKind::And)
                } else if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::BitAndAssign)
                } else {
                    self.make_token(TokenKind::BitAnd)
                }
            }
            b'|' => {
                if self.check(b'|') {
                    self.advance();
                    self.make_token(TokenKind::Or)
                } else if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::BitOrAssign)
                } else {
                    self.make_token(TokenKind::BitOr)
                }
            }
            b'^' => {
                if self.check(b'^') {
                    self.advance();
                    self.make_token(TokenKind::Xor)
                } else if self.check(b'=') {
                    self.advance();
                    self.make_token(TokenKind::BitXorAssign)
                } else {
                    self.make_token(TokenKind::BitXor)
                }
            }

            // Ternary and null-coalescing
            b'?' => {
                if self.check(b'?') {
                    self.advance();
                    if self.check(b'=') {
                        self.advance();
                        self.make_token(TokenKind::NullCoalesceAssign)
                    } else {
                        self.make_token(TokenKind::NullCoalesce)
                    }
                } else if self.check(b'.') {
                    // Check if next is a digit (then it's just ?)
                    if self.peek_next().is_none_or(|c| !c.is_ascii_digit()) {
                        self.advance();
                        self.make_token(TokenKind::NullDot)
                    } else {
                        self.make_token(TokenKind::Question)
                    }
                } else if self.check(b'[') {
                    self.advance();
                    self.make_token(TokenKind::NullBracket)
                } else {
                    self.make_token(TokenKind::Question)
                }
            }
            b':' => self.make_token(TokenKind::Colon),

            // Strings
            b'"' => self.string(),
            b'\'' => self.string_single(),

            // Hex numbers and $ accessor
            b'$' => {
                if self.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                    self.hex_number()
                } else {
                    self.make_token(TokenKind::Dollar)
                }
            }

            // Preprocessor
            b'#' => self.preprocessor(),

            // Numbers
            c if c.is_ascii_digit() => self.number(),

            // Identifiers and keywords
            c if is_ident_start(c) => self.identifier(),

            // Backslash - line continuation (used in macros) or escape
            b'\\' => {
                // If followed by newline, it's a line continuation - skip both and get next token
                if self.peek() == Some(b'\n') {
                    self.advance();
                    self.next_token()
                } else if self.peek() == Some(b'\r') {
                    self.advance();
                    if self.peek() == Some(b'\n') {
                        self.advance();
                    }
                    self.next_token()
                } else {
                    // Standalone backslash - just skip it and continue
                    self.next_token()
                }
            }

            // Unknown character
            _ => self.make_token(TokenKind::Error(format!(
                "Unexpected character: '{}'",
                c as char
            ))),
        }
    }

    // ========== Helper methods ==========

    fn is_at_end(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<u8> {
        self.bytes.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> u8 {
        let c = self.bytes[self.pos];
        self.pos += 1;
        c
    }

    fn check(&self, expected: u8) -> bool {
        self.peek() == Some(expected)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                b' ' | b'\t' | b'\r' | b'\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn make_token(&self, kind: TokenKind<'a>) -> Token<'a> {
        Token::new(
            kind,
            Span::new(self.token_start as u32, self.pos as u32),
        )
    }

    fn current_lexeme(&self) -> &'a str {
        &self.source[self.token_start..self.pos]
    }

    // ========== Token-specific handlers ==========

    fn handle_slash(&mut self) -> Token<'a> {
        if self.check(b'/') {
            self.advance();
            // Check for doc comment ///
            if self.check(b'/') {
                self.advance();
                self.doc_comment()
            } else {
                self.line_comment()
            }
        } else if self.check(b'*') {
            self.advance();
            self.block_comment()
        } else if self.check(b'=') {
            self.advance();
            self.make_token(TokenKind::SlashAssign)
        } else {
            self.make_token(TokenKind::Slash)
        }
    }

    fn handle_bracket(&mut self) -> Token<'a> {
        // Check for special accessor syntax: [| [? [# [$
        match self.peek() {
            Some(b'|') => {
                self.advance();
                self.make_token(TokenKind::ListAccessor)
            }
            Some(b'?') => {
                self.advance();
                self.make_token(TokenKind::MapAccessor)
            }
            Some(b'#') => {
                self.advance();
                self.make_token(TokenKind::GridAccessor)
            }
            Some(b'$') => {
                self.advance();
                self.make_token(TokenKind::StructAccessor)
            }
            Some(b'@') => {
                self.advance();
                self.make_token(TokenKind::ArrayAccessor)
            }
            _ => self.make_token(TokenKind::LeftBracket),
        }
    }

    fn line_comment(&mut self) -> Token<'a> {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c == b'\n' {
                break;
            }
            self.advance();
        }
        let content = &self.source[start..self.pos];
        self.make_token(TokenKind::LineComment(content))
    }

    fn doc_comment(&mut self) -> Token<'a> {
        // Skip leading space if present
        if self.check(b' ') {
            self.advance();
        }
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c == b'\n' {
                break;
            }
            self.advance();
        }
        let content = &self.source[start..self.pos];
        self.make_token(TokenKind::DocComment(content))
    }

    fn block_comment(&mut self) -> Token<'a> {
        let start = self.pos;
        let mut depth = 1;
        
        while depth > 0 && !self.is_at_end() {
            if self.check(b'/') && self.peek_next() == Some(b'*') {
                self.advance();
                self.advance();
                depth += 1;
            } else if self.check(b'*') && self.peek_next() == Some(b'/') {
                self.advance();
                self.advance();
                depth -= 1;
            } else {
                self.advance();
            }
        }

        if depth > 0 {
            return self.make_token(TokenKind::Error("Unterminated block comment".to_string()));
        }

        let content = &self.source[start..self.pos - 2];
        self.make_token(TokenKind::BlockComment(content))
    }

    fn string(&mut self) -> Token<'a> {
        // Zero-allocation string: just return the slice of the source including quotes
        // The consumer/parser will unescape if needed.
        while let Some(c) = self.peek() {
            if c == b'"' {
                self.advance();
                return self.make_token(TokenKind::String(self.current_lexeme()));
            }
            if c == b'\\' {
                self.advance();
                if self.peek().is_some() {
                    self.advance();
                }
            } else if c == b'\n' {
                return self.make_token(TokenKind::Error("Unterminated string".to_string()));
            } else {
                self.advance();
            }
        }

        self.make_token(TokenKind::Error("Unterminated string".to_string()))
    }

    fn string_single(&mut self) -> Token<'a> {
        while let Some(c) = self.peek() {
            if c == b'\'' {
                self.advance();
                return self.make_token(TokenKind::String(self.current_lexeme()));
            }
            if c == b'\n' {
                return self.make_token(TokenKind::Error("Unterminated string".to_string()));
            }
            self.advance();
        }

        self.make_token(TokenKind::Error("Unterminated string".to_string()))
    }

    fn number(&mut self) -> Token<'a> {
        // Check for hex literal 0x... (the first digit 0 was already consumed)
        let first_digit = self.bytes[self.token_start];
        if first_digit == b'0' && self.check(b'x') || self.check(b'X') {
            self.advance(); // consume 'x'
            return self.hex_number_0x();
        }
        
        // Check for binary literal 0b... (GMEdit support)
        if first_digit == b'0' && self.check(b'b') || self.check(b'B') {
            self.advance(); // consume 'b'
            return self.binary_number();
        }
        
        // Consume integer part
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Check for decimal
        if self.check(b'.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            self.advance(); // consume '.'
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            // Parse as float
            let value: f64 = self.current_lexeme().parse().unwrap_or(0.0);
            return self.make_token(TokenKind::Float(value));
        }

        // Parse as integer
        let value: i64 = self.current_lexeme().parse().unwrap_or(0);
        self.make_token(TokenKind::Integer(value))
    }

    fn hex_number(&mut self) -> Token<'a> {
        // $ followed by hex digits
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }
        
        let hex_str = &self.source[start..self.pos];
        let value = i64::from_str_radix(hex_str, 16).unwrap_or(0);
        self.make_token(TokenKind::Integer(value))
    }

    fn hex_number_0x(&mut self) -> Token<'a> {
        // 0x followed by hex digits (0x already consumed)
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }
        
        let hex_str = &self.source[start..self.pos];
        if hex_str.is_empty() {
            return self.make_token(TokenKind::Error("Empty hex literal".to_string()));
        }
        let value = i64::from_str_radix(hex_str, 16).unwrap_or(0);
        self.make_token(TokenKind::Integer(value))
    }

    fn binary_number(&mut self) -> Token<'a> {
        // 0b followed by binary digits (0b already consumed)
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c == b'0' || c == b'1' || c == b'_' {
                self.advance();
            } else {
                break;
            }
        }
        
        let bin_str: String = self.source[start..self.pos].chars().filter(|c| *c != '_').collect();
        if bin_str.is_empty() {
            return self.make_token(TokenKind::Error("Empty binary literal".to_string()));
        }
        let value = i64::from_str_radix(&bin_str, 2).unwrap_or(0);
        self.make_token(TokenKind::Integer(value))
    }

    fn identifier(&mut self) -> Token<'a> {
        while let Some(c) = self.peek() {
            if is_ident_continue(c) {
                self.advance();
            } else {
                break;
            }
        }

        let text = self.current_lexeme();
        let kind = match text {
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "do" => TokenKind::Do,
            "until" => TokenKind::Until,
            "repeat" => TokenKind::Repeat,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "exit" => TokenKind::Exit,
            "with" => TokenKind::With,
            "var" => TokenKind::Var,
            "globalvar" => TokenKind::Globalvar,
            "function" => TokenKind::Function,
            "constructor" => TokenKind::Constructor,
            "new" => TokenKind::New,
            "delete" => TokenKind::Delete,
            "static" => TokenKind::Static,
            "enum" => TokenKind::Enum,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "finally" => TokenKind::Finally,
            "throw" => TokenKind::Throw,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "undefined" => TokenKind::Undefined,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "xor" => TokenKind::Xor,
            "div" => TokenKind::Div,
            "mod" => TokenKind::Mod,
            _ => TokenKind::Identifier(text),
        };

        self.make_token(kind)
    }

    fn preprocessor(&mut self) -> Token<'a> {
        if let Some(c) = self.peek() {
            if c.is_ascii_hexdigit() {
                let mut count = 0;
                while count < 6 {
                    if let Some(ch) = self.bytes.get(self.pos + count) {
                        if ch.is_ascii_hexdigit() {
                            count += 1;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                if count == 6 {
                    let next_char = self.bytes.get(self.pos + 6);
                    if next_char.is_none_or(|c| !c.is_ascii_hexdigit()) {
                        for _ in 0..6 {
                            self.advance();
                        }
                        let hex_str = &self.current_lexeme()[1..];
                        let value = i64::from_str_radix(hex_str, 16).unwrap_or(0);
                        return self.make_token(TokenKind::Integer(value));
                    }
                }
            }
        }
        
        while let Some(c) = self.peek() {
            if is_ident_continue(c) {
                self.advance();
            } else {
                break;
            }
        }

        let text = self.current_lexeme();
        match text {
            "#macro" => {
                self.skip_whitespace();
                let name_start = self.pos;
                while let Some(c) = self.peek() {
                    if is_ident_continue(c) {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let name = &self.source[name_start..self.pos];
                
                // Consume the rest of the macro definition (with backslashes)
                let body_start = self.pos;
                loop {
                    let mut last_backslash_pos: Option<usize> = None;
                    while let Some(c) = self.peek() {
                        if c == b'\n' || c == b'\r' { break; }
                        if c == b'\\' {
                            last_backslash_pos = Some(self.pos);
                            self.advance();
                        } else if c == b'/' && self.bytes.get(self.pos + 1) == Some(&b'/') {
                            while let Some(cc) = self.peek() {
                                if cc == b'\n' || cc == b'\r' { break; }
                                self.advance();
                            }
                            break;
                        } else if c.is_ascii_whitespace() {
                            self.advance();
                        } else {
                            last_backslash_pos = None;
                            self.advance();
                        }
                    }
                    if self.peek() == Some(b'\r') { self.advance(); }
                    if self.peek() == Some(b'\n') { self.advance(); }
                    if last_backslash_pos.is_none() { break; }
                }
                let body = self.source[body_start..self.pos].trim();
                let body = if body.is_empty() { None } else { Some(body) };
                self.make_token(TokenKind::Macro(name, body))
            }
            "#define" => {
                self.skip_whitespace();
                let name_start = self.pos;
                while let Some(c) = self.peek() {
                    if is_ident_continue(c) { self.advance(); } else { break; }
                }
                let name = &self.source[name_start..self.pos];
                while let Some(c) = self.peek() {
                    if c == b'\n' { break; }
                    self.advance();
                }
                self.make_token(TokenKind::Define(name))
            }
            "#region" | "#section" => {
                self.skip_whitespace();
                let name_start = self.pos;
                while let Some(c) = self.peek() {
                    if c == b'\n' || c == b'\r' { break; }
                    self.advance();
                }
                let name = self.source[name_start..self.pos].trim();
                self.make_token(TokenKind::Region(name))
            }
            "#endregion" => {
                while let Some(c) = self.peek() {
                    if c == b'\n' || c == b'\r' { break; }
                    self.advance();
                }
                self.make_token(TokenKind::EndRegion)
            }
            _ => self.make_token(TokenKind::Hash),
        }
    }
}


/// Check if a character can start an identifier
fn is_ident_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

/// Check if a character can continue an identifier
fn is_ident_continue(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "var x = 42;";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        assert!(matches!(tokens[0].kind, TokenKind::Var));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Assign));
        assert!(matches!(tokens[3].kind, TokenKind::Integer(42)));
        assert!(matches!(tokens[4].kind, TokenKind::Semicolon));
        assert!(matches!(tokens[5].kind, TokenKind::Eof));
    }

    #[test]
    fn test_function() {
        let source = "function foo(a, b) { return a + b; }";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        assert!(matches!(tokens[0].kind, TokenKind::Function));
        if let TokenKind::Identifier(name) = tokens[1].kind {
            assert_eq!(name, "foo");
        } else {
            panic!("Expected identifier");
        }
    }

    #[test]
    fn test_string() {
        let source = r#""hello world""#;
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        if let TokenKind::String(content) = tokens[0].kind {
            assert_eq!(content, r#""hello world""#);
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_macro() {
        let source = "#macro UI_TEXT_RENDERER scribble";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        if let TokenKind::Macro(name, body) = &tokens[0].kind {
            assert_eq!(*name, "UI_TEXT_RENDERER");
            assert_eq!(*body, Some("scribble"));
        } else {
            panic!("Expected macro");
        }
    }

    #[test]
    fn test_hex_color() {
        let source = "$FF00FF";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        assert!(matches!(tokens[0].kind, TokenKind::Integer(0xFF00FF)));
    }

    #[test]
    fn test_doc_comment() {
        let source = "/// @description This is a function";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        assert!(matches!(&tokens[0].kind, TokenKind::DocComment(_)));
    }

    #[test]
    fn test_null_coalesce() {
        let source = "a ?? b";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        
        assert!(matches!(tokens[2].kind, TokenKind::NullCoalesce));
    }
}
