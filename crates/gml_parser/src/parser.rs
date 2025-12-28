//! GML Parser implementation
//!
//! A hand-written recursive descent parser for GML.

use gml_lexer::{Lexer, Span, Token, TokenKind};
use crate::ast::*;


/// Parse error
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

/// The GML parser
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
    errors: Vec<ParseError>,
}


impl<'a> Parser<'a> {
    /// Create a new parser from source code
    pub fn new(source: &'a str) -> Self {
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }


    /// Parse the source into a program
    pub fn parse(mut self) -> Result<Program<'a>, Vec<ParseError>> {

        let start = self.current_span();
        // Pre-allocate based on token count: ~1 statement per 10 tokens on average
        let mut statements = Vec::with_capacity(self.tokens.len() / 10);

        while !self.is_at_end() {
            // Comments are now automatically skipped by peek()/advance()
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }

        let end = self.previous_span();
        let span = Span::new(start.start, end.end);

        if self.errors.is_empty() {
            Ok(Program { statements, span })
        } else {
            Err(self.errors)
        }
    }

    /// Get parse errors (for partial parsing)
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }

    // ========== Statement parsing ==========

    fn parse_statement(&mut self) -> Result<Stmt<'a>, ParseError> {

        // Handle preprocessor directives (comments are auto-skipped by peek/advance)
        if matches!(self.peek_kind(), Some(TokenKind::Macro(_, _)) | Some(TokenKind::Region(_)) | Some(TokenKind::EndRegion) | Some(TokenKind::Define(_))) {
            let span = self.current_span();
            self.advance();
            // Lexer already consumed the line/body, so we don't need to skip
            return Ok(Stmt::Empty { span });
        }

        if matches!(self.peek_kind(), Some(TokenKind::Hash)) {
            let span = self.current_span();
            self.advance();
            self.skip_to_line_end();
            return Ok(Stmt::Empty { span });
        }

        if self.check(&TokenKind::Var) {
            self.parse_var_decl()
        } else if self.check(&TokenKind::Static) {
            self.parse_static_var_decl()
        } else if self.check(&TokenKind::Globalvar) {
            self.parse_globalvar_decl()
        } else if self.check(&TokenKind::Function) {
            self.parse_function_decl()
        } else if self.check(&TokenKind::If) {
            self.parse_if_stmt()
        } else if self.check(&TokenKind::For) {
            self.parse_for_stmt()
        } else if self.check(&TokenKind::While) {
            self.parse_while_stmt()
        } else if self.check(&TokenKind::Do) {
            self.parse_do_until_stmt()
        } else if self.check(&TokenKind::Repeat) {
            self.parse_repeat_stmt()
        } else if self.check(&TokenKind::With) {
            self.parse_with_stmt()
        } else if self.check(&TokenKind::Switch) {
            self.parse_switch_stmt()
        } else if self.check(&TokenKind::Return) {
            self.parse_return_stmt()
        } else if self.check(&TokenKind::Exit) {
            self.parse_exit_stmt()
        } else if self.check(&TokenKind::Break) {
            self.parse_break_stmt()
        } else if self.check(&TokenKind::Continue) {
            self.parse_continue_stmt()
        } else if self.check(&TokenKind::Throw) {
            self.parse_throw_stmt()
        } else if self.check(&TokenKind::Try) {
            self.parse_try_catch_stmt()
        } else if self.check(&TokenKind::Enum) {
            self.parse_enum_decl()
        } else if self.check(&TokenKind::LeftBrace) {
            self.parse_block().map(Stmt::Block)
        } else if self.check(&TokenKind::Semicolon) {
            let span = self.current_span();
            self.advance();
            Ok(Stmt::Empty { span })
        } else {
            self.parse_expr_stmt()
        }
    }

    fn parse_static_var_decl(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::Static)?;
        
        let mut declarations = Vec::new();
        // Parse comma-separated static variable declarations
        loop {
            let item_start = self.current_span();
            let name = self.expect_identifier()?;
            
            let init = if self.match_token(&TokenKind::Assign) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            let item_end = self.previous_span();
            declarations.push(VarDeclItem {
                name,
                init,
                span: Span::new(item_start.start, item_end.end),
            });
            
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        // Optional semicolon
        self.match_token(&TokenKind::Semicolon);
        
        let end = self.previous_span();
        Ok(Stmt::VarDecl {
            declarations,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_var_decl(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::Var)?;
        
        let mut declarations = Vec::new();
        
        // Parse comma-separated variable declarations: var a, b = 1, c;
        loop {
            let item_start = self.current_span();
            let name = self.expect_identifier()?;
            
            let init = if self.match_token(&TokenKind::Assign) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            let item_end = self.previous_span();
            declarations.push(VarDeclItem {
                name,
                init,
                span: Span::new(item_start.start, item_end.end),
            });
            
            // Check for comma to continue, or break
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        // Optional semicolon
        self.match_token(&TokenKind::Semicolon);
        
        let end = self.previous_span();
        Ok(Stmt::VarDecl {
            declarations,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_globalvar_decl(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::Globalvar)?;
        
        let name = self.expect_identifier()?;
        self.match_token(&TokenKind::Semicolon);
        
        let end = self.previous_span();
        Ok(Stmt::GlobalVarDecl {
            name,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_function_decl(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Function)?;
        
        // Function name is optional for anonymous functions (but this is Stmt::FunctionDecl which usually has a name)
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LeftParen)?;
        
        let params = self.parse_params()?;
        self.expect(&TokenKind::RightParen)?;
        
        // Check for constructor keyword or parent constructor call
        let mut is_constructor = self.match_token(&TokenKind::Constructor);
        
        // Handle inheritance: function Foo() : Bar() constructor {}
        if self.match_token(&TokenKind::Colon) {
            // Skip parent constructor call
            self.expect_identifier()?; // Parent constructor name
            self.expect(&TokenKind::LeftParen)?;
            self.parse_arguments()?;
            self.expect(&TokenKind::RightParen)?;
            // Might have constructor keyword after the parent call
            if self.match_token(&TokenKind::Constructor) {
                is_constructor = true;
            }
        }
        
        let body = self.parse_block()?;
        
        let end = self.previous_span();
        Ok(Stmt::FunctionDecl {
            name,
            params,
            body,
            is_constructor,
            span: Span::new(start.start, end.end),
        })
    }

    /// Parse an anonymous function expression: function() { } or function(a, b) { }
    fn parse_function_expr(&mut self) -> Result<Expr<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Function)?;
        
        // Anonymous function - name is optional (may be present for debugging)
        let _optional_name = if let Some(TokenKind::Identifier(_)) = self.peek_kind() {
            // Named function expression (like JavaScript's named function expressions)
            Some(self.expect_identifier()?)
        } else {
            None
        };
        
        self.expect(&TokenKind::LeftParen)?;
        let params = self.parse_params()?;
        self.expect(&TokenKind::RightParen)?;
        
        // Check for constructor keyword
        let mut is_constructor = self.match_token(&TokenKind::Constructor);
        
        // Handle inheritance for constructor expressions
        if self.match_token(&TokenKind::Colon) {
            // Skip parent constructor call
            if let Some(TokenKind::Identifier(_)) = self.peek_kind() {
                self.advance();
                if self.match_token(&TokenKind::LeftParen) {
                    self.parse_arguments()?;
                    self.expect(&TokenKind::RightParen)?;
                }
            }
            if self.match_token(&TokenKind::Constructor) {
                is_constructor = true;
            }
        }
        
        let body = self.parse_block()?;
        
        let end = self.previous_span();
        Ok(Expr::FunctionExpr {
            params,
            body,
            is_constructor,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param<'a>>, ParseError> {
        let mut params = Vec::new();
        
        if !self.check(&TokenKind::RightParen) {
            loop {
                let param_start = self.current_span();
                let name = self.expect_identifier()?;
                
                let default = if self.match_token(&TokenKind::Assign) {
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                
                let param_end = self.previous_span();
                params.push(Param {
                    name,
                    default,
                    span: Span::new(param_start.start, param_end.end),
                });
                
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::LeftBrace)?;
        
        let mut statements = Vec::new();
        
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            if self.check_comment() {
                self.advance();
                continue;
            }
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }
        }
        
        self.expect(&TokenKind::RightBrace)?;
        let end = self.previous_span();
        
        Ok(Block::new(statements, Span::new(start.start, end.end)))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::If)?;
        
        // GMEdit approach: just read the expression directly
        // If there are parens, they're part of the expression as grouping
        // The expression parser handles binary operators like || after grouping ends
        let condition = self.parse_expression()?;
        
        let then_branch = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            // Single statement
            let stmt = self.parse_statement()?;
            Block::new(vec![stmt], self.previous_span())
        };
        
        let else_branch = if self.match_token(&TokenKind::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        
        let end = self.previous_span();
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
            has_parentheses: false,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::For)?;
        self.expect(&TokenKind::LeftParen)?;
        
        let init = if self.check(&TokenKind::Semicolon) {
            None
        } else if self.check(&TokenKind::Var) {
            Some(Box::new(self.parse_var_decl()?))
        } else {
            Some(Box::new(self.parse_expr_stmt()?))
        };
        
        // Semicolon after init (may already be consumed by var_decl)
        if !matches!(self.previous().kind, TokenKind::Semicolon) {
            self.match_token(&TokenKind::Semicolon);
        }
        
        let condition = if self.check(&TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(&TokenKind::Semicolon)?;
        
        let update = if self.check(&TokenKind::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        // GML allows trailing semicolon in for-loop like: for(;;i++;)
        self.match_token(&TokenKind::Semicolon);
        self.expect(&TokenKind::RightParen)?;
        
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            let stmt = self.parse_statement()?;
            Block::new(vec![stmt], self.previous_span())
        };
        
        let end = self.previous_span();
        Ok(Stmt::For {
            init,
            condition,
            update,
            body,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::While)?;
        
        let condition = self.parse_expression()?;
        
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            let stmt = self.parse_statement()?;
            Block::new(vec![stmt], self.previous_span())
        };
        
        let end = self.previous_span();
        Ok(Stmt::While {
            condition,
            body,
            has_parentheses: false,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_do_until_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::Do)?;
        
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            let stmt = self.parse_statement()?;
            Block::new(vec![stmt], self.previous_span())
        };
        
        self.expect(&TokenKind::Until)?;
        let condition = self.parse_expression()?;
        self.match_token(&TokenKind::Semicolon);
        
        let end = self.previous_span();
        Ok(Stmt::DoUntil {
            body,
            condition,
            has_parentheses: false,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_repeat_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::Repeat)?;
        
        let count = self.parse_expression()?;
        
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            let stmt = self.parse_statement()?;
            Block::new(vec![stmt], self.previous_span())
        };
        
        let end = self.previous_span();
        Ok(Stmt::Repeat {
            count,
            body,
            has_parentheses: false,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_with_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::With)?;
        
        let target = self.parse_expression()?;
        
        let body = if self.check(&TokenKind::LeftBrace) {
            self.parse_block()?
        } else {
            let stmt = self.parse_statement()?;
            Block::new(vec![stmt], self.previous_span())
        };
        
        let end = self.previous_span();
        Ok(Stmt::With {
            target,
            body,
            has_parentheses: false,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_switch_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {

        let start = self.current_span();
        self.expect(&TokenKind::Switch)?;
        
        let value = self.parse_expression()?;
        
        self.expect(&TokenKind::LeftBrace)?;
        
        let mut cases = Vec::new();
        
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            if self.check_comment() {
                self.advance();
                continue;
            }
            
            if matches!(self.peek_kind(), Some(TokenKind::Region(_)) | Some(TokenKind::EndRegion) | Some(TokenKind::Macro(_, _)) | Some(TokenKind::Define(_))) {
                self.advance();
                continue;
            }

            let case_start = self.current_span();
            let case_value = if self.match_token(&TokenKind::Case) {
                let val = self.parse_expression()?;
                self.expect(&TokenKind::Colon)?;
                Some(val)
            } else if self.match_token(&TokenKind::Default) {
                self.expect(&TokenKind::Colon)?;
                None
            } else {
                return Err(ParseError::new("Expected 'case' or 'default'", self.current_span()));
            };
            
            let mut body = Vec::new();
            while !self.check(&TokenKind::Case) 
                && !self.check(&TokenKind::Default) 
                && !self.check(&TokenKind::RightBrace) 
                && !self.is_at_end() 
            {
                if self.check_comment() {
                    self.advance();
                    continue;
                }
                
                match self.parse_statement() {
                    Ok(stmt) => body.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        self.synchronize();
                    }
                }
            }
            
            let case_end = self.previous_span();
            cases.push(Case {
                value: case_value,
                body,
                span: Span::new(case_start.start, case_end.end),
            });
        }
        
        self.expect(&TokenKind::RightBrace)?;
        let end = self.previous_span();
        
        Ok(Stmt::Switch {
            value,
            cases,
            has_parentheses: false,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Return)?;
        
        let value = if !self.check(&TokenKind::Semicolon) && !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        self.match_token(&TokenKind::Semicolon);
        let end = self.previous_span();
        
        Ok(Stmt::Return {
            value,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_exit_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Exit)?;
        self.match_token(&TokenKind::Semicolon);
        let end = self.previous_span();
        
        Ok(Stmt::Exit {
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Break)?;
        self.match_token(&TokenKind::Semicolon);
        let end = self.previous_span();
        
        Ok(Stmt::Break {
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Continue)?;
        self.match_token(&TokenKind::Semicolon);
        let end = self.previous_span();
        
        Ok(Stmt::Continue {
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Throw)?;
        let value = self.parse_expression()?;
        self.match_token(&TokenKind::Semicolon);
        let end = self.previous_span();
        
        Ok(Stmt::Throw {
            value,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_try_catch_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Try)?;
        let try_block = self.parse_block()?;
        
        let mut catch_var = None;
        let mut catch_block = None;
        if self.match_token(&TokenKind::Catch) {
            if self.match_token(&TokenKind::LeftParen) {
                catch_var = Some(self.expect_identifier()?);
                self.expect(&TokenKind::RightParen)?;
            }
            catch_block = Some(self.parse_block()?);
        }
        
        let mut finally_block = None;
        if self.match_token(&TokenKind::Finally) {
            finally_block = Some(self.parse_block()?);
        }
        
        let end = self.previous_span();
        Ok(Stmt::TryCatch {
            try_block,
            catch_var,
            catch_block,
            finally_block,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_enum_decl(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        self.expect(&TokenKind::Enum)?;
        let name = self.expect_identifier()?;
        
        self.expect(&TokenKind::LeftBrace)?;
        
        let mut members = Vec::new();
        loop {
            // Skip any comments
            self.skip_comments();
            
            // Check for end of enum
            if self.check(&TokenKind::RightBrace) || self.is_at_end() {
                break;
            }
            
            let member_start = self.current_span();
            let member_name = self.expect_identifier()?;
            
            let value = if self.match_token(&TokenKind::Assign) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            // Skip any trailing comments on this line
            self.skip_comments();
            
            let member_end = self.previous_span();
            members.push(EnumMember {
                name: member_name,
                value,
                span: Span::new(member_start.start, member_end.end),
            });
            
            // Skip any comments before checking for comma
            self.skip_comments();
            
            // Optional comma (allow trailing comma)
            if !self.match_token(&TokenKind::Comma) {
                // No comma - might be end of enum or just missing comma
                // Skip comments and check if we're at the end
                self.skip_comments();
                if !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                    // There's another member without a comma - that's okay in GML
                    // Continue parsing
                }
            }
        }
        
        self.expect(&TokenKind::RightBrace)?;
        let end = self.previous_span();
        
        Ok(Stmt::Enum {
            name,
            members,
            span: Span::new(start.start, end.end),
        })
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt<'a>, ParseError> {
        let start = self.current_span();
        let expr = self.parse_expression()?;
        self.match_token(&TokenKind::Semicolon);
        let end = self.previous_span();
        
        Ok(Stmt::Expr {
            expr,
            span: Span::new(start.start, end.end),
        })
    }

    // ========== Expression parsing (precedence climbing) ==========

    fn parse_expression(&mut self) -> Result<Expr<'a>, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr<'a>, ParseError> {
        let expr = self.parse_ternary()?;
        
        // Check for assignment operators
        if let Some(op) = self.match_assign_op() {
            let value = self.parse_assignment()?;
            let span = Span::new(expr.span().start, value.span().end);
            return Ok(Expr::Assignment {
                target: Box::new(expr),
                op,
                value: Box::new(value),
                span,
            });
        }
        
        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<Expr<'a>, ParseError> {
        let expr = self.parse_null_coalesce()?;
        
        if self.match_token(&TokenKind::Question) {
            let then_expr = self.parse_expression()?;
            self.expect(&TokenKind::Colon)?;
            let else_expr = self.parse_ternary()?;
            let span = Span::new(expr.span().start, else_expr.span().end);
            return Ok(Expr::Ternary {
                condition: Box::new(expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
                span,
            });
        }
        
        Ok(expr)
    }

    fn parse_null_coalesce(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_or()?;
        
        while self.match_token(&TokenKind::NullCoalesce) {
            let right = self.parse_or()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::NullCoalesce {
                left: Box::new(expr),
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_and()?;
        
        while self.match_token(&TokenKind::Or) {
            let right = self.parse_and()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_bit_or()?;
        
        while self.match_token(&TokenKind::And) {
            let right = self.parse_bit_or()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_bit_or(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_bit_xor()?;
        
        while self.match_token(&TokenKind::BitOr) {
            let right = self.parse_bit_xor()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::BitOr,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_bit_xor(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_bit_and()?;
        
        while self.match_token(&TokenKind::BitXor) || self.match_token(&TokenKind::Xor) {
            let right = self.parse_bit_and()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::BitXor,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_bit_and(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_equality()?;
        
        while self.match_token(&TokenKind::BitAnd) {
            let right = self.parse_equality()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::BitAnd,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_comparison()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Equal) {
                BinaryOp::Equal
            } else if self.match_token(&TokenKind::NotEqual) {
                BinaryOp::NotEqual
            } else {
                break;
            };
            
            let right = self.parse_comparison()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_shift()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Less) {
                BinaryOp::Less
            } else if self.match_token(&TokenKind::LessEqual) {
                BinaryOp::LessEqual
            } else if self.match_token(&TokenKind::Greater) {
                BinaryOp::Greater
            } else if self.match_token(&TokenKind::GreaterEqual) {
                BinaryOp::GreaterEqual
            } else {
                break;
            };
            
            let right = self.parse_shift()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_term()?;
        
        loop {
            let op = if self.match_token(&TokenKind::ShiftLeft) {
                BinaryOp::ShiftLeft
            } else if self.match_token(&TokenKind::ShiftRight) {
                BinaryOp::ShiftRight
            } else {
                break;
            };
            
            let right = self.parse_term()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_factor()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Plus) {
                BinaryOp::Add
            } else if self.match_token(&TokenKind::Minus) {
                BinaryOp::Sub
            } else {
                break;
            };
            
            let right = self.parse_factor()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_unary()?;
        
        loop {
            let op = if self.match_token(&TokenKind::Star) {
                BinaryOp::Mul
            } else if self.match_token(&TokenKind::Slash) {
                BinaryOp::Div
            } else if self.match_token(&TokenKind::Percent) || self.match_token(&TokenKind::Mod) {
                BinaryOp::Mod
            } else if self.match_token(&TokenKind::Div) {
                BinaryOp::IDiv
            } else {
                break;
            };
            
            let right = self.parse_unary()?;
            let span = Span::new(expr.span().start, right.span().end);
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
                span,
            };
        }
        
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr<'a>, ParseError> {
        let start = self.current_span();
        
        if self.match_token(&TokenKind::Not) {
            let operand = self.parse_unary()?;
            let span = Span::new(start.start, operand.span().end);
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                operand: Box::new(operand),
                span,
            });
        }
        
        if self.match_token(&TokenKind::Minus) {
            let operand = self.parse_unary()?;
            let span = Span::new(start.start, operand.span().end);
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
                span,
            });
        }
        
        if self.match_token(&TokenKind::BitNot) {
            let operand = self.parse_unary()?;
            let span = Span::new(start.start, operand.span().end);
            return Ok(Expr::Unary {
                op: UnaryOp::BitNot,
                operand: Box::new(operand),
                span,
            });
        }
        
        // Pre-increment/decrement
        if self.match_token(&TokenKind::Increment) {
            let operand = self.parse_unary()?;
            let span = Span::new(start.start, operand.span().end);
            return Ok(Expr::Update {
                operand: Box::new(operand),
                op: UpdateOp::Increment,
                prefix: true,
                span,
            });
        }
        
        if self.match_token(&TokenKind::Decrement) {
            let operand = self.parse_unary()?;
            let span = Span::new(start.start, operand.span().end);
            return Ok(Expr::Update {
                operand: Box::new(operand),
                op: UpdateOp::Decrement,
                prefix: true,
                span,
            });
        }
        
        // New expression
        if self.match_token(&TokenKind::New) {
            let callee = self.parse_call()?;
            // If it's a call expression, extract callee and args
            if let Expr::Call { callee: inner_callee, args, span } = callee {
                return Ok(Expr::New {
                    callee: inner_callee,
                    args,
                    span,
                });
            }
            let span = Span::new(start.start, callee.span().end);
            return Ok(Expr::New {
                callee: Box::new(callee),
                args: vec![],
                span,
            });
        }
        
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr<'a>, ParseError> {
        let expr = self.parse_call()?;
        
        // Post-increment/decrement
        if self.match_token(&TokenKind::Increment) {
            let span = Span::new(expr.span().start, self.previous_span().end);
            return Ok(Expr::Update {
                operand: Box::new(expr),
                op: UpdateOp::Increment,
                prefix: false,
                span,
            });
        }
        
        if self.match_token(&TokenKind::Decrement) {
            let span = Span::new(expr.span().start, self.previous_span().end);
            return Ok(Expr::Update {
                operand: Box::new(expr),
                op: UpdateOp::Decrement,
                prefix: false,
                span,
            });
        }
        
        Ok(expr)
    }

    fn parse_call(&mut self) -> Result<Expr<'a>, ParseError> {
        let mut expr = self.parse_primary()?;
        
        loop {
            if self.match_token(&TokenKind::LeftParen) {
                let args = self.parse_arguments()?;
                self.expect(&TokenKind::RightParen)?;
                let span = Span::new(expr.span().start, self.previous_span().end);
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                    span,
                };
            } else if self.match_token(&TokenKind::Dot) {
                let field = self.expect_identifier()?;
                let span = Span::new(expr.span().start, self.previous_span().end);
                expr = Expr::Member {
                    object: Box::new(expr),
                    field,
                    span,
                };
            } else if self.match_token(&TokenKind::GridAccessor) {
                let index1 = self.parse_expression()?;
                self.expect(&TokenKind::Comma)?;
                let index2 = self.parse_expression()?;
                self.expect(&TokenKind::RightBracket)?;
                let span = Span::new(expr.span().start, self.previous_span().end);
                expr = Expr::Index2D {
                    object: Box::new(expr),
                    index1: Box::new(index1),
                    index2: Box::new(index2),
                    span,
                };
            } else if self.match_token(&TokenKind::LeftBracket) || self.match_token(&TokenKind::ArrayAccessor) || self.match_token(&TokenKind::GridAccessor) {
                let index1 = self.parse_expression()?;
                if self.match_token(&TokenKind::Comma) {
                    let index2 = self.parse_expression()?;
                    self.expect(&TokenKind::RightBracket)?;
                    let span = Span::new(expr.span().start, self.previous_span().end);
                    expr = Expr::Index2D {
                        object: Box::new(expr),
                        index1: Box::new(index1),
                        index2: Box::new(index2),
                        span,
                    };
                } else {
                    self.expect(&TokenKind::RightBracket)?;
                    let span = Span::new(expr.span().start, self.previous_span().end);
                    expr = Expr::Index {
                        object: Box::new(expr),
                        index: Box::new(index1),
                        span,
                    };
                }
            } else if self.match_token(&TokenKind::ListAccessor) || self.match_token(&TokenKind::MapAccessor) || self.match_token(&TokenKind::StructAccessor) {
                let index = self.parse_expression()?;
                self.expect(&TokenKind::RightBracket)?;
                let span = Span::new(expr.span().start, self.previous_span().end);
                expr = Expr::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                    span,
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr<'a>>, ParseError> {
        let mut args = Vec::new();
        
        if !self.check(&TokenKind::RightParen) {
            loop {
                if self.check(&TokenKind::Comma) || self.check(&TokenKind::RightParen) {
                    args.push(Expr::Literal { value: Literal::Undefined, span: self.current_span() });
                } else {

                    args.push(self.parse_expression()?);
                }
                
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }
        
        Ok(args)
    }

    fn parse_primary(&mut self) -> Result<Expr<'a>, ParseError> {
        let span = self.current_span();
        
        if let Some(TokenKind::Integer(n)) = self.peek_kind() {
            let n = *n;
            self.advance();
            return Ok(Expr::Literal { value: Literal::Integer(n), span });
        }

        
        if let Some(TokenKind::Float(n)) = self.peek_kind() {
            let n = *n;
            self.advance();
            return Ok(Expr::Literal { value: Literal::Float(n), span });
        }

        
        if let Some(TokenKind::String(s)) = self.peek_kind() {
            let s = *s;
            self.advance();
            return Ok(Expr::Literal { value: Literal::String(s), span });
        }

        
        if self.match_token(&TokenKind::True) {
            return Ok(Expr::Literal { value: Literal::Boolean(true), span });
        }

        
        if self.match_token(&TokenKind::False) {
            return Ok(Expr::Literal { value: Literal::Boolean(false), span });
        }

        
        if self.match_token(&TokenKind::Undefined) {
            return Ok(Expr::Literal { value: Literal::Undefined, span });
        }

        
        if self.check(&TokenKind::Function) {
            return self.parse_function_expr();
        }
        
        if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
            let name = *name;
            self.advance();
            return Ok(Expr::Identifier { name, span });
        }
        
        if self.match_token(&TokenKind::LeftParen) {
            let expr = self.parse_expression()?;
            self.expect(&TokenKind::RightParen)?;
            let end = self.previous_span();
            return Ok(Expr::Grouping {
                expr: Box::new(expr),
                span: Span::new(span.start, end.end),
            });
        }
        
        if self.match_token(&TokenKind::LeftBracket) {
            let mut elements = Vec::new();
            if !self.check(&TokenKind::RightBracket) {
                loop {
                    elements.push(self.parse_expression()?);
                    if !self.match_token(&TokenKind::Comma) {
                        break;
                    }
                    if self.check(&TokenKind::RightBracket) {
                        break;
                    }
                }
            }
            self.expect(&TokenKind::RightBracket)?;
            let end = self.previous_span();
            return Ok(Expr::Array {
                elements,
                span: Span::new(span.start, end.end),
            });
        }
        
        if self.match_token(&TokenKind::LeftBrace) {
            let mut fields = Vec::new();
            if !self.check(&TokenKind::RightBrace) {
                loop {
                    let key = if let Some(TokenKind::String(s)) = self.peek_kind() {
                        let s = *s;
                        self.advance();
                        s
                    } else {
                        self.expect_identifier()?
                    };
                    self.expect(&TokenKind::Colon)?;
                    let value = self.parse_expression()?;
                    fields.push((key, value));
                    if !self.match_token(&TokenKind::Comma) {
                        break;
                    }
                    if self.check(&TokenKind::RightBrace) {
                        break;
                    }
                }
            }
            self.expect(&TokenKind::RightBrace)?;
            let end = self.previous_span();
            return Ok(Expr::Struct {
                fields,
                span: Span::new(span.start, end.end),
            });
        }
        
        Err(ParseError::new(
            format!("Expected expression, got {:?}", self.peek_kind()),
            span,
        ))
    }

    // ========== Helper methods ==========


    fn is_at_end(&self) -> bool {
        // Use peek_kind() which skips comments, so comment-only files reach Eof properly
        matches!(self.peek_kind(), Some(TokenKind::Eof) | None)
    }



    /// Peek at current token, automatically skipping any comments
    fn peek(&self) -> Option<&Token<'a>> {

        let mut idx = self.current;
        while idx < self.tokens.len() {
            match &self.tokens[idx].kind {
                TokenKind::LineComment(_) | TokenKind::BlockComment(_) | TokenKind::DocComment(_) => {
                    idx += 1;
                }
                _ => return Some(&self.tokens[idx]),
            }
        }
        None
    }

    fn peek_kind(&self) -> Option<&TokenKind<'a>> {
        self.peek().map(|t| &t.kind)
    }


    fn previous(&self) -> &Token<'a> {
        &self.tokens[self.current.saturating_sub(1)]
    }


    fn current_span(&self) -> Span {
        self.peek().map(|t| t.span).unwrap_or(Span::new(0, 0))
    }

    fn previous_span(&self) -> Span {
        self.previous().span
    }

    /// Advance to next non-comment token
    fn advance(&mut self) -> &Token<'a> {

        // First, skip any comments at current position (so we consume the real token)
        while self.current < self.tokens.len() {
            match &self.tokens[self.current].kind {
                TokenKind::LineComment(_) | TokenKind::BlockComment(_) | TokenKind::DocComment(_) => {
                    self.current += 1;
                }
                _ => break,
            }
        }
        
        // Now consume one token (the non-comment token we're at)
        let consumed = self.current;
        if self.current < self.tokens.len() {
            self.current += 1;
        }
        
        // Return the token we actually consumed
        &self.tokens[consumed.min(self.tokens.len().saturating_sub(1))]
    }

    fn check(&self, kind: &TokenKind) -> bool {
        self.peek_kind().is_some_and(|k| std::mem::discriminant(k) == std::mem::discriminant(kind))
    }

    fn check_comment(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(TokenKind::LineComment(_))
                | Some(TokenKind::BlockComment(_))
                | Some(TokenKind::DocComment(_))
        )
    }

    fn skip_comments(&mut self) {
        while self.check_comment() {
            self.advance();
        }
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: &TokenKind<'a>) -> Result<&Token<'a>, ParseError> {

        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::new(
                format!("Expected {:?}, got {:?}", kind, self.peek_kind()),
                self.current_span(),
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<&'a str, ParseError> {
        if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
            let name = *name;
            self.advance();
            Ok(name)
        } else {
            Err(ParseError::new(
                format!("Expected identifier, got {:?}", self.peek_kind()),
                self.current_span(),
            ))
        }
    }


    fn match_assign_op(&mut self) -> Option<AssignOp> {
        let op = match self.peek_kind()? {
            TokenKind::Assign => AssignOp::Assign,
            TokenKind::PlusAssign => AssignOp::Add,
            TokenKind::MinusAssign => AssignOp::Sub,
            TokenKind::StarAssign => AssignOp::Mul,
            TokenKind::SlashAssign => AssignOp::Div,
            TokenKind::PercentAssign => AssignOp::Mod,
            TokenKind::BitAndAssign => AssignOp::BitAnd,
            TokenKind::BitOrAssign => AssignOp::BitOr,
            TokenKind::BitXorAssign => AssignOp::BitXor,
            TokenKind::ShiftLeftAssign => AssignOp::ShiftLeft,
            TokenKind::ShiftRightAssign => AssignOp::ShiftRight,
            TokenKind::NullCoalesceAssign => AssignOp::NullCoalesce,
            _ => return None,
        };
        self.advance();
        Some(op)
    }




    fn skip_to_line_end(&mut self) {
        // Skip tokens until we hit a statement starter keyword
        // This is used for preprocessor directives like #region, #macro
        while !self.is_at_end() {
            match self.peek_kind() {
                // Stop at statement starters and braces
                Some(TokenKind::Function)
                | Some(TokenKind::Var)
                | Some(TokenKind::Static)
                | Some(TokenKind::If)
                | Some(TokenKind::For)
                | Some(TokenKind::While)
                | Some(TokenKind::Do)
                | Some(TokenKind::Repeat)
                | Some(TokenKind::With)
                | Some(TokenKind::Return)
                | Some(TokenKind::Exit)
                | Some(TokenKind::LeftBrace)
                | Some(TokenKind::RightBrace)
                | Some(TokenKind::Macro(_, _))
                | Some(TokenKind::Region(_))
                | Some(TokenKind::EndRegion)
                | Some(TokenKind::Switch)
                | Some(TokenKind::Break)
                | Some(TokenKind::Continue)
                | Some(TokenKind::Enum)
                | Some(TokenKind::Try)
                | Some(TokenKind::Throw)
                | Some(TokenKind::Eof) => break,
                // Skip any other tokens (identifiers, operators, etc. on same line)
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Recover from an error by skipping to a synchronization point
    fn synchronize(&mut self) {
        self.advance();
        
        while !self.is_at_end() {
            // After a semicolon, we're probably at a new statement
            if matches!(self.previous().kind, TokenKind::Semicolon) {
                return;
            }
            
            // At statement-starting keywords, we're at a new statement
            match self.peek_kind() {
                Some(TokenKind::Function)
                | Some(TokenKind::Var)
                | Some(TokenKind::Static)
                | Some(TokenKind::For)
                | Some(TokenKind::If)
                | Some(TokenKind::While)
                | Some(TokenKind::Return)
                | Some(TokenKind::Switch)
                | Some(TokenKind::With)
                | Some(TokenKind::Repeat) => return,
                _ => {}
            }
            
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_var_decl() {
        let parser = Parser::new("var x = 42;");
        let result = parser.parse();
        assert!(result.is_ok());
        let program = result.unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_function() {
        let parser = Parser::new("function foo(a, b) { return a + b; }");
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_if_else() {
        let parser = Parser::new("if (x > 0) { y = 1; } else { y = 0; }");
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_for_loop() {
        let parser = Parser::new("for (var i = 0; i < 10; i++) { x += i; }");
        let result = parser.parse();
        assert!(result.is_ok());
    }
}
