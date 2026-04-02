use std::{iter::Peekable, rc::Rc, vec::IntoIter};

use crate::{hfs::{
    ScopeKind,
    ast::*,
    error::{CompileError, DiagnosticInfo},
    parser_errors::{Expectable, ParserError, ParserErrorKind},
    token::*,
    unresolved_ast::*,
}, parser_error};

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>, // Own the tokens, iterate by value
    arena: UnresolvedAstArena,
}

impl Parser {
    fn expect(&mut self, token_kind: TokenKind) -> Result<Token, Box<dyn CompileError>> {
        match self.tokens.next() {
            Some(token) if std::mem::discriminant(&token.kind) == std::mem::discriminant(&token_kind) => Ok(token),
            Some(found) => parser_error!(
                ParserErrorKind::ExpectedButFound(vec![Expectable::Token(token_kind)], Some(found.kind)),
                &self.arena,
                vec![found.source_info]
            ),
            None => parser_error!(
                ParserErrorKind::ExpectedButFound(vec![Expectable::Token(token_kind)], None),
                &self.arena,
                vec![self.arena.diagnostic_info.eof_pos.clone()]
            ),
        }
    }

    fn expect_identifier(&mut self) -> Result<(String, Token), Box<dyn CompileError>> {
        let token = match self.tokens.next() {
            Some(token) => token,
            None =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::Identifier], None),
                    &self.arena,
                    vec![self.arena.diagnostic_info.eof_pos.clone()]
                ),
        };
        match &token.kind {
            TokenKind::Identifier(name) => Ok((name.clone(), token)),
            _ => parser_error!(
                ParserErrorKind::ExpectedButFound(vec![Expectable::Identifier], Some(token.kind)),
                &self.arena,
                vec![token.source_info]
            ),
        }
    }

    fn expect_stack_keyword(&mut self) -> Result<(String, Token), Box<dyn CompileError>> {
        let token = match self.tokens.next() {
            Some(token) => token,
            None =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::StackKeyword], None),
                    &self.arena,
                    vec![self.arena.diagnostic_info.eof_pos.clone()]
                ),
        };
        match &token.kind {
            TokenKind::StackKeyword(name) => Ok((name.clone(), token)),
            _ => parser_error!(
                ParserErrorKind::ExpectedButFound(vec![Expectable::StackKeyword], Some(token.kind)),
                &self.arena,
                vec![token.source_info]
            ),
        }
    }

    fn consume_token_chain(&mut self, kind: TokenKind) -> usize {
        let mut tkn_count = 0;
        while let Some(token) = self.tokens.peek()
            && token.kind == kind
        {
            self.tokens.next();
            tkn_count += 1;
        }
        tkn_count
    }

    fn expect_type(&mut self) -> Result<TypeId, Box<dyn CompileError>> {
        let token = match self.tokens.peek() {
            Some(token) => token.clone(),
            None =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::Type], None),
                    &self.arena,
                    vec![self.arena.diagnostic_info.eof_pos.clone()]
                ),
        };
        match &token.kind {
            kind if kind.is_type() => {
                self.tokens.next();
                let ptr_count = self.consume_token_chain(TokenKind::Star);
                Ok(self.arena.to_type(token, ptr_count))
            },
            TokenKind::LeftParen => {
                let hfs_type = Type::Tuple { type_ids: self.type_list()?, ptr_count: 0 };
                let ptr_count = self.consume_token_chain(TokenKind::Star);
                Ok(self.arena.alloc_type(
                    Type::Tuple {
                        type_ids: match hfs_type {
                            Type::Tuple { type_ids, .. } => type_ids,
                            _ => unreachable!(),
                        },
                        ptr_count,
                    },
                    token,
                ))
            },
            _ => parser_error!(
                ParserErrorKind::ExpectedButFound(vec![Expectable::Type], Some(token.kind)),
                &self.arena,
                vec![token.source_info]
            ),
        }
    }

    fn type_list(&mut self) -> Result<Vec<TypeId>, Box<dyn CompileError>> {
        self.expect(TokenKind::LeftParen)?;
        let mut types = Vec::<TypeId>::new();
        loop {
            if let Some(token) = self.tokens.peek()
                && token.kind == TokenKind::RightParen
            {
                self.tokens.next();
                break;
            }
            types.push(self.expect_type()?);
        }
        Ok(types)
    }

}

// Declarations
impl Parser {
    // <function_decl> ::= "fn" <identifier> ":" <signature> "{" <block_scope> "}"
    fn function_declaration(&mut self) -> Result<UnresolvedFuncId, Box<dyn CompileError>> {
        self.expect(TokenKind::Fn)?;
        let (name, token) = self.expect_identifier()?;
        let (param_types, return_types) = self.function_signature()?;
        let body = self.block_scope(ScopeKind::Function)?;
        Ok(self.arena.alloc_unresolved_function(
            UnresolvedFunctionDeclaration { name, param_type: param_types, return_type: return_types, body },
            token,
        ))
    }
    // <var_decl> ::= "let" <identifier> ":" <type> ";"
    fn variable_declaration(&mut self) -> Result<UnresolvedVarId, Box<dyn CompileError>> {
        self.expect(TokenKind::Let)?;
        let (name, token) = self.expect_identifier()?;
        self.expect(TokenKind::Colon)?;
        let hfs_type = self.expect_type()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(self.arena.alloc_unresolved_var(UnresolvedVarDeclaration { name, hfs_type }, token))
    }
    // <signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"
    fn function_signature(&mut self) -> Result<(TypeId, TypeId), Box<dyn CompileError>> {
        self.expect(TokenKind::Colon)?;
        let param_types = self.expect_type()?; // tuple or single type

        self.expect(TokenKind::Arrow)?;
        let return_types = self.expect_type()?;

        Ok((param_types, return_types))
    }
}

// RD Parser for Henceforth (check 'henceforth-bnf.md')
impl Parser {
    // <top_level_node> ::= <var_decl> | <function_decl> | <statement>
    #[must_use]
    pub fn parse_tokens(
        tokens: Vec<Token>,
        diagnostic_info: Rc<DiagnosticInfo>,
    ) -> Result<(Vec<UnresolvedTopLevelId>, UnresolvedAstArena), Box<dyn CompileError>> {
        let mut parser = Parser {
            tokens: tokens.into_iter().peekable(),
            arena: UnresolvedAstArena::new(diagnostic_info.clone()),
        };
        let mut top_level = Vec::<UnresolvedTopLevelId>::new();
        
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => top_level.push(UnresolvedTopLevelId::VariableDecl(parser.variable_declaration()?)),
                TokenKind::Fn => top_level.push(UnresolvedTopLevelId::FunctionDecl(parser.function_declaration()?)),
                _ =>
                    return parser_error!(
                        ParserErrorKind::ExpectedButFound(
                            vec![Expectable::FunctionDecl, Expectable::VariableDecl],
                            Some(token.kind.clone()),
                        ),
                        &parser.arena,
                        vec![token.source_info.clone()]
                    ),
            };
        }
        Ok((top_level, parser.arena))
    }
    // <block_scope> ::= "{" <top_level_node>* "}"
    fn block_scope(&mut self, scope_kind: ScopeKind) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let mut top_level_ids = Vec::<UnresolvedTopLevelId>::new();
        let token = self.expect(TokenKind::LeftBrace)?;
        loop {
            let token = match self.tokens.peek() {
                Some(token) => token,
                None =>
                    return parser_error!(
                        ParserErrorKind::ExpectedButFound(
                            vec![
                                Expectable::VariableDecl,
                                Expectable::FunctionDecl,
                                Expectable::Statement,
                                Expectable::Token(TokenKind::RightBrace),
                            ],
                            None,
                        ),
                        &self.arena,
                        vec![self.arena.diagnostic_info.eof_pos.clone()]
                    ),
            };
            match &token.kind {
                kind if *kind == TokenKind::RightBrace => break,
                TokenKind::Let => top_level_ids.push(UnresolvedTopLevelId::VariableDecl(self.variable_declaration()?)),
                TokenKind::Fn => top_level_ids.push(UnresolvedTopLevelId::FunctionDecl(self.function_declaration()?)),
                TokenKind::At => top_level_ids.push(UnresolvedTopLevelId::Statement(self.stack_block()?)),
                _ => top_level_ids.push(UnresolvedTopLevelId::Statement(self.statement()?)),
            };
        }
        // consume the '}'
        self.tokens.next();
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::BlockScope(top_level_ids, scope_kind), token))
    }

    // <statement> ::= <if_stmt> | <stack_block> | <while_stmt> | <return_stmt>
    //               | <break_stmt> | <continue_stmt> | <assignment_stmt> | ";"
    fn statement(&mut self) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let token = match self.tokens.peek() {
            Some(token) => token,
            None =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::Statement], None),
                    &self.arena,
                    vec![self.arena.diagnostic_info.eof_pos.clone()]
                ),
        };
        match token.kind {
            TokenKind::If => Ok(self.if_statement()?),
            TokenKind::At => Ok(self.stack_block()?),
            TokenKind::LeftBrace => Ok(self.block_scope(ScopeKind::Block)?),
            TokenKind::While => Ok(self.while_statement()?),
            TokenKind::Break | TokenKind::Continue | TokenKind::Return => {
                let token = match self.tokens.next() {
                    Some(token) => token,
                    None => panic!("[internal error] peeked token but couldn't next it"),
                };
                self.expect(TokenKind::Semicolon)?;
                match token.kind {
                    TokenKind::Break => Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::Break, token)),
                    TokenKind::Continue => Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::Continue, token)),
                    TokenKind::Return => Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::Return, token)),
                    _ => unreachable!(),
                }
            },
            TokenKind::Semicolon => {
                let token = match self.tokens.next() {
                    Some(token) => token,
                    None => panic!("[internal error] peeked token but couldn't next it"),
                };
                Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::Empty, token))
            },
            TokenKind::CopyAssign => Ok(self.assignment(false)?),
            TokenKind::MoveAssign => Ok(self.assignment(true)?),
            TokenKind::CopyCall => Ok(self.function_call(false)?),
            TokenKind::MoveCall => Ok(self.function_call(true)?),
            TokenKind::StackKeyword(_) => Ok(self.stack_keyword_outside_stack_block()?),

            _ =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::Statement], Some(token.kind.clone())),
                    &self.arena,
                    vec![token.source_info.clone()]
                ),
        }
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let token = self.expect(TokenKind::If)?;
        let cond = self.stack_block()?;
        let body = self.block_scope(ScopeKind::IfStmt)?;
        let else_stmt = self.else_statement()?;
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::If { cond, body, else_stmt }, token))
    }

    // <else_stmt> ::= "else" "if" <stack_block>  <block_scope> <else_stmt>?
    //               | "else" <block_scope>
    fn else_statement(&mut self) -> Result<Option<UnresolvedStmtId>, Box<dyn CompileError>> {
        let token = match self.tokens.peek() {
            Some(token) => token,
            None =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::AnyToken], None),
                    &self.arena,
                    vec![self.arena.diagnostic_info.eof_pos.clone()]
                ),
        };
        match token.kind {
            TokenKind::Else => {
                self.tokens.next();
                let token = match self.tokens.peek() {
                    Some(token) => token,
                    None =>
                        return parser_error!(
                            ParserErrorKind::ExpectedButFound(vec![Expectable::AnyToken], None),
                            &self.arena,
                            vec![self.arena.diagnostic_info.eof_pos.clone()]
                        ),
                };
                if token.kind == TokenKind::If {
                    // TODO: check this part of the code
                    let token = self.expect(TokenKind::If)?;
                    let cond = self.stack_block()?;
                    let body = self.block_scope(ScopeKind::IfStmt)?;
                    let else_stmt = self.else_statement()?;
                    Ok(Some(self.arena.alloc_unresolved_stmt(UnresolvedStatement::ElseIf { cond, body, else_stmt }, token)))
                } else {
                    let tkn = token.clone();
                    let body = self.block_scope(ScopeKind::ElseStmt)?;
                    Ok(Some(self.arena.alloc_unresolved_stmt(UnresolvedStatement::Else(body), tkn)))
                }
            },
            _ => Ok(None),
        }
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let token = self.expect(TokenKind::While)?;
        let cond = self.stack_block()?;
        let body = self.block_scope(ScopeKind::WhileLoop)?;
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::While { cond, body }, token))
    }

    // <stack_block> ::= "@" "(" <expression>* ")"
    fn stack_block(&mut self) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let token = self.expect(TokenKind::At)?;

        let mut expressions = Vec::new();

        self.expect(TokenKind::LeftParen)?;
        loop {
            match self.tokens.peek() {
                Some(token) if token.kind == TokenKind::RightParen => {
                    self.expect(TokenKind::RightParen)?;
                    break;
                },
                Some(_) => expressions.push(self.stack_expression()?),
                None =>
                    return parser_error!(
                        ParserErrorKind::ExpectedButFound(
                            vec![Expectable::StackExpression, Expectable::Token(TokenKind::RightParen)],
                            None,
                        ),
                        &self.arena,
                        vec![self.arena.diagnostic_info.eof_pos.clone()]
                    ),
            };
        }
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::StackBlock(expressions), token))
    }
    // <stack_expression> ::= <stack_operation> | <identifier> | <literal> | <function_call>
    fn stack_expression(&mut self) -> Result<UnresolvedExprId, Box<dyn CompileError>> {
        let token = match self.tokens.peek() {
            Some(token) => token,
            None => panic!("[internal error] peeked stack expression the first time but failed the second time"),
        };
        match &token.kind {
            kind if kind.is_stack_operator() => self.stack_operation(),
            TokenKind::StackKeyword(_) => self.stack_keyword_expr(),
            TokenKind::Identifier(_) => {
                let (name, token) = self.expect_identifier()?;
                Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token))
            },
            TokenKind::Literal(lit) =>
                Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Literal(lit.clone()), self.tokens.next().unwrap())),
            TokenKind::LeftParen => self.tuple_expression(),
            _ =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::StackExpression], Some(token.kind.clone())),
                    &self.arena,
                    vec![token.source_info.clone()]
                ),
        }
    }
    // <stack_operation> ::= <binary_op> | <unary_op>
    // <binary_op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"
    // <unary_op>  ::= "!" | "~"
    fn stack_operation(&mut self) -> Result<UnresolvedExprId, Box<dyn CompileError>> {
        let token = match self.tokens.next() {
            Some(token) => token,
            None => panic!("[internal error] couldn't next peeked stack operation"), // maybe change to parser error in the future
        };
        let kind = token.kind.clone();
        match kind {
            kind if kind.is_binary_operator() => match kind {
                TokenKind::Plus =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Add), token)),
                TokenKind::Minus =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Sub), token)),
                TokenKind::Star =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Mul), token)),
                TokenKind::Slash =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Div), token)),
                TokenKind::Percent =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Mod), token)),
                TokenKind::Equal =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Equal), token)),
                TokenKind::NotEqual =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::NotEqual), token)),
                TokenKind::Less =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Less), token)),
                TokenKind::LessEqual =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::LessEqual), token)),
                TokenKind::Greater =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Greater), token)),
                TokenKind::GreaterEqual => Ok(self
                    .arena
                    .alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::GreaterEqual), token)),
                TokenKind::And =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::And), token)),
                TokenKind::Or =>
                    Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Or), token)),
                _ => unreachable!(),
            },
            TokenKind::Not =>
                Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Not), token)),
            TokenKind::Dereference =>
                Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Dereference), token)),
            TokenKind::AddressOf =>
                Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::AddressOf), token)),
            _ =>
                return parser_error!(
                    ParserErrorKind::ExpectedButFound(vec![Expectable::StackOperation], Some(kind)),
                    &self.arena,
                    vec![token.source_info.clone()] // shouldn't happen
                ),
        }
    }

    // <tuple_expression> ::= "(" <stack_expression>* ")"
    fn tuple_expression(&mut self) -> Result<UnresolvedExprId, Box<dyn CompileError>> {
        let mut expressions = Vec::<UnresolvedExprId>::new();
        let token = self.expect(TokenKind::LeftParen)?;
        loop {
            match self.tokens.next() {
                Some(token) if token.kind == TokenKind::RightParen => break,
                _ => expressions.push(self.stack_expression()?),
            };
        }
        Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::Tuple { expressions }, token))
    }

    // <assignment> ::= "&=" <identifier> ";"
    //                | ":=" <identifier> ";"
    fn assignment(&mut self, is_move: bool) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier()?;
        let deref_count = self.consume_token_chain(TokenKind::Dereference);
        let identifier = self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token);

        self.expect(TokenKind::Semicolon)?;
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::Assignment { identifier, is_move, deref_count }, assign_tkn))
    }

    // <function_call> ::= "&>" <identifier> ";"
    //                   | ":>" <identifier> ";"
    fn function_call(&mut self, is_move: bool) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier()?;
        let identifier = self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token);

        self.expect(TokenKind::Semicolon)?;
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::FunctionCall { identifier, is_move }, assign_tkn))
    }

    // <stack-keyword> ::= @pop | @pop_all | @dup | @swap | @over | @rot | @rrot | @nip | @tuck
    fn stack_keyword_outside_stack_block(&mut self) -> Result<UnresolvedStmtId, Box<dyn CompileError>> {
        let (name, token) = self.expect_stack_keyword()?;

        // allocate as an expression inside a fake stack block
        // converts `@pop` into `@(@pop)`
        let keyword = self.arena.alloc_unresolved_expr(UnresolvedExpression::StackKeyword(name), token.clone());
        Ok(self.arena.alloc_unresolved_stmt(UnresolvedStatement::StackBlock(vec![keyword]), token))
    }

    // <stack-keyword> ::= @pop | @pop_all | @dup | @swap | @over | @rot | @rrot | @nip | @tuck | @print
    fn stack_keyword_expr(&mut self) -> Result<UnresolvedExprId, Box<dyn CompileError>> {
        let (name, token) = self.expect_stack_keyword()?;
        Ok(self.arena.alloc_unresolved_expr(UnresolvedExpression::StackKeyword(name), token))
    }
}

