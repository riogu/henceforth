use crate::ast_node::*;
use crate::token::*;

use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>, // Own the tokens, iterate by value
    arena: AstArena<'a>,
}

impl<'a> Parser<'a> {
    fn expect(&mut self, token_kind: TokenKind) -> Token<'a> {
        match self.tokens.next() {
            Some(token) if std::mem::discriminant(&token.kind) 
                        == std::mem::discriminant(&token_kind) => token,
            Some(_) | None => panic!("expected '{:?}'", token_kind),
        }
    }

    fn expect_identifier(&mut self) -> (String, Token<'a>) {
        let token = self.tokens.next().expect("unexpected end of input");
        match &token.kind {
            TokenKind::Identifier(name) => (name.clone(), token),
            _ => panic!("expected identifier, got {:?}", token.kind),
        }
    }
    fn expect_type(&mut self) -> Type {
        let token = self.tokens.next().expect("unexpected end of input");
        match &token.kind {
            kind if kind.is_type() => kind.to_type(),
            _ => panic!("expected type, got {:?}", token.kind),
        }
    }
}

// Declarations
impl<'a> Parser<'a> {

    // <function_decl> ::= "fn" <identifier> ":" <signature> "{" <block_scope> "}"
    fn function_declaration(&mut self) -> FuncId {
        let (name, token) = self.expect_identifier();
        let (param_types, return_types) = self.function_signature();
        let body = self.block_scope();
        self.arena.alloc_function(FunctionDeclaration { name, param_types, return_types, body }, token)
    }
    // <var_decl> ::= "let" <identifier> ":" <type> ";"
    fn variable_declaration(&mut self) -> VarId {
        let (name, token) = self.expect_identifier();
        self.expect(TokenKind::Colon);
        let hfs_type = self.expect_type();
        self.expect(TokenKind::Comma);
        self.arena.alloc_var(VarDeclaration { name , hfs_type }, token)
    }
    // <signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"
    fn function_signature(&mut self) -> (Vec<Type>, Vec<Type>) {
        self.expect(TokenKind::Colon);
        let param_types = self.type_list();

        self.expect(TokenKind::Minus);
        self.expect(TokenKind::Greater);
        let return_types = self.type_list();

        (param_types, return_types)
    }
    fn type_list(&mut self) -> Vec<Type> {
        self.expect(TokenKind::LeftParen);
        let mut types = Vec::<Type>::new();
        loop {
            match self.tokens.next().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => break,
                token if token.is_type() => types.push(token.to_type()),
                token @ _ => panic!("expected type, found {:?}", token.kind),
            };
        }
        types
    }
}


// RD Parser for Henceforth (check 'henceforth-bnf.md')
impl<'a> Parser<'a> {
    // <top_level_node> ::= <var_decl> | <function_decl> | <statement>
    #[must_use] pub fn parse_tokens(tokens: Vec<Token>) -> Vec<TopLevelId> {
        let mut parser = Parser {
            tokens: tokens.into_iter().peekable(),
            arena: AstArena::new(),
        };
        let mut top_level = Vec::<TopLevelId>::new();
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => top_level.push(TopLevelId::VariableDecl(parser.variable_declaration())),
                TokenKind::Fn  => top_level.push(TopLevelId::FunctionDecl(parser.function_declaration())),
                _ => panic!("expected variable or function declaration"),
            };
        }
        top_level
    }

    // <statement> ::= <if_stmt> | <stack_block> | <while_stmt> | <return_stmt> 
    //               | <break_stmt> | <continue_stmt> | <assignment_stmt> | ";" 
    fn statement(&mut self) -> StmtId {
        let token = self.tokens.next().expect("unexpected end of input while parsing statement");
        match token.kind {
            TokenKind::If        => self.if_statement(),
            TokenKind::At        => self.stack_block(),
            TokenKind::LeftBrace => self.block_scope(),
            TokenKind::While     => self.while_statement(),
            TokenKind::Return    => { self.expect(TokenKind::Semicolon); self.arena.alloc_stmt(Statement::Return, token) }
            TokenKind::Break     => self.arena.alloc_stmt(Statement::Break, token),
            TokenKind::Continue  => self.arena.alloc_stmt(Statement::Continue, token),
            TokenKind::MoveAssign | TokenKind::CopyAssign => self.assignment(),
            TokenKind::Semicolon => self.arena.alloc_stmt(Statement::Empty, token),
            _ => panic!("expected statement"),
        }
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> StmtId {
        let token = self.expect(TokenKind::If);
        let cond = self.stack_block();
        let body = self.block_scope();
        let else_stmt = None;
        self.arena.alloc_stmt(Statement::If { cond, body, else_stmt}, token)
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> StmtId {
        let token = self.expect(TokenKind::While);
        let cond = self.stack_block();
        let body = self.block_scope();
        self.arena.alloc_stmt(Statement::While { cond, body }, token)
    }

    // <stack_block> ::= "@" "(" <expression>* ")"
    fn stack_block(&mut self) -> StmtId {
        let token = self.expect(TokenKind::At);
        self.expect(TokenKind::LeftParen);

        let statements = Vec::<ExprId>::new();

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                kind @ _ if kind.is_stack_operator() => self.stack_operation(),
                _ => panic!("expected variable or function declaration"),
            };
        }
        self.expect(TokenKind::RightBrace);
        self.arena.alloc_stmt(Statement::StackBlock(statements), token)
    }
    // <scope_block> ::= (<var_decl> | <statement>)*
    fn block_scope(&mut self) -> StmtId {
        let token = self.expect(TokenKind::LeftBrace);
        let top_level_ids = Vec::<TopLevelId>::new();

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Let => TopLevelId::VariableDecl(self.variable_declaration()),
                TokenKind::Fn  => TopLevelId::FunctionDecl(self.function_declaration()),
                TokenKind::At  => TopLevelId::Statement(self.stack_block()),
                _ => panic!("expected variable or function declaration"),
            };
        }
        self.expect(TokenKind::RightParen);
        self.arena.alloc_stmt(Statement::BlockScope(top_level_ids), token)
    }

// <stack_expression> ::= <stack_operation> | <identifier> | <literal> | <function_call>
    fn stack_expression(&mut self) -> ExprId {
        let kind = self.tokens.peek().expect("unexpected end of input while parsing statement").kind.clone();
        match kind {
            kind if kind.is_stack_operator() => self.stack_operation(),
            TokenKind::Identifier(_) => {
                let (name, token) = self.expect_identifier();
                self.arena.push_expr(Expression::Identifier(Identifier::Unresolved(name)), token)
            }
            TokenKind::Literal(lit) => self.arena.push_expr(Expression::Literal(lit), self.tokens.next().unwrap()),
            TokenKind::LeftParen    => self.function_call(),
            _ => panic!("expected expression")
        }

    }
// <stack_operation> ::= <binary_op> | <unary_op>
// <binary_op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"
// <unary_op>  ::= "!" | "~"
    fn stack_operation(&mut self) -> ExprId {
        match self.tokens.next().expect("unexpected end of input while parsing statement").kind {
            TokenKind::Plus => todo!(),
            TokenKind::Minus => todo!(),
            TokenKind::Star => todo!(),
            TokenKind::Slash => todo!(),
            TokenKind::Percent => todo!(),
            TokenKind::Equal => todo!(),
            TokenKind::NotEqual => todo!(),
            TokenKind::Less => todo!(),
            TokenKind::LessEqual => todo!(),
            TokenKind::Greater => todo!(),
            TokenKind::GreaterEqual => todo!(),
            TokenKind::And => todo!(),
            TokenKind::Or => todo!(),
            TokenKind::Not => todo!(),
            _ => todo!(),
        }
        todo!()
    }
    fn function_call(&mut self) -> ExprId {
        todo!()
    }

    // <assignment> ::= "&=" <identifier> ";"
    //                | ":=" <identifier> ";"
    fn assignment(&mut self) -> StmtId {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier();

        let value = if assign_tkn.kind == TokenKind::MoveAssign {
            self.arena.pop_or_error(&format!("tried moving into '{}' from empty stack", name))
        } else {
            self.arena.last_or_error(&format!("tried copying into '{}' from empty stack", name))
        };

        let identifier = self.arena.push_expr(Expression::Identifier(Identifier::Unresolved(name)), token);
        self.arena.alloc_stmt(Statement::Assignment{value, identifier}, assign_tkn)
    }
}
