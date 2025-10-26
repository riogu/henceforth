use crate::hfs::ast::*;
use crate::hfs::token::*;

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
        let kind = self.tokens.peek().expect("unexpected end of input").kind.clone();
        match &kind {
            kind if kind.is_type() => {
                self.tokens.next();
                kind.to_type()
            }
            TokenKind::LeftParen => Type::Tuple(self.type_list()),
            _ => panic!("expected type, got {:?}", kind),
        }
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

// Declarations
impl<'a> Parser<'a> {
    // <function_decl> ::= "fn" <identifier> ":" <signature> "{" <block_scope> "}"
    fn function_declaration(&mut self) -> FuncId {
        let (name, token) = self.expect_identifier();
        let (param_types, return_types) = self.function_signature();
        let body = self.block_scope();
        self.arena.alloc_function(FunctionDeclaration { name, param_type: param_types, return_type: return_types, body }, token)
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
    fn function_signature(&mut self) -> (Type, Type) {
        self.expect(TokenKind::Colon);
        let param_types = self.expect_type(); // tuple or single type

        self.expect(TokenKind::Minus);
        self.expect(TokenKind::Greater);
        let return_types = self.expect_type();

        (param_types, return_types)
    }
}

// TODO: remove stack management code from here and move to StackParser

// RD Parser for Henceforth (check 'henceforth-bnf.md')
impl<'a> Parser<'a> {
    // <top_level_node> ::= <var_decl> | <function_decl> | <statement>
    #[must_use] pub fn parse_tokens(tokens: Vec<Token>) -> (Vec<TopLevelId>, AstArena) {
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
        (top_level, parser.arena)
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
            TokenKind::CopyAssign => self.assignment(false), 
            TokenKind::MoveAssign => self.assignment(true),
            TokenKind::Semicolon => self.arena.alloc_stmt(Statement::Empty, token),
            _ => panic!("expected statement"),
        }
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> StmtId {
        let token = self.expect(TokenKind::If);
        self.stack_block();
        let cond = self.arena.last_or_error("expected boolean or operation on stack");
        let body = self.block_scope();
        let else_stmt = self.else_statement();
        self.arena.alloc_stmt(Statement::If { cond, body, else_stmt}, token)
    }

    // <else_stmt> ::= "else" "if" <stack_block>  <block_scope> <else_stmt>?
    //               | "else" <block_scope>
    fn else_statement(&mut self) -> Option<ElseStmt> {
        let token = self.tokens.peek().expect("unexpected end of input while parsing statement");
        match token.kind {
            TokenKind::Else => {
                self.tokens.next();
                let token = self.tokens.peek().expect("unexpected end of input while parsing statement");
                if token.kind == TokenKind::If {
                    Some(ElseStmt::ElseIf(self.if_statement()))
                } else {
                    Some(ElseStmt::Else(self.block_scope()))
                }
            }
            _ => None
        }
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> StmtId {
        let token = self.expect(TokenKind::While);
        self.stack_block();
        let cond = self.arena.last_or_error("expected boolean or operation on stack");
        let body = self.block_scope();
        self.arena.alloc_stmt(Statement::While { cond, body }, token)
    }

    // <block_scope> ::= "{" <top_level_node>* "}"
    fn block_scope(&mut self) -> StmtId {
        let mut top_level_ids = Vec::<TopLevelId>::new();
        let token = self.expect(TokenKind::LeftBrace);
        loop {
            match self.tokens.next().expect("unexpected end of input").kind {
                kind if kind == TokenKind::RightBrace => break,
                TokenKind::Let => top_level_ids.push(TopLevelId::VariableDecl(self.variable_declaration())),
                TokenKind::Fn  => top_level_ids.push(TopLevelId::FunctionDecl(self.function_declaration())),
                TokenKind::At  => top_level_ids.push(TopLevelId::Statement(self.stack_block())),
                _ => panic!("expected variable or function declaration"),
            };
        }
        self.arena.alloc_stmt(Statement::BlockScope(top_level_ids), token)
    }

    // <stack_block> ::= "@" "(" <expression>* ")"
    fn stack_block(&mut self) -> StmtId {
        let token = self.expect(TokenKind::At);

        let mut expressions = Vec::<ExprId>::new();

        self.expect(TokenKind::LeftParen);
        loop {
            match self.tokens.next().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => break,
                _ => expressions.push(self.stack_expression()),
            };
        }
        self.arena.alloc_stmt(Statement::StackBlock(expressions), token)
    }
// <stack_expression> ::= <stack_operation> | <identifier> | <literal> | <function_call>
    fn stack_expression(&mut self) -> ExprId {
        let kind = self.tokens.peek().expect("unexpected end of input while parsing statement").kind.clone();
        match kind {
            kind if kind.is_stack_operator() => self.stack_operation(),
            TokenKind::Identifier(_) => {
                let (name, token) = self.expect_identifier();
                self.arena.push_and_alloc_expr(Expression::Identifier(Identifier::Unresolved(name)), token)
            }
            TokenKind::Literal(lit) => self.arena.push_and_alloc_expr(Expression::Literal(lit), self.tokens.next().unwrap()),
            TokenKind::LeftParen    => self.function_call(),
            _ => panic!("expected expression")
        }

    }
// <stack_operation> ::= <binary_op> | <unary_op>
// <binary_op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"
// <unary_op>  ::= "!" | "~"
    fn stack_operation(&mut self) -> ExprId {
        let token = self.tokens.next().expect("unexpected end of input while parsing statement");
        let kind = token.kind.clone();
        match kind {
            kind if kind.is_binary_operator() => {
                let (lhs, rhs) = self.arena.pop2_or_error(&format!(
                    "expected at least 2 values in stack for binary operator {}",
                    kind
                ));
                match kind {
                    TokenKind::Plus         => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Add(lhs, rhs)), token),
                    TokenKind::Minus        => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Sub(lhs, rhs)), token),
                    TokenKind::Star         => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Mul(lhs, rhs)), token),
                    TokenKind::Slash        => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Div(lhs, rhs)), token),
                    TokenKind::Percent      => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Mod(lhs, rhs)), token),
                    TokenKind::Equal        => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Equal(lhs, rhs)), token),
                    TokenKind::NotEqual     => self.arena.push_and_alloc_expr(Expression::Operation(Operation::NotEqual(lhs, rhs)), token),
                    TokenKind::Less         => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Less(lhs, rhs)), token),
                    TokenKind::LessEqual    => self.arena.push_and_alloc_expr(Expression::Operation(Operation::LessEqual(lhs, rhs)), token),
                    TokenKind::Greater      => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Greater(lhs, rhs)), token),
                    TokenKind::GreaterEqual => self.arena.push_and_alloc_expr(Expression::Operation(Operation::GreaterEqual(lhs, rhs)), token),
                    TokenKind::And          => self.arena.push_and_alloc_expr(Expression::Operation(Operation::And(lhs, rhs)), token),
                    TokenKind::Or           => self.arena.push_and_alloc_expr(Expression::Operation(Operation::Or(lhs, rhs)), token),
                    _ => unreachable!()
                }
            }
            TokenKind::Not => {
                let expr = self.arena.pop_or_error("expected at least 1 value in stack for '-' unary operator.");
                self.arena.push_and_alloc_expr(Expression::Operation(Operation::Not(expr)), token)
            }
            _ => panic!("expected stack operator")
        }
    }

    // <function_call> ::= <tuple_expression> <identifier>
    fn function_call(&mut self) -> ExprId {
        let tuple = self.tuple_expression(); // for now we build a tuple explicitly
        // later we want to allow tuples anywhere and functions consume a tuple from the stack
        let (identifier, token) = self.expect_identifier();
        self.arena.push_and_alloc_expr(Expression::FunctionCall { tuple, identifier: Identifier::Unresolved(identifier) }, token)
    }

    // <tuple_expression> ::= "(" <stack_expression>* ")"
    fn tuple_expression(&mut self) -> ExprId {
        let mut expressions = Vec::<ExprId>::new();
        let token = self.expect(TokenKind::LeftParen);
        let variadic = self.tokens.peek().unwrap().kind == TokenKind::DotDotDot;
        if variadic { // not sure what to do with this in the parsing step... ill just add a boolean
            self.tokens.next();
        };
        loop {
            match self.tokens.next().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => break,
                _ => expressions.push(self.stack_expression()),
            };
        }
        self.arena.push_and_alloc_expr(Expression::Tuple{expressions, variadic}, token)
    }

    // <assignment> ::= "&=" <identifier> ";"
    //                | ":=" <identifier> ";"
    fn assignment(&mut self, is_move: bool) -> StmtId {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier();

        let value = if assign_tkn.kind == TokenKind::MoveAssign {
            self.arena.pop_or_error(&format!("tried moving into '{}' from empty stack", name))
        } else {
            self.arena.last_or_error(&format!("tried copying into '{}' from empty stack", name))
        };

        let identifier = self.arena.push_and_alloc_expr(Expression::Identifier(Identifier::Unresolved(name)), token);
        self.arena.alloc_stmt(Statement::Assignment{value, identifier, is_move}, assign_tkn)
    }
}
