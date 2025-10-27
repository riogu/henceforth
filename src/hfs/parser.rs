use crate::hfs::ast::*;
use crate::hfs::unresolved_ast::*;
use crate::hfs::token::*;
use crate::hfs::ScopeKind;

use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<'a> {
    tokens: Peekable<IntoIter<Token<'a>>>, // Own the tokens, iterate by value
    arena: UnresolvedAstArena<'a>,
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
    fn expect_type(&mut self) -> TypeId {
        let token = self.tokens.peek().expect("unexpected end of input").clone();
        match &token.kind {
            kind if kind.is_type() => {
                self.tokens.next();
                self.arena.to_type(token)
            }
            TokenKind::LeftParen => {
                let hfs_type = Type::Tuple(self.type_list());
                self.arena.alloc_type(hfs_type, token)
            }
            kind => panic!("expected type, got {:?}", kind),
        }
    }
    fn type_list(&mut self) -> Vec<TypeId> {
        self.expect(TokenKind::LeftParen);
        let mut types = Vec::<TypeId>::new();
        loop {
            match self.tokens.next().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => break,
                token if token.is_type() => types.push(self.arena.to_type(token)),
                token => panic!("expected type, found {:?}", token.kind),
            };
        }
        types
    }
}

// Declarations
impl<'a> Parser<'a> {
    // <function_decl> ::= "fn" <identifier> ":" <signature> "{" <block_scope> "}"
    fn function_declaration(&mut self) -> UnresolvedFuncId {
        let (name, token) = self.expect_identifier();
        let (param_types, return_types) = self.function_signature();
        let body = self.block_scope(ScopeKind::Function);
        self.arena.alloc_unresolved_function(
            UnresolvedFunctionDeclaration { name, param_type: param_types, return_type: return_types, body, },
            token,
        )
    }
    // <var_decl> ::= "let" <identifier> ":" <type> ";"
    fn variable_declaration(&mut self) -> UnresolvedVarId {
        let (name, token) = self.expect_identifier();
        self.expect(TokenKind::Colon);
        let hfs_type = self.expect_type();
        self.expect(TokenKind::Comma);
        self.arena.alloc_unresolved_var(UnresolvedVarDeclaration { name , hfs_type }, token)
    }
    // <signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"
    fn function_signature(&mut self) -> (TypeId, TypeId) {
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
    #[must_use] pub fn parse_tokens(tokens: Vec<Token>) -> (Vec<UnresolvedTopLevelId>, UnresolvedAstArena) {
        let mut parser = Parser {
            tokens: tokens.into_iter().peekable(),
            arena: UnresolvedAstArena::new(),
        };
        let mut top_level = Vec::<UnresolvedTopLevelId>::new();
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => top_level.push(UnresolvedTopLevelId::VariableDecl(parser.variable_declaration())),
                TokenKind::Fn  => top_level.push(UnresolvedTopLevelId::FunctionDecl(parser.function_declaration())),
                _ => panic!("expected variable or function declaration"),
            };
        }
        (top_level, parser.arena)
    }
    // <block_scope> ::= "{" <top_level_node>* "}"
    fn block_scope(&mut self, scope_kind: ScopeKind) -> UnresolvedStmtId {
        let mut top_level_ids = Vec::<UnresolvedTopLevelId>::new();
        let token = self.expect(TokenKind::LeftBrace);
        loop {
            match self.tokens.next().expect("unexpected end of input").kind {
                kind if kind == TokenKind::RightBrace => break,
                TokenKind::Let => top_level_ids.push(UnresolvedTopLevelId::VariableDecl(self.variable_declaration())),
                TokenKind::Fn  => top_level_ids.push(UnresolvedTopLevelId::FunctionDecl(self.function_declaration())),
                TokenKind::At  => top_level_ids.push(UnresolvedTopLevelId::Statement(self.stack_block())),
                _ =>  top_level_ids.push(UnresolvedTopLevelId::Statement(self.statement())),
            };
        }
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::BlockScope(top_level_ids, scope_kind), token)
    }

    // <statement> ::= <if_stmt> | <stack_block> | <while_stmt> | <return_stmt> 
    //               | <break_stmt> | <continue_stmt> | <assignment_stmt> | ";" 
    fn statement(&mut self) -> UnresolvedStmtId {
        let token = self.tokens.next().expect("unexpected end of input while parsing statement");
        match token.kind {
            TokenKind::If        => self.if_statement(),
            TokenKind::At        => self.stack_block(),
            TokenKind::LeftBrace => self.block_scope(ScopeKind::Block),
            TokenKind::While => self.while_statement(),
            TokenKind::Return => {
                self.expect(TokenKind::Semicolon);
                self.arena .alloc_unresolved_stmt(UnresolvedStatement::Return, token)
            }
            TokenKind::Break => {
                self.expect(TokenKind::Semicolon);
                self.arena.alloc_unresolved_stmt(UnresolvedStatement::Break, token)
            }
            TokenKind::Continue => {
                self.expect(TokenKind::Semicolon);
                self.arena.alloc_unresolved_stmt(UnresolvedStatement::Continue, token)
            }
            TokenKind::CopyAssign => self.assignment(false),
            TokenKind::MoveAssign => self.assignment(true),
            TokenKind::Semicolon  => self.arena.alloc_unresolved_stmt(UnresolvedStatement::Empty, token),
            _ => panic!("expected statement"),
        }
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> UnresolvedStmtId {
        let token = self.expect(TokenKind::If);
        self.stack_block();
        let body = self.block_scope(ScopeKind::IfStmt);
        let else_stmt = self.else_statement();
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::If { body, else_stmt}, token)
    }

    // <else_stmt> ::= "else" "if" <stack_block>  <block_scope> <else_stmt>?
    //               | "else" <block_scope>
    fn else_statement(&mut self) -> Option<UnresolvedElseStmt> {
        let token = self.tokens.peek().expect("unexpected end of input while parsing statement");
        match token.kind {
            TokenKind::Else => {
                self.tokens.next();
                let token = self.tokens.peek().expect("unexpected end of input while parsing statement");
                if token.kind == TokenKind::If {
                  Some(UnresolvedElseStmt::ElseIf(self.if_statement()))
                } else {
                    Some(UnresolvedElseStmt::Else(self.block_scope(ScopeKind::ElseStmt)))
                }
            }
            _ => None
        }
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> UnresolvedStmtId {
        let token = self.expect(TokenKind::While);
        self.stack_block();
        let body = self.block_scope(ScopeKind::WhileLoop);
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::While { body }, token)
    }

    // <stack_block> ::= "@" "(" <expression>* ")"
    fn stack_block(&mut self) -> UnresolvedStmtId {
        let token = self.expect(TokenKind::At);

        let mut expressions = Vec::<UnresolvedExprId>::new();

        self.expect(TokenKind::LeftParen);
        loop {
            match self.tokens.next().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => break,
                _ => expressions.push(self.stack_expression()),
            };
        }
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::StackBlock(expressions), token)
    }
// <stack_expression> ::= <stack_operation> | <identifier> | <literal> | <function_call>
    fn stack_expression(&mut self) -> UnresolvedExprId {
        let kind = self.tokens.peek().expect("unexpected end of input while parsing statement").kind.clone();
        match kind {
            kind if kind.is_stack_operator() => self.stack_operation(),
            TokenKind::Identifier(_) => {
                let (name, token) = self.expect_identifier();
                self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token)
            }
            TokenKind::Literal(lit) => self.arena.alloc_unresolved_expr(UnresolvedExpression::Literal(lit), self.tokens.next().unwrap()),
            TokenKind::LeftParen    => self.function_call_or_tuple_expr(),
            _ => panic!("expected expression")
        }

    }
// <stack_operation> ::= <binary_op> | <unary_op>
// <binary_op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"
// <unary_op>  ::= "!" | "~"
    fn stack_operation(&mut self) -> UnresolvedExprId {
        let token = self.tokens.next().expect("unexpected end of input while parsing statement");
        let kind = token.kind.clone();
        match kind {
            kind if kind.is_binary_operator() => {
                match kind {
                    TokenKind::Plus         => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Add), token),
                    TokenKind::Minus        => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Sub), token),
                    TokenKind::Star         => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Mul), token),
                    TokenKind::Slash        => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Div), token),
                    TokenKind::Percent      => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Mod), token),
                    TokenKind::Equal        => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Equal), token),
                    TokenKind::NotEqual     => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::NotEqual), token),
                    TokenKind::Less         => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Less), token),
                    TokenKind::LessEqual    => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::LessEqual), token),
                    TokenKind::Greater      => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Greater), token),
                    TokenKind::GreaterEqual => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::GreaterEqual), token),
                    TokenKind::And          => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::And), token),
                    TokenKind::Or           => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Or), token),
                    _ => unreachable!()
                }
            }
            TokenKind::Not => {
                self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Not), token)
            }
            _ => panic!("expected stack operator")
        }
    }

    // <function_call> ::= <tuple_expression> <identifier>?
    fn function_call_or_tuple_expr(&mut self) -> UnresolvedExprId {
        // we want to allow tuples anywhere and functions consume a tuple from the stack
        // this looks for an identifier immediatelly after the tuple, if found it assumes we have a
        // function call. later, StackAnalyzer will verify if this was a function call, or if it
        // was just a variable that was pushed to the stack
        let tuple = self.tuple_expression(); // for now we build a tuple explicitly
        let token = self.tokens.peek().expect("unexpected end of input");
        match &token.kind {
            TokenKind::Identifier(name) =>  {
                let (identifier, token) = self.expect_identifier();
                let identifier = self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(identifier), token.clone());
                self.arena.alloc_unresolved_expr(UnresolvedExpression::FunctionCall { identifier }, token)
            }
            _ => tuple
        }
    }

    // <tuple_expression> ::= "(" <stack_expression>* ")"
    fn tuple_expression(&mut self) -> UnresolvedExprId {
        let mut expressions = Vec::<UnresolvedExprId>::new();
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
        if variadic {
            self.expect(TokenKind::Arrow); // consume the '->'
            let called_func_name = match &self.tokens.peek().expect("unexpected end of input").kind {
                TokenKind::Identifier(name) => Some(name.clone()),
                _ => panic!("implicit '(...)' tuples are only allowed to be used as function call arguments")
            };
            
            self.arena.alloc_unresolved_expr(UnresolvedExpression::Tuple{expressions, variadic, called_func_name}, token)
        } else {
            self.arena.alloc_unresolved_expr(UnresolvedExpression::Tuple{expressions, variadic, called_func_name: None}, token)
        }
    }

    // <assignment> ::= "&=" <identifier> ";"
    //                | ":=" <identifier> ";"
    fn assignment(&mut self, is_move: bool) -> UnresolvedStmtId {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier();
        let identifier = self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token);

        self.expect(TokenKind::Semicolon); 
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::Assignment{identifier, is_move}, assign_tkn)
    }
}
