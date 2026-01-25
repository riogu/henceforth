use std::{iter::Peekable, vec::IntoIter};

use crate::hfs::{ast::*, token::*, unresolved_ast::*, ScopeKind};

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>, // Own the tokens, iterate by value
    arena: UnresolvedAstArena,
}

impl Parser {
    fn expect(&mut self, token_kind: TokenKind) -> Token {
        match self.tokens.next() {
            Some(token) if std::mem::discriminant(&token.kind) == std::mem::discriminant(&token_kind) => token,
            Some(found) => panic!("expected '{:?}', found '{:?}", token_kind, found.kind),
            None => panic!("expected '{:?}', token stream was empty", token_kind),
        }
    }

    fn expect_identifier(&mut self) -> (String, Token) {
        let token = self.tokens.next().expect("unexpected end of input");
        match &token.kind {
            TokenKind::Identifier(name) => (name.clone(), token),
            _ => panic!("expected identifier, got {:?}", token.kind),
        }
    }

    fn expect_stack_keyword(&mut self) -> (String, Token) {
        let token = self.tokens.next().expect("unexpected end of input");
        match &token.kind {
            TokenKind::StackKeyword(name) => (name.clone(), token),
            _ => panic!("expected stack keyword, got {:?}", token.kind),
        }
    }
    fn expect_type(&mut self) -> TypeId {
        let token = self.tokens.peek().expect("unexpected end of input").clone();
        match &token.kind {
            kind if kind.is_type() => {
                self.tokens.next();
                self.arena.to_type(token)
            },
            TokenKind::LeftParen => {
                let hfs_type = Type::Tuple(self.type_list());
                self.arena.alloc_type(hfs_type, token)
            },
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
impl Parser {
    // <function_decl> ::= "fn" <identifier> ":" <signature> "{" <block_scope> "}"
    fn function_declaration(&mut self) -> UnresolvedFuncId {
        self.expect(TokenKind::Fn);
        let (name, token) = self.expect_identifier();
        let (param_types, return_types) = self.function_signature();
        let body = self.block_scope(ScopeKind::Function);
        self.arena.alloc_unresolved_function(
            UnresolvedFunctionDeclaration { name, param_type: param_types, return_type: return_types, body },
            token,
        )
    }
    // <var_decl> ::= "let" <identifier> ":" <type> ";"
    fn variable_declaration(&mut self) -> UnresolvedVarId {
        self.expect(TokenKind::Let);
        let (name, token) = self.expect_identifier();
        self.expect(TokenKind::Colon);
        let hfs_type = self.expect_type();
        self.expect(TokenKind::Semicolon);
        self.arena.alloc_unresolved_var(UnresolvedVarDeclaration { name, hfs_type }, token)
    }
    // <signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"
    fn function_signature(&mut self) -> (TypeId, TypeId) {
        self.expect(TokenKind::Colon);
        let param_types = self.expect_type(); // tuple or single type

        self.expect(TokenKind::Arrow);
        let return_types = self.expect_type();

        (param_types, return_types)
    }
}

// RD Parser for Henceforth (check 'henceforth-bnf.md')
impl Parser {
    // <top_level_node> ::= <var_decl> | <function_decl> | <statement>
    #[must_use]
    pub fn parse_tokens(tokens: Vec<Token>) -> (Vec<UnresolvedTopLevelId>, UnresolvedAstArena) {
        let mut parser = Parser { tokens: tokens.into_iter().peekable(), arena: UnresolvedAstArena::new() };
        let mut top_level = Vec::<UnresolvedTopLevelId>::new();
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => top_level.push(UnresolvedTopLevelId::VariableDecl(parser.variable_declaration())),
                TokenKind::Fn => top_level.push(UnresolvedTopLevelId::FunctionDecl(parser.function_declaration())),
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
            match &self.tokens.peek().expect("unexpected end of input").kind {
                kind if *kind == TokenKind::RightBrace => break,
                TokenKind::Let => top_level_ids.push(UnresolvedTopLevelId::VariableDecl(self.variable_declaration())),
                TokenKind::Fn => top_level_ids.push(UnresolvedTopLevelId::FunctionDecl(self.function_declaration())),
                TokenKind::At => top_level_ids.push(UnresolvedTopLevelId::Statement(self.stack_block())),
                _ => top_level_ids.push(UnresolvedTopLevelId::Statement(self.statement())),
            };
        }
        // consume the '}'
        self.tokens.next();
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::BlockScope(top_level_ids, scope_kind), token)
    }

    // <statement> ::= <if_stmt> | <stack_block> | <while_stmt> | <return_stmt>
    //               | <break_stmt> | <continue_stmt> | <assignment_stmt> | ";"
    fn statement(&mut self) -> UnresolvedStmtId {
        let token = self.tokens.peek().expect("unexpected end of input while parsing statement");
        match token.kind {
            TokenKind::If => self.if_statement(),
            TokenKind::At => self.stack_block(),
            TokenKind::LeftBrace => self.block_scope(ScopeKind::Block),
            TokenKind::While => self.while_statement(),
            TokenKind::Break | TokenKind::Continue | TokenKind::Return => {
                let token = self.tokens.next().expect("unexpected end of input while parsing statement");
                self.expect(TokenKind::Semicolon);
                match token.kind {
                    TokenKind::Break => self.arena.alloc_unresolved_stmt(UnresolvedStatement::Break, token),
                    TokenKind::Continue => self.arena.alloc_unresolved_stmt(UnresolvedStatement::Continue, token),
                    TokenKind::Return => self.arena.alloc_unresolved_stmt(UnresolvedStatement::Return, token),
                    _ => unreachable!(),
                }
            },
            TokenKind::Semicolon => {
                let token = self.tokens.next().expect("unexpected end of input while parsing statement");
                self.arena.alloc_unresolved_stmt(UnresolvedStatement::Empty, token)
            },
            TokenKind::CopyAssign => self.assignment(false),
            TokenKind::MoveAssign => self.assignment(true),
            TokenKind::CopyCall => self.function_call(false),
            TokenKind::MoveCall => self.function_call(true),

            _ => panic!("expected statement, found '{:?}'", token.kind),
        }
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> UnresolvedStmtId {
        let token = self.expect(TokenKind::If);
        let cond = self.stack_block();
        let body = self.block_scope(ScopeKind::IfStmt);
        let else_stmt = self.else_statement();
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::If { cond, body, else_stmt }, token)
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
            },
            _ => None,
        }
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> UnresolvedStmtId {
        let token = self.expect(TokenKind::While);
        let cond = self.stack_block();
        let body = self.block_scope(ScopeKind::WhileLoop);
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::While { cond, body }, token)
    }

    // <stack_block> ::= "@" "(" <expression>* ")"
    fn stack_block(&mut self) -> UnresolvedStmtId {
        let token = self.expect(TokenKind::At);

        let mut expressions = Vec::new();

        self.expect(TokenKind::LeftParen);
        loop {
            match self.tokens.peek().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => {
                    self.expect(TokenKind::RightParen);
                    break;
                },
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
            TokenKind::StackKeyword(keyword) => self.stack_keyword_expr(),
            TokenKind::Identifier(_) => {
                let (name, token) = self.expect_identifier();
                self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token)
            },
            TokenKind::Literal(lit) =>
                self.arena.alloc_unresolved_expr(UnresolvedExpression::Literal(lit), self.tokens.next().unwrap()),
            TokenKind::LeftParen => self.tuple_expression(),
            _ => panic!("expected expression, found '{:?}'", kind),
        }
    }
    // <stack_operation> ::= <binary_op> | <unary_op>
    // <binary_op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"
    // <unary_op>  ::= "!" | "~"
    fn stack_operation(&mut self) -> UnresolvedExprId {
        let token = self.tokens.next().expect("unexpected end of input while parsing statement");
        let kind = token.kind.clone();
        match kind {
            kind if kind.is_binary_operator() => match kind {
                TokenKind::Plus =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Add), token),
                TokenKind::Minus =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Sub), token),
                TokenKind::Star =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Mul), token),
                TokenKind::Slash =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Div), token),
                TokenKind::Percent =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Mod), token),
                TokenKind::Equal =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Equal), token),
                TokenKind::NotEqual =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::NotEqual), token),
                TokenKind::Less =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Less), token),
                TokenKind::LessEqual =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::LessEqual), token),
                TokenKind::Greater =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Greater), token),
                TokenKind::GreaterEqual =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::GreaterEqual), token),
                TokenKind::And =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::And), token),
                TokenKind::Or =>
                    self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Or), token),
                _ => unreachable!(),
            },
            TokenKind::Not => self.arena.alloc_unresolved_expr(UnresolvedExpression::Operation(UnresolvedOperation::Not), token),
            _ => panic!("expected stack operator"),
        }
    }

    // <tuple_expression> ::= "(" <stack_expression>* ")"
    fn tuple_expression(&mut self) -> UnresolvedExprId {
        let mut expressions = Vec::<UnresolvedExprId>::new();
        let token = self.expect(TokenKind::LeftParen);
        loop {
            match self.tokens.next().expect("unexpected end of input") {
                token if token.kind == TokenKind::RightParen => break,
                _ => expressions.push(self.stack_expression()),
            };
        }
        self.arena.alloc_unresolved_expr(UnresolvedExpression::Tuple { expressions }, token)
    }

    // <assignment> ::= "&=" <identifier> ";"
    //                | ":=" <identifier> ";"
    fn assignment(&mut self, is_move: bool) -> UnresolvedStmtId {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier();
        let identifier = self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token);

        self.expect(TokenKind::Semicolon);
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::Assignment { identifier, is_move }, assign_tkn)
    }

    // <function_call> ::= "&>" <identifier> ";"
    //                   | ":>" <identifier> ";"
    fn function_call(&mut self, is_move: bool) -> UnresolvedStmtId {
        let assign_tkn = self.tokens.next().unwrap();

        let (name, token) = self.expect_identifier();
        let identifier = self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name), token);

        self.expect(TokenKind::Semicolon);
        self.arena.alloc_unresolved_stmt(UnresolvedStatement::FunctionCall { identifier, is_move }, assign_tkn)
    }

    // <stack-keyword> ::= @pop | @pop_all
    fn stack_keyword_expr(&mut self) -> UnresolvedExprId {
        let (name, token) = self.expect_stack_keyword();
        self.arena.alloc_unresolved_expr(UnresolvedExpression::StackKeyword(name), token)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;

    use crate::hfs::{
        builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
        parser_builder::ParserBuilder,
        File, Lexer, Parser, Type, UnresolvedAstArena,
    };

    fn parse_file(file: &File) -> UnresolvedAstArena {
        let tokens = Lexer::tokenize(&file);
        let (_, ast) = Parser::parse_tokens(tokens);
        ast
    }
    #[test]
    fn test_simple_main() {
        let path = PathBuf::from("test/simple_main.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);

        let expected = ParserBuilder::new().func_with("main", None, None).body().end_body().build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_function_with_lots_of_arguments() {
        let path = PathBuf::from("test/function_with_lots_of_arguments.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with(
                "func_with_lots_of_arguments",
                Some(vec![Type::Int, Type::Float, Type::String, Type::Bool]),
                Some(vec![Type::Int, Type::Float, Type::Bool, Type::String]),
            )
            .body()
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(5)
            .push_literal(5.0)
            .push_literal(false)
            .push_literal("test")
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_function_with_no_arguments() {
        let path = PathBuf::from("test/function_with_no_arguments.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("no_args", None, Some(vec![Type::Int]))
            .body()
            .stack_block()
            .push_literal(4)
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_function_with_no_return_type() {
        let path = PathBuf::from("test/function_with_no_return_type.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("no_return_type", Some(vec![Type::Int]), None)
            .body()
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_variable_declarations() {
        let path = PathBuf::from("test/variable_declarations.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);

        let expected = ParserBuilder::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::Int)
            .variable("b", Type::Float)
            .variable("c", Type::String)
            .variable("d", Type::Bool)
            .end_body()
            .build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_copy_and_move() {
        let path = PathBuf::from("test/copy_and_move.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("main", None, None)
            .body()
            .variable("copy", Type::Int)
            .variable("move", Type::Int)
            .stack_block()
            .push_literal(5)
            .end_stack_block(false)
            .assign_to("copy", PassMode::Copy)
            .assign_to("move", PassMode::Move)
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_operations() {
        let path = PathBuf::from("test/operations.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(1)
            .push_operation(BuilderOperation::Add)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Divide)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .push_literal(2)
            .push_operation(BuilderOperation::Modulo)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(false)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Or)
            .push_operation(BuilderOperation::Not)
            .push_literal(true)
            .push_operation(BuilderOperation::And)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_while_loop() {
        let path = PathBuf::from("test/while_loop.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::Int)
            .variable("b", Type::Int)
            .stack_block()
            .push_literal(100)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_literal(0)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .while_loop()
            .stack_block()
            .push_variable("a")
            .push_literal(0)
            .push_operation(BuilderOperation::GreaterThan)
            .push_variable("b")
            .push_literal(200)
            .push_operation(BuilderOperation::LessThan)
            .push_operation(BuilderOperation::And)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_variable("a")
            .push_literal(1)
            .push_operation(BuilderOperation::Subtract)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_variable("b")
            .push_literal(2)
            .push_operation(BuilderOperation::Add)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .end_body()
            .return_statement()
            .end_body()
            .build();
        assert_eq!(ast, expected)
    }

    #[test]
    fn test_simple_if_else() {
        let path = PathBuf::from("test/simple_if_else.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("main", None, None)
            .body()
            .if_statement()
            .stack_block()
            .push_literal(5)
            .push_literal(2)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(2)
            .push_literal(3)
            .push_literal(4)
            .end_stack_block(true)
            .push_stack_keyword("@pop_all", true)
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .return_statement()
            .end_body()
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_if_elif_else() {
        let path = PathBuf::from("test/if_elif_else.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("fizz_buzz", Some(vec![Type::Int]), Some(vec![Type::String]))
            .body()
            .if_statement()
            .stack_block()
            .push_literal(15)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(3)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(5)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("buzz")
            .end_stack_block(true)
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal("no fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(530)
            .end_stack_block(false)
            .call_function("fizz_buzz", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_copy_and_move_func_calls() {
        let path = PathBuf::from("test/copy_and_move_func_calls.hfs");
        let file = File::new(&path);
        let ast = parse_file(&file);
        let expected = ParserBuilder::new()
            .func_with("max", Some(vec![Type::Int, Type::Int]), Some(vec![Type::Int]))
            .body()
            .if_statement()
            .stack_block()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@dup", false)
            .push_stack_keyword("@rot", false)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .end_body()
            .func_with("max3", Some(vec![Type::Int, Type::Int, Type::Int]), Some(vec![Type::Int]))
            .body()
            .push_stack_keyword("@rrot", true)
            .call_function("max", PassMode::Move)
            .call_function("max", PassMode::Copy)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", false)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(50)
            .push_literal(10)
            .push_literal(30)
            .end_stack_block(false)
            .call_function("max3", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(ast, expected)
    }
}
