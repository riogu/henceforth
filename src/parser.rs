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
}

impl<'a> Parser<'a> {

    // declarations
    fn function_declaration(&mut self) -> FuncId {
        todo!()
    }
    fn variable_declaration(&mut self) -> VarId {
        let (name, token) = self.expect_identifier();
        self.expect(TokenKind::Colon);
        let type_token = self.expect_identifier();
        self.expect(TokenKind::Comma);
        self.arena.push_var(VarDeclaration { name }, token)
    }
}


impl<'a> Parser<'a> {
    // RD Parser for Henceforth (check 'henceforth-bnf.md')
    #[must_use]
    pub fn parse_tokens(tokens: Vec<Token>) -> Vec<TopLevelId> {
        let mut parser = Parser {
            tokens: tokens.into_iter().peekable(),
            arena: AstArena::new(),
        };
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => TopLevelId::VariableDecl(parser.variable_declaration()),
                TokenKind::Fn => TopLevelId::FunctionDecl(parser.function_declaration()),
                _ => panic!("expected variable or function declaration"),
            };
        }
        todo!()
    }

    // <statement> ::= <if_stmt> | <stack_block> | <while_stmt> | <assignment>
    //               | <return_stmt> | <break_stmt> | <continue_stmt> | ";"
    fn statement(&mut self) -> StmtId {
        let token = self.tokens.next().expect("unexpected end of input while parsing statement");
        let stmt = match token.kind {
            TokenKind::If        => self.if_statement(),
            TokenKind::At        => self.stack_block(),
            TokenKind::LeftBrace => self.block_scope(),
            TokenKind::While     => self.while_statement(),
            TokenKind::Return    => {
                self.expect(TokenKind::Semicolon);
                self.arena.push_stmt(Statement::Return, token)
            }
            TokenKind::Break     => self.arena.push_stmt(Statement::Break, token),
            TokenKind::Continue  => self.arena.push_stmt(Statement::Continue, token),
            TokenKind::Semicolon => self.arena.push_stmt(Statement::Empty, token),
            _ => panic!("expected statement"),
        };
        todo!()
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> StmtId {
        let token = self.expect(TokenKind::If);
        let cond = self.stack_block();
        let body = self.block_scope();
        let else_stmt = None;
        self.arena.push_stmt(Statement::If { cond, body, else_stmt}, token)
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> StmtId {
        let token = self.expect(TokenKind::While);
        let cond = self.stack_block();
        let body = self.block_scope();
        self.arena.push_stmt(Statement::While { cond, body }, token)
    }

    // <stack_block> ::= "@" "(" <expression_list> ")"
    fn stack_block(&mut self) -> StmtId {
        let token = self.expect(TokenKind::At);
        self.expect(TokenKind::LeftParen);

        let statements = Vec::<ExprId>::new();

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                kind @ _ if kind.is_stack_operator() => self.operation(),
                _ => panic!("expected variable or function declaration"),
            };
        }
        self.expect(TokenKind::RightBrace);
        self.arena.push_stmt(Statement::StackBlock(statements), token)
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
        self.arena.push_stmt(Statement::BlockScope(top_level_ids), token)
    }
    fn operation(&mut self) -> ExprId{
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
}
