use crate::ast_node::*;
use crate::token::*;

use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser<'a> {
    nodes: Vec<TopLevelNode>,
    tokens: Peekable<IntoIter<Token<'a>>>, // Own the tokens, iterate by value
}

impl<'a> Parser<'a> {
    // utils
    fn push(&mut self, node: TopLevelNode) {
        self.nodes.push(node);
    }
    fn expect(&mut self, token_kind: TokenKind) -> Token<'a> {
        match self.tokens.next() {
            Some(token) if std::mem::discriminant(&token.kind) 
                        == std::mem::discriminant(&token_kind) => token,
            Some(_) | None => panic!("expected '{:?}'", token_kind),
        }
    }
}

impl<'a> Parser<'a> {
    // declarations
    fn function_declaration(&mut self) -> TopLevelNode {
        todo!()
    }
    fn variable_declaration(&mut self) -> TopLevelNode {
        let id_token = self.expect(TokenKind::Identifier(String::new()));
        self.expect(TokenKind::Colon);
        let type_token = self.expect(TokenKind::Identifier(String::new()));
        self.expect(TokenKind::Comma);
        todo!()
    }
}

impl<'a> Parser<'a> {
    // RD Parser for Henceforth (check 'henceforth-bnf.md')
    #[must_use]
    pub fn parse_tokens(tokens: Vec<Token>) -> Vec<TopLevelNode> {
        let mut parser = Parser {
            nodes: Vec::new(),
            tokens: tokens.into_iter().peekable(),
        };
        while let Some(token) = parser.tokens.peek() {
            match &token.kind {
                TokenKind::Let => parser.variable_declaration(),
                TokenKind::Fn => parser.function_declaration(),
                _ => panic!("expected variable or function declaration"),
            };
        }
        return parser.nodes;
    }

    // <statement> ::= <if_stmt> | <stack_block> | <while_stmt> | <assignment>
    //               | <return_stmt> | <break_stmt> | <continue_stmt> | ";"
    fn statement(&mut self) -> TopLevelNode {
        match self.tokens.next().expect("unexpected end of input while parsing statement").kind {
            TokenKind::If => self.if_statement(),
            TokenKind::At => self.stack_block(),
            TokenKind::LeftBrace => self.block_scope(),
            TokenKind::While => self.while_statement(),
            TokenKind::Return => { self.expect(TokenKind::Semicolon); TopLevelNode::Statement(Statement::Return) }
            TokenKind::Break => TopLevelNode::Statement(Statement::Return),
            TokenKind::Continue => TopLevelNode::Statement(Statement::Continue),
            TokenKind::Semicolon => TopLevelNode::Statement(Statement::Empty),
            _ => panic!("expected statement"),
        }
    }

    // <if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
    fn if_statement(&mut self) -> TopLevelNode {
        self.expect(TokenKind::If);
        self.expect(TokenKind::At);
        self.expect(TokenKind::LeftParen);
        let stack_block = self.stack_block();

        self.expect(TokenKind::LeftBrace);
        self.block_scope();

        todo!()
        // Statement::If(IfStmt{})
    }

    // <while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"
    fn while_statement(&mut self) -> TopLevelNode {
        self.expect(TokenKind::While);
        self.stack_block();

        let statements: Vec<TopLevelNode>;
        self.block_scope();
        todo!()
    }

    // <stack_block> ::= "@" "(" <expression_list> ")"
    fn stack_block(&mut self) -> TopLevelNode {
        self.expect(TokenKind::At);
        let statements: Vec<TopLevelNode>;

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Let => self.variable_declaration(),
                TokenKind::Fn => self.function_declaration(),
                kind @ _ if kind.is_stack_operator() => todo!(),
                _ => panic!("expected variable or function declaration"),
            };
        }
        self.expect(TokenKind::RightBrace);
        todo!();
    }
    // <scope_block> ::= (<var_decl> | <statement>)*
    fn block_scope(&mut self) -> TopLevelNode {
        self.expect(TokenKind::LeftBrace);
        let expressions: Vec<Expression>;

        while let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Let => self.variable_declaration(),
                TokenKind::Fn => self.function_declaration(),
                TokenKind::At => self.stack_block(),
                _ => panic!("expected variable or function declaration"),
            };
        }
        self.expect(TokenKind::RightParen);
        todo!()
    }
    fn operation(&mut self) {
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
