#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
    Identifier(String),
    Bool(bool),
}

impl From<i32> for Literal {
    fn from(value: i32) -> Self {
        Literal::Integer(value)
    }
}
impl From<f32> for Literal {
    fn from(value: f32) -> Self {
        Literal::Float(value)
    }
}
impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Literal::String(value.to_string())
    }
}
impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Literal::Bool(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Literal(Literal),
    Let,
    Fn,
    If,
    Else,
    Elif,
    While,
    Break,
    Continue,
    Return,

    // Types
    Int,
    String,
    Bool,
    Float,

    // Operators - Arithmetic
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %

    // Operators - Comparison
    Equal,        // ==
    NotEqual,     // !=
    Less,         //
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=

    // Operators - Logical
    And, // && or 'and'
    Or,  // || or 'or'
    Not, // ! or 'not'

    // Assignment
    CopyAssign, // &=
    MoveAssign, // :=
    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    At,

    // Punctuation
    Semicolon, // ;
    Comma,     // ,
    Dot,       // .
    Colon,     // :
    Arrow,     // ->

    // Special
    Newline, // if significant whitespace
    Eof,
}

use std::fmt;

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Literal(literal) => write!(f, "{:?}", literal),
            TokenKind::Let => write!(f, "let"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::And => write!(f, "&&"),
            TokenKind::Or => write!(f, "||"),
            TokenKind::Not => write!(f, "!"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Newline => write!(f, "\\n"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::CopyAssign => write!(f, ":="),
            TokenKind::MoveAssign => write!(f, "&="),
            TokenKind::At => write!(f, "@"),
            TokenKind::Elif => write!(f, "elif"),
            TokenKind::Int => write!(f, "i32"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Bool => write!(f, "bool"),
            TokenKind::Float => write!(f, "f32"),
        }
    }
}

pub struct SourceInfo<'a> {
    line_number: usize,
    line_offset: usize,
    token_width: usize,
    line_string: &'a str,
}

impl<'a> SourceInfo<'a> {
    pub fn new(
        line_number: usize,
        line_offset: usize,
        token_width: usize,
        line_string: &'a str,
    ) -> Self {
        Self {
            line_number,
            line_offset,
            token_width,
            line_string,
        }
    }
}

pub struct Token<'a> {
    pub kind: TokenKind,
    pub source_info: SourceInfo<'a>,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, source_info: SourceInfo<'a>) -> Self {
        Self { kind, source_info }
    }
}
