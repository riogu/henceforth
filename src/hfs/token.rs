#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
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

pub const VALID_STACK_KEYWORDS: &[&str] = &[
    "@pop", "@pop_all", "@dup", "@swap", "@over", "@rot", "@rrot", "@nip", "@tuck",
];

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Literal(Literal),
    Identifier(String),
    StackKeyword(String),
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
    // Call
    CopyCall, // &=
    MoveCall, // :=
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
    DotDotDot, // .
    Colon,     // :
    Arrow,     // ->

    // Special
    Newline, // if significant whitespace
}

impl From<BuilderOperation> for TokenKind {
    fn from(value: BuilderOperation) -> Self {
        match value {
            BuilderOperation::Add => TokenKind::Plus,
            BuilderOperation::Subtract => TokenKind::Minus,
            BuilderOperation::Multiply => TokenKind::Star,
            BuilderOperation::Divide => TokenKind::Slash,
            BuilderOperation::Modulo => TokenKind::Percent,
            BuilderOperation::Not => TokenKind::Not,
            BuilderOperation::Or => TokenKind::Or,
            BuilderOperation::And => TokenKind::And,
            BuilderOperation::GreaterThan => TokenKind::Greater,
            BuilderOperation::GreaterThanEq => TokenKind::GreaterEqual,
            BuilderOperation::Equals => TokenKind::Equal,
            BuilderOperation::NotEquals => TokenKind::NotEqual,
            BuilderOperation::LessThan => TokenKind::Less,
            BuilderOperation::LessThanEq => TokenKind::LessEqual,
        }
    }
}

impl From<Type> for TokenKind {
    fn from(value: Type) -> Self {
        match value {
            Type::Int => TokenKind::Int,
            Type::String => TokenKind::String,
            Type::Bool => TokenKind::Bool,
            Type::Float => TokenKind::Float,
            Type::Tuple(_) => TokenKind::LeftParen,
        }
    }
}

use std::fmt;

use crate::{hfs::ast::Type, hfs::builder::BuilderOperation};

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
            TokenKind::DotDotDot => write!(f, "..."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Newline => write!(f, "\\n"),
            TokenKind::CopyAssign => write!(f, ":="),
            TokenKind::MoveAssign => write!(f, "&="),
            TokenKind::At => write!(f, "@"),
            TokenKind::Elif => write!(f, "elif"),
            TokenKind::Int => write!(f, "i32"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Bool => write!(f, "bool"),
            TokenKind::Float => write!(f, "f32"),
            TokenKind::Identifier(_) => todo!(),
            TokenKind::CopyCall => todo!(),
            TokenKind::MoveCall => todo!(),
            TokenKind::StackKeyword(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub source_info: SourceInfo<'a>,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, source_info: SourceInfo<'a>) -> Self {
        Self { kind, source_info }
    }

    pub fn is_type(&self) -> bool {
        match self.kind {
            TokenKind::Int | TokenKind::String | TokenKind::Bool | TokenKind::Float => true,
            _ => false,
        }
    }
    pub fn is_keyword(&self) -> bool {
        match self.kind {
            TokenKind::Let
            | TokenKind::Fn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Elif
            | TokenKind::While
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::Return => true,
            _ => false,
        }
    }

    pub fn is_stack_operator(&self) -> bool {
        match self.kind {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Not => true,
            _ => false,
        }
    }

    pub fn is_binary_operator(&self) -> bool {
        match self.kind {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or => true,
            _ => false,
        }
    }
}
impl TokenKind {
    pub fn to_type(&self) -> Type {
        match self {
            TokenKind::Int => Type::Int,
            TokenKind::String => Type::String,
            TokenKind::Bool => Type::Bool,
            TokenKind::Float => Type::Float,
            TokenKind::Identifier(_) => {
                panic!("[internal hfs error]: this is not how you convert identifiers to types")
            }
            _ => panic!(
                "[internal hfs error]: expected token that has a type, got {:?}",
                self
            ),
        }
    }
    pub fn is_type(&self) -> bool {
        match self {
            TokenKind::Int | TokenKind::String | TokenKind::Bool | TokenKind::Float => true,
            _ => false,
        }
    }
    pub fn is_keyword(&self) -> bool {
        match self {
            TokenKind::Let
            | TokenKind::Fn
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Elif
            | TokenKind::While
            | TokenKind::Break
            | TokenKind::Continue
            | TokenKind::Return => true,
            _ => false,
        }
    }
    pub fn is_stack_operator(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Not => true,
            _ => false,
        }
    }
    pub fn is_binary_operator(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or => true,
            _ => false,
        }
    }
}
