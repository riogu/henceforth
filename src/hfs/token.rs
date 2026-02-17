#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
}
impl Literal {
    fn get_width(&self) -> usize {
        match self {
            Literal::Integer(int) => int.to_string().len(),
            Literal::Float(float) => float.to_string().len(),
            Literal::String(string) => string.len() + 2,
            Literal::Bool(bool) => bool.to_string().len(),
        }
    }
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

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(val) => write!(f, "{}", val),
            Literal::Float(val) => write!(f, "{}", val),
            Literal::String(val) => write!(f, "{}", val),
            Literal::Bool(val) => write!(f, "{}", val),
        }
    }
}

pub const VALID_STACK_KEYWORDS: &[&str] = &["@pop", "@pop_all", "@dup", "@swap", "@over", "@rot", "@rrot", "@nip", "@tuck"];

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
    And,         // && or 'and'
    Or,          // || or 'or'
    Not,         // ! or 'not'
    AddressOf,   // &
    Dereference, // ^
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
    At,           // @

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
            Type::Int { .. } => TokenKind::Int,
            Type::String { .. } => TokenKind::String,
            Type::Bool { .. } => TokenKind::Bool,
            Type::Float { .. } => TokenKind::Float,
            Type::Tuple { .. } => TokenKind::LeftParen,
        }
    }
}

use std::fmt;

use crate::hfs::{ast::Type, builder::builder::BuilderOperation};

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Literal(literal) => write!(f, "{}", literal),
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
            TokenKind::Identifier(name) => write!(f, "{}", name),
            TokenKind::CopyCall => write!(f, ":>"),
            TokenKind::MoveCall => write!(f, "&>"),
            TokenKind::StackKeyword(name) => write!(f, "{}", name),
            TokenKind::AddressOf => todo!(),
            TokenKind::Dereference => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Default, Ord, PartialOrd)]
pub struct SourceInfo {
    pub line_number: usize,
    pub line_offset: usize,
    pub token_width: usize,
    // line_string: &'a str,
}

impl SourceInfo {
    pub fn new(line_number: usize, line_offset: usize, token_width: usize) -> Self {
        Self { line_number, line_offset, token_width }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub source_info: SourceInfo,
}

impl Token {
    pub fn new(kind: TokenKind, source_info: SourceInfo) -> Self {
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
            TokenKind::Int => Type::new_int(0),
            TokenKind::String => Type::new_string(0),
            TokenKind::Bool => Type::new_bool(0),
            TokenKind::Float => Type::new_float(0),
            TokenKind::Identifier(_) => {
                panic!("[internal hfs error]: this is not how you convert identifiers to types")
            },
            _ => panic!("[internal hfs error]: expected token that has a type, got {:?}", self),
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

    pub fn get_width(&self) -> usize {
        match self {
            TokenKind::Literal(literal) => literal.get_width(),
            TokenKind::Identifier(str) => str.len(),
            TokenKind::StackKeyword(str) => str.len() + 1,
            TokenKind::Float | TokenKind::Int | TokenKind::DotDotDot | TokenKind::Let => 3,
            TokenKind::LessEqual
            | TokenKind::GreaterEqual
            | TokenKind::Arrow
            | TokenKind::CopyAssign
            | TokenKind::MoveAssign
            | TokenKind::CopyCall
            | TokenKind::MoveCall
            | TokenKind::NotEqual
            | TokenKind::Fn
            | TokenKind::If
            | TokenKind::Or
            | TokenKind::Equal
            | TokenKind::And => 2,
            TokenKind::Bool | TokenKind::Else | TokenKind::Elif => 4,
            TokenKind::While | TokenKind::Break => 5,
            TokenKind::Continue => 8,
            TokenKind::Return | TokenKind::String => 6,
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Greater
            | TokenKind::Less
            | TokenKind::LeftParen
            | TokenKind::RightParen
            | TokenKind::LeftBrace
            | TokenKind::RightBrace
            | TokenKind::LeftBracket
            | TokenKind::RightBracket
            | TokenKind::Colon
            | TokenKind::Not
            | TokenKind::At
            | TokenKind::Semicolon
            | TokenKind::AddressOf
            | TokenKind::Dereference
            | TokenKind::Comma => 1,
            TokenKind::Newline => 0,
        }
    }
}

pub fn get_eof_source_info(tokens: &Vec<Token>) -> SourceInfo {
    if tokens.len() > 0 {
        let last = tokens.last().expect("[internal error] no tokens after len > 0 check");
        SourceInfo::new(last.source_info.line_number, last.source_info.line_offset + last.source_info.token_width, 1)
    } else {
        SourceInfo::new(1, 1, 1)
    }
}
