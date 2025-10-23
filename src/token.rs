#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    Float(f32),
    String(String),
    Identifier(String),
    Bool(bool),
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

impl TokenKind {
    pub fn str(&self) -> String {
        match self {
            TokenKind::Literal(literal) => format!("{:?}", literal),
            TokenKind::Let => "let".to_string(),
            TokenKind::If => "if".to_string(),
            TokenKind::Else => "else".to_string(),
            TokenKind::While => "while".to_string(),
            TokenKind::Break => "break".to_string(),
            TokenKind::Continue => "continue".to_string(),
            TokenKind::Return => "return".to_string(),
            TokenKind::Fn => "fn".to_string(),
            TokenKind::Plus => "+".to_string(),
            TokenKind::Minus => "-".to_string(),
            TokenKind::Star => "*".to_string(),
            TokenKind::Slash => "/".to_string(),
            TokenKind::Percent => "%".to_string(),
            TokenKind::Equal => "==".to_string(),
            TokenKind::NotEqual => "!=".to_string(),
            TokenKind::Less => "<".to_string(),
            TokenKind::LessEqual => "<=".to_string(),
            TokenKind::Greater => ">".to_string(),
            TokenKind::GreaterEqual => ">=".to_string(),
            TokenKind::And => "&&".to_string(),
            TokenKind::Or => "||".to_string(),
            TokenKind::Not => "!".to_string(),
            TokenKind::LeftParen => "(".to_string(),
            TokenKind::RightParen => ")".to_string(),
            TokenKind::LeftBrace => "{".to_string(),
            TokenKind::RightBrace => "}".to_string(),
            TokenKind::LeftBracket => "[".to_string(),
            TokenKind::RightBracket => "]".to_string(),
            TokenKind::Semicolon => ";".to_string(),
            TokenKind::Comma => ",".to_string(),
            TokenKind::Dot => ".".to_string(),
            TokenKind::Colon => ":".to_string(),
            TokenKind::Arrow => "->".to_string(),
            TokenKind::Newline => "\\n".to_string(),
            TokenKind::Eof => "EOF".to_string(),
            TokenKind::CopyAssign => ":=".to_string(),
            TokenKind::MoveAssign => "&=".to_string(),
            TokenKind::At => "@".to_string(),
            TokenKind::Elif => "elif".to_string(),
            TokenKind::Int => "i32".to_string(),
            TokenKind::String => "string".to_string(),
            TokenKind::Bool => "bool".to_string(),
            TokenKind::Float => "f32".to_string(),
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
