#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Literal(Literal),
    Let,
    Mut,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    Function,
    Fn,
    
    // Operators - Arithmetic
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    
    // Operators - Comparison
    Equal,          // ==
    NotEqual,       // !=
    Less,           // 
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    
    // Operators - Logical
    And,            // && or 'and'
    Or,             // || or 'or'
    Not,            // ! or 'not'
    
    // Assignment
    Assign,         // =
    PlusAssign,     // +=
    MinusAssign,    // -=
    StarAssign,     // *=
    SlashAssign,    // /=
    
    // Delimiters
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]
    
    // Punctuation
    Semicolon,      // ;
    Comma,          // ,
    Dot,            // .
    Colon,          // :
    Arrow,          // ->
    
    // Special
    Newline,        // if significant whitespace
    Eof,
}



