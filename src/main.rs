pub mod tokens;
pub mod lexer;

use lexer::Lexer;
fn main() {

    let lexer = Lexer::new();
    let tokens = lexer.tokenize();
}
