use crate::token::Token;

pub trait ASTNode {
}

struct IfStmt {
}
struct ReturnStmt {
}

impl ASTNode for IfStmt {
}

enum ASTNode {
    IfStmt(IfStmt),
    ReturnStmt(ReturnStmt),
}
