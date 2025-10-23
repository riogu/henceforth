pub enum  IfStmt {}
pub enum  ReturnStmt {}

pub enum Identifier {
    VarIdentifier       (String),
    FunctionIdentifier  (String),
}

pub enum  Declaration<'a> {
    VarDeclaration      (&'a Expression<'a>, &'a Expression<'a>),
    FunctionDeclaration (&'a Vec<Expression<'a>>),
}
pub enum Operation<'a> {
    Add      (&'a Expression<'a>, &'a Expression<'a>),
    Subtract (&'a Expression<'a>, &'a Expression<'a>),
    Multiply (&'a Expression<'a>, &'a Expression<'a>),
    Divide   (&'a Expression<'a>, &'a Expression<'a>),
    Negate   (&'a Expression<'a>, &'a Expression<'a>),
    Or       (&'a Expression<'a>, &'a Expression<'a>),
    And      (&'a Expression<'a>, &'a Expression<'a>),
    Not      (&'a Expression<'a>),
}
pub enum Expression<'a> {
    Operation  (Operation<'a>),
    Assignment (&'a Expression<'a>),
    Identifier (&'a Identifier),
}
pub enum Statement<'a> {
    If          (&'a IfStmt),
    Return      (&'a ReturnStmt),
    StackBlocks ()
}

pub enum RootNode<'a> {
    Declaration (&'a Declaration<'a>),
    Statement   (&'a Statement<'a>),
}

pub struct Type {}
pub trait Typed {
    fn get_type(&self) -> Type;

}
pub trait ASTNode {
    fn analyze(&self);
}


