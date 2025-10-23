
#[derive(Debug)]
pub enum  IfStmt {}
#[derive(Debug)]
pub enum  ReturnStmt {}

#[derive(Debug)]
pub enum Identifier {
    VarIdentifier       (String),
    FunctionIdentifier  (String),
}

#[derive(Debug)]
pub enum  Declaration<'a> {
    VarDeclaration      (&'a mut Expression<'a>, &'a mut Expression<'a>),
    FunctionDeclaration (&'a mut Vec<Expression<'a>>),
}
#[derive(Debug)]
pub enum Operation<'a> {
    Add      (&'a mut Expression<'a>, &'a mut Expression<'a>),
    Subtract (&'a mut Expression<'a>, &'a mut Expression<'a>),
    Multiply (&'a mut Expression<'a>, &'a mut Expression<'a>),
    Divide   (&'a mut Expression<'a>, &'a mut Expression<'a>),
    Negate   (&'a mut Expression<'a>, &'a mut Expression<'a>),
    Or       (&'a mut Expression<'a>, &'a mut Expression<'a>),
    And      (&'a mut Expression<'a>, &'a mut Expression<'a>),
    Not      (&'a mut Expression<'a>),
}
#[derive(Debug)]
pub enum Expression<'a> {
    Operation  (Operation<'a>),
    Assignment (&'a mut Expression<'a>),
    Identifier (&'a mut Identifier),
}
#[derive(Debug)]
pub enum Statement<'a> {
    If          (&'a mut IfStmt),
    Return      (&'a mut ReturnStmt),
    StackBlocks ()
}

#[derive(Debug)]
pub enum TopLevelNode<'a> {
    Declaration (&'a mut Declaration<'a>),
    Statement   (&'a mut Statement<'a>),
}

pub struct Type {}

pub trait Typed {
    fn get_type(&self) -> Type;

}
pub trait Analyze {
    fn analyze(&self);
}




