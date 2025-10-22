pub struct Type {}
pub trait ASTNode {
    fn get_type(&self) -> Type;
    fn analyze(&self);
}

pub enum  IfStmt {}
pub enum  ReturnStmt {}

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
    Operation           (Operation<'a>),
    VarIdentifier       (String),
    FunctionIdentifier  (String),
    Assignment          (&'a Expression<'a>),
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


impl<'a> ASTNode for Declaration<'a> {
    fn get_type(&self) -> Type {
        match self {
            Declaration::VarDeclaration(expression, value) => todo!(),
            Declaration::FunctionDeclaration(vec) => todo!(),
        }
    }
    fn analyze(&self) {
        match self {
            Declaration::VarDeclaration(expression, expression1) => todo!(),
            Declaration::FunctionDeclaration(expressions) => todo!(),
        }
    }
}

impl ASTNode for Operation<'_> {
    fn analyze(&self) {
        match self {
            Operation::Add(lhs, rhs) => todo!(),
            Operation::Subtract(lhs, rhs) => todo!(),
            Operation::Multiply(lhs, rhs) => todo!(),
            Operation::Divide(lhs, rhs) => todo!(),
            Operation::Negate(lhs, rhs) => todo!(),
            Operation::Or(lhs, rhs) => todo!(),
            Operation::And(lhs, rhs) => todo!(),
            Operation::Not(expr) => todo!(),
        }
    }
    fn get_type(&self) -> Type {
        match self {
            Operation::Add(lhs, rhs) => todo!(),
            Operation::Subtract(lhs, rhs) => todo!(),
            Operation::Multiply(lhs, rhs) => todo!(),
            Operation::Divide(lhs, rhs) => todo!(),
            Operation::Negate(lhs, rhs) => todo!(),
            Operation::Or(lhs, rhs) => todo!(),
            Operation::And(lhs, rhs) => todo!(),
            Operation::Not(expr) => todo!(),
        }
    }
}
impl<'a> ASTNode for Expression<'a> {
    fn get_type(&self) -> Type {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::VarIdentifier(_) => todo!(),
            Expression::FunctionIdentifier(_) => todo!(),
            Expression::Assignment(_) => todo!(),
        }
    }

    fn analyze(&self) {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::VarIdentifier(_) => todo!(),
            Expression::FunctionIdentifier(_) => todo!(),
            Expression::Assignment(_) => todo!(),
        }
    }
}
