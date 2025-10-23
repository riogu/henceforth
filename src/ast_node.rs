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
    Add      (&'a mut Expression<'a>, &'a mut Expression<'a>),
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
pub trait ASTNode {
    fn get_type(&self) -> Type;
    fn analyze(&self);
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
            Expression::Assignment(_) => todo!(),
            Expression::Identifier(identifier) => todo!(),
        }
    }

    fn analyze(&self) {
        match self {
            Expression::Operation(operation) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::Identifier(identifier) => todo!(),
        }
    }
}

impl<'a> ASTNode for Statement<'a> {
    fn get_type(&self) -> Type {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return(return_stmt) => todo!(),
            Statement::StackBlocks() => todo!(),
        }
    }

    fn analyze(&self) {
        match self {
            Statement::If(if_stmt) => todo!(),
            Statement::Return(return_stmt) => todo!(),
            Statement::StackBlocks() => todo!(),
        }
    }
}

impl<'a> ASTNode for RootNode<'a> {
    fn get_type(&self) -> Type {
        match self {
            RootNode::Declaration(declaration) => todo!(),
            RootNode::Statement(statement) => todo!(),
        }
    }

    fn analyze(&self) {
        match self {
            RootNode::Declaration(declaration) => todo!(),
            RootNode::Statement(statement) => todo!(),
        }
    }
}
