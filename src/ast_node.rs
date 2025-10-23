#[derive(Debug)]
pub struct BlockScope<'a> {
    nodes: Vec<TopLevelNode<'a>>,
}

#[derive(Debug)]
pub enum ElseStmt<'a> {
    ElseIf(&'a mut IfStmt<'a>),
    Else(Expression<'a>),
}
#[derive(Debug)]
pub struct IfStmt<'a> {
    cond: &'a Expression<'a>,
    body: &'a BlockScope<'a>,
    else_stmt: ElseStmt<'a>,
}
#[derive(Debug)]
pub struct WhileStmt<'a> {
    cond: Expression<'a>,
    body: Vec<&'a Expression<'a>>,
}

#[derive(Debug)]
pub enum Identifier {
    VarIdentifier(String),
    FunctionIdentifier(String),
}

#[derive(Debug)]
pub enum Operation<'a> {
    Add(&'a mut Expression<'a>, &'a mut Expression<'a>),
    Subtract(&'a mut Expression<'a>, &'a mut Expression<'a>),
    Multiply(&'a mut Expression<'a>, &'a mut Expression<'a>),
    Divide(&'a mut Expression<'a>, &'a mut Expression<'a>),
    Negate(&'a mut Expression<'a>, &'a mut Expression<'a>),
    Or(&'a mut Expression<'a>, &'a mut Expression<'a>),
    And(&'a mut Expression<'a>, &'a mut Expression<'a>),
    Not(&'a mut Expression<'a>),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Operation(Operation<'a>),
    Assignment(&'a mut Expression<'a>),
    Identifier(&'a mut Identifier),
}

#[derive(Debug)]
pub enum Statement<'a> {
    If(&'a mut IfStmt<'a>),
    StackBlock(Vec<&'a Expression<'a>>),
    While(&'a Expression<'a>, Vec<&'a Expression<'a>>),
    Return,
    Break,
    Continue,
}

#[derive(Debug)]
pub enum TopLevelNode<'a> {
    VarDeclaration(&'a mut Identifier),
    FunctionDeclaration(&'a mut Vec<&'a Expression<'a>>),
    Statement(&'a mut Statement<'a>),
}

pub enum Type {
    Int,
    String,
    Bool,
    Float,
    // Struct(Identifier),
}

pub trait Typed {
    fn get_type(&self) -> Type;
}

pub trait Analyze {
    fn analyze(&self);
}
