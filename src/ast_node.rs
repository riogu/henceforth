pub trait Node {
    fn get_type(&self) -> TypeName;
}
struct IfStmt {}
struct ReturnStmt {}

enum TypeName {
    String,
    Int,
    Bool,
}

enum Declaration<'a> {
    VarDeclaration(&'a Expression<'a>, &'a Expression<'a>),
    FunctionDeclaration(&'a Expression<'a>, &'a Expression<'a>),
}

enum Operation<'a> {
    Add(&'a Expression<'a>, &'a Expression<'a>),
    Subtract(&'a Expression<'a>, &'a Expression<'a>),
    Multiply(&'a Expression<'a>, &'a Expression<'a>),
    Divide(&'a Expression<'a>, &'a Expression<'a>),
    Negate(&'a Expression<'a>, &'a Expression<'a>),
    Or(&'a Expression<'a>, &'a Expression<'a>),
    And(&'a Expression<'a>, &'a Expression<'a>),
    Not(&'a Expression<'a>),
}

enum Expression<'a> {
    Operation(Operation<'a>),
    VarIdentifier(String),
    FunctionIdentifier(String),
}

enum Statement<'a> {
    IfStmt(&'a IfStmt),
    ReturnStmt(&'a ReturnStmt),
}

enum RootNode<'a> {
    Declaration(&'a Declaration<'a>),
    Expression(&'a Expression<'a>),
    Statement(&'a Statement<'a>),
}

impl<'a> Node for Declaration<'a> {
    fn get_type(&self) -> TypeName {
        match self {
            Declaration::VarDeclaration(expression, value) => todo!(),
            Declaration::FunctionDeclaration(expression, expression1) => todo!(),
        }
    }
}

impl<'a> Node for RootNode<'a> {
    fn get_type(&self) -> TypeName {
        match self {
            RootNode::Declaration(declaration) => declaration.get_type(),
            RootNode::Expression(expression) => todo!(),
            RootNode::Statement(statement) => todo!(),
        }
    }
}

// impl Eval for Operation {
//     fn eval(&self) {
//         match self {
//             Operation::Add(expression, expression1) => todo!(),
//             Operation::Subtract(expression, expression1) => todo!(),
//         }
//     }
// }

// fn analyze(ast: Vec<TopLevelNode>) {
//     for node in ast {
//         match node {
//             TopLevelNode::Declaration(declaration) => {
//                 declaration.check_type().check_isadopkom().jiodas
//             }
//             TopLevelNode::Expression(expression) => todo!(),
//         }
//     }
// }
