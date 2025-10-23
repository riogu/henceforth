use crate::ast_node::*;

impl<'a> Typed for Declaration<'a> {
    fn get_type(&self) -> Type {
        match self {
            Declaration::VarDeclaration(expression, value) => todo!(),
            Declaration::FunctionDeclaration(vec) => todo!(),
        }
    }
}

