use std::collections::HashMap;
use crate::hfs::ast::*;
use crate::hfs::types::*;
use crate::hfs::token::*;

//---------------------------------------------------------------------------
// Runtime values
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
}

//---------------------------------------------------------------------------
pub struct Interpreter<'a> {
    arena: &'a AstArena<'a>,
    runtime_hfs_stack: Vec<RuntimeValue>,
}

impl<'a> Interpreter<'a> {
    pub fn new(arena: &'a AstArena<'a>) -> Self {
        Self {
            arena,
            runtime_hfs_stack: Vec::new(),
        }
    }

    pub fn interpret(top_level_nodes: &[TopLevelId], arena: &AstArena) {
        let mut interpreter = Interpreter::new(arena);
        
        for node in top_level_nodes {
            interpreter.interpret_top_level(*node);
        }
    }

    fn interpret_top_level(&mut self, node: TopLevelId) {
        match node {
            TopLevelId::VariableDecl(var_id) => self.interpret_var_decl(var_id),
            TopLevelId::FunctionDecl(func_id) => self.interpret_func_decl(func_id),
            TopLevelId::Statement(stmt_id) => self.interpret_stmt(stmt_id),
        }
    }

    fn interpret_var_decl(&mut self, var_id: VarId) {
        todo!()
    }

    fn interpret_func_decl(&mut self, func_id: FuncId) {
        todo!()
    }

    fn interpret_stmt(&mut self, stmt_id: StmtId) {
        match self.arena.get_stmt(stmt_id) {
            Statement::If { cond, body, else_stmt } => todo!(),
            Statement::While { cond, body } => todo!(),
            Statement::StackBlock(expressions) => todo!(),
            Statement::BlockScope(top_level_nodes) => todo!(),
            Statement::Return => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Assignment { value, identifier, is_move } => todo!(),
            Statement::Empty => {}
        }
    }

    fn interpret_expr(&mut self, expr_id: ExprId) {
        match self.arena.get_expr(expr_id) {
            Expression::Operation(op) => self.interpret_operation(op),
            Expression::Identifier(id) => self.interpret_identifier(id),
            Expression::Literal(lit) => self.interpret_literal(lit),
            Expression::FunctionCall { tuple_args, identifier } => todo!(),
            Expression::Tuple { expressions, variadic } => todo!(),
            Expression::Parameter(type_id) => todo!(),
            Expression::ReturnValue(type_id) => todo!(),
        }
    }

    fn interpret_identifier(&mut self, id: &Identifier) {
        match id {
            Identifier::Variable(var_id) => todo!(),
            Identifier::Function(func_id) => todo!(),
        }
    }

    fn interpret_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Integer(i) => todo!(),
            Literal::Float(f) => todo!(),
            Literal::String(s) => todo!(),
            Literal::Bool(b) => todo!(),
        }
    }

    fn interpret_operation(&mut self, op: &Operation) {
        match op {
            Operation::Add(l, r) => todo!(),
            Operation::Sub(l, r) => todo!(),
            Operation::Mul(l, r) => todo!(),
            Operation::Div(l, r) => todo!(),
            Operation::Mod(l, r) => todo!(),
            Operation::Equal(l, r) => todo!(),
            Operation::NotEqual(l, r) => todo!(),
            Operation::Less(l, r) => todo!(),
            Operation::LessEqual(l, r) => todo!(),
            Operation::Greater(l, r) => todo!(),
            Operation::GreaterEqual(l, r) => todo!(),
            Operation::Or(l, r) => todo!(),
            Operation::And(l, r) => todo!(),
            Operation::Not(expr) => todo!(),
        }
    }
}
