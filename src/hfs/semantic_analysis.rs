use std::collections::HashMap;
use crate::hfs::ast::*;
use crate::hfs::types::*;
// TODO: use these in semantic analysis for type solving.
// its perfectly fine to type check and solve symbols in the same pass
// the language is simple enough for this to be perfectly viable

//---------------------------------------------------------------------------
// Type stack methods (manage the type stack for type checking in semantic analysis)
impl<'a> AstArena<'a> {

    pub fn push_hfs_type(&mut self, hfs_type: Type) {
        self.hfs_type_stack.push(hfs_type);
    }
    pub fn pop_hfs_type_stack(&mut self) -> Vec<Type> {
        let temp = self.hfs_type_stack.clone();
        self.hfs_type_stack.clear();
        temp
    }
    pub fn pop_type_or_error(&mut self, msg: &str) -> Type { 
        self.hfs_type_stack.pop().unwrap_or_else(|| panic!("{}", msg))
    }
    
    pub fn last_type_or_error(&mut self, msg: &str) -> Type { 
        self.hfs_type_stack.last().cloned().unwrap_or_else(|| panic!("{}", msg))
    }
    
    pub fn pop2_types_or_error(&mut self, msg: &str) -> (Type, Type) { 
        (
            self.hfs_type_stack.pop().unwrap_or_else(|| panic!("{}", msg)),
            self.hfs_type_stack.pop().unwrap_or_else(|| panic!("{}", msg)),
        )
    }
    
    pub fn peek_type(&self) -> Option<&Type> {
        self.hfs_type_stack.last()
    }
}
//---------------------------------------------------------------------------
pub struct TypeAnalyzer<'a> {
    arena: AstArena<'a>,
}
impl<'a> TypeAnalyzer<'a> {
    pub fn new(mut arena: AstArena<'a>) -> TypeAnalyzer<'a> {
        arena.pop_hfs_stack();
        TypeAnalyzer {
            arena,
        }
    }
}
// separate analysis like solving identifiers and type checking
impl<'a> TypeAnalyzer<'a> {
    fn analyze_identifier(&mut self, id: &Identifier) {
        // resolve identifiers, also push them to the stack
        match id {
            Identifier::Unresolved(name) => todo!(),
            Identifier::Variable(_) => {}
            Identifier::Function(_) => {}
        }
    }
    // validate the types against the current stack state
    // basically, it will always compare the top of the stack with our type
    fn analyze_type(&mut self, hfs_type: Type) {
        let last_expr_id = self.arena.last_or_error("tried consuming stack but stack was empty.");
        let last_expr_type = self.arena.get_type_of_expr(last_expr_id);
        if hfs_type != last_expr_type {
            panic!("expected '{:?}' type on top of stack, found '{:?}' instead.", hfs_type, last_expr_type)
        }
    }

}
// main recursive analysis loop
// our goal is just to make sure types match all across the code, and solve identifiers
// which means we manage a stack of types across analysis, since we dont actually care about the
// values at all, we just care about the types themselves matching
impl<'a> TypeAnalyzer<'a> {
    pub fn analyze(top_level_nodes: &[TopLevelId], arena: AstArena<'a>) {
        let mut analyzer = TypeAnalyzer::new(arena);
        for node in top_level_nodes {
            analyzer.analyze_top_level(*node);
        }
    }

    fn analyze_top_level(&mut self, node: TopLevelId) {
        match node {
            TopLevelId::VariableDecl(var_id) => self.analyze_var_decl(var_id),
            TopLevelId::FunctionDecl(func_id) => self.analyze_func_decl(func_id),
            TopLevelId::Statement(stmt_id) => self.analyze_stmt(stmt_id),
        }
    }
    fn analyze_var_decl(&mut self, var_id: VarId) {
        // just add it lol
        let var = self.arena.get_var(var_id);
    }
    fn analyze_func_decl(&mut self, func_id: FuncId) {
        // we dont need to analyze anything in the types since there is no user types
        // so we dont have to solve any symbols at all here (need this later if we add user types)
        let func = self.arena.get_func(func_id);
        self.analyze_stmt(func.body); // will push a bunch of stuff to the stack
    }

    fn analyze_stmt(&mut self, stmt_id: StmtId) {
        match self.arena.get_stmt(stmt_id) {
            Statement::If { cond, body, else_stmt } => todo!(),
            Statement::While { cond, body } => {
                todo!();
                self.analyze_expr(*cond);
                self.analyze_stmt(*body);
            }
            Statement::StackBlock(expressions) => {
                for expr_id in expressions.clone() {
                    self.analyze_expr(expr_id);
                }
                todo!()
            }
            Statement::BlockScope(_) => {
                todo!();
            }
            Statement::Return => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Assignment { value, identifier, is_move } => todo!(),
            Statement::Empty => {}
        }
    }

    fn analyze_expr(&mut self, expr_id: ExprId) {
        let expr = self.arena.get_expr(expr_id);
        match expr {
            Expression::Operation(_) => todo!(),
            Expression::Identifier(_) => todo!(),
            Expression::Literal(_) => {}
            Expression::FunctionCall { tuple, identifier } => {
                let parent_hfs_stack = self.arena.pop_hfs_stack(); // stack resets after function call
                todo!()
            }
            Expression::Tuple { expressions, variadic } => todo!(),
            Expression::Parameter(_) => todo!(),
        }
    }

    fn analyze_operation(&mut self, op: &Operation) {
        match op {
            Operation::Add(l, r)
            | Operation::Sub(l, r)
            | Operation::Mul(l, r)
            | Operation::Div(l, r)
            | Operation::Mod(l, r)
            | Operation::Equal(l, r)
            | Operation::NotEqual(l, r)
            | Operation::Less(l, r)
            | Operation::LessEqual(l, r)
            | Operation::Greater(l, r)
            | Operation::GreaterEqual(l, r)
            | Operation::Or(l, r)
            | Operation::And(l, r) => todo!(),
            Operation::Not(_) => todo!(),
        }
    }
}

