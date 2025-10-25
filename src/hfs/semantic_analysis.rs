use crate::hfs::ast_node::*;

#[derive(Debug, Clone, Copy)]

pub enum ScopeKind {
    Function, // func_name()::
    Block,    // 0::, 1::, etc
    Global,
}

struct Scope {
    name: String,
    kind: ScopeKind,
    inner_count: usize, // for blocks within this frame
}

pub struct ScopeStack { stack: Vec<Scope> }

impl ScopeStack {
    pub fn new(file_name: String) -> Self {
        Self { stack: vec![Scope { name: file_name + "::", kind: ScopeKind::Global, inner_count: 0, }], }
    }
    pub fn push_function(&mut self, name: String) {
        if let Some(parent) = self.stack.last_mut() {
            let func_name = format!("{}{}::", parent.name, name);
            self.stack.push(Scope { name: format!("{}()::", func_name), kind: ScopeKind::Function, inner_count: 0, });
        }
    }
    pub fn push_block(&mut self) {
        if let Some(parent) = self.stack.last_mut() {
            let block_name = format!("{}{}::", parent.name, parent.inner_count);
            parent.inner_count += 1;
            self.stack.push(Scope { name: block_name, kind: ScopeKind::Block, inner_count: 0, });
        }
    }
    pub fn pop(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        }
    }
}
pub struct Analyzer<'a> {
    scope_stack: ScopeStack,
    arena: AstArena<'a>,
}
impl<'a> Analyzer<'a> {
    pub fn new(file_name: String, arena: AstArena<'a>) -> Analyzer<'a> {
        Analyzer { scope_stack: ScopeStack::new(file_name), arena}
    }
    pub fn analyze(top_level_nodes: &[TopLevelId], file_name: String, arena: AstArena<'a>) {
        let mut analyzer = Analyzer::new(file_name, arena);
        for node in top_level_nodes {
            analyzer.analyze_top_level(*node);
        }
    }
}

impl<'a> Analyzer<'a> {

    fn analyze_top_level(&mut self, node: TopLevelId) {
        match node {
            TopLevelId::VariableDecl(var_id) => self.analyze_var_decl(var_id),
            TopLevelId::FunctionDecl(func_id) => self.analyze_func_decl(func_id),
            TopLevelId::Statement(stmt_id) => self.analyze_stmt(stmt_id),
        }
    }
    fn analyze_identifier(&mut self, id: &Identifier) {
        match id {
            Identifier::Unresolved(name) => todo!(),
            Identifier::Variable(_) => {}
            Identifier::Function(_) => {}
        }
    }

    fn analyze_func_decl(&mut self, func_id: FuncId) {
        let func = self.arena.get_func(func_id);
        self.scope_stack.push_function(func.name.clone());
        // TODO: validate param/return types and analyze body
        self.scope_stack.pop();
    }

    fn analyze_var_decl(&mut self, var_id: VarId) {
        let var = self.arena.get_var(var_id);
        // TODO: validate type
    }

    fn analyze_stmt(&mut self, stmt_id: StmtId) {
        let stmt = self.arena.get_stmt(stmt_id);
        match stmt {
            Statement::If { cond, body, else_stmt } => todo!(),
            Statement::While { cond, body } => {
                todo!();
                self.analyze_stmt(*cond);
                self.analyze_stmt(*body);
            }
            Statement::StackBlock(_) => todo!(),
            Statement::BlockScope(_) => {
                self.scope_stack.push_block();
                todo!();
                self.scope_stack.pop();
            }
            Statement::Return => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Assignment { value, identifier } => todo!(),
            Statement::Empty => {}
        }
    }

    fn analyze_expr(&mut self, expr_id: ExprId) {
        let expr = self.arena.get_expr(expr_id);
        match expr {
            Expression::Operation(_) => todo!(),
            Expression::Identifier(_) => todo!(),
            Expression::Literal(_) => {}
            Expression::FunctionCall { tuple, identifier } => todo!(),
            Expression::Tuple { expressions, variadic } => todo!(),
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
