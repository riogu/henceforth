use std::collections::HashMap;
use crate::hfs::ast_node::*;
use crate::hfs::types::*;

#[derive(Debug, Clone, Copy)]

pub enum ScopeKind {
    Global,
    Function, // func_name()::
    Block,    // 0::, 1::, etc
}

struct Scope {
    name: String,
    kind: ScopeKind,
    inner_count: usize, // for blocks within this frame
}

pub struct ScopeStack {
    stack: Vec<Scope>,
    mangled_global_vars: HashMap<String, VarId>,
    mangled_locals: HashMap<String, VarId>,
    mangled_functions: HashMap<String, FuncId>,
}

impl ScopeStack {
    pub fn new(file_name: String) -> Self {
        Self {
            stack: vec![Scope { name: file_name + "%", kind: ScopeKind::Global, inner_count: 0, }],
            mangled_global_vars: HashMap::new(),
            mangled_locals: HashMap::new(),
            mangled_functions: HashMap::new(),
        }
    }
    pub fn push_function_and_scope(&mut self, name: &str, func_id: FuncId) {
        let parent = self.stack.last_mut().expect("[internal hfs error] couldn't push function, scopes were set up wrong.");
        let mangled_name = format!("{}{}::", parent.name, name);
        self.stack.push(Scope { name: format!("{}()::", mangled_name), kind: ScopeKind::Function, inner_count: 0, });
        self.mangled_functions.insert(mangled_name, func_id);
    }
    pub fn push_block(&mut self) {
        let parent = self.stack.last_mut().expect("[internal hfs error] couldn't push block, scopes were set up wrong.");
        let block_name = format!("{}{}::", parent.name, parent.inner_count);
        parent.inner_count += 1;
        self.stack.push(Scope { name: block_name, kind: ScopeKind::Block, inner_count: 0 });
    }
    pub fn push_variable(&mut self, name: &str, var_id: VarId) {
        let curr_stack = self.stack.last().expect("[internal hfs error] scopes were set up wrong.");
        let mangled_name = format!("{}{}", curr_stack.name, name);
        match curr_stack.kind {
            ScopeKind::Global => self.mangled_global_vars.insert(mangled_name, var_id),
            ScopeKind::Function => self.mangled_locals.insert(mangled_name, var_id),
            ScopeKind::Block => self.mangled_locals.insert(mangled_name, var_id),
        };
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
    pub fn new(file_name: String, mut arena: AstArena<'a>) -> Analyzer<'a> {
        arena.pop_hfs_stack();
        Analyzer {
            scope_stack: ScopeStack::new(file_name),
            arena,
        }
    }
}

// separate analysis like solving identifiers and type checking
impl<'a> Analyzer<'a> {
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
impl<'a> Analyzer<'a> {
    pub fn analyze(top_level_nodes: &[TopLevelId], file_name: String, arena: AstArena<'a>) {
        let mut analyzer = Analyzer::new(file_name, arena);
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
    fn analyze_func_decl(&mut self, func_id: FuncId) {
        let (name, body, param_type, return_type) = {
            let func = self.arena.get_func(func_id);
            (&func.name, func.body, func.param_type.clone(), func.return_type.clone())
        }; // gotta drop the borrow else rust gets angry
        self.scope_stack.push_function_and_scope(&name, func_id); 
        self.analyze_type(param_type);
        self.analyze_stmt(body); // will push a bunch of stuff to the stack
        self.analyze_type(return_type); // will now compare against that stack

        self.scope_stack.pop();
    }

    fn analyze_var_decl(&mut self, var_id: VarId) {
        // just add it lol
        let var = self.arena.get_var(var_id);
        self.scope_stack.push_variable(&var.name, var_id);
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
            Expression::FunctionCall { tuple, identifier } => {
                let parent_hfs_stack = self.arena.pop_hfs_stack(); // stack resets after function call
                todo!()
            }
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

