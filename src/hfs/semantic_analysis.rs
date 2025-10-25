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
        Self { stack: vec![Scope { name: file_name, kind: ScopeKind::Global, inner_count: 0, }], }
    }
    pub fn push_function(&mut self, name: String) {
        self.stack.push(Scope { name: format!("{}()::", name), kind: ScopeKind::Function, inner_count: 0, });
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
struct Analyzer {
    top_level_nodes: Vec<TopLevelId>,
    scope_stack: ScopeStack,
}
impl Analyzer {
    pub fn new(top_level_nodes: Vec<TopLevelId>, file_name: String) -> Analyzer {
        Analyzer { top_level_nodes, scope_stack: ScopeStack::new(file_name) }
    }
}


impl Analyze for Identifier {
    fn analyze(&self) {
        match self {
            Identifier::Unresolved(s) => todo!("Resolve: {}", s),
            Identifier::Variable(_) => {},
            Identifier::Function(_) => {},
        }
    }
}

impl Analyze for Operation {
    fn analyze(&self) {
        match self {
            Operation::Add(l, r) | Operation::Sub(l, r) | Operation::Mul(l, r) |
            Operation::Div(l, r) | Operation::Mod(l, r) | Operation::Equal(l, r) |
            Operation::NotEqual(l, r) | Operation::Less(l, r) | Operation::LessEqual(l, r) |
            Operation::Greater(l, r) | Operation::GreaterEqual(l, r) | Operation::Or(l, r) |
            Operation::And(l, r) => {
                // TODO: analyze l and r
            }
            Operation::Negate(e) | Operation::Not(e) => {
                // TODO: analyze e
            }
        }
    }
}

impl Analyze for Expression {
    fn analyze(&self) {
        match self {
            Expression::Operation(op) => op.analyze(),
            Expression::Identifier(id) => id.analyze(),
            Expression::Literal(_) => {},
            Expression::FunctionCall { tuple, identifier } => {
                identifier.analyze();
                // TODO: analyze tuple
            }
            Expression::Tuple { expressions, variadic } => {
                // TODO: analyze exprs
            }
        }
    }
}

impl Analyze for VarDeclaration {
    fn analyze(&self) {
        // TODO: validate type
    }
}

impl Analyze for FunctionDeclaration {
    fn analyze(&self) {
        // TODO: validate param/return types and analyze body
    }
}

impl Analyze for Statement {
    fn analyze(&self) {
        match self {
            Statement::If { cond, body, else_stmt } => {
                // TODO: analyze cond, body, else_stmt
            }
            Statement::While { cond, body } => {
                // TODO: analyze cond, body
            }
            Statement::StackBlock(exprs) => {
                // TODO: analyze exprs
            }
            Statement::BlockScope(items) => {
                // TODO: analyze items
            }
            Statement::Return | Statement::Break | Statement::Continue | Statement::Empty => {},
            Statement::Assignment { value, identifier } => {
                // TODO: analyze value, identifier
            }
        }
    }
}

impl Analyze for TopLevelId {
    fn analyze(&self) {
        match self {
            TopLevelId::VariableDecl(_) => todo!("Analyze var decl"),
            TopLevelId::FunctionDecl(_) => todo!("Analyze func decl"),
            TopLevelId::Statement(_) => todo!("Analyze statement"),
        }
    }
}
