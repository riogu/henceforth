use crate::token::*;
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    // Struct(Identifier),
}
impl Type {
    pub fn to_token(&self) -> TokenKind {
        match self {
            Type::Int => TokenKind::Int,
            Type::String => TokenKind::String,
            Type::Bool => TokenKind::Bool,
            Type::Float => TokenKind::Float,
        }
    }
}
pub trait Typed { fn get_type(&self) -> Type; }
pub trait Analyze { fn analyze(&self); }
// ============================================================================
// Type-safe ID types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct VarId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct FunctionId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct ExprId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct StmtId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct ScopeId(usize);

// ============================================================================
// AST Node Structures
// ============================================================================

#[derive(Debug)]
pub enum Identifier {
    Var(VarId),
    Function(FunctionId),
}

#[derive(Debug)]
pub enum Operation {
    Add(ExprId, ExprId),
    Subtract(ExprId, ExprId),
    Multiply(ExprId, ExprId),
    Divide(ExprId, ExprId),
    Negate(ExprId),
    Or(ExprId, ExprId),
    And(ExprId, ExprId),
    Not(ExprId),
}

#[derive(Debug)]
pub enum Expression {
    Operation(Operation),
    Assignment(ExprId, ExprId), 
    Identifier(Identifier),
    Literal(Literal),
    FunctionCall(FunctionId, Vec<ExprId>),
}

#[derive(Debug)]
pub enum Statement {
    If(IfStmt),
    StackBlock(Vec<ExprId>),
    BlockScope(ScopeId),
    While(ExprId, Vec<StmtId>),
    Return,
    Break,
    Continue,
    Empty,
}
#[derive(Debug)]
pub enum TopLevelNode {
    VariableDecl(VarId),
    FunctionDecl(FunctionId),
    Statement(StmtId),
}

#[derive(Debug)]
pub struct VarDeclaration {
    name: String,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: String,
    params: Vec<VarId>,
    body: ScopeId,
}

#[derive(Debug)]
pub struct BlockScope {
    nodes: Vec<TopLevelNode>,
}
#[derive(Debug)]
pub struct StackBlock {
    nodes: Vec<ExprId>,
}

#[derive(Debug)]
pub enum ElseStmt {
    ElseIf(StmtId), // Points to an IfStmt
    Else(ExprId),
}

#[derive(Debug)]
pub struct IfStmt {
    cond: ExprId,
    body: ScopeId,
    else_stmt: Option<ElseStmt>,
}

#[derive(Debug)]
pub struct WhileStmt {
    cond: ExprId,
    body: Vec<ExprId>,
}

// ============================================================================
// Arena storage for all AST nodes

#[derive(Debug, Default)]
// ============================================================================
pub struct AstArena {
    exprs: Vec<Expression>,
    stmts: Vec<Statement>,
    block_scopes: Vec<BlockScope>,
    stack_blocks: Vec<StackBlock>,
    vars: Vec<VarDeclaration>,
    functions: Vec<FunctionDeclaration>,
}

impl AstArena {
    pub fn new() -> Self {
        Self::default()
    }

    // Allocation methods
    pub fn alloc_expr(&mut self, expr: Expression) -> ExprId {
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        id
    }

    pub fn alloc_stmt(&mut self, stmt: Statement) -> StmtId {
        let id = StmtId(self.stmts.len());
        self.stmts.push(stmt);
        id
    }

    pub fn alloc_block_scope(&mut self, scope: BlockScope) -> ScopeId {
        let id = ScopeId(self.block_scopes.len());
        self.block_scopes.push(scope);
        id
    }

    pub fn alloc_var(&mut self, var: VarDeclaration) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        id
    }

    pub fn alloc_function(&mut self, func: FunctionDeclaration) -> FunctionId {
        let id = FunctionId(self.functions.len());
        self.functions.push(func);
        id
    }
    // Expression builders
    pub fn add(&mut self, left: ExprId, right: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Add(left, right)))
    }
    
    pub fn subtract(&mut self, left: ExprId, right: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Subtract(left, right)))
    }
    
    pub fn multiply(&mut self, left: ExprId, right: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Multiply(left, right)))
    }
    
    pub fn divide(&mut self, left: ExprId, right: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Divide(left, right)))
    }
    
    pub fn negate(&mut self, expr: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Negate(expr)))
    }
    
    pub fn or(&mut self, left: ExprId, right: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Or(left, right)))
    }
    
    pub fn and(&mut self, left: ExprId, right: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::And(left, right)))
    }
    
    pub fn not(&mut self, expr: ExprId) -> ExprId {
        self.alloc_expr(Expression::Operation(Operation::Not(expr)))
    }
    
    pub fn assignment(&mut self, target: ExprId, value: ExprId) -> ExprId {
        self.alloc_expr(Expression::Assignment(target, value))
    }
    
    pub fn identifier(&mut self, id: Identifier) -> ExprId {
        self.alloc_expr(Expression::Identifier(id))
    }
    
    pub fn literal(&mut self, lit: Literal) -> ExprId {
        self.alloc_expr(Expression::Literal(lit))
    }
    
    pub fn function_call(&mut self, func: FunctionId, args: Vec<ExprId>) -> ExprId {
        self.alloc_expr(Expression::FunctionCall(func, args))
    }

    // Statement builders
    pub fn if_stmt(&mut self, cond: ExprId, body: ScopeId, else_stmt: Option<ElseStmt>) -> StmtId {
        self.alloc_stmt(Statement::If(IfStmt { cond, body, else_stmt }))
    }
    
    pub fn stack_block_stmt(&mut self, exprs: Vec<ExprId>) -> StmtId {
        self.alloc_stmt(Statement::StackBlock(exprs))
    }
    
    pub fn block_scope_stmt(&mut self, scope: ScopeId) -> StmtId {
        self.alloc_stmt(Statement::BlockScope(scope))
    }
    
    pub fn while_stmt(&mut self, cond: ExprId, body: Vec<StmtId>) -> StmtId {
        self.alloc_stmt(Statement::While(cond, body))
    }
    
    pub fn return_stmt(&mut self) -> StmtId {
        self.alloc_stmt(Statement::Return)
    }
    
    pub fn break_stmt(&mut self) -> StmtId {
        self.alloc_stmt(Statement::Break)
    }
    
    pub fn continue_stmt(&mut self) -> StmtId {
        self.alloc_stmt(Statement::Continue)
    }
    
    pub fn empty_stmt(&mut self) -> StmtId {
        self.alloc_stmt(Statement::Empty)
    }

    // Scope/Block builders
    pub fn block_scope(&mut self, nodes: Vec<TopLevelNode>) -> ScopeId {
        self.alloc_block_scope(BlockScope { nodes })
    }

    // Declaration builders
    pub fn var_declaration(&mut self, name: String) -> VarId {
        self.alloc_var(VarDeclaration { name })
    }
    
    pub fn function_declaration(&mut self, name: String, params: Vec<VarId>, body: ScopeId) -> FunctionId {
        self.alloc_function(FunctionDeclaration { name, params, body })
    }
}
// Export this one
#[macro_export]
macro_rules! push_to {
    ($arena:expr, Expression::Operation(Operation::Add($left:expr, $right:expr))) => {
        $arena.add($left, $right)
    };
    ($arena:expr, Expression::Operation(Operation::Subtract($left:expr, $right:expr))) => {
        $arena.subtract($left, $right)
    };
    ($arena:expr, Expression::Operation(Operation::Multiply($left:expr, $right:expr))) => {
        $arena.multiply($left, $right)
    };
    ($arena:expr, Expression::Operation(Operation::Divide($left:expr, $right:expr))) => {
        $arena.divide($left, $right)
    };
    ($arena:expr, Expression::Operation(Operation::Negate($expr:expr))) => {
        $arena.negate($expr)
    };
    ($arena:expr, Expression::Operation(Operation::Or($left:expr, $right:expr))) => {
        $arena.or($left, $right)
    };
    ($arena:expr, Expression::Operation(Operation::And($left:expr, $right:expr))) => {
        $arena.and($left, $right)
    };
    ($arena:expr, Expression::Operation(Operation::Not($expr:expr))) => {
        $arena.not($expr)
    };
    ($arena:expr, Expression::Assignment($target:expr, $value:expr)) => {
        $arena.assignment($target, $value)
    };
    ($arena:expr, Expression::Identifier($id:expr)) => {
        $arena.identifier($id)
    };
    ($arena:expr, Expression::Literal($lit:expr)) => {
        $arena.literal($lit)
    };
    ($arena:expr, Expression::FunctionCall($func:expr, $args:expr)) => {
        $arena.function_call($func, $args)
    };
    ($arena:expr, Statement::If($cond:expr, $body:expr, $else_stmt:expr)) => {
        $arena.if_stmt($cond, $body, $else_stmt)
    };
    ($arena:expr, Statement::StackBlock($exprs:expr)) => {
        $arena.stack_block_stmt($exprs)
    };
    ($arena:expr, Statement::BlockScope($scope:expr)) => {
        $arena.block_scope_stmt($scope)
    };
    ($arena:expr, Statement::While($cond:expr, $body:expr)) => {
        $arena.while_stmt($cond, $body)
    };
    ($arena:expr, Statement::Return) => {
        $arena.return_stmt()
    };
    ($arena:expr, Statement::Break) => {
        $arena.break_stmt()
    };
    ($arena:expr, Statement::Continue) => {
        $arena.continue_stmt()
    };
    ($arena:expr, Statement::Empty) => {
        $arena.empty_stmt()
    };
    ($arena:expr, VarDeclaration { name: $name:expr }) => {
        $arena.var_declaration($name)
    };
    ($arena:expr, FunctionDeclaration { name: $name:expr, params: $params:expr, body: $body:expr }) => {
        $arena.function_declaration($name, $params, $body)
    };
    ($arena:expr, VarDeclaration($name:expr)) => {
        $arena.var_declaration($name)
    };
    ($arena:expr, FunctionDeclaration($name:expr, $params:expr, $body:expr)) => {
        $arena.function_declaration($name, $params, $body)
    };
}
