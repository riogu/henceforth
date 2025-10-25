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
pub enum Operation<'a> {
    Add(Token<'a>, ExprId, ExprId),
    Subtract(Token<'a>, ExprId, ExprId),
    Multiply(Token<'a>, ExprId, ExprId),
    Divide(Token<'a>, ExprId, ExprId),
    Negate(Token<'a>, ExprId),
    Or(Token<'a>, ExprId, ExprId),
    And(Token<'a>, ExprId, ExprId),
    Not(Token<'a>, ExprId),
}

#[derive(Debug)]
pub enum Expression<'a> {
    Operation(Token<'a>, Operation<'a>),
    Assignment(Token<'a>, ExprId, ExprId), 
    Identifier(Token<'a>, Identifier),
    Literal(Token<'a>, Literal),
    FunctionCall(Token<'a>, FunctionId, Vec<ExprId>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    If(IfStmt<'a>),
    StackBlock(StackBlock<'a>),
    BlockScope(BlockScope<'a>),
    While(WhileStmt<'a>),
    Return(Token<'a>),
    Break(Token<'a>),
    Continue(Token<'a>),
    Empty(Token<'a>),
}

#[derive(Debug)]
pub enum TopLevelNode {
    VariableDecl(VarId),
    FunctionDecl(FunctionId),
    Statement(StmtId),
}

#[derive(Debug)]
pub struct VarDeclaration<'a> {
    pub token: Token<'a>,
    pub name: String,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub token: Token<'a>,
    pub name: String,
    pub params: Vec<VarId>,
    pub body: ScopeId,
}

#[derive(Debug)]
pub struct BlockScope<'a> {
    pub token: Token<'a>,
    pub nodes: Vec<TopLevelNode>,
}

#[derive(Debug)]
pub struct StackBlock<'a> {
    pub token: Token<'a>,
    pub nodes: Vec<ExprId>,
}

#[derive(Debug)]
pub enum ElseStmt<'a> {
    ElseIf(StmtId), // Points to an IfStmt
    Else(Token<'a>, ExprId),
}

#[derive(Debug)]
pub struct IfStmt<'a> {
    pub token: Token<'a>,
    pub cond: ExprId,
    pub body: ScopeId,
    pub else_stmt: Option<ElseStmt<'a>>,
}

#[derive(Debug)]
pub struct WhileStmt<'a> {
    pub token: Token<'a>,
    pub cond: ExprId,
    pub body: Vec<ExprId>,
}

// ============================================================================
// Arena storage for all AST nodes

#[derive(Debug, Default)]
// ============================================================================
pub struct AstArena<'a> {
    exprs: Vec<Expression<'a>>,
    stmts: Vec<Statement<'a>>,
    block_scopes: Vec<BlockScope<'a>>,
    stack_blocks: Vec<StackBlock<'a>>,
    vars: Vec<VarDeclaration<'a>>,
    functions: Vec<FunctionDeclaration<'a>>,
}

impl<'a> AstArena<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    // Allocation methods
    pub fn alloc_expr(&mut self, expr: Expression<'a>) -> ExprId {
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        id
    }

    pub fn alloc_stmt(&mut self, stmt: Statement<'a>) -> StmtId {
        let id = StmtId(self.stmts.len());
        self.stmts.push(stmt);
        id
    }

    pub fn alloc_block_scope(&mut self, scope: BlockScope<'a>) -> ScopeId {
        let id = ScopeId(self.block_scopes.len());
        self.block_scopes.push(scope);
        id
    }

    pub fn alloc_var(&mut self, var: VarDeclaration<'a>) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        id
    }

    pub fn alloc_function(&mut self, func: FunctionDeclaration<'a>) -> FunctionId {
        let id = FunctionId(self.functions.len());
        self.functions.push(func);
        id
    }
    // Accessor methods
    pub fn get_expr(&self, id: ExprId) -> &Expression {
        &self.exprs[id.0]
    }
    
    pub fn get_stmt(&self, id: StmtId) -> &Statement {
        &self.stmts[id.0]
    }
    
    pub fn get_block_scope(&self, id: ScopeId) -> &BlockScope {
        &self.block_scopes[id.0]
    }
    
    pub fn get_stack_block(&self, id: ScopeId) -> &StackBlock {
        &self.stack_blocks[id.0]
    }
    pub fn get_var(&self, id: VarId) -> &VarDeclaration {
        &self.vars[id.0]
    }
    
    pub fn get_function(&self, id: FunctionId) -> &FunctionDeclaration {
        &self.functions[id.0]
    }
}
