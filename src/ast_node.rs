use crate::token::*;
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
    VarDeclId(VarId),
    FuncDeclId(FunctionId),
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

    // Access methods
    pub fn expr(&self, id: ExprId) -> &Expression {
        &self.exprs[id.0]
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut Expression {
        &mut self.exprs[id.0]
    }

    pub fn stmt(&self, id: StmtId) -> &Statement {
        &self.stmts[id.0]
    }

    pub fn stmt_mut(&mut self, id: StmtId) -> &mut Statement {
        &mut self.stmts[id.0]
    }

    pub fn stack_block(&self, id: ScopeId) -> &BlockScope {
        &self.block_scopes[id.0]
    }

    pub fn stack_block_mut(&mut self, id: ScopeId) -> &mut BlockScope {
        &mut self.block_scopes[id.0]
    }
    pub fn block_scope(&self, id: ScopeId) -> &BlockScope {
        &self.block_scopes[id.0]
    }

    pub fn block_scope_mut(&mut self, id: ScopeId) -> &mut BlockScope {
        &mut self.block_scopes[id.0]
    }

    pub fn var(&self, id: VarId) -> &VarDeclaration {
        &self.vars[id.0]
    }

    pub fn var_mut(&mut self, id: VarId) -> &mut VarDeclaration {
        &mut self.vars[id.0]
    }

    pub fn function(&self, id: FunctionId) -> &FunctionDeclaration {
        &self.functions[id.0]
    }

    pub fn function_mut(&mut self, id: FunctionId) -> &mut FunctionDeclaration {
        &mut self.functions[id.0]
    }
}

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

pub trait Typed {
    fn get_type(&self) -> Type;
}

pub trait Analyze {
    fn analyze(&self);
}
use std::collections::HashMap;
