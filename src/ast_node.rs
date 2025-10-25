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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct FuncId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct ExprId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct StmtId(usize);

// ============================================================================
// Unified NodeId for common token API
// ============================================================================
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeId {
    Expr(ExprId),
    Stmt(StmtId),
    Var(VarId),
    Function(FuncId),
}

impl From<ExprId> for NodeId {
    fn from(id: ExprId) -> Self {
        NodeId::Expr(id)
    }
}

impl From<StmtId> for NodeId {
    fn from(id: StmtId) -> Self {
        NodeId::Stmt(id)
    }
}

impl From<VarId> for NodeId {
    fn from(id: VarId) -> Self {
        NodeId::Var(id)
    }
}

impl From<FuncId> for NodeId {
    fn from(id: FuncId) -> Self {
        NodeId::Function(id)
    }
}

// ============================================================================
// AST Node Structures (no tokens, no lifetimes!)
// ============================================================================

#[derive(Debug)]
pub enum Identifier {
    Var(VarId),
    Function(FuncId),
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
    CopyAssignment(ExprId, ExprId),
    MoveAssignment(ExprId, ExprId),
    Identifier(Identifier),
    Literal(Literal),
    FunctionCall(FuncId, Vec<ExprId>),
}


#[derive(Debug)]
pub enum TopLevelId {
    VariableDecl(VarId),
    FunctionDecl(FuncId),
    Statement(StmtId),
}

// ============================================================================
// Declarations (shared)
#[derive(Debug)]
pub struct VarDeclaration {
    pub name: String,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<VarId>,
    pub body: StmtId,
}

// ============================================================================
// Statements (not shared)
#[derive(Debug)]
pub enum Statement {
    If {
        cond: StmtId, // points to StackBlock
        body: StmtId, // points to BlockScope
        else_stmt: Option<ElseStmt>,
    },
    While {
        cond: StmtId, // points to StackBlock
        body: StmtId, // points to BlockScope
    },
    StackBlock(Vec<ExprId>),
    BlockScope(Vec<TopLevelId>),
    Return,
    Break,
    Continue,
    Empty,
}

#[derive(Debug)]
pub enum ElseStmt {
    ElseIf(StmtId),  // Points to an IfStmt
    Else(StmtId),    // Points to a BlockScope
}
#[derive(Debug)]
pub struct IfStmt {
    cond: StmtId, // is a StackBlock
    body: StmtId, // is a BlockScope
    else_stmt: Option<ElseStmt>, // is ElseIf/Else
}

// ============================================================================

// ============================================================================
// Arena storage with token tracking
// ============================================================================
#[derive(Debug, Default)]
pub struct AstArena<'a> {
    // AST nodes
    exprs: Vec<Expression>,
    stmts: Vec<Statement>,
    vars: Vec<VarDeclaration>,
    functions: Vec<FunctionDeclaration>,

    // Token storage (parallel arrays)
    expr_tokens: Vec<Token<'a>>,
    stmt_tokens: Vec<Token<'a>>,
    var_tokens: Vec<Token<'a>>,
    function_tokens: Vec<Token<'a>>,
}

impl<'a> AstArena<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    // Allocation methods (now take tokens separately)
    pub fn alloc_expr(&mut self, expr: Expression, token: Token<'a>) -> ExprId {
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        self.expr_tokens.push(token);
        id
    }

    pub fn push_stmt(&mut self, stmt: Statement, token: Token<'a>) -> StmtId {
        let id = StmtId(self.stmts.len());
        self.stmts.push(stmt);
        self.stmt_tokens.push(token);
        id
    }


    pub fn push_var(&mut self, var: VarDeclaration, token: Token<'a>) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        self.var_tokens.push(token);
        id
    }

    pub fn push_function(&mut self, func: FunctionDeclaration, token: Token<'a>) -> FuncId {
        let id = FuncId(self.functions.len());
        self.functions.push(func);
        self.function_tokens.push(token);
        id
    }

    // Accessor methods
    pub fn get_expr(&self, id: ExprId) -> &Expression {
        &self.exprs[id.0]
    }

    pub fn get_stmt(&self, id: StmtId) -> &Statement {
        &self.stmts[id.0]
    }

    pub fn get_var(&self, id: VarId) -> &VarDeclaration {
        &self.vars[id.0]
    }

    pub fn get_function(&self, id: FuncId) -> &FunctionDeclaration {
        &self.functions[id.0]
    }

    // Token accessor methods
    pub fn get_expr_token(&self, id: ExprId) -> &Token<'a> {
        &self.expr_tokens[id.0]
    }

    pub fn get_stmt_token(&self, id: StmtId) -> &Token<'a> {
        &self.stmt_tokens[id.0]
    }

    pub fn get_var_token(&self, id: VarId) -> &Token<'a> {
        &self.var_tokens[id.0]
    }

    pub fn get_function_token(&self, id: FuncId) -> &Token<'a> {
        &self.function_tokens[id.0]
    }

    // Unified token API using NodeId
    pub fn get_token(&self, node_id: NodeId) -> &Token<'a> {
        match node_id {
            NodeId::Expr(id) => self.get_expr_token(id),
            NodeId::Stmt(id) => self.get_stmt_token(id),
            NodeId::Var(id) => self.get_var_token(id),
            NodeId::Function(id) => self.get_function_token(id),
        }
    }
}
