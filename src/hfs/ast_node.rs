use crate::hfs::token::*;

// ============================================================================
// Type-safe ID types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct VarId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct FuncId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct ExprId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct StmtId(usize);

// ============================================================================
// AST Node Structures
// ============================================================================

#[derive(Debug)]
pub enum Identifier {
    Unresolved(String),
    Variable(VarId),
    Function(FuncId),
}

#[derive(Debug)]
pub enum Operation {
    Add(ExprId, ExprId),
    Sub(ExprId, ExprId),
    Mul(ExprId, ExprId),
    Div(ExprId, ExprId),
    Mod(ExprId, ExprId),
    Equal(ExprId, ExprId),
    NotEqual(ExprId, ExprId),
    Less(ExprId, ExprId),
    LessEqual(ExprId, ExprId),
    Greater(ExprId, ExprId),
    GreaterEqual(ExprId, ExprId),
    Negate(ExprId),
    Or(ExprId, ExprId),
    And(ExprId, ExprId),
    Not(ExprId),
}

#[derive(Debug)]
pub enum Expression {
    Operation(Operation),
    Identifier(Identifier),
    Literal(Literal),
    FunctionCall{ tuple: ExprId, identifier: Identifier },
    Tuple { expressions: Vec<ExprId>, variadic: bool }
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
    pub hfs_type: Type,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub param_types: Vec<Type>,
    pub return_types: Vec<Type>,
    pub body: StmtId,
}

// ============================================================================
// Statements 

#[derive(Debug)]
pub enum ElseStmt {
    ElseIf(StmtId),  // Points to an IfStmt
    Else(StmtId),    // Points to a BlockScope
}

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
    Assignment { value: ExprId, identifier: ExprId },
}


// -----------------------------------------------------------
// Types
#[derive(Debug)]
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    Tuple,
}
impl Type {
    pub fn to_token(&self) -> TokenKind {
        match self {
            Type::Int => TokenKind::Int,
            Type::String => TokenKind::String,
            Type::Bool => TokenKind::Bool,
            Type::Float => TokenKind::Float,
            Type::Float => TokenKind::Float,
            Type::Tuple => TokenKind::LeftParen,
        }
    }
}
pub trait Typed { fn get_type(&self) -> Type; }
pub trait Analyze { fn analyze(&self); }



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

    hfs_stack: Vec<ExprId>, // keeps track of the state of our stack
}

// had to move this here because i wanted to the arena's private members to be available to the parser
// for better code structure (without making arena members public)
impl<'a> AstArena<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, msg: &str) -> ExprId { 
        // should start using our own error structs instead
        self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg))
    }
    pub fn last_or_error(&mut self, msg: &str) -> ExprId { 
        // should start using our own error structs instead
        *self.hfs_stack.last().unwrap_or_else(|| panic!("{}", msg))
    }
    pub fn push_expr(&mut self, expr: Expression, token: Token<'a>)-> ExprId {
        // allocates AND pushes to the stack 
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        self.expr_tokens.push(token);
        self.hfs_stack.push(id);
        id
    }
    pub fn pop2_or_error(&mut self, msg: &str) -> (ExprId, ExprId) { 
        // should start using our own error structs instead
        (
            self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg)),
            self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg)),
        )
    }

    // Allocation methods 
    pub fn alloc_stmt(&mut self, stmt: Statement, token: Token<'a>) -> StmtId {
        let id = StmtId(self.stmts.len());
        self.stmts.push(stmt);
        self.stmt_tokens.push(token);
        id
    }

    pub fn alloc_var(&mut self, var: VarDeclaration, token: Token<'a>) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        self.var_tokens.push(token);
        id
    }

    pub fn alloc_function(&mut self, func: FunctionDeclaration, token: Token<'a>) -> FuncId {
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
