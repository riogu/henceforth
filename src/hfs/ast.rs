use crate::hfs::token::*;

// ============================================================================
// Type-safe ID types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct VarId (pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct FuncId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct ExprId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct StmtId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct TypeId(pub usize);

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
    Tuple { expressions: Vec<ExprId>, variadic: bool },
    Parameter(TypeId),
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    pub hfs_type: TypeId,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub param_type: TypeId,     // either a tuple or a single type
    pub return_type: TypeId,    // either a tuple or a single type
    pub body: StmtId,
    pub params: Vec<ExprId>,  // Vec<Expression::Parameter>
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
        cond: ExprId, // boolean from the stack or operation
        body: StmtId, // points to BlockScope
        else_stmt: Option<ElseStmt>,
    },
    While {
        cond: ExprId, // boolean from the stack or operation
        body: StmtId, // points to BlockScope
    },
    StackBlock(Vec<ExprId>),
    BlockScope(Vec<TopLevelId>),
    Return,
    Break,
    Continue,
    Empty,
    Assignment { value: ExprId, identifier: ExprId, is_move: bool },
}


// -----------------------------------------------------------
// Types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    String,
    Bool,
    Float,
    Tuple(Vec<TypeId>),
}
impl Type {
    pub fn to_token(&self) -> TokenKind {
        match self {
            Type::Int => TokenKind::Int,
            Type::String => TokenKind::String,
            Type::Bool => TokenKind::Bool,
            Type::Float => TokenKind::Float,
            Type::Float => TokenKind::Float,
            Type::Tuple(_) => TokenKind::LeftParen,
        }
    }
}

// ============================================================================
// Arena storage with token tracking
// ============================================================================
#[derive(Debug, Default)]
pub struct AstArena<'a> {
    // AST nodes
    pub(crate) exprs: Vec<Expression>,
    pub(crate) stmts: Vec<Statement>,
    pub(crate) vars: Vec<VarDeclaration>,
    pub(crate) functions: Vec<FunctionDeclaration>,
    pub(crate) types: Vec<Type>,

    // Token storage (parallel arrays)
    pub(crate) expr_tokens: Vec<Token<'a>>,
    pub(crate) stmt_tokens: Vec<Token<'a>>,
    pub(crate) var_tokens: Vec<Token<'a>>,
    pub(crate) function_tokens: Vec<Token<'a>>,
    pub(crate) type_tokens: Vec<Token<'a>>,

    pub(crate) hfs_stack: Vec<ExprId>, // keeps track of the state of our stack
    pub(crate) hfs_type_stack: Vec<TypeId>,  // keeps track of the state of our types,
    // for semantic analysis
}

// had to move this here because i wanted to the arena's private members to be available to the parser
// for better code structure (without making arena members public)
impl<'a> AstArena<'a> {
    pub fn new() -> Self {
        Self::default()
    }
    // Allocation methods 
    pub fn alloc_expr(&mut self, expr: Expression, token: Token<'a>) -> ExprId {
        // allocates AND pushes to the stack 
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        id
    }

    pub fn alloc_stmt(&mut self, stmt: Statement, token: Token<'a>) -> StmtId {
        let id = StmtId(self.stmts.len());
        self.stmts.push(stmt);
        id
    }

    pub fn alloc_var(&mut self, var: VarDeclaration, token: Token<'a>) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        id
    }

    pub fn alloc_function(&mut self, func: FunctionDeclaration, token: Token<'a>) -> FuncId {
        let id = FuncId(self.functions.len());
        self.functions.push(func);
        id
    }

    pub fn alloc_type(&mut self, hfs_type: Type, token: Token<'a>) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type);
        id
    }
    // this is necessary to allow recursive functions
    pub fn temporarily_get_next_stmt_id(&mut self) -> StmtId {
        StmtId(self.stmts.len())
    }

    // Immutable accessor methods
    pub fn get_expr(&self, id: ExprId) -> &Expression {
        &self.exprs[id.0]
    }
    pub fn get_stmt(&self, id: StmtId) -> &Statement {
        &self.stmts[id.0]
    }
    pub fn get_var(&self, id: VarId) -> &VarDeclaration {
        &self.vars[id.0]
    }
    pub fn get_func(&self, id: FuncId) -> &FunctionDeclaration {
        &self.functions[id.0]
    }
    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    // Mutable accessor methods
    pub fn get_expr_mut(&mut self, id: ExprId) -> &mut Expression {
        &mut self.exprs[id.0]
    }
    pub fn get_stmt_mut(&mut self, id: StmtId) -> &mut Statement {
        &mut self.stmts[id.0]
    }
    pub fn get_var_mut(&mut self, id: VarId) -> &mut VarDeclaration {
        &mut self.vars[id.0]
    }
    pub fn get_func_mut(&mut self, id: FuncId) -> &mut FunctionDeclaration {
        &mut self.functions[id.0]
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

}

