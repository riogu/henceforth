use crate::hfs::token::*;
use crate::hfs::ast::*;

// ============================================================================
// First-pass "dumb" AST (no stack resolution)
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationKind {
    // Binary
    Add, Sub, Mul, Div, Mod,
    Equal, NotEqual,
    Less, LessEqual, Greater, GreaterEqual,
    Or, And,
    // Unary
    Not,
}

impl OperationKind {
    pub fn is_binary(&self) -> bool {
        !matches!(self, OperationKind::Not)
    }
    
    pub fn is_unary(&self) -> bool {
        matches!(self, OperationKind::Not)
    }
}

#[derive(Debug)]
pub enum UnresolvedExpression {
    Operation(OperationKind),
    Identifier(String),
    Literal(Literal),
    FunctionCall { identifier: String }, // tuple comes from stack
    Tuple { expressions: Vec<UnresolvedExprId>, variadic: bool }
}

#[derive(Debug)]
pub enum UnresolvedStatement {
    If {
        // cond comes from stack
        body: UnresolvedStmtId,
        else_stmt: Option<UnresolvedElseStmt>,
    },
    While {
        // cond comes from stack
        body: UnresolvedStmtId,
    },
    StackBlock(Vec<UnresolvedExprId>),
    BlockScope(Vec<UnresolvedTopLevelId>),
    Return,
    Break,
    Continue,
    Empty,
    Assignment { 
        identifier: String, // just the name
        is_move: bool,      // true for &=, false for :=
        // value comes from stack
    },
}

#[derive(Debug)]
pub enum UnresolvedElseStmt {
    ElseIf(UnresolvedStmtId),
    Else(UnresolvedStmtId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnresolvedExprId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnresolvedStmtId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnresolvedTopLevelId {
    VariableDecl(VarId),
    FunctionDecl(FuncId),
    Statement(UnresolvedStmtId),
}

// ============================================================================
// First-pass Arena (parallel to AstArena)
// ============================================================================

#[derive(Debug, Default)]
pub struct UnresolvedAstArena<'a> {
    // Unresolved AST nodes
    pub(crate) unresolved_exprs: Vec<UnresolvedExpression>,
    pub(crate) unresolved_stmts: Vec<UnresolvedStatement>,
    
    // Declarations are the same
    pub(crate) vars: Vec<VarDeclaration>,
    pub(crate) functions: Vec<FunctionDeclaration>,

    // Token storage
    pub(crate) unresolved_expr_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_stmt_tokens: Vec<Token<'a>>,
    pub(crate) var_tokens: Vec<Token<'a>>,
    pub(crate) function_tokens: Vec<Token<'a>>,
}

impl<'a> UnresolvedAstArena<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_unresolved_expr(&mut self, expr: UnresolvedExpression, token: Token<'a>) -> UnresolvedExprId {
        let id = UnresolvedExprId(self.unresolved_exprs.len());
        self.unresolved_exprs.push(expr);
        self.unresolved_expr_tokens.push(token);
        id
    }

    pub fn alloc_unresolved_stmt(&mut self, stmt: UnresolvedStatement, token: Token<'a>) -> UnresolvedStmtId {
        let id = UnresolvedStmtId(self.unresolved_stmts.len());
        self.unresolved_stmts.push(stmt);
        self.unresolved_stmt_tokens.push(token);
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
}

