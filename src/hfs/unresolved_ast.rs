use crate::hfs::token::*;
use crate::hfs::ast::Type;

// ============================================================================
// First-pass AST (no stack resolution)
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnresolvedOperation {
    // Binary
    Add, Sub, Mul, Div, Mod, Or, And,
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    // Unary
    Not,
}

impl UnresolvedOperation {
    pub fn is_binary(&self) -> bool {
        !matches!(self, UnresolvedOperation::Not)
    }
    
    pub fn is_unary(&self) -> bool {
        matches!(self, UnresolvedOperation::Not)
    }
}

#[derive(Debug, Clone)]
pub enum UnresolvedExpression {
    Operation(UnresolvedOperation),
    Identifier(String),
    Literal(Literal),
    FunctionCall { identifier: String }, // tuple comes from stack
    Tuple { expressions: Vec<UnresolvedExprId>, variadic: bool }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnresolvedElseStmt {
    ElseIf(UnresolvedStmtId), // Points to an UnresolvedIfStmt
    Else(UnresolvedStmtId),   // Points to a UnresolvedBlockScope
}

// Unresolved declarations (mirror the resolved ones but use UnresolvedStmtId)
#[derive(Debug, Clone)]
pub struct UnresolvedVarDeclaration {
    pub name: String,
    pub hfs_type: Type,
}

#[derive(Debug, Clone)]
pub struct UnresolvedFunctionDeclaration {
    pub name: String,
    pub param_type: Type,
    pub return_type: Type,
    pub body: UnresolvedStmtId, // Uses unresolved ID
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct UnresolvedVarId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct UnresolvedFuncId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct UnresolvedExprId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)] pub struct UnresolvedStmtId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnresolvedTopLevelId {
    VariableDecl(UnresolvedVarId),
    FunctionDecl(UnresolvedFuncId),
    Statement(UnresolvedStmtId),
}

// ============================================================================
// First-pass Arena (completely separate from AstArena)
// ============================================================================

#[derive(Debug, Default)]
pub struct UnresolvedAstArena<'a> {
    // Unresolved AST nodes
    pub(crate) unresolved_exprs: Vec<UnresolvedExpression>,
    pub(crate) unresolved_stmts: Vec<UnresolvedStatement>,
    pub(crate) unresolved_vars: Vec<UnresolvedVarDeclaration>,
    pub(crate) unresolved_functions: Vec<UnresolvedFunctionDeclaration>,

    // Token storage
    pub(crate) unresolved_expr_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_stmt_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_var_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_function_tokens: Vec<Token<'a>>,
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

    pub fn alloc_unresolved_var(&mut self, var: UnresolvedVarDeclaration, token: Token<'a>) -> UnresolvedVarId {
        let id = UnresolvedVarId(self.unresolved_vars.len());
        self.unresolved_vars.push(var);
        self.unresolved_var_tokens.push(token);
        id
    }

    pub fn alloc_unresolved_function(&mut self, func: UnresolvedFunctionDeclaration, token: Token<'a>) -> UnresolvedFuncId {
        let id = UnresolvedFuncId(self.unresolved_functions.len());
        self.unresolved_functions.push(func);
        self.unresolved_function_tokens.push(token);
        id
    }

    // Immutable accessor methods
    pub fn get_unresolved_expr(&self, id: UnresolvedExprId) -> &UnresolvedExpression {
        &self.unresolved_exprs[id.0]
    }
    pub fn get_unresolved_stmt(&self, id: UnresolvedStmtId) -> &UnresolvedStatement {
        &self.unresolved_stmts[id.0]
    }
    pub fn get_unresolved_var(&self, id: UnresolvedVarId) -> &UnresolvedVarDeclaration {
        &self.unresolved_vars[id.0]
    }
    pub fn get_unresolved_func(&self, id: UnresolvedFuncId) -> &UnresolvedFunctionDeclaration {
        &self.unresolved_functions[id.0]
    }
}
