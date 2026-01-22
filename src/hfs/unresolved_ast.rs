use crate::hfs::{ast::*, token::*, ScopeKind};

// ============================================================================
// First-pass AST (no stack resolution)
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnresolvedOperation {
    // Binary
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Or,
    And,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
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

#[derive(Debug, Clone, PartialEq)]
pub enum UnresolvedExpression {
    Operation(UnresolvedOperation),
    Identifier(String),
    Literal(Literal),
    Tuple { expressions: Vec<UnresolvedExprId> },
    StackKeyword(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnresolvedStatement {
    If {
        cond: UnresolvedStmtId,
        body: UnresolvedStmtId,
        else_stmt: Option<UnresolvedElseStmt>,
    },
    While {
        cond: UnresolvedStmtId,
        body: UnresolvedStmtId,
    },
    StackBlock(Vec<UnresolvedExprId>),
    BlockScope(Vec<UnresolvedTopLevelId>, ScopeKind),
    Return,
    Break,
    Continue,
    Empty,
    Assignment {
        identifier: UnresolvedExprId, // just the name
        is_move: bool,                // true for &=, false for :=
                                      // value comes from stack
    },
    FunctionCall {
        identifier: UnresolvedExprId,
        is_move: bool, // true for &>, false for :>
                       // value comes from stack
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnresolvedElseStmt {
    ElseIf(UnresolvedStmtId), // Points to an UnresolvedIfStmt
    Else(UnresolvedStmtId),   // Points to a UnresolvedBlockScope
}

// Unresolved declarations (mirror the resolved ones but use UnresolvedStmtId)
#[derive(Debug, Clone, PartialEq)]
pub struct UnresolvedVarDeclaration {
    pub name: String,
    pub hfs_type: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnresolvedFunctionDeclaration {
    pub name: String,
    pub param_type: TypeId,
    pub return_type: TypeId,
    pub body: UnresolvedStmtId, // Uses unresolved ID
}

#[derive(Debug, Clone)]
pub struct UnresolvedStackKeyword {
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnresolvedVarId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnresolvedFuncId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnresolvedExprId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnresolvedStmtId(pub usize);

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
    pub(crate) types: Vec<Type>,

    // Token storage
    pub(crate) unresolved_expr_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_stmt_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_var_tokens: Vec<Token<'a>>,
    pub(crate) unresolved_function_tokens: Vec<Token<'a>>,
    pub(crate) type_tokens: Vec<Token<'a>>,
}

impl<'a> PartialEq for UnresolvedAstArena<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.unresolved_exprs == other.unresolved_exprs
            && self.unresolved_stmts == other.unresolved_stmts
            && self.unresolved_vars == other.unresolved_vars
            && self.unresolved_functions == other.unresolved_functions
            && self.types == other.types
    }
}

impl<'a> UnresolvedAstArena<'a> {
    pub fn new() -> Self {
        let mut arena = Self::default();
        arena.alloc_type_uncached(Type::Int, Token { kind: TokenKind::Int, source_info: SourceInfo::new(0, 0, 0, "Int") });
        arena.alloc_type_uncached(Type::Float, Token { kind: TokenKind::Float, source_info: SourceInfo::new(0, 0, 0, "Float") });
        arena.alloc_type_uncached(Type::Bool, Token { kind: TokenKind::Bool, source_info: SourceInfo::new(0, 0, 0, "Bool") });
        arena.alloc_type_uncached(Type::String, Token {
            kind: TokenKind::String,
            source_info: SourceInfo::new(0, 0, 0, "String"),
        });
        arena
    }

    fn alloc_type_uncached(&mut self, hfs_type: Type, token: Token<'a>) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_tokens.push(token);
        id
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

    pub fn alloc_type(&mut self, hfs_type: Type, token: Token<'a>) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type);
        self.type_tokens.push(token);
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

    pub fn get_type(&self, id: TypeId) -> &Type {
        &self.types[id.0]
    }

    // Token accessor methods
    pub fn get_unresolved_expr_token(&self, id: UnresolvedExprId) -> Token<'a> {
        self.unresolved_expr_tokens[id.0].clone()
    }

    pub fn get_unresolved_stmt_token(&self, id: UnresolvedStmtId) -> Token<'a> {
        self.unresolved_stmt_tokens[id.0].clone()
    }

    pub fn get_unresolved_var_token(&self, id: UnresolvedVarId) -> Token<'a> {
        self.unresolved_var_tokens[id.0].clone()
    }

    pub fn get_unresolved_func_token(&self, id: UnresolvedFuncId) -> Token<'a> {
        self.unresolved_function_tokens[id.0].clone()
    }

    pub fn get_type_token(&self, id: TypeId) -> Token<'a> {
        self.type_tokens[id.0].clone()
    }

    pub fn to_type(&mut self, token: Token<'a>) -> TypeId {
        match token.kind {
            TokenKind::Int => self.alloc_type(Type::Int, token),
            TokenKind::String => self.alloc_type(Type::String, token),
            TokenKind::Bool => self.alloc_type(Type::Bool, token),
            TokenKind::Float => self.alloc_type(Type::Float, token),
            TokenKind::Identifier(_) => {
                panic!("[internal hfs error]: this is not how you convert identifiers to types")
            },
            _ => panic!("[internal hfs error]: expected token that has a type, got {:?}", token.kind),
        }
    }
}
