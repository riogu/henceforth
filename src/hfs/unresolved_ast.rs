use crate::hfs::{ScopeKind, ast::*, token::*};

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
    AddressOf,
    Dereference,
}

impl UnresolvedOperation {
    pub fn is_binary(&self) -> bool {
        !(matches!(self, UnresolvedOperation::Not)
            || matches!(self, UnresolvedOperation::AddressOf)
            || matches!(self, UnresolvedOperation::Dereference))
    }

    pub fn is_unary(&self) -> bool {
        matches!(self, UnresolvedOperation::Not)
            || matches!(self, UnresolvedOperation::AddressOf)
            || matches!(self, UnresolvedOperation::Dereference)
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
    ElseIf {
        cond: UnresolvedStmtId,
        body: UnresolvedStmtId,
        else_stmt: Option<UnresolvedStmtId>,
    },
    Else(UnresolvedStmtId), // Points to a UnresolvedBlockScope
    If {
        cond: UnresolvedStmtId,
        body: UnresolvedStmtId,
        else_stmt: Option<UnresolvedStmtId>,
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
        deref_count: i32
    },
    FunctionCall {
        identifier: UnresolvedExprId,
        is_move: bool, // true for &>, false for :>
                       // value comes from stack
    },
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

#[derive(Debug, Default, Clone)]
pub struct UnresolvedAstArena {
    // Unresolved AST nodes
    pub(crate) unresolved_exprs: Vec<UnresolvedExpression>,
    pub(crate) unresolved_stmts: Vec<UnresolvedStatement>,
    pub(crate) unresolved_vars: Vec<UnresolvedVarDeclaration>,
    pub(crate) unresolved_functions: Vec<UnresolvedFunctionDeclaration>,
    pub(crate) types: Vec<Type>,

    // Token storage
    pub(crate) unresolved_expr_tokens: Vec<Token>,
    pub(crate) unresolved_stmt_tokens: Vec<Token>,
    pub(crate) unresolved_var_tokens: Vec<Token>,
    pub(crate) unresolved_function_tokens: Vec<Token>,
    pub(crate) type_tokens: Vec<Token>,
}

impl PartialEq for UnresolvedAstArena {
    fn eq(&self, other: &Self) -> bool {
        self.unresolved_exprs == other.unresolved_exprs
            && self.unresolved_stmts == other.unresolved_stmts
            && self.unresolved_vars == other.unresolved_vars
            && self.unresolved_functions == other.unresolved_functions
            && self.types == other.types
    }
}

impl UnresolvedAstArena {
    pub fn new() -> Self {
        let mut arena = Self::default();
        arena.alloc_type_uncached(Type::new_int(0), Token { kind: TokenKind::Int, source_info: SourceInfo::new(0, 0, 0) });
        arena.alloc_type_uncached(Type::new_float(0), Token { kind: TokenKind::Float, source_info: SourceInfo::new(0, 0, 0) });
        arena.alloc_type_uncached(Type::new_bool(0), Token { kind: TokenKind::Bool, source_info: SourceInfo::new(0, 0, 0) });
        arena.alloc_type_uncached(Type::new_string(0), Token { kind: TokenKind::String, source_info: SourceInfo::new(0, 0, 0) });
        arena
    }

    fn alloc_type_uncached(&mut self, hfs_type: Type, token: Token) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_tokens.push(token);
        id
    }

    pub fn alloc_unresolved_expr(&mut self, expr: UnresolvedExpression, token: Token) -> UnresolvedExprId {
        let id = UnresolvedExprId(self.unresolved_exprs.len());
        self.unresolved_exprs.push(expr);
        self.unresolved_expr_tokens.push(token);
        id
    }

    pub fn alloc_unresolved_stmt(&mut self, stmt: UnresolvedStatement, token: Token) -> UnresolvedStmtId {
        let id = UnresolvedStmtId(self.unresolved_stmts.len());
        self.unresolved_stmts.push(stmt);
        self.unresolved_stmt_tokens.push(token);
        id
    }

    pub fn alloc_unresolved_var(&mut self, var: UnresolvedVarDeclaration, token: Token) -> UnresolvedVarId {
        let id = UnresolvedVarId(self.unresolved_vars.len());
        self.unresolved_vars.push(var);
        self.unresolved_var_tokens.push(token);
        id
    }

    pub fn alloc_unresolved_function(&mut self, func: UnresolvedFunctionDeclaration, token: Token) -> UnresolvedFuncId {
        let id = UnresolvedFuncId(self.unresolved_functions.len());
        self.unresolved_functions.push(func);
        self.unresolved_function_tokens.push(token);
        id
    }

    pub fn alloc_type(&mut self, hfs_type: Type, token: Token) -> TypeId {
        if let Some(id) = self.types.iter().position(|t| *t == hfs_type) {
            return TypeId(id);
        }
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
    pub fn get_unresolved_expr_token(&self, id: UnresolvedExprId) -> Token {
        self.unresolved_expr_tokens[id.0].clone()
    }

    pub fn get_unresolved_stmt_token(&self, id: UnresolvedStmtId) -> Token {
        self.unresolved_stmt_tokens[id.0].clone()
    }

    pub fn get_unresolved_var_token(&self, id: UnresolvedVarId) -> Token {
        self.unresolved_var_tokens[id.0].clone()
    }

    pub fn get_unresolved_func_token(&self, id: UnresolvedFuncId) -> Token {
        self.unresolved_function_tokens[id.0].clone()
    }

    pub fn get_type_token(&self, id: TypeId) -> Token {
        self.type_tokens[id.0].clone()
    }

    pub fn to_type(&mut self, token: Token, ptr_count: i32) -> TypeId {
        match token.kind {
            TokenKind::Int => self.alloc_type(Type::new_int(ptr_count), token),
            TokenKind::String => self.alloc_type(Type::new_string(ptr_count), token),
            TokenKind::Bool => self.alloc_type(Type::new_bool(ptr_count), token),
            TokenKind::Float => self.alloc_type(Type::new_float(ptr_count), token),
            TokenKind::Identifier(_) => {
                panic!("[internal hfs error]: this is not how you convert identifiers to types")
            },
            _ => panic!("[internal hfs error]: expected token that has a type, got {:?}", token.kind),
        }
    }
}
