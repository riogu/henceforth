use std::rc::Rc;

use crate::hfs::{ScopeKind, Type, UnresolvedType, ast::*, error::DiagnosticInfo, token::*};

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
    ArrayAccess,
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
        cond: Vec<UnresolvedStmtId>,
        body: UnresolvedStmtId,
        else_stmt: Option<UnresolvedStmtId>,
    },
    Else(UnresolvedStmtId), // Points to a UnresolvedBlockScope
    If {
        cond: Vec<UnresolvedStmtId>,
        body: UnresolvedStmtId,
        else_stmt: Option<UnresolvedStmtId>,
    },
    While {
        cond: Vec<UnresolvedStmtId>,
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
        deref_count: usize,
    },
    FunctionCall {
        identifier: UnresolvedExprId,
        is_move: bool, // true for &>, false for :>
                       // value comes from stack
    },
    ArrayAssignment {
        identifier: UnresolvedExprId,
        is_move: bool,
        deref_count: usize,
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
    pub(crate) types: Vec<UnresolvedType>,

    // Span storage
    pub(crate) unresolved_expr_spans: Vec<Span>,
    pub(crate) unresolved_stmt_spans: Vec<Span>,
    pub(crate) unresolved_var_spans: Vec<Span>,
    pub(crate) unresolved_function_spans: Vec<Span>,
    pub(crate) type_spans: Vec<Span>,

    pub diagnostic_info: Rc<DiagnosticInfo>,
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
    pub fn new(diagnostic_info: Rc<DiagnosticInfo>) -> Self {
        let mut arena = Self::default();
        arena.diagnostic_info = diagnostic_info;
        arena.alloc_type_uncached(UnresolvedType::new_int(0), Span::default());
        arena.alloc_type_uncached(UnresolvedType::new_float(0), Span::default());
        arena.alloc_type_uncached(UnresolvedType::new_bool(0), Span::default());
        arena.alloc_type_uncached(UnresolvedType::new_string(0), Span::default());
        arena
    }

    fn alloc_type_uncached(&mut self, hfs_type: UnresolvedType, span: Span) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_spans.push(span);
        id
    }

    pub fn alloc_unresolved_expr(&mut self, expr: UnresolvedExpression, span: Span) -> UnresolvedExprId {
        let id = UnresolvedExprId(self.unresolved_exprs.len());
        self.unresolved_exprs.push(expr);
        self.unresolved_expr_spans.push(span);
        id
    }

    pub fn alloc_unresolved_stmt(&mut self, stmt: UnresolvedStatement, span: Span) -> UnresolvedStmtId {
        let id = UnresolvedStmtId(self.unresolved_stmts.len());
        self.unresolved_stmts.push(stmt);
        self.unresolved_stmt_spans.push(span);
        id
    }

    pub fn alloc_unresolved_var(&mut self, var: UnresolvedVarDeclaration, span: Span) -> UnresolvedVarId {
        let id = UnresolvedVarId(self.unresolved_vars.len());
        self.unresolved_vars.push(var);
        self.unresolved_var_spans.push(span);
        id
    }

    pub fn alloc_unresolved_function(&mut self, func: UnresolvedFunctionDeclaration, span: Span) -> UnresolvedFuncId {
        let id = UnresolvedFuncId(self.unresolved_functions.len());
        self.unresolved_functions.push(func);
        self.unresolved_function_spans.push(span);
        id
    }

    pub fn alloc_type(&mut self, hfs_type: UnresolvedType, span: Span) -> TypeId {
        if let Some(id) = self.types.iter().position(|t| *t == hfs_type) {
            if self.type_spans[id] == Span::new(0, 0, 0) {
                self.type_spans[id] = span;
            }
            return TypeId(id);
        }
        let id = TypeId(self.types.len());
        self.types.push(hfs_type);
        self.type_spans.push(span);
        id
    }

    // Immutable accessor methods
    pub fn get_unresolved_expr(&self, id: UnresolvedExprId) -> &UnresolvedExpression { &self.unresolved_exprs[id.0] }

    pub fn get_unresolved_stmt(&self, id: UnresolvedStmtId) -> &UnresolvedStatement { &self.unresolved_stmts[id.0] }

    pub fn get_unresolved_var(&self, id: UnresolvedVarId) -> &UnresolvedVarDeclaration { &self.unresolved_vars[id.0] }

    pub fn get_unresolved_func(&self, id: UnresolvedFuncId) -> &UnresolvedFunctionDeclaration { &self.unresolved_functions[id.0] }

    pub fn get_type(&self, id: TypeId) -> &UnresolvedType { &self.types[id.0] }

    // Token accessor methods
    pub fn get_unresolved_expr_span(&self, id: UnresolvedExprId) -> Span { self.unresolved_expr_spans[id.0].clone() }

    pub fn get_unresolved_stmt_span(&self, id: UnresolvedStmtId) -> Span { self.unresolved_stmt_spans[id.0].clone() }

    pub fn get_unresolved_var_span(&self, id: UnresolvedVarId) -> Span { self.unresolved_var_spans[id.0].clone() }

    pub fn get_unresolved_func_span(&self, id: UnresolvedFuncId) -> Span { self.unresolved_function_spans[id.0].clone() }

    pub fn get_type_span(&self, id: TypeId) -> Span { self.type_spans[id.0].clone() }

    pub fn to_type(&mut self, token: Token, ptr_count: usize) -> TypeId {
        match token.kind {
            TokenKind::Int => self.alloc_type(UnresolvedType::new_int(ptr_count), token.span),
            TokenKind::String => self.alloc_type(UnresolvedType::new_string(ptr_count), token.span),
            TokenKind::Bool => self.alloc_type(UnresolvedType::new_bool(ptr_count), token.span),
            TokenKind::Float => self.alloc_type(UnresolvedType::new_float(ptr_count), token.span),
            TokenKind::Identifier(_) => {
                panic!("[internal hfs error]: this is not how you convert identifiers to types")
            },
            _ => panic!("[internal hfs error]: expected token that has a type, got {:?}", token.kind),
        }
    }
}
