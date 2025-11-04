use crate::hfs::{token::*, ScopeKind};
use std::collections::HashMap;

// ============================================================================
// Type-safe ID types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

// ============================================================================
// AST Node Structures
// ============================================================================

#[derive(Debug, Clone, Copy)]
pub enum Identifier {
    GlobalVar(VarId),
    Variable(VarId),
    Function(FuncId),
}
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub enum Expression {
    Operation(Operation),
    Identifier(Identifier),
    Literal(Literal),
    Tuple { expressions: Vec<ExprId> },
    Parameter(TypeId),   // will be converted into another expression
    ReturnValue(TypeId), // we replace this ExprId with another one when computed
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

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub param_type: TypeId,  // either a tuple or a single type
    pub return_type: TypeId, // either a tuple or a single type
    pub body: StmtId,
    pub params: Vec<ExprId>, // Vec<Expression::Parameter>
}

// ============================================================================
// Statements

#[derive(Debug)]
pub enum ElseStmt {
    ElseIf(StmtId), // Points to an IfStmt
    Else(StmtId),   // Points to a BlockScope
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
    BlockScope(Vec<TopLevelId>, ScopeKind),
    Return,
    Break,
    Continue,
    Empty,
    Assignment {
        value: ExprId,
        identifier: ExprId,
        is_move: bool,
    },
    FunctionCall {
        args: Vec<ExprId>,
        identifier: FuncId,
        return_values: Vec<ExprId>,
        is_move: bool,
    },
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

    // Type deduplication cache
    type_cache: HashMap<Type, TypeId>,
}

// had to move this here because i wanted to the arena's private members to be available to the parser
// for better code structure (without making arena members public)
impl<'a> AstArena<'a> {
    pub fn new() -> Self {
        let mut arena = Self::default();
        arena.alloc_type_uncached(
            Type::Int,
            Token {
                kind: TokenKind::Int,
                source_info: SourceInfo::new(0, 0, 0, "Int"),
            },
        );
        arena.alloc_type_uncached(
            Type::Float,
            Token {
                kind: TokenKind::Float,
                source_info: SourceInfo::new(0, 0, 0, "Float"),
            },
        );
        arena.alloc_type_uncached(
            Type::Bool,
            Token {
                kind: TokenKind::Bool,
                source_info: SourceInfo::new(0, 0, 0, "Bool"),
            },
        );
        arena.alloc_type_uncached(
            Type::String,
            Token {
                kind: TokenKind::String,
                source_info: SourceInfo::new(0, 0, 0, "String"),
            },
        );
        arena
    }

    pub fn alloc_and_push_to_hfs_stack(&mut self, expr: Expression, token: Token<'a>) -> ExprId {
        // theres no reason to not push to the stack when making a new expression
        // so this is the only method available
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        self.hfs_stack.push(id);
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

    // Internal: allocate without checking cache
    fn alloc_type_uncached(&mut self, hfs_type: Type, token: Token<'a>) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_tokens.push(token);
        self.type_cache.insert(hfs_type, id);
        id
    }
    pub fn alloc_type(&mut self, hfs_type: Type, token: Token<'a>) -> TypeId {
        // Check if this type already exists
        if let Some(&existing_id) = self.type_cache.get(&hfs_type) {
            return existing_id;
        }

        // If not, allocate it
        self.alloc_type_uncached(hfs_type, token)
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

    pub fn get_type_token(&self, id: TypeId) -> &Token<'a> {
        &self.type_tokens[id.0]
    }
}
