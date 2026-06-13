use std::{collections::HashMap, rc::Rc};

use crate::hfs::{
    ElaboratedType, ScopeKind, Type,
    error::{CompileError, DiagnosticInfo},
    token::*,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct VarId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct FuncId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ExprId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StmtId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId(pub usize);

// ============================================================================
// AST Node Structures
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Identifier {
    GlobalVar(VarId),
    Variable(VarId),
    Function(FuncId),
}

impl Identifier {
    pub fn get_source_info(&self, arena: &AstArena) -> SourceInfo {
        match self {
            Identifier::GlobalVar(var_id) => arena.get_var_token(*var_id).source_info.clone(),
            Identifier::Variable(var_id) => arena.get_var_token(*var_id).source_info.clone(),
            Identifier::Function(func_id) => arena.get_function_token(*func_id).source_info.clone(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    ArrayAccess(ExprId, ExprId),
    Or(ExprId, ExprId),
    And(ExprId, ExprId),
    Not(ExprId),
    AddressOf(ExprId),   // &
    Dereference(ExprId), // ^
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Operation(Operation),
    Identifier(Identifier),
    Literal(Literal),
    Tuple {
        expressions: Vec<ExprId>,
    },
    Parameter {
        index: usize,    // Which parameter (0, 1, 2...)
        type_id: TypeId, // Parameter type
    },
    // type of the temporary returned from a function
    ReturnValue(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TopLevelId {
    VariableDecl(VarId),
    FunctionDecl(FuncId),
    Statement(StmtId),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprProvenance {
    CompiletimeValue, // Constant or literal
    RuntimeValue,     // Needs computation at runtime
}

// ============================================================================
// Declarations (shared)
#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration {
    pub name: String,
    pub hfs_type: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub param_type: TypeId,  // either a tuple or a single type
    pub return_type: TypeId, // either a tuple or a single type
    pub body: StmtId,
    pub parameter_exprs: Vec<ExprId>,
}

#[derive(Debug, Clone)]
pub struct StackKeywordDeclaration<'a> {
    pub name: &'a str,
    pub expected_args_size: Option<usize>,
    pub return_size: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StackKeyword {
    pub name: String,
    pub parameter_exprs: Vec<ExprId>,
    pub param_type: TypeId,
    pub return_type: TypeId,
    pub return_values: Vec<ExprId>,
}

// ============================================================================
// Statements

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ElseIf {
        condition: Vec<StmtId>,
        body: StmtId, // points to BlockScope
        else_stmt: Option<StmtId>,
    },
    Else(StmtId), // Points to a BlockScope
    If {
        condition: Vec<StmtId>, // boolean from the stack or operation
        body: StmtId,           // points to BlockScope
        else_stmt: Option<StmtId>,
    },
    While {
        cond: Vec<StmtId>, // boolean from the stack or operation
        body: StmtId,      // points to BlockScope
    },
    StackBlock {
        expr_ids: Vec<ExprId>,
        consumed_count: usize,
    },
    BlockScope(Vec<TopLevelId>, ScopeKind),
    Return,
    Break,
    Continue,
    Empty,
    ArrayAssignment {
        position: ExprId,
        identifier: Identifier,
        is_move: bool,
        deref_count: usize,
    },
    Assignment {
        identifier: Identifier,
        is_move: bool,
        deref_count: usize,
    },
    FunctionCall {
        arg_count: usize,
        func_id: FuncId,
        is_move: bool,
        return_values: Vec<ExprId>,
    },
}

// ============================================================================
// Arena storage with token tracking
// ============================================================================
#[derive(Debug, Default, Clone)]
pub struct AstArena {
    // AST nodes
    pub(crate) exprs: Vec<Expression>,
    pub(crate) stmts: Vec<Statement>,
    pub(crate) vars: Vec<VarDeclaration>,
    pub(crate) functions: Vec<FunctionDeclaration>,

    pub(crate) types: Vec<ElaboratedType>,

    // Token storage (parallel arrays)
    pub(crate) expr_tokens: Vec<Token>,
    pub(crate) stmt_tokens: Vec<Token>,
    pub(crate) var_tokens: Vec<Token>,
    pub(crate) function_tokens: Vec<Token>,
    pub(crate) type_tokens: Vec<Token>,

    pub(crate) hfs_stack: Vec<ExprId>, // keeps track of the state of our stack

    // Type deduplication cache
    type_cache: HashMap<ElaboratedType, TypeId>,
    // compile-time analysis
    // expr provenances shouldn't change after creation
    pub expr_provenances: Vec<ExprProvenance>, // indexed by ExprId
    // variables should continuously change their provenance for analysis
    pub curr_var_provenances: Vec<ExprProvenance>,       // indexed by VarId
    pub curr_func_call_provenances: Vec<ExprProvenance>, // indexed by FuncId

    pub diagnostic_info: Rc<DiagnosticInfo>,
}

impl PartialEq for AstArena {
    fn eq(&self, other: &Self) -> bool {
        self.exprs == other.exprs
            && self.stmts == other.stmts
            && self.vars == other.vars
            && self.functions == other.functions
            && self.types == other.types
    }
}

// had to move this here because i wanted to the arena's private members to be available to the parser
// for better code structure (without making arena members public)
impl AstArena {
    pub fn new(diagnostic_info: Rc<DiagnosticInfo>) -> Self {
        let mut arena = Self::default();
        arena.diagnostic_info = diagnostic_info;
        arena.alloc_type_uncached(ElaboratedType::new_int(0), Token {
            kind: TokenKind::Int,
            source_info: SourceInfo::new(0, 0, 0),
        });
        arena.alloc_type_uncached(ElaboratedType::new_float(0), Token {
            kind: TokenKind::Float,
            source_info: SourceInfo::new(0, 0, 0),
        });
        arena.alloc_type_uncached(ElaboratedType::new_bool(0), Token {
            kind: TokenKind::Bool,
            source_info: SourceInfo::new(0, 0, 0),
        });
        arena.alloc_type_uncached(ElaboratedType::new_string(0), Token {
            kind: TokenKind::String,
            source_info: SourceInfo::new(0, 0, 0),
        });
        arena
    }

    pub fn alloc_and_push_to_hfs_stack(&mut self, expr: Expression, provenance: ExprProvenance, token: Token) -> ExprId {
        // theres no reason to not push to the stack when making a new expression
        // so this is the only method available
        let id = ExprId(self.exprs.len());
        self.exprs.push(expr);
        self.hfs_stack.push(id);
        self.expr_provenances.push(provenance);
        self.expr_tokens.push(token);
        id
    }

    pub fn alloc_stmt(&mut self, stmt: Statement, token: Token) -> StmtId {
        let id = StmtId(self.stmts.len());
        self.stmts.push(stmt);
        self.stmt_tokens.push(token);
        id
    }

    pub fn alloc_var(&mut self, var: VarDeclaration, token: Token) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(var);
        self.curr_var_provenances.push(ExprProvenance::CompiletimeValue);
        self.var_tokens.push(token);
        // by default, variables start as compile time variables.
        // this is changed throughout semantic analysis, they could become runtime
        // if they are assigned a RuntimeValue (they can regain compile time if we reassign them)
        // its to track variable state for semantic analysis
        id
    }

    pub fn alloc_function(&mut self, func: FunctionDeclaration, token: Token) -> FuncId {
        let id = FuncId(self.functions.len());
        self.functions.push(func);
        // we assume a function call is runtime by default (im not gonna invest too much on this)
        // this isnt really used extensively, its here to match variables if we want it later for
        // compile-time analysis of functions and other things
        self.curr_func_call_provenances.push(ExprProvenance::RuntimeValue);
        self.function_tokens.push(token);
        id
    }

    // Internal: allocate without checking cache
    fn alloc_type_uncached(&mut self, hfs_type: ElaboratedType, token: Token) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(hfs_type.clone());
        self.type_tokens.push(token);
        self.type_cache.insert(hfs_type, id);
        id
    }
    pub fn alloc_type(&mut self, hfs_type: ElaboratedType, token: Token) -> TypeId {
        // Check if this type already exists
        if let Some(&existing_id) = self.type_cache.get(&hfs_type) {
            return existing_id;
        }
        // If not, allocate it
        self.alloc_type_uncached(hfs_type, token)
    }
    pub fn get_stack_change(&self, stack_start: Vec<ExprId>, mut hfs_stack: Vec<ExprId>) -> (Vec<ExprId>, usize) {
        let common_prefix = hfs_stack.iter().zip(&stack_start).take_while(|(a, b)| a == b).count();
        hfs_stack.drain(0..common_prefix);
        let consumed_count = stack_start.len() - common_prefix;
        (hfs_stack, consumed_count)
    }
    //---------------------------------------------------------------------------------
    // provenance methods
    pub fn get_expr_provenance(&self, expr: ExprId) -> &ExprProvenance { &self.expr_provenances[expr.0] }
    pub fn get_identifier_provenance(&self, id: Identifier) -> &ExprProvenance {
        match id {
            Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => &self.curr_var_provenances[var_id.0],
            Identifier::Function(func_id) => &self.curr_func_call_provenances[func_id.0],
        }
    }
    //---------------------------------------------------------------------------------
    // Immutable accessor methods
    pub fn get_expr(&self, id: ExprId) -> &Expression { &self.exprs[id.0] }
    pub fn get_stmt(&self, id: StmtId) -> &Statement { &self.stmts[id.0] }
    pub fn get_var(&self, id: VarId) -> &VarDeclaration { &self.vars[id.0] }
    pub fn get_func(&self, id: FuncId) -> &FunctionDeclaration { &self.functions[id.0] }
    pub fn get_type(&self, id: TypeId) -> &ElaboratedType { &self.types[id.0] }

    pub fn get_stack_keyword_from_name(&self, name: &str) -> &StackKeywordDeclaration<'_> {
        if let Some(keyword) = STACK_KEYWORDS.iter().find(|keyword| keyword.name == name) {
            keyword
        } else {
            panic!("[internal error] invalid stack keyword (lexer resolves this)");
        }
    }

    // Mutable accessor methods
    pub fn get_expr_mut(&mut self, id: ExprId) -> &mut Expression { &mut self.exprs[id.0] }
    pub fn get_stmt_mut(&mut self, id: StmtId) -> &mut Statement { &mut self.stmts[id.0] }
    pub fn get_var_mut(&mut self, id: VarId) -> &mut VarDeclaration { &mut self.vars[id.0] }
    pub fn get_func_mut(&mut self, id: FuncId) -> &mut FunctionDeclaration { &mut self.functions[id.0] }

    // Token accessor methods
    pub fn get_expr_token(&self, id: ExprId) -> &Token { &self.expr_tokens[id.0] }

    pub fn get_stmt_token(&self, id: StmtId) -> &Token { &self.stmt_tokens[id.0] }

    pub fn get_var_token(&self, id: VarId) -> &Token { &self.var_tokens[id.0] }

    pub fn get_function_token(&self, id: FuncId) -> &Token { &self.function_tokens[id.0] }

    pub fn get_type_token(&self, id: TypeId) -> &Token { &self.type_tokens[id.0] }

    pub fn to_type(&mut self, token: Token) -> TypeId {
        match token.kind {
            TokenKind::Int => self.alloc_type(ElaboratedType::new_int(0), token),
            TokenKind::String => self.alloc_type(ElaboratedType::new_string(0), token),
            TokenKind::Bool => self.alloc_type(ElaboratedType::new_bool(0), token),
            TokenKind::Float => self.alloc_type(ElaboratedType::new_float(0), token),
            TokenKind::Identifier(_) => {
                panic!("[internal hfs error]: this is not how you convert identifiers to types")
            },
            _ => panic!("[internal hfs error]: expected token that has a type, got {:?}", token.kind),
        }
    }
}

impl AstArena {
    // Only expressions have types!
    pub fn get_type_of_operation(&mut self, op: &Operation) -> Result<TypeId, Box<dyn CompileError>> {
        match op {
            Operation::Add(lhs, _rhs)
            | Operation::Sub(lhs, _rhs)
            | Operation::Mul(lhs, _rhs)
            | Operation::Div(lhs, _rhs)
            | Operation::Mod(lhs, _rhs) => {
                let lhs_type = self.get_type_id_of_expr(*lhs)?;
                Ok(lhs_type)
            },
            Operation::Or(_, _)
            | Operation::And(_, _)
            | Operation::Not(_)
            | Operation::Equal(_, _)
            | Operation::NotEqual(_, _)
            | Operation::Less(_, _)
            | Operation::LessEqual(_, _)
            | Operation::Greater(_, _)
            | Operation::GreaterEqual(_, _) => Ok(ElaboratedType::new_bool(0).type_id()),
            Operation::AddressOf(_) => todo!(),
            Operation::Dereference(_) => todo!(),
            Operation::ArrayAccess(array, ..) => Ok(match self.get_type_of_expr(*array)? {
                ElaboratedType::Array { hfs_type, .. } => *hfs_type,
                _ => {
                    panic!("[internal error] trying to index into a non-array after typechecking")
                },
            }),
        }
    }

    pub fn get_type_id_of_expr(&mut self, expr_id: ExprId) -> Result<TypeId, Box<dyn CompileError>> {
        match self.get_expr(expr_id).clone() {
            Expression::Operation(operation) => Ok(self.get_type_of_operation(&operation)?),
            Expression::Identifier(identifier) => match identifier {
                Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                    let var = self.get_var(var_id);
                    Ok(var.hfs_type)
                },
                Identifier::Function(func_id) => {
                    let func = self.get_func(func_id);
                    Ok(func.return_type)
                },
            },
            Expression::Literal(literal) => match literal {
                Literal::Integer(_) => Ok(ElaboratedType::new_int(0).type_id()),
                Literal::Float(_) => Ok(ElaboratedType::new_float(0).type_id()),
                Literal::String(_) => Ok(ElaboratedType::new_string(0).type_id()),
                Literal::Bool(_) => Ok(ElaboratedType::new_bool(0).type_id()),
            },
            Expression::Tuple { expressions } => {
                let token = self.get_expr_token(expr_id).clone();
                // Build tuple type from element types
                let mut element_types = Vec::new();
                for expr_id in expressions.clone() {
                    let elem_type = self.get_type_id_of_expr(expr_id)?;
                    element_types.push(elem_type);
                }

                let tuple_type = ElaboratedType::Tuple { type_ids: element_types, ptr_count: 0 };
                Ok(self.alloc_type(tuple_type, token))
            },
            Expression::Parameter { index: _, type_id } => Ok(type_id),
            Expression::ReturnValue(type_id) => Ok(type_id),
        }
    }
    pub fn get_type_of_var(&self, var_id: VarId) -> &ElaboratedType { self.get_type(self.get_var(var_id).hfs_type) }
    pub fn get_type_of_func(&self, func_id: FuncId) -> &ElaboratedType { self.get_type(self.get_func(func_id).return_type) }

    pub fn get_type_of_expr(&mut self, expr_id: ExprId) -> Result<&ElaboratedType, Box<dyn CompileError>> {
        let type_id = self.get_type_id_of_expr(expr_id)?;
        Ok(self.get_type(type_id))
    }
}

// Grabs a signature with the syntax (a b) -> (a b c) and creates a lambda that performs that operation with a vector of types
#[macro_export]
macro_rules! effect {
    (_) => {
        |_: Vec<Expression>| -> Vec<Expression> {
            vec![]
        }
    };

    (($($arg:ident)*) -> ($($return:ident)*)) => {
        |types: Vec<Expression>| -> Vec<Expression> {
            let mut _idx = 0;
            $(
                let $arg = types[_idx].clone();
                _idx += 1;
            )*

            vec![$(Expression::ReturnValue($return.clone())),*]
        }
    };
}

// ============================================================================
// Type-safe ID types
// ============================================================================

const STACK_KEYWORDS: &[StackKeywordDeclaration] = &[
    StackKeywordDeclaration { name: "@pop", expected_args_size: Some(1), return_size: 0 },
    StackKeywordDeclaration { name: "@pop_all", expected_args_size: None, return_size: 0 },
    StackKeywordDeclaration { name: "@dup", expected_args_size: Some(1), return_size: 2 },
    StackKeywordDeclaration { name: "@swap", expected_args_size: Some(2), return_size: 2 },
    StackKeywordDeclaration { name: "@over", expected_args_size: Some(2), return_size: 3 },
    StackKeywordDeclaration { name: "@rot", expected_args_size: Some(3), return_size: 3 },
    StackKeywordDeclaration { name: "@rrot", expected_args_size: Some(3), return_size: 3 },
    StackKeywordDeclaration { name: "@nip", expected_args_size: Some(2), return_size: 1 },
    StackKeywordDeclaration { name: "@tuck", expected_args_size: Some(2), return_size: 3 },
    StackKeywordDeclaration { name: "@print", expected_args_size: Some(1), return_size: 1 },
];
