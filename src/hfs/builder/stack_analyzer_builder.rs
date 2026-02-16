use std::collections::HashMap;

use crate::hfs::{
    AstArena, ExprId, ExprProvenance, Expression, FuncId, FunctionDeclaration, Identifier, Literal, Operation, ScopeKind,
    SourceInfo, StackKeyword, Statement, StmtId, Token, TokenKind, TopLevelId, Type, TypeId, VarDeclaration, VarId,
    builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
};

pub struct StackAnalyzerBuilder {
    arena: AstArena,
    current_function: Option<FunctionContext>,
    stack_block_exprs: Vec<ExprId>,
    context_stack: Vec<BuilderContext>,
    variable_scopes: Vec<HashMap<String, VarId>>,
    function_scope: HashMap<String, FuncId>,
}

impl StackAnalyzerBuilder {
    fn dummy_token() -> Token {
        Token { kind: TokenKind::Let, source_info: SourceInfo::new(0, 0, 0) }
    }

    fn types_to_tuple_id(&mut self, types: Vec<Type>) -> TypeId {
        let type_ids = types.into_iter().map(|ty| self.type_to_id(ty)).collect();
        self.arena.alloc_type(Type::Tuple { type_ids, ptr_count: 0 }, Self::dummy_token())
    }

    fn type_to_id(&mut self, typename: Type) -> TypeId {
        self.arena.to_type(Token::new(typename.to_token(), SourceInfo::new(0, 0, 0)))
    }

    fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.variable_scopes.pop();
    }

    fn add_variable(&mut self, name: &str, var_id: VarId) {
        if let Some(scope) = self.variable_scopes.last_mut() {
            scope.insert(name.to_string(), var_id);
        }
    }

    fn add_function(&mut self, name: &str, func_id: FuncId) {
        self.function_scope.insert(name.to_string(), func_id);
    }

    fn find_variable(&self, name: &str) -> VarId {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(&var_id) = scope.get(name) {
                return var_id;
            }
        }
        VarId(0)
    }

    fn find_function(&self, name: &str) -> FuncId {
        self.function_scope.get(name).copied().unwrap_or(FuncId(0))
    }
}

pub enum StmtOrExpr {
    Expr(Expression),
    Stmt(Statement),
}

struct FunctionContext {
    name: String,
    param_type: TypeId,
    return_type: TypeId,
    body_stmt_id: Option<StmtId>,
}

enum BuilderContext {
    WhileLoop { cond: StmtId },
    IfStatement { cond: StmtId },
    StackBlock,
    BlockScope { items: Vec<TopLevelId>, scope_kind: ScopeKind },
}

impl Builder<StmtOrExpr> for StackAnalyzerBuilder {
    type Built = AstArena;

    fn new() -> Self {
        StackAnalyzerBuilder {
            arena: AstArena::new(),
            current_function: None,
            stack_block_exprs: Vec::new(),
            context_stack: Vec::new(),
            variable_scopes: vec![HashMap::new()],
            function_scope: HashMap::new(),
        }
    }

    fn push(mut self, elem: StmtOrExpr) -> Self {
        match elem {
            StmtOrExpr::Expr(expression) => {
                self.arena.exprs.push(expression);
                self.arena.expr_tokens.push(Self::dummy_token());
            },
            StmtOrExpr::Stmt(statement) => {
                self.arena.stmts.push(statement);
                self.arena.stmt_tokens.push(Self::dummy_token());
            },
        }
        self
    }

    fn push_many(mut self, elems: Vec<StmtOrExpr>) -> Self {
        for elem in elems {
            self = self.push(elem);
        }
        self
    }

    fn build(self) -> Self::Built {
        self.arena
    }
}

impl VariableOps for StackAnalyzerBuilder {
    fn variable(mut self, name: &str, typename: Type) -> Self {
        let type_id = self.arena.to_type(Token::new(typename.to_token(), SourceInfo::new(0, 0, 0)));
        let var_decl = VarDeclaration { name: name.to_string(), hfs_type: type_id };
        let token = Self::dummy_token();
        let var_id = self.arena.alloc_var(var_decl, token.clone());
        self.arena.var_tokens.push(token);

        self.add_variable(name, var_id);

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(TopLevelId::VariableDecl(var_id));
        }

        self
    }

    fn assign_to(mut self, name: &str, mode: PassMode) -> Self {
        let var_id = self.find_variable(name);
        let identifier = Identifier::Variable(var_id);

        let is_move = matches!(mode, PassMode::Move);
        let stmt = Statement::Assignment { identifier, is_move, deref_count: todo!("joao this needs a real value") };
        let token = Self::dummy_token();
        let stmt_id = self.arena.alloc_stmt(stmt, token.clone());
        self.arena.stmt_tokens.push(token);

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(TopLevelId::Statement(stmt_id));
        }

        self
    }
}

impl StackOps for StackAnalyzerBuilder {
    fn stack_block(mut self) -> Self {
        self.context_stack.push(BuilderContext::StackBlock);
        self.stack_block_exprs = Vec::new();
        self
    }

    fn end_stack_block(mut self, semicolon: bool) -> Self {
        if let Some(BuilderContext::StackBlock) = self.context_stack.pop() {
            let exprs = std::mem::take(&mut self.stack_block_exprs);
            let stmt = Statement::StackBlock(exprs);
            let stmt_id = self.arena.alloc_stmt(stmt, Self::dummy_token());

            if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
                items.push(TopLevelId::Statement(stmt_id));
                if semicolon {
                    let id = self.arena.alloc_stmt(Statement::Empty, Self::dummy_token());
                    items.push(TopLevelId::Statement(id));
                }
            }
        }
        self
    }

    fn push_literal<L: Into<Literal>>(mut self, lit: L) -> Self {
        let expr = Expression::Literal(lit.into());
        let expr_id = ExprId(self.arena.exprs.len());
        self.arena.exprs.push(expr);
        self.arena.expr_tokens.push(Self::dummy_token());
        self.arena.expr_provenances.push(ExprProvenance::CompiletimeValue);

        self.stack_block_exprs.push(expr_id);
        self
    }

    fn push_operation(mut self, op: BuilderOperation) -> Self {
        let operation = match op {
            BuilderOperation::Add => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for add");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for add");
                Operation::Add(lhs, rhs)
            },
            BuilderOperation::Subtract => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for sub");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for sub");
                Operation::Sub(lhs, rhs)
            },
            BuilderOperation::Multiply => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for mul");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for mul");
                Operation::Mul(lhs, rhs)
            },
            BuilderOperation::Divide => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for div");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for div");
                Operation::Div(lhs, rhs)
            },
            BuilderOperation::Modulo => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for mod");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for mod");
                Operation::Mod(lhs, rhs)
            },
            BuilderOperation::Equals => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for equal");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for equal");
                Operation::Equal(lhs, rhs)
            },
            BuilderOperation::NotEquals => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for not equal");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for not equal");
                Operation::NotEqual(lhs, rhs)
            },
            BuilderOperation::LessThan => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for less");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for less");
                Operation::Less(lhs, rhs)
            },
            BuilderOperation::LessThanEq => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for less equal");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for less equal");
                Operation::LessEqual(lhs, rhs)
            },
            BuilderOperation::GreaterThan => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for greater");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for greater");
                Operation::Greater(lhs, rhs)
            },
            BuilderOperation::GreaterThanEq => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for greater equal");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for greater equal");
                Operation::GreaterEqual(lhs, rhs)
            },
            BuilderOperation::And => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for and");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for and");
                Operation::And(lhs, rhs)
            },
            BuilderOperation::Or => {
                let rhs = self.stack_block_exprs.pop().expect("need rhs for or");
                let lhs = self.stack_block_exprs.pop().expect("need lhs for or");
                Operation::Or(lhs, rhs)
            },
            BuilderOperation::Not => {
                let expr = self.stack_block_exprs.pop().expect("need expr for not");
                Operation::Not(expr)
            },
        };

        let expr = Expression::Operation(operation);
        let expr_id = ExprId(self.arena.exprs.len());
        self.arena.exprs.push(expr);
        self.arena.expr_tokens.push(Self::dummy_token());
        self.arena.expr_provenances.push(ExprProvenance::RuntimeValue);

        self.stack_block_exprs.push(expr_id);
        self
    }

    fn push_stack_keyword(mut self, keyword: &str, semicolon: bool) -> Self {
        let stack_keyword = StackKeyword {
            name: keyword.to_string(),
            parameter_exprs: todo!(),
            param_type: todo!(),
            return_type: todo!(),
            return_values: todo!(),
        };
        let expr = Expression::StackKeyword(stack_keyword);
        let expr_id = ExprId(self.arena.exprs.len());
        self.arena.exprs.push(expr);
        self.arena.expr_tokens.push(Self::dummy_token());
        self.arena.expr_provenances.push(ExprProvenance::RuntimeValue);

        if semicolon {
            self.arena.alloc_stmt(Statement::Empty, Self::dummy_token());
        }

        self.stack_block_exprs.push(expr_id);
        self
    }

    fn push_variable(mut self, name: &str) -> Self {
        let var_id = self.find_variable(name);
        let identifier = Identifier::Variable(var_id);
        let expr = Expression::Identifier(identifier);
        let expr_id = ExprId(self.arena.exprs.len());
        self.arena.exprs.push(expr);
        self.arena.expr_tokens.push(Self::dummy_token());
        self.arena.expr_provenances.push(ExprProvenance::RuntimeValue);

        self.stack_block_exprs.push(expr_id);
        self
    }
}

impl FunctionOps for StackAnalyzerBuilder {
    fn args(mut self, args: Option<Vec<Type>>) -> Self {
        let param_type = self.types_to_tuple_id(args.unwrap_or_default());

        if let Some(func) = self.current_function.as_mut() {
            func.param_type = param_type;
        }

        self
    }

    fn return_types(mut self, return_types: Option<Vec<Type>>) -> Self {
        let return_type = self.types_to_tuple_id(return_types.unwrap_or_default());

        if let Some(func) = self.current_function.as_mut() {
            func.return_type = return_type;
        }

        self
    }

    fn func_with(mut self, name: &str, args: Option<Vec<Type>>, return_type: Option<Vec<Type>>) -> Self {
        self.current_function =
            Some(FunctionContext { name: name.to_string(), param_type: TypeId(69), return_type: TypeId(69), body_stmt_id: None });

        let param_type = self.args(args);
        self = param_type;
        let ret_type = self.return_types(return_type);
        self = ret_type;

        self.push_scope();

        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::Function });

        self
    }

    fn body(mut self) -> Self {
        self
    }

    fn end_body(mut self) -> Self {
        if let Some(func) = self.current_function.take() {
            if let Some(BuilderContext::BlockScope { items, scope_kind }) = self.context_stack.pop() {
                let body = self.arena.alloc_stmt(Statement::BlockScope(items, scope_kind), Self::dummy_token());

                let func_decl = FunctionDeclaration {
                    name: func.name.clone(),
                    param_type: func.param_type,
                    return_type: func.return_type,
                    body,
                    parameter_exprs: Vec::new(),
                };

                let func_id = self.arena.alloc_function(func_decl, Self::dummy_token());

                self.add_function(&func.name, func_id);

                self.pop_scope();
            }
        }
        self
    }

    fn call_function(mut self, name: &str, mode: PassMode) -> Self {
        let func_id = self.find_function(name);

        let arg_count = if func_id.0 < self.arena.functions.len() {
            let func = &self.arena.functions[func_id.0];
            if let Type::Tuple { type_ids: param_types, .. } = self.arena.get_type(func.param_type) {
                param_types.len()
            } else {
                0
            }
        } else {
            0
        };

        let is_move = matches!(mode, PassMode::Move);
        // let stmt = Statement::FunctionCall { arg_count, func_id, is_move };
        let stmt = todo!("sorry joao i broke your testing code with ast changes");
        let token = Self::dummy_token();
        let stmt_id = self.arena.alloc_stmt(stmt, token.clone());
        self.arena.stmt_tokens.push(token);

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(TopLevelId::Statement(stmt_id));
        }

        self
    }

    fn return_statement(mut self) -> Self {
        let stmt = Statement::Return;
        let token = Self::dummy_token();
        let stmt_id = self.arena.alloc_stmt(stmt, token.clone());
        self.arena.stmt_tokens.push(token);

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(TopLevelId::Statement(stmt_id));
        }

        self
    }
}

impl LoopOps for StackAnalyzerBuilder {
    fn while_loop(mut self) -> Self {
        let cond = StmtId(self.arena.stmts.len().saturating_sub(1));
        self.context_stack.push(BuilderContext::WhileLoop { cond });
        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::WhileLoop });

        self
    }
}

impl ControlFlowOps for StackAnalyzerBuilder {
    fn if_statement(mut self) -> Self {
        let cond = StmtId(self.arena.stmts.len().saturating_sub(1));
        self.context_stack.push(BuilderContext::IfStatement { cond });
        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::IfStmt });

        self
    }

    fn elif_statement(mut self) -> Self {
        let _prev_block = self.context_stack.pop();
        let cond = StmtId(self.arena.stmts.len().saturating_sub(1));

        self.context_stack.push(BuilderContext::IfStatement { cond });
        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::IfStmt });

        self
    }

    fn else_statement(mut self) -> Self {
        let _prev_block = self.context_stack.pop();
        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::IfStmt });

        self
    }
}
