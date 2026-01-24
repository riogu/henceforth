use crate::hfs::{
    self,
    builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
    Literal, ScopeKind, SourceInfo, Token, Type, TypeId, UnresolvedAstArena, UnresolvedExprId, UnresolvedExpression,
    UnresolvedFunctionDeclaration, UnresolvedStatement, UnresolvedStmtId, UnresolvedTopLevelId, UnresolvedVarDeclaration,
};

pub struct ParserBuilder {
    arena: UnresolvedAstArena,
    current_function: Option<FunctionContext>,
    stack_block_exprs: Vec<UnresolvedExprId>,
    context_stack: Vec<BuilderContext>,
}

pub enum UnresolvedStmtOrExpr {
    Expr(UnresolvedExpression),
    Stmt(UnresolvedStatement),
}

struct FunctionContext {
    name: String,
    param_type: TypeId,
    return_type: TypeId,
}

enum BuilderContext {
    WhileLoop { cond: UnresolvedStmtId },
    IfStatement { cond: UnresolvedStmtId },
    StackBlock,
    BlockScope { items: Vec<UnresolvedTopLevelId>, scope_kind: ScopeKind },
}

impl ParserBuilder {
    fn dummy_token() -> Token {
        Token { kind: crate::hfs::TokenKind::Let, source_info: SourceInfo::new(0, 0, 0) }
    }

    fn types_to_tuple_id(&mut self, types: Vec<Type>) -> TypeId {
        if types.is_empty() {
            self.arena.alloc_type(Type::Tuple(vec![]), Self::dummy_token())
        } else if types.len() == 1 {
            self.type_to_id(types[0].clone())
        } else {
            let type_ids: Vec<TypeId> = types.into_iter().map(|ty| self.type_to_id(ty)).collect();
            self.arena.alloc_type(Type::Tuple(type_ids), Self::dummy_token())
        }
    }

    fn type_to_id(&mut self, typename: Type) -> TypeId {
        self.arena.to_type(Token::new(typename.to_token(), SourceInfo::new(0, 0, 0)))
    }
}

impl Builder<UnresolvedStmtOrExpr> for ParserBuilder {
    type Built = UnresolvedAstArena;
    fn new() -> Self {
        ParserBuilder {
            arena: UnresolvedAstArena::new(),
            current_function: None,
            stack_block_exprs: Vec::new(),
            context_stack: Vec::new(),
        }
    }

    fn push(mut self, elem: UnresolvedStmtOrExpr) -> Self {
        match elem {
            UnresolvedStmtOrExpr::Expr(unresolved_expression) => {
                self.arena.unresolved_exprs.push(unresolved_expression);
                self.arena.unresolved_expr_tokens.push(Self::dummy_token());
            },
            UnresolvedStmtOrExpr::Stmt(unresolved_statement) => {
                self.arena.unresolved_stmts.push(unresolved_statement);
                self.arena.unresolved_stmt_tokens.push(Self::dummy_token());
            },
        }
        self
    }

    fn push_many(mut self, elems: Vec<UnresolvedStmtOrExpr>) -> Self {
        for elem in elems {
            self = self.push(elem);
        }
        self
    }

    fn build(self) -> UnresolvedAstArena {
        self.arena
    }
}

impl VariableOps for ParserBuilder {
    fn variable(mut self, name: &str, typename: Type) -> Self {
        let type_id = self.arena.to_type(Token::new(typename.to_token(), SourceInfo::new(0, 0, 0)));
        let var_decl = UnresolvedVarDeclaration { name: name.to_string(), hfs_type: type_id };
        let var_id = self.arena.alloc_unresolved_var(var_decl, Self::dummy_token());

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(UnresolvedTopLevelId::VariableDecl(var_id));
        }

        self
    }

    fn assign_to(mut self, name: &str, mode: PassMode) -> Self {
        let identifier =
            self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name.to_string()), Self::dummy_token());

        let is_move = matches!(mode, PassMode::Move);
        let stmt = UnresolvedStatement::Assignment { identifier, is_move };
        let stmt_id = self.arena.alloc_unresolved_stmt(stmt, Self::dummy_token());

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(UnresolvedTopLevelId::Statement(stmt_id));
        }

        self
    }
}

impl LoopOps for ParserBuilder {
    fn while_loop(mut self) -> Self {
        let cond = UnresolvedStmtId(self.arena.unresolved_stmts.len().saturating_sub(1));
        self.context_stack.push(BuilderContext::WhileLoop { cond });
        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::WhileLoop });

        self
    }
}

impl ControlFlowOps for ParserBuilder {
    fn if_statement(mut self) -> Self {
        let cond = UnresolvedStmtId(self.arena.unresolved_stmts.len().saturating_sub(1));
        self.context_stack.push(BuilderContext::IfStatement { cond });
        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::IfStmt });

        self
    }

    fn elif_statement(mut self) -> Self {
        let prev_block = self.context_stack.pop();
        let cond = UnresolvedStmtId(self.arena.unresolved_stmts.len().saturating_sub(1));

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

impl StackOps for ParserBuilder {
    fn stack_block(mut self) -> Self {
        self.context_stack.push(BuilderContext::StackBlock);
        self.stack_block_exprs = Vec::new();
        self
    }

    fn end_stack_block(mut self, semicolon: bool) -> Self {
        if let Some(BuilderContext::StackBlock) = self.context_stack.pop() {
            let exprs = std::mem::take(&mut self.stack_block_exprs);
            let stmt = UnresolvedStatement::StackBlock(exprs);
            let stmt_id = self.arena.alloc_unresolved_stmt(stmt, Self::dummy_token());

            if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
                items.push(UnresolvedTopLevelId::Statement(stmt_id));
            }
            if semicolon {
                self.arena.alloc_unresolved_stmt(UnresolvedStatement::Empty, Self::dummy_token());
            }
        }
        self
    }

    fn push_literal<L: Into<Literal>>(mut self, lit: L) -> Self {
        let expr = UnresolvedExpression::Literal(lit.into());
        let expr_id = self.arena.alloc_unresolved_expr(expr, Self::dummy_token());

        self.stack_block_exprs.push(expr_id);
        self
    }

    fn push_operation(mut self, op: BuilderOperation) -> Self {
        let unresolved_op = op.to_unresolved_op();
        let expr = UnresolvedExpression::Operation(unresolved_op);
        let expr_id = self.arena.alloc_unresolved_expr(expr, Self::dummy_token());

        self.stack_block_exprs.push(expr_id);
        self
    }

    fn push_stack_keyword(mut self, keyword: &str, semicolon: bool) -> Self {
        let expr = UnresolvedExpression::StackKeyword(keyword.to_string());
        let expr_id = self.arena.alloc_unresolved_expr(expr, Self::dummy_token());
        if semicolon {
            self.arena.alloc_unresolved_stmt(UnresolvedStatement::Empty, Self::dummy_token());
        }

        self.stack_block_exprs.push(expr_id);
        self
    }

    fn push_variable(mut self, name: &str) -> Self {
        let expr = UnresolvedExpression::Identifier(name.to_string());
        let expr_id = self.arena.alloc_unresolved_expr(expr, Self::dummy_token());

        self.stack_block_exprs.push(expr_id);
        self
    }
}

impl FunctionOps for ParserBuilder {
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
        let param_type = self.types_to_tuple_id(args.unwrap_or_default());
        let ret_type = self.types_to_tuple_id(return_type.unwrap_or_default());

        self.current_function = Some(FunctionContext { name: name.to_string(), param_type, return_type: ret_type });

        self.context_stack.push(BuilderContext::BlockScope { items: Vec::new(), scope_kind: ScopeKind::Function });

        self
    }

    fn body(self) -> Self {
        self
    }

    fn end_body(mut self) -> Self {
        if let Some(func) = self.current_function.take() {
            if let Some(BuilderContext::BlockScope { items, scope_kind }) = self.context_stack.pop() {
                let body =
                    self.arena.alloc_unresolved_stmt(UnresolvedStatement::BlockScope(items, scope_kind), Self::dummy_token());

                let func_decl = UnresolvedFunctionDeclaration {
                    name: func.name,
                    param_type: func.param_type,
                    return_type: func.return_type,
                    body,
                };

                self.arena.alloc_unresolved_function(func_decl, Self::dummy_token());
            }
        }
        self
    }

    fn call_function(mut self, name: &str, mode: PassMode) -> Self {
        let identifier =
            self.arena.alloc_unresolved_expr(UnresolvedExpression::Identifier(name.to_string()), Self::dummy_token());

        let is_move = matches!(mode, PassMode::Move);
        let stmt = UnresolvedStatement::FunctionCall { identifier, is_move };
        let stmt_id = self.arena.alloc_unresolved_stmt(stmt, Self::dummy_token());

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(UnresolvedTopLevelId::Statement(stmt_id));
        }

        self
    }

    fn return_statement(mut self) -> Self {
        let stmt = UnresolvedStatement::Return;
        let stmt_id = self.arena.alloc_unresolved_stmt(stmt, Self::dummy_token());

        if let Some(BuilderContext::BlockScope { items, .. }) = self.context_stack.last_mut() {
            items.push(UnresolvedTopLevelId::Statement(stmt_id));
        }

        self
    }
}
