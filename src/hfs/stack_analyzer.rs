use super::*;
// Expression stack methods (used to finish building up the AST)
// we need to make sure expressions have been pushed throughout the program
// to do this, we keep track of which expressions have been pushed
// (aka, the contents of a `@(123 123 123)` stack block along the program 
// note that if you just wanted to interpret the language, we could already be done.
// this is 'basically' interpreting, but only at the level of "is it in the stack or not"
// the goal is to make Henceforth a compiled language, therefore we do multiple stack passes
impl<'a> AstArena<'a> {
    // clears the stack and returns it to the user
    pub fn pop_hfs_stack(&mut self) -> Vec<ExprId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        temp
    }
    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, msg: &str) -> ExprId { 
        // should start using our own error structs instead
        self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg))
    }
    pub fn last_or_error(&mut self, msg: &str) -> ExprId { 
        // should start using our own error structs instead
        *self.hfs_stack.last().unwrap_or_else(|| panic!("{}", msg))
    }
    pub fn pop2_or_error(&mut self, msg: &str) -> (ExprId, ExprId) { 
        // should start using our own error structs instead
        (
            self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg)),
            self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg)),
        )
    }
    pub fn push_to_hfs_stack(&mut self, expr_id: ExprId) {
        self.hfs_stack.push(expr_id)
    }
    pub fn alloc_and_push_to_hfs_stack(&mut self, expr: Expression, token: Token<'a>) -> ExprId {
        let id = self.alloc_expr(expr, token);
        self.hfs_stack.push(id);
        id
    }
    pub fn validate_return_stack(&mut self, return_type: TypeId) -> Result<(), String> {
        let Type::Tuple(return_types) = self.get_type(return_type) else { panic!("[internal error] functions only return tuples at the moment.") };
        let expected_count = return_types.len();
        let actual_count = self.hfs_stack.len();
        if expected_count != actual_count {
            return Err(format!("expected {} values on stack for return, found {}", expected_count, actual_count));
        }
        let back_of_stack = &self.hfs_stack[actual_count - expected_count..];

        for (&expr_id, &expected_type_id) in back_of_stack.iter().zip(return_types.iter()) {
            let actual_type_id = self.get_type_of_expr(expr_id);
            if self.get_type(actual_type_id) != self.get_type(expected_type_id) {
                return Err(format!(
                    "return type mismatch: expected '{:?}', found '{:?}'",
                    self.get_type(expected_type_id),
                    self.get_type(actual_type_id)
                ));
            }
        }
        Ok(())
    }

    // pub fn validate_func_call_args(&mut self, param_type: TypeId) -> bool {
    //     if let Type::Tuple(param_types) = self.get_type(param_type) {
    //         if param_types.len() != self.hfs_stack.len() {
    //             return false;
    //         }
    //         for param_type in param_types { // function parameter type is always a tuple
    //         }
    //     }
    //     todo!()
    // }
}

// ============================================================================
// Stack Analyzer [2nd pass] (finish AST + solve identifiers)
// this is used to finish up the AST we started in the parser pass
// we also need to solve identifiers here, otherwise we cant check 
// how function calls/assignments change the stack
// ============================================================================
pub struct StackAnalyzer<'a> {
    unresolved_arena: UnresolvedAstArena<'a>,
    arena: AstArena<'a>,
    scope_resolution_stack: ScopeStack,
}

impl<'a> StackAnalyzer<'a> {
    pub fn new(unresolved: UnresolvedAstArena<'a>, file_name: String) -> Self {
        Self {
            unresolved_arena: unresolved,
            arena: AstArena::new(),
            scope_resolution_stack: ScopeStack::new(file_name),
        }
    }

    pub fn resolve(top_level: Vec<UnresolvedTopLevelId>, unresolved: UnresolvedAstArena<'a>, file_name: String) -> (Vec<TopLevelId>, AstArena<'a>) {
        let mut stack_parser = StackAnalyzer::new(unresolved, file_name);
        let resolved_top_level = stack_parser.resolve_top_level(top_level);
        (resolved_top_level, stack_parser.arena)
    }

    fn resolve_top_level(&mut self, nodes: Vec<UnresolvedTopLevelId>) -> Vec<TopLevelId> {
        let mut resolved_nodes = Vec::<TopLevelId>::new();
        for node in nodes {
            let new_node = match node {
                UnresolvedTopLevelId::VariableDecl(id) => TopLevelId::VariableDecl(self.resolve_var_decl(id)),
                UnresolvedTopLevelId::FunctionDecl(id) => TopLevelId::FunctionDecl(self.resolve_func_decl(id)),
                UnresolvedTopLevelId::Statement(_)    => panic!("there are no top level statements."),
            };
            resolved_nodes.push(new_node);
        }
        resolved_nodes
    }

    fn resolve_var_decl(&mut self, id: UnresolvedVarId) -> VarId {
        let unresolved_var = self.unresolved_arena.get_unresolved_var(id);
        let token = self.unresolved_arena.get_unresolved_var_token(id);
        
        let var_id = self.arena.alloc_var(
            VarDeclaration { 
                name: unresolved_var.name.clone(), 
                hfs_type: unresolved_var.hfs_type.clone() 
            },
            token
        );
        self.scope_resolution_stack.push_variable(&unresolved_var.name, var_id);
        var_id
    }

    fn resolve_func_decl(&mut self, id: UnresolvedFuncId) -> FuncId {
        let token = self.unresolved_arena.get_unresolved_func_token(id);
        let (name, param_type, return_type, unresolved_body, params) = {
            let unresolved_func = &self.unresolved_arena.get_unresolved_func(id);
            // deconstruct parameter tuple into Vec<Expression::Parameter>
            let mut params = Vec::<ExprId>::new();
            if let Type::Tuple(param_types) = self.unresolved_arena.get_type(unresolved_func.param_type) {
                for param_type in param_types { // function parameter type is always a tuple
                    let token = self.unresolved_arena.get_type_token(*param_type);
                    params.push(self.arena.alloc_and_push_to_hfs_stack(Expression::Parameter(*param_type), token))
                }
            }
            (
                unresolved_func.name.clone(),
                unresolved_func.param_type.clone(),
                unresolved_func.return_type.clone(),
                unresolved_func.body,
                params,
            )
        };

        let func = FunctionDeclaration {
            name: name.clone(),
            param_type,
            return_type,
            body: self.arena.temporarily_get_next_stmt_id(),
            params, // we need to create our function BEFORE analyzing the body 
        }; // needed for recursive functions AND to match the correct token
        //------------------------------------------------------------
        // we push scopes so the body can solve identifiers
        let func_id = self.push_function_and_scope_and_alloc(&name, func, token); 
        let body = self.resolve_stmt(unresolved_body);
        self.scope_resolution_stack.pop();
        //------------------------------------------------------------
        self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type());
        func_id
    }
    // this method should be used since it guarantees that we set up everything right
    fn push_function_and_scope_and_alloc(&mut self, name: &str, func: FunctionDeclaration, token: Token<'a>) -> FuncId {
        let ret_type = func.return_type.clone();
        let func_id = self.arena.alloc_function(func, token);
        self.scope_resolution_stack.push_function_and_scope(name, func_id, ret_type);
        func_id
    }

    fn resolve_stmt(&mut self, id: UnresolvedStmtId) -> StmtId {
        let token = self.unresolved_arena.get_unresolved_stmt_token(id);
        let unresolved_stmt = self.unresolved_arena.get_unresolved_stmt(id).clone();
        
        match unresolved_stmt {
            UnresolvedStatement::If { body, else_stmt } => {
                let cond = self.arena.last_or_error("expected boolean or operation on stack for if statement argument");
                let body = self.resolve_stmt(body);
                let else_stmt = match else_stmt {
                    Some(UnresolvedElseStmt::Else(stmt_id))   => Some(ElseStmt::Else(self.resolve_stmt(stmt_id))),
                    Some(UnresolvedElseStmt::ElseIf(stmt_id)) => Some(ElseStmt::ElseIf(self.resolve_stmt(stmt_id))),
                    None => None,
                };
                self.arena.alloc_stmt(Statement::If { cond, body, else_stmt }, token)
            }
            UnresolvedStatement::While { body } => {
                let cond = self.arena.last_or_error("expected boolean or operation on stack for while loop argument");
                let body = self.resolve_stmt(body);
                self.arena.alloc_stmt(Statement::While { cond, body }, token)
            }
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => {
                let mut expr_ids = Vec::<ExprId>::new();
                for expr_id in unresolved_expr_ids {
                    expr_ids.push(self.resolve_expr(expr_id));
                }
                self.arena.alloc_stmt(Statement::StackBlock(expr_ids), token)
            }
            UnresolvedStatement::BlockScope(unresolved_top_level_ids) => {
                self.scope_resolution_stack.push_block_scope();
                let top_level_ids = self.resolve_top_level(unresolved_top_level_ids);
                self.scope_resolution_stack.pop();

                self.arena.alloc_stmt(Statement::BlockScope(top_level_ids), token)
            }
            UnresolvedStatement::Return => {
                self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type());
                self.arena.alloc_stmt(Statement::Return, token)
            }
            UnresolvedStatement::Break => {
                if self.scope_resolution_stack.curr_scope_kind() != ScopeKind::WhileLoop {
                }
                self.arena.alloc_stmt(Statement::Break, token)
            }
            UnresolvedStatement::Continue => {
                self.arena.alloc_stmt(Statement::Continue, token)
            }
            UnresolvedStatement::Empty => self.arena.alloc_stmt(Statement::Empty, token),
            UnresolvedStatement::Assignment { identifier, is_move } => todo!(),
        }
    }

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> ExprId {
        let token = self.unresolved_arena.get_unresolved_expr_token(id);
        match self.unresolved_arena.get_unresolved_expr(id) {
            UnresolvedExpression::Operation(unresolved_operation) => todo!(),
            UnresolvedExpression::Identifier(_) => todo!(),
            UnresolvedExpression::Literal(literal) => todo!(),
            UnresolvedExpression::FunctionCall { identifier } => todo!(),
            UnresolvedExpression::Tuple { expressions, variadic } => todo!(),
        }
    }

    fn resolve_operation(&mut self, op: &UnresolvedOperation) -> ExprId {
        match op {
            UnresolvedOperation::Add => todo!(),
            UnresolvedOperation::Sub => todo!(),
            UnresolvedOperation::Mul => todo!(),
            UnresolvedOperation::Div => todo!(),
            UnresolvedOperation::Mod => todo!(),
            UnresolvedOperation::Or => todo!(),
            UnresolvedOperation::And => todo!(),
            UnresolvedOperation::Equal => todo!(),
            UnresolvedOperation::NotEqual => todo!(),
            UnresolvedOperation::Less => todo!(),
            UnresolvedOperation::LessEqual => todo!(),
            UnresolvedOperation::Greater => todo!(),
            UnresolvedOperation::GreaterEqual => todo!(),
            UnresolvedOperation::Not => todo!(),
        }
    }

}
