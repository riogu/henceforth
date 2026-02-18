use std::{collections::HashMap, rc::Rc};

// Expression stack methods (used to finish building up the AST)
// we need to make sure expressions have been pushed throughout the program
// to do this, we keep track of which expressions have been pushed
// (aka, the contents of a `@(123 123 123)` stack block along the program
// note that if you just wanted to interpret the language, we could already be done.
// this is 'basically' interpreting, but only at the level of "is it in the stack or not"
// the goal is to make Henceforth a compiled language, therefore we do multiple stack passes
use super::*;
use crate::{
    hfs::{
        error::{CompileError, DiagnosticInfo},
        stack_analyzer_errors::{StackAnalyzerError, StackAnalyzerErrorKind},
    },
    type_effect,
};

impl AstArena {
    // clears the stack and returns it to the user
    pub fn pop_entire_hfs_stack(&mut self) -> Vec<ExprId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        temp
    }
    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, tokens: Vec<Token>) -> Result<ExprId, Box<dyn CompileError>> {
        match self.hfs_stack.pop() {
            Some(id) => Ok(id),
            None => StackAnalyzerError::new(
                StackAnalyzerErrorKind::StackUnderflow,
                self.diagnostic_info.path.clone(),
                tokens.iter().map(|tkn| tkn.clone().source_info).collect(),
            ),
        }
    }
    pub fn last_or_error(&self, tokens: Vec<Token>) -> Result<ExprId, Box<dyn CompileError>> {
        match self.hfs_stack.last() {
            Some(id) => Ok(*id),
            None => StackAnalyzerError::new(
                StackAnalyzerErrorKind::ExpectedItemOnStack,
                self.diagnostic_info.path.clone(),
                tokens.iter().map(|tkn| tkn.clone().source_info).collect(),
            ),
        }
    }
    pub fn pop2_or_error(&mut self, tokens: Vec<Token>) -> Result<(ExprId, ExprId), Box<dyn CompileError>> {
        let rhs = self.pop_or_error(tokens.clone())?;
        let lhs = self.pop_or_error(tokens)?;
        Ok((lhs, rhs))
    }

    pub fn popn_or_error(&mut self, n: usize, tokens: Vec<Token>) -> Result<Vec<ExprId>, Box<dyn CompileError>> {
        let mut popped: Result<Vec<ExprId>, Box<dyn CompileError>> = (0..n).map(|_| self.pop_or_error(tokens.clone())).collect();
        popped.map(|mut v| {
            v.reverse();
            v
        })
    }

    pub fn validate_return_stack(&mut self, return_type: TypeId, tokens: Vec<Token>) -> Result<(), Box<dyn CompileError>> {
        let Type::Tuple { type_ids: return_types, .. } = self.get_type(return_type) else {
            panic!("[internal error] functions only return tuples at the moment.")
        };
        let return_types = return_types.clone();
        let expected_count = return_types.len();
        let actual_count = self.hfs_stack.len();
        if expected_count != actual_count {
            return StackAnalyzerError::new(
                StackAnalyzerErrorKind::IncorrectNumberReturnValues(expected_count, actual_count),
                self.diagnostic_info.path.clone(),
                tokens.iter().map(|tkn| tkn.clone().source_info).collect(),
            );
        }
        let stack_copy = &self.hfs_stack.clone();

        for (expr_id, expected_type_id) in stack_copy.iter().zip(return_types.iter()) {
            let actual_type_id = self.get_type_id_of_expr(*expr_id)?;
            if let Err(err) = self.compare_types(actual_type_id, *expected_type_id, tokens.clone()) {
                return StackAnalyzerError::new(
                    StackAnalyzerErrorKind::TypeMismatchReturnValues(
                        self.get_type(*expected_type_id).clone(),
                        self.get_type(actual_type_id).clone(),
                    ),
                    self.diagnostic_info.path.clone(),
                    tokens.iter().map(|tkn| tkn.clone().source_info).collect(),
                );
            }
        }
        Ok(())
    }

    pub fn validate_func_call(
        &self,
        param_type: TypeId,
        arg_type: TypeId,
        arg_tokens: Vec<Token>,
    ) -> Result<(), Box<dyn CompileError>> {
        let Type::Tuple { .. } = self.get_type(arg_type) else {
            panic!("[internal error] expected tuple on stack before function call")
        };
        let Type::Tuple { .. } = self.get_type(param_type) else {
            panic!("[internal error] function parameter type must be a tuple")
        };

        self.compare_types(arg_type, param_type, arg_tokens)
    }

    pub fn compare_types(
        &self,
        actual_type_id: TypeId,
        expected_type_id: TypeId,
        tokens: Vec<Token>,
    ) -> Result<(), Box<dyn CompileError>> {
        let actual_type = self.get_type(actual_type_id);
        let expected_type = self.get_type(expected_type_id);

        match (actual_type, expected_type) {
            (
                Type::Tuple { type_ids: actual_types, ptr_count: actual_ptr_count },
                Type::Tuple { type_ids: expected_types, ptr_count: expected_ptr_count },
            ) => {
                if actual_types.len() != expected_types.len() {
                    return StackAnalyzerError::new(
                        StackAnalyzerErrorKind::IncorrectTupleLength(expected_types.len(), actual_types.len()),
                        self.diagnostic_info.path.clone(),
                        tokens.iter().map(|token| token.source_info.clone()).collect(),
                    );
                }
                if actual_ptr_count != expected_ptr_count {
                    return StackAnalyzerError::new(
                        StackAnalyzerErrorKind::IncorrectPointerCount(*actual_ptr_count, *expected_ptr_count),
                        self.diagnostic_info.path.clone(),
                        tokens.iter().map(|token| token.source_info.clone()).collect(),
                    );
                }
                // Recursively validate each element
                for (i, (&actual_elem_id, &expected_elem_id)) in actual_types.iter().zip(expected_types.iter()).enumerate() {
                    let elem_token = match tokens.get(i) {
                        Some(token) => token.clone(),
                        None => panic!("[internal error] wrong number of tokens passed"),
                    };
                    self.compare_types(actual_elem_id, expected_elem_id, vec![elem_token]);
                }
                Ok(())
            },
            (actual, expected) if actual == expected => Ok(()),
            (actual, expected) => StackAnalyzerError::new(
                StackAnalyzerErrorKind::TypeMismatch(expected.clone(), actual.clone()),
                self.diagnostic_info.path.clone(),
                tokens.iter().map(|token| token.source_info.clone()).collect(),
            ),
        }
    }
}

// ============================================================================
// Stack Analyzer [2nd pass] (finish AST + solve identifiers)
// this is used to finish up the AST we started in the parser pass
// we also need to solve identifiers here, otherwise we cant check
// how function calls/assignments change the stack
// ============================================================================
pub struct StackAnalyzer {
    unresolved_arena: UnresolvedAstArena,
    arena: AstArena,
    scope_resolution_stack: ScopeStack,
}

impl StackAnalyzer {
    pub fn new(unresolved: UnresolvedAstArena, diagnostic_info: Rc<DiagnosticInfo>) -> Self {
        let mut arena = AstArena::new(diagnostic_info.clone());
        arena.types.extend_from_slice(&unresolved.types[PRIMITIVE_TYPE_COUNT..]);
        Self { arena, unresolved_arena: unresolved, scope_resolution_stack: ScopeStack::new(&diagnostic_info.path) }
    }

    pub fn resolve(
        top_level: Vec<UnresolvedTopLevelId>,
        unresolved: UnresolvedAstArena,
        diagnostic_info: Rc<DiagnosticInfo>,
    ) -> Result<(Vec<TopLevelId>, AstArena, ScopeStack), Box<dyn CompileError>> {
        let mut stack_parser = StackAnalyzer::new(unresolved, diagnostic_info);
        let resolved_top_level = stack_parser.resolve_top_level(top_level)?;
        Ok((resolved_top_level, stack_parser.arena, stack_parser.scope_resolution_stack))
    }

    fn resolve_top_level(&mut self, nodes: Vec<UnresolvedTopLevelId>) -> Result<Vec<TopLevelId>, Box<dyn CompileError>> {
        let mut resolved_nodes = Vec::<TopLevelId>::new();
        for node in nodes.clone() {
            let new_node = match node {
                UnresolvedTopLevelId::VariableDecl(id) => TopLevelId::VariableDecl(self.resolve_var_decl(id)),
                UnresolvedTopLevelId::FunctionDecl(id) => TopLevelId::FunctionDecl(self.resolve_func_decl(id)?),
                UnresolvedTopLevelId::Statement(id) => TopLevelId::Statement(self.resolve_stmt(id)?),
            };
            resolved_nodes.push(new_node);
        }
        Ok(resolved_nodes)
    }

    fn resolve_var_decl(&mut self, id: UnresolvedVarId) -> VarId {
        // TODO: havent checked variable redeclarations
        let unresolved_var = self.unresolved_arena.get_unresolved_var(id);
        let token = self.unresolved_arena.get_unresolved_var_token(id);

        let var_id = self
            .arena
            .alloc_var(VarDeclaration { name: unresolved_var.name.clone(), hfs_type: unresolved_var.hfs_type.clone() }, token);
        self.scope_resolution_stack.push_variable(&unresolved_var.name, var_id);
        var_id
    }

    fn resolve_func_decl(&mut self, id: UnresolvedFuncId) -> Result<FuncId, Box<dyn CompileError>> {
        let token = self.unresolved_arena.get_unresolved_func_token(id);
        let (name, param_type, return_type, unresolved_body, parameter_exprs) = {
            let unresolved_func = &self.unresolved_arena.get_unresolved_func(id);
            // deconstruct parameter tuple into Vec<Expression::Parameter>
            let mut params = Vec::<ExprId>::new();
            if let Type::Tuple { type_ids: param_types, .. } = self.unresolved_arena.get_type(unresolved_func.param_type) {
                for (index, param_type) in param_types.iter().enumerate() {
                    // function parameter type is always a tuple
                    let token = self.unresolved_arena.get_type_token(*param_type);
                    params.push(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Parameter { index, type_id: *param_type },
                        ExprProvenance::RuntimeValue,
                        token,
                    ))
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

        // needed for recursive functions AND to match the correct token
        let func = FunctionDeclaration { name: name.clone(), param_type, return_type, body: StmtId(69420), parameter_exprs };
        //------------------------------------------------------------
        // we push scopes so the body can solve identifiers
        let func_id = self.push_function_and_scope_and_alloc(&name, func, token.clone());
        self.arena.get_func_mut(func_id).body = self.resolve_stmt(unresolved_body)?;
        //------------------------------------------------------------

        self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type(), vec![token])?;
        self.scope_resolution_stack.pop();
        self.arena.pop_entire_hfs_stack(); // context should be reset after each function!
        Ok(func_id)
    }
    // this method should be used since it guarantees that we set up everything right
    fn push_function_and_scope_and_alloc(&mut self, name: &str, func: FunctionDeclaration, token: Token) -> FuncId {
        let ret_type = func.return_type.clone();
        let func_id = self.arena.alloc_function(func, token);
        self.scope_resolution_stack.push_function_and_scope(name, func_id, ret_type);
        func_id
    }

    fn resolve_stmt(&mut self, id: UnresolvedStmtId) -> Result<StmtId, Box<dyn CompileError>> {
        let token = self.unresolved_arena.get_unresolved_stmt_token(id);
        let unresolved_stmt = self.unresolved_arena.get_unresolved_stmt(id).clone();
        match unresolved_stmt {
            UnresolvedStatement::ElseIf { cond, body, else_stmt } | UnresolvedStatement::If { body, else_stmt, cond } => {
                let stack_before_branches = self.arena.hfs_stack.clone();
                // actually adds the condition to the hfs_stack, because the stack analyzer only "sees" the if statement itself
                let cond_stack_block = self.resolve_stmt(cond)?;
                // condition isnt included in the stack depth count
                let cond = self.arena.pop_or_error(vec![self.unresolved_arena.get_unresolved_stmt_token(body)])?;
                let cond_type = self.arena.get_type_id_of_expr(cond)?;
                self.arena.compare_types(cond_type, self.arena.bool_type(), vec![self.arena.get_expr_token(cond).clone()])?;

                // Analyze if body
                let if_depth_before = self.arena.hfs_stack.len();
                let body = self.resolve_stmt(body)?;
                let if_depth_after = self.arena.hfs_stack.len();

                let else_stmt = match else_stmt {
                    Some(else_stmt_id) => {
                        match self.unresolved_arena.get_unresolved_stmt(else_stmt_id) {
                            UnresolvedStatement::Else(stmt_id) => {
                                let stmt_id = *stmt_id;
                                // Reset stack to pre-if state for else branch
                                self.arena.hfs_stack = stack_before_branches;
                                let else_body = self.resolve_stmt(stmt_id)?;
                                let else_depth_after = self.arena.hfs_stack.len();

                                // Both branches must have same stack effect
                                if if_depth_after != else_depth_after {
                                    let else_body = self.unresolved_arena.get_unresolved_stmt(stmt_id.clone());
                                    if let UnresolvedStatement::BlockScope(stmts, _) = else_body {
                                        let mut else_body_source_infos = Vec::new();
                                        for stmt in stmts {
                                            match stmt {
                                                UnresolvedTopLevelId::VariableDecl(unresolved_var_id) => else_body_source_infos
                                                    .push(
                                                        self.unresolved_arena
                                                            .get_unresolved_var_token(*unresolved_var_id)
                                                            .source_info,
                                                    ),
                                                UnresolvedTopLevelId::FunctionDecl(unresolved_func_id) => else_body_source_infos
                                                    .push(
                                                        self.unresolved_arena
                                                            .get_unresolved_func_token(*unresolved_func_id)
                                                            .source_info,
                                                    ),
                                                UnresolvedTopLevelId::Statement(unresolved_stmt_id) => else_body_source_infos
                                                    .push(
                                                        self.unresolved_arena
                                                            .get_unresolved_stmt_token(*unresolved_stmt_id)
                                                            .source_info,
                                                    ),
                                            }
                                        }
                                        return StackAnalyzerError::new(
                                            StackAnalyzerErrorKind::MismatchingStackDepths(
                                                if_depth_after - if_depth_before,
                                                else_depth_after - if_depth_before,
                                            ),
                                            self.arena.diagnostic_info.path.clone(),
                                            else_body_source_infos,
                                        );
                                    } else {
                                        panic!(
                                            "[internal error] else body is not a block scope (should already be resolved in \
                                             parser)"
                                        )
                                    }
                                }
                                Some(self.arena.alloc_stmt(Statement::Else(else_body), token.clone()))
                            },
                            UnresolvedStatement::ElseIf { cond, body, else_stmt } => {
                                // Reset stack to pre-if state for elseif branch
                                self.arena.hfs_stack = stack_before_branches;
                                Some(self.resolve_stmt(else_stmt_id)?)
                            },
                            _ => {
                                panic!("can't have other statements in else statement")
                            },
                        }
                    },
                    None => {
                        // If no else, the if body must have net-zero stack effect
                        if if_depth_before != if_depth_after {
                            let if_body = self.arena.get_stmt(body.clone());
                            if let Statement::BlockScope(stmts, _) = if_body {
                                let mut if_body_source_infos = Vec::new();
                                for stmt in stmts {
                                    match stmt {
                                        TopLevelId::VariableDecl(var_id) =>
                                            if_body_source_infos.push(self.arena.get_var_token(*var_id).source_info.clone()),
                                        TopLevelId::FunctionDecl(func_id) =>
                                            if_body_source_infos.push(self.arena.get_function_token(*func_id).source_info.clone()),
                                        TopLevelId::Statement(stmt_id) =>
                                            if_body_source_infos.push(self.arena.get_stmt_token(*stmt_id).source_info.clone()),
                                    }
                                }
                                return StackAnalyzerError::new(
                                    StackAnalyzerErrorKind::ExpectedNetZeroStackEffectIfStmt(if_depth_after - if_depth_before),
                                    self.arena.diagnostic_info.path.clone(),
                                    if_body_source_infos,
                                );
                            }
                        }
                        None
                    },
                };

                Ok(self.arena.alloc_stmt(Statement::If { cond_stack_block, body, else_stmt }, token))
            },
            UnresolvedStatement::While { body, cond } => {
                self.resolve_stmt(cond);

                let cond = self.arena.pop_or_error(vec![self.unresolved_arena.get_unresolved_stmt_token(body)])?;
                // condition isnt included in the stack depth count since its popped when entering
                // each while loop (our stack state is left right after the condition is popped)
                let stack_depth_before = self.arena.hfs_stack.len();

                let cond_type = self.arena.get_type_id_of_expr(cond)?;
                self.arena.compare_types(cond_type, self.arena.bool_type(), vec![self.arena.get_expr_token(cond).clone()])?;

                // Analyze the body
                let body = self.resolve_stmt(body)?;

                // Enforce stack balance
                let stack_depth_after = self.arena.hfs_stack.len();
                if stack_depth_before != stack_depth_after {
                    if let UnresolvedStatement::BlockScope(stmts, _) = self.unresolved_arena.get_unresolved_stmt(id) {
                        let mut while_body_source_infos = Vec::new();
                        for stmt in stmts {
                            match stmt {
                                UnresolvedTopLevelId::VariableDecl(var_id) => while_body_source_infos
                                    .push(self.unresolved_arena.get_unresolved_var_token(*var_id).source_info.clone()),
                                UnresolvedTopLevelId::FunctionDecl(func_id) => while_body_source_infos
                                    .push(self.unresolved_arena.get_unresolved_func_token(*func_id).source_info.clone()),
                                UnresolvedTopLevelId::Statement(stmt_id) => while_body_source_infos
                                    .push(self.unresolved_arena.get_unresolved_stmt_token(*stmt_id).source_info.clone()),
                            }
                        }
                        return StackAnalyzerError::new(
                            StackAnalyzerErrorKind::ExpectedNetZeroStackEffectWhileLoop(stack_depth_after - stack_depth_before),
                            self.arena.diagnostic_info.path.clone(),
                            while_body_source_infos,
                        );
                    }
                }

                Ok(self.arena.alloc_stmt(Statement::While { cond, body }, token))
            },
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => {
                let stack_start = self.arena.hfs_stack.clone();

                for expr_id in unresolved_expr_ids {
                    self.resolve_expr(expr_id)?;
                }
                // when were resolving a stack block, we can consume the previous stack state
                // e.g: if we had:
                // @(1 2 3);
                // @(+);
                // we consumed the 2 and the 3,
                // so to find what to push, we grab the initial stack state, and on a cloned version of the current stack,
                // we pop until they differ, so if we had [1, 2, 3] and [1, Add(2, 3)]
                // we would pop the 1 ending up with [Add(2, 3)]
                let mut expr_ids = Vec::<ExprId>::new();
                let stack_change = self.arena.get_stack_change(stack_start, self.arena.hfs_stack.clone());
                for &expr_id in &stack_change {
                    expr_ids.push(expr_id);
                }
                Ok(self.arena.alloc_stmt(Statement::StackBlock(expr_ids), token))
            },
            UnresolvedStatement::BlockScope(unresolved_top_level_ids, scope_kind) => {
                // FIXME: this needs to stop when it finds return/continue/break, just like
                // the interpreter does, otherwise it will count the stack depth wrong
                // move the validation of each branch to StackAnalyzer, and dont perform any checks
                // in the interpreter. there, you just want to run whatever node you recieve,
                // because we have checked that every branch is valid beforehand here
                let top_level_ids = if scope_kind != ScopeKind::Function {
                    // Functions should push their own scopes
                    self.scope_resolution_stack.push_scope(scope_kind);
                    let temp = self.resolve_top_level(unresolved_top_level_ids);
                    self.scope_resolution_stack.pop();
                    temp?
                } else {
                    self.resolve_top_level(unresolved_top_level_ids)?
                };

                Ok(self.arena.alloc_stmt(Statement::BlockScope(top_level_ids, scope_kind), token))
            },
            UnresolvedStatement::Return => {
                // this is the case where we validate manually written 'return' statements
                // rather than the implicit return at the end of functions
                self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type(), vec![token.clone()])?;
                Ok(self.arena.alloc_stmt(Statement::Return, token))
            },
            UnresolvedStatement::Break => {
                if !self.scope_resolution_stack.is_in_while_loop_context() {
                    panic!("found break statement outside while loop.")
                }
                Ok(self.arena.alloc_stmt(Statement::Break, token))
            },
            UnresolvedStatement::Continue => {
                if !self.scope_resolution_stack.is_in_while_loop_context() {
                    panic!("found continue statement outside while loop.")
                }
                Ok(self.arena.alloc_stmt(Statement::Continue, token))
            },
            UnresolvedStatement::Empty => Ok(self.arena.alloc_stmt(Statement::Empty, token)),
            UnresolvedStatement::Assignment { identifier, is_move, deref_count } => {
                // FIXME: do stuff with deref_count
                let value = if is_move {
                    self.arena.pop_or_error(vec![
                        self.unresolved_arena.get_unresolved_stmt_token(id),
                        self.unresolved_arena.get_unresolved_expr_token(identifier),
                    ])?
                } else {
                    self.arena.last_or_error(vec![
                        self.unresolved_arena.get_unresolved_stmt_token(id),
                        self.unresolved_arena.get_unresolved_expr_token(identifier),
                    ])?
                };
                let identifier =
                    self.resolve_var_assignment_identifier(identifier, *self.arena.get_expr_provenance(value), deref_count);

                Ok(self.arena.alloc_stmt(Statement::Assignment { identifier, is_move, deref_count }, token))
            },
            UnresolvedStatement::FunctionCall { identifier, is_move } => {
                // just checks if we actually had a function and finds the identifier
                let func_id = self.resolve_func_call_identifier(identifier);
                let func_decl = self.arena.get_func(func_id).clone(); // make borrow checker happy

                let Type::Tuple { type_ids: param_types, .. } = self.arena.get_type(func_decl.param_type) else {
                    panic!("[internal error] functions only recieve tuples at the moment.")
                };
                let mut arg_count = 0;
                let mut arg_types = Vec::new();
                let mut arg_expr_tokens = Vec::new();
                for _ in 0..param_types.len() {
                    let arg_expr = self.arena.pop_or_error(vec![
                        self.unresolved_arena.get_unresolved_stmt_token(id),
                        self.unresolved_arena.get_unresolved_expr_token(identifier),
                    ])?;
                    arg_expr_tokens.push(self.arena.get_expr_token(arg_expr).clone());
                    arg_types.push(self.arena.get_type_id_of_expr(arg_expr)?);
                    arg_count += 1;
                }
                let arg_type_id = self.arena.alloc_type(Type::Tuple { type_ids: arg_types, ptr_count: 0 }, Token {
                    kind: TokenKind::LeftParen,
                    source_info: SourceInfo::new(0, 0, 0),
                });
                // first make sure calling this function is valid given the stack state
                self.arena.validate_func_call(func_decl.param_type, arg_type_id, arg_expr_tokens);

                // now make sure the stack is updated based on the return type of the function
                let Type::Tuple { type_ids: return_types, .. } = self.arena.get_type(func_decl.return_type) else {
                    panic!("[internal error] functions only return tuples at the moment.")
                };
                let mut return_values = Vec::new();
                for type_id in return_types.clone() {
                    self.arena.alloc_and_push_to_hfs_stack(
                        Expression::ReturnValue(type_id),
                        ExprProvenance::RuntimeValue,
                        token.clone(), // TODO: (joao) some tokens are kinda not precise enough for error reporting.
                                       // you will wanna get more pinpointed tokens for better location info
                    );
                }
                // function calls dont go to the stack
                Ok(self.arena.alloc_stmt(Statement::FunctionCall { arg_count, func_id, is_move, return_values }, token))
            },
            UnresolvedStatement::Else(unresolved_stmt_id) => panic!("[internal error] else statements are not solved here"),
        }
    }

    fn resolve_var_assignment_identifier(
        &mut self,
        id: UnresolvedExprId,
        provenance: ExprProvenance,
        deref_count: usize,
    ) -> Identifier {
        // neither this or func_call identifier allocate an expression
        let token = self.unresolved_arena.get_unresolved_expr_token(id);
        match self.unresolved_arena.get_unresolved_expr(id) {
            UnresolvedExpression::Identifier(identifier) => {
                let identifier = self.scope_resolution_stack.find_identifier(&identifier);
                match identifier {
                    Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                        self.arena.curr_var_provenances[var_id.0] = provenance;
                        // note that solving an identifier updates the provenance
                        if deref_count > 0 {
                            let ptr_count = self.arena.get_type(self.arena.get_var(var_id).hfs_type).get_ptr_count();
                            if deref_count > ptr_count {
                                panic!(
                                    // FIXME: use joao's errors instead of a panic
                                    "cannot dereference '{}' {} time(s) — type only has {} level(s) of indirection",
                                    self.arena.get_var(var_id).name,
                                    deref_count,
                                    ptr_count
                                );
                            }
                        }
                        identifier
                    },
                    Identifier::Function(_) => panic!("cannot assign value to a function"),
                }
            },
            _ => {
                panic!("[internal error] you're assigning to something that isn't an identifier")
            },
        }
    }
    fn resolve_func_call_identifier(&mut self, id: UnresolvedExprId) -> FuncId {
        // dont allocate an expression for these cases
        let identifier = self.unresolved_arena.get_unresolved_expr(id);
        let UnresolvedExpression::Identifier(identifier) = identifier else {
            panic!("[internal error] function call must have identifier")
        };
        let identifier = self.scope_resolution_stack.find_identifier(&identifier);
        match identifier {
            Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                panic!("variable '{}' cannot be called as a function.", self.arena.get_var(var_id).name)
            },
            Identifier::Function(func_id) => {
                self.arena.curr_func_call_provenances[func_id.0] = ExprProvenance::RuntimeValue;
                // doing it this way because we dont really care about evaluating functions at
                // compile time for now (so we jut make it runtime)
                func_id
            },
        }
    }

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> Result<ExprId, Box<dyn CompileError>> {
        let token = self.unresolved_arena.get_unresolved_expr_token(id);
        match self.unresolved_arena.get_unresolved_expr(id).clone() {
            UnresolvedExpression::Operation(unresolved_operation) => {
                let expr_id = self.resolve_operation(&unresolved_operation, token)?;
                self.arena.get_type_of_expr(expr_id)?;
                Ok(expr_id)
            },
            UnresolvedExpression::Identifier(identifier) => {
                // there should only be identifiers here that were inside the stack scope
                // @(1 2 var foo) // like this example
                let identifier = self.scope_resolution_stack.find_identifier(&identifier);
                Ok(self.arena.alloc_and_push_to_hfs_stack(
                    Expression::Identifier(identifier),
                    *self.arena.get_identifier_provenance(identifier),
                    token,
                ))
            },
            UnresolvedExpression::Literal(literal) =>
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Literal(literal), ExprProvenance::CompiletimeValue, token)),
            UnresolvedExpression::Tuple { expressions } => {
                // the tuple's type is formed recursively whenever someone wants it
                // by calling arena.get_type_of_expr(tuple_expr_id); (dont create it here)
                let mut expr_ids = Vec::<ExprId>::new();
                let mut tuple_provenance = ExprProvenance::CompiletimeValue;
                for expr_id in expressions {
                    let expr_id = self.resolve_expr(expr_id)?;
                    expr_ids.push(expr_id);
                    self.arena.pop_or_error(vec![self.arena.get_expr_token(expr_id).clone()]);
                    // clear the stack after we pushed, so that the only thing left on the
                    // stack is the tuple itself, not its arguments (keeps the API agnostic)
                    // (we allocate by pushing and dont want to add a new API just for this to
                    // enforce this intended stack behavior)
                    if matches!(self.arena.get_expr_provenance(expr_id), ExprProvenance::RuntimeValue) {
                        // if any element is runtime, drop comptime from this tuple
                        tuple_provenance = ExprProvenance::RuntimeValue
                    }
                }
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Tuple { expressions: expr_ids }, tuple_provenance, token))
            },
            UnresolvedExpression::StackKeyword(name) => {
                let kw_declaration = self.arena.get_stack_keyword_from_name(name.as_str());

                let effect = kw_declaration.type_effect;

                let args = match kw_declaration.expected_args_size {
                    Some(n) => self.arena.popn_or_error(n, vec![self.unresolved_arena.get_unresolved_expr_token(id)])?,
                    None => self.arena.pop_entire_hfs_stack(), // for @pop_all
                };

                // simulate stack
                let mut simulated_stack: Vec<TypeId> = Vec::new();
                for arg in &args {
                    simulated_stack.push(self.arena.get_type_id_of_expr(*arg)?);
                }

                let return_values = effect(simulated_stack.clone());
                let mut return_value_ids = Vec::new();
                return_values.iter().for_each(|ret| {
                    if let Expression::ReturnValue(id) = ret {
                        let return_val_id = self.arena.alloc_and_push_to_hfs_stack(
                            ret.clone(),
                            ExprProvenance::RuntimeValue,
                            self.arena.get_type_token(*id).clone(),
                        );
                        return_value_ids.push(return_val_id);
                    } else {
                        panic!("[internal error] stack keyword effect should return a Expression::ReturnValue")
                    }
                });

                let mut return_value_types: Vec<TypeId> = Vec::new();
                for id in &return_value_ids {
                    return_value_types.push(self.arena.get_type_id_of_expr(*id)?);
                }

                // create tuples for param and return type
                let param_type = self.arena.alloc_type(Type::Tuple { type_ids: simulated_stack, ptr_count: 0 }, Token {
                    kind: TokenKind::LeftParen,
                    source_info: SourceInfo { line_number: 0, line_offset: 0, token_width: 0 },
                });
                let return_type = self.arena.alloc_type(Type::Tuple { type_ids: return_value_types, ptr_count: 0 }, Token {
                    kind: TokenKind::LeftParen,
                    source_info: SourceInfo { line_number: 0, line_offset: 0, token_width: 0 },
                });

                let id = self.arena.alloc_and_push_to_hfs_stack(
                    Expression::StackKeyword(StackKeyword {
                        name,
                        parameter_exprs: args,
                        param_type,
                        return_type,
                        return_values: return_value_ids,
                    }),
                    ExprProvenance::RuntimeValue,
                    token.clone(),
                );
                self.arena.pop_or_error(vec![token]);
                Ok(id)
            },
        }
    }

    fn resolve_operation(&mut self, op: &UnresolvedOperation, token: Token) -> Result<ExprId, Box<dyn CompileError>> {
        match op {
            op if op.is_binary() => {
                let (lhs_expr, rhs_expr) = self.arena.pop2_or_error(vec![token.clone()])?;
                let provenance = if matches!(self.arena.get_expr_provenance(lhs_expr), ExprProvenance::CompiletimeValue)
                    && matches!(self.arena.get_expr_provenance(lhs_expr), ExprProvenance::CompiletimeValue)
                {
                    ExprProvenance::CompiletimeValue
                } else {
                    ExprProvenance::RuntimeValue
                };
                match op {
                    UnresolvedOperation::Add => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Add(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Sub => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Sub(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Mul => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Mul(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Div => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Div(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Mod => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Mod(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Or => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Or(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::And => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::And(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Equal => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Equal(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::NotEqual => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::NotEqual(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Less => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Less(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::LessEqual => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::LessEqual(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::Greater => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Greater(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    UnresolvedOperation::GreaterEqual => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::GreaterEqual(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    )),
                    _ => Ok(unreachable!()),
                }
            },
            UnresolvedOperation::Not => {
                let expr = self.arena.pop_or_error(vec![token.clone()])?;
                let provenance = *self.arena.get_expr_provenance(expr);
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Operation(Operation::Not(expr)), provenance, token))
            },
            UnresolvedOperation::Dereference => Ok(todo!()),
            UnresolvedOperation::AddressOf => Ok(todo!()),
            _ => Ok(panic!("missing semantic analysis for unary operation '{:?}'", op)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;

    use crate::hfs::{
        builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
        stack_analyzer_builder::StackAnalyzerBuilder,
        utils::{run_until, Phase},
        AstArena, Type,
    };

    fn analyze_file(name: &str) -> AstArena {
        run_until(name, Phase::StackAnalyzer)
            .expect("compilation failed")
            .as_any()
            .downcast_ref::<AstArena>()
            .expect("Expected AstArena from StackAnalyzer")
            .clone()
    }
    #[test]
    fn test_simple_main() {
        let ast = analyze_file("test/simple_main.hfs");

        let expected = StackAnalyzerBuilder::new().func_with("main", None, None).body().end_body().build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_function_with_lots_of_arguments() {
        let ast = analyze_file("test/function_with_lots_of_arguments.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with(
                "func_with_lots_of_arguments",
                Some(vec![Type::new_int(0), Type::new_float(0), Type::new_string(0), Type::new_bool(0)]),
                Some(vec![Type::new_int(0), Type::new_float(0), Type::new_bool(0), Type::new_string(0)]),
            )
            .body()
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(5)
            .push_literal(5.0)
            .push_literal(false)
            .push_literal("test")
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_function_with_no_arguments() {
        let ast = analyze_file("test/function_with_no_arguments.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("no_args", None, Some(vec![Type::new_int(0)]))
            .body()
            .stack_block()
            .push_literal(4)
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_function_with_no_return_type() {
        let ast = analyze_file("test/function_with_no_return_type.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("no_return_type", Some(vec![Type::new_int(0)]), None)
            .body()
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_variable_declarations() {
        let ast = analyze_file("test/variable_declarations.hfs");

        let expected = StackAnalyzerBuilder::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::new_int(0))
            .variable("b", Type::new_float(0))
            .variable("c", Type::new_string(0))
            .variable("d", Type::new_bool(0))
            .end_body()
            .build();

        assert_eq!(ast, expected);
    }

    #[test]
    fn test_copy_and_move() {
        let ast = analyze_file("test/copy_and_move.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("main", None, None)
            .body()
            .variable("copy", Type::new_int(0))
            .variable("move", Type::new_int(0))
            .stack_block()
            .push_literal(5)
            .end_stack_block(false)
            .assign_to("copy", PassMode::Copy)
            .assign_to("move", PassMode::Move)
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_operations() {
        let ast = analyze_file("test/operations.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(1)
            .push_operation(BuilderOperation::Add)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Divide)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .push_literal(2)
            .push_operation(BuilderOperation::Modulo)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(false)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Or)
            .push_operation(BuilderOperation::Not)
            .push_literal(true)
            .push_operation(BuilderOperation::And)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_while_loop() {
        let ast = analyze_file("test/while_loop.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::new_int(0))
            .variable("b", Type::new_int(0))
            .stack_block()
            .push_literal(100)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_literal(0)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .while_loop()
            .stack_block()
            .push_variable("a")
            .push_literal(0)
            .push_operation(BuilderOperation::GreaterThan)
            .push_variable("b")
            .push_literal(200)
            .push_operation(BuilderOperation::LessThan)
            .push_operation(BuilderOperation::And)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_variable("a")
            .push_literal(1)
            .push_operation(BuilderOperation::Subtract)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_variable("b")
            .push_literal(2)
            .push_operation(BuilderOperation::Add)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .end_body()
            .return_statement()
            .end_body()
            .build();
        assert_eq!(ast, expected)
    }

    #[test]
    fn test_simple_if_else() {
        let ast = analyze_file("test/simple_if_else.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("main", None, None)
            .body()
            .if_statement()
            .stack_block()
            .push_literal(5)
            .push_literal(2)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(2)
            .push_literal(3)
            .push_literal(4)
            .end_stack_block(true)
            .push_stack_keyword("@pop_all", true)
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .return_statement()
            .end_body()
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_if_elif_else() {
        let ast = analyze_file("test/if_elif_else.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("fizz_buzz", Some(vec![Type::new_int(0)]), Some(vec![Type::new_string(0)]))
            .body()
            .if_statement()
            .stack_block()
            .push_literal(15)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(3)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(5)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("buzz")
            .end_stack_block(true)
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal("no fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(530)
            .end_stack_block(false)
            .call_function("fizz_buzz", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_copy_and_move_func_calls() {
        let ast = analyze_file("test/copy_and_move_func_calls.hfs");
        let expected = StackAnalyzerBuilder::new()
            .func_with("max", Some(vec![Type::new_int(0), Type::new_int(0)]), Some(vec![Type::new_int(0)]))
            .body()
            .if_statement()
            .stack_block()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@dup", false)
            .push_stack_keyword("@rot", false)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .end_body()
            .func_with("max3", Some(vec![Type::new_int(0), Type::new_int(0), Type::new_int(0)]), Some(vec![Type::new_int(0)]))
            .body()
            .push_stack_keyword("@rrot", true)
            .call_function("max", PassMode::Move)
            .call_function("max", PassMode::Copy)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", false)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(50)
            .push_literal(10)
            .push_literal(30)
            .end_stack_block(false)
            .call_function("max3", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(ast, expected)
    }
}
