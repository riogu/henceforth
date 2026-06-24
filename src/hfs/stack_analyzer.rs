use std::rc::Rc;

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
        stack_analyzer_errors::{JumpKeyword, StackAnalyzerError, StackAnalyzerErrorKind},
    },
    stack_analyzer_error,
};

impl AstArena {
    // clears the stack and returns it to the user
    pub fn pop_entire_hfs_stack(&mut self) -> Vec<ExprId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        temp
    }
    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, span: Span) -> Result<ExprId, Box<dyn CompileError>> {
        match self.hfs_stack.pop() {
            Some(id) => Ok(id),
            None => stack_analyzer_error!(StackAnalyzerErrorKind::StackUnderflow, &*self, span),
        }
    }
    pub fn last_or_error(&self, span: Span) -> Result<ExprId, Box<dyn CompileError>> {
        match self.hfs_stack.last() {
            Some(id) => Ok(*id),
            None => stack_analyzer_error!(StackAnalyzerErrorKind::ExpectedItemOnStack, &*self, span),
        }
    }
    pub fn pop2_or_error(&mut self, span: Span) -> Result<(ExprId, ExprId), Box<dyn CompileError>> {
        let rhs = self.pop_or_error(span)?;
        let lhs = self.pop_or_error(span)?;
        Ok((lhs, rhs))
    }

    pub fn last2_or_error(&mut self, span: Span) -> Result<(ExprId, ExprId), Box<dyn CompileError>> {
        match self.hfs_stack.last_chunk::<2>() {
            Some(&[lhs, rhs]) => Ok((lhs, rhs)),
            None => stack_analyzer_error!(StackAnalyzerErrorKind::ExpectedItemOnStack, &*self, span),
        }
    }

    pub fn popn_or_error(&mut self, n: usize, span: Span) -> Result<Vec<ExprId>, Box<dyn CompileError>> {
        let popped: Result<Vec<ExprId>, Box<dyn CompileError>> = (0..n).map(|_| self.pop_or_error(span)).collect();
        popped.map(|mut v| {
            v.reverse();
            v
        })
    }

    pub fn validate_return_stack(&mut self, return_type: TypeId, span: Span) -> Result<(), Box<dyn CompileError>> {
        let ElaboratedType::Tuple { type_ids: return_types, .. } = self.get_type(return_type) else {
            panic!("[internal error] functions only return tuples at the moment.")
        };
        let return_types = return_types.clone();
        let expected_count = return_types.len();
        let actual_count = self.hfs_stack.len();
        if expected_count != actual_count {
            return stack_analyzer_error!(
                StackAnalyzerErrorKind::IncorrectNumberReturnValues(expected_count, actual_count),
                &*self,
                span
            );
        }
        let stack_copy = &self.hfs_stack.clone();

        for (expr_id, expected_type_id) in stack_copy.iter().zip(return_types.iter()) {
            let actual_type_id = self.get_type_id_of_expr(*expr_id)?;
            if let Err(_) = self.compare_types(actual_type_id, *expected_type_id, vec![span]) {
                return stack_analyzer_error!(
                    StackAnalyzerErrorKind::TypeMismatchReturnValues(
                        self.get_type(*expected_type_id).get_repr(&self),
                        self.get_type(actual_type_id).get_repr(&self),
                    ),
                    &*self,
                    span
                );
            }
        }
        Ok(())
    }

    pub fn validate_func_call(
        &self,
        param_type: TypeId,
        arg_type: TypeId,
        arg_spans: Vec<Span>,
    ) -> Result<(), Box<dyn CompileError>> {
        let ElaboratedType::Tuple { .. } = self.get_type(arg_type) else {
            panic!("[internal error] expected tuple on stack before function call")
        };
        let ElaboratedType::Tuple { .. } = self.get_type(param_type) else {
            panic!("[internal error] function parameter type must be a tuple")
        };

        self.compare_types(arg_type, param_type, arg_spans)
    }

    pub fn compare_types(
        &self,
        actual_type_id: TypeId,
        expected_type_id: TypeId,
        spans: Vec<Span>,
    ) -> Result<(), Box<dyn CompileError>> {
        let span = merge_spans(spans);
        let actual_type = self.get_type(actual_type_id);
        let expected_type = self.get_type(expected_type_id);
        match (actual_type, expected_type) {
            (
                ElaboratedType::Tuple { type_ids: actual_types, ptr_count: actual_ptr_count },
                ElaboratedType::Tuple { type_ids: expected_types, ptr_count: expected_ptr_count },
            ) => {
                if actual_types.len() != expected_types.len() {
                    return stack_analyzer_error!(
                        StackAnalyzerErrorKind::IncorrectTupleLength(expected_types.len(), actual_types.len()),
                        self,
                        span
                    );
                }
                if actual_ptr_count != expected_ptr_count {
                    return stack_analyzer_error!(
                        StackAnalyzerErrorKind::IncorrectPointerCount(*actual_ptr_count, *expected_ptr_count),
                        self,
                        span
                    );
                }
                // Recursively validate each element
                for (&actual_elem_id, &expected_elem_id) in actual_types.iter().zip(expected_types.iter()) {
                    self.compare_types(actual_elem_id, expected_elem_id, vec![*self.get_type_span(actual_elem_id)])?;
                }
                Ok(())
            },
            (actual, expected) if actual == expected => Ok(()),
            (actual, expected) => stack_analyzer_error!(
                StackAnalyzerErrorKind::TypeMismatch(expected.get_repr(&self), actual.get_repr(&self)),
                self,
                span
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
        let arena = AstArena::new(diagnostic_info.clone());
        Self { arena, unresolved_arena: unresolved, scope_resolution_stack: ScopeStack::new(diagnostic_info) }
    }

    pub fn resolve(
        top_level: Vec<UnresolvedTopLevelId>,
        unresolved: UnresolvedAstArena,
        diagnostic_info: Rc<DiagnosticInfo>,
    ) -> Result<(Vec<TopLevelId>, AstArena, ScopeStack), Box<dyn CompileError>> {
        let mut stack_parser = StackAnalyzer::new(unresolved.clone(), diagnostic_info);
        for unresolved_type in unresolved.types[PRIMITIVE_TYPE_COUNT..].iter() {
            let elaborated = stack_parser.elaborate(unresolved_type.clone())?;
            stack_parser.arena.types.push(elaborated);
        }
        let resolved_top_level = stack_parser.resolve_top_level(top_level)?;
        Ok((resolved_top_level, stack_parser.arena, stack_parser.scope_resolution_stack))
    }

    fn resolve_top_level(&mut self, nodes: Vec<UnresolvedTopLevelId>) -> Result<Vec<TopLevelId>, Box<dyn CompileError>> {
        let mut resolved_nodes = Vec::<TopLevelId>::new();
        for node in nodes.clone() {
            let new_node = match node {
                UnresolvedTopLevelId::VariableDecl(id) => TopLevelId::VariableDecl(self.resolve_var_decl(id)?),
                UnresolvedTopLevelId::FunctionDecl(id) => TopLevelId::FunctionDecl(self.resolve_func_decl(id)?),
                UnresolvedTopLevelId::Statement(id) => TopLevelId::Statement(self.resolve_stmt(id)?),
            };
            resolved_nodes.push(new_node);
        }
        Ok(resolved_nodes)
    }

    fn resolve_var_decl(&mut self, id: UnresolvedVarId) -> Result<VarId, Box<dyn CompileError>> {
        // TODO: havent checked variable redeclarations
        let unresolved_var = self.unresolved_arena.get_unresolved_var(id).clone();
        let span = self.unresolved_arena.get_unresolved_var_span(id);

        // we check if the declaration is an array type so we can resolve its length
        let hfs_type =
            self.resolve_array(unresolved_var.hfs_type, span, |name| StackAnalyzerErrorKind::ArrayLengthMustBeCompileTime(name))?;
        let var_id = self.arena.alloc_var(VarDeclaration { name: unresolved_var.name.clone(), hfs_type }, span);
        self.scope_resolution_stack.push_variable(&unresolved_var.name, var_id);
        Ok(var_id)
    }

    fn resolve_func_decl(&mut self, id: UnresolvedFuncId) -> Result<FuncId, Box<dyn CompileError>> {
        let span = self.unresolved_arena.get_unresolved_func_span(id);
        let (name, param_type, return_type, unresolved_body, parameter_exprs) = {
            let unresolved_func = &self.unresolved_arena.get_unresolved_func(id).clone();
            // deconstruct parameter tuple into Vec<Expression::Parameter>
            let mut params = Vec::<ExprId>::new();
            let mut new_param_types: Vec<TypeId> = Vec::new();
            if let UnresolvedType::Tuple { type_ids: param_types, .. } =
                self.unresolved_arena.get_type(unresolved_func.param_type).clone()
            {
                for (index, param_type) in param_types.iter().enumerate() {
                    // function parameter type is always a tuple
                    let span = self.unresolved_arena.get_type_span(*param_type).clone();
                    let hfs_type = self
                        .resolve_array(*param_type, span, |_| StackAnalyzerErrorKind::ArrayLengthMustBeCompileTimeOnParameter)?;
                    new_param_types.push(hfs_type);

                    params.push(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Parameter { index, type_id: hfs_type },
                        ExprProvenance::RuntimeValue,
                        span,
                    ))
                }
            }
            let return_type = self.resolve_array(unresolved_func.return_type, span, |_name| {
                StackAnalyzerErrorKind::ArrayLengthMustBeCompileTimeOnReturnType
            })?;
            let param_type =
                self.arena.alloc_type(ElaboratedType::Tuple { type_ids: new_param_types, ptr_count: 0 }, span.clone());
            (unresolved_func.name.clone(), param_type, return_type, unresolved_func.body, params)
        };

        // needed for recursive functions AND to match the correct token
        let func = FunctionDeclaration { name: name.clone(), param_type, return_type, body: StmtId(69420), parameter_exprs };
        //------------------------------------------------------------
        // we push scopes so the body can solve identifiers
        let func_id = self.push_function_and_scope_and_alloc(&name, func, span);
        self.arena.get_func_mut(func_id).body = self.resolve_stmt(unresolved_body)?;
        //------------------------------------------------------------

        self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type(), span)?;
        self.scope_resolution_stack.pop();
        self.arena.pop_entire_hfs_stack(); // context should be reset after each function!
        Ok(func_id)
    }
    // this method should be used since it guarantees that we set up everything right
    fn push_function_and_scope_and_alloc(&mut self, name: &str, func: FunctionDeclaration, span: Span) -> FuncId {
        let ret_type = func.return_type.clone();
        let func_id = self.arena.alloc_function(func, span);
        self.scope_resolution_stack.push_function_and_scope(name, func_id, ret_type);
        func_id
    }

    fn resolve_stmt(&mut self, id: UnresolvedStmtId) -> Result<StmtId, Box<dyn CompileError>> {
        let span = self.unresolved_arena.get_unresolved_stmt_span(id);
        let unresolved_stmt = self.unresolved_arena.get_unresolved_stmt(id).clone();
        match unresolved_stmt {
            UnresolvedStatement::ElseIf { ref cond, body, else_stmt } | UnresolvedStatement::If { ref cond, body, else_stmt } => {
                let stack_before_branches = self.arena.hfs_stack.clone();
                // actually adds the condition to the hfs_stack, because the stack analyzer only "sees" the if statement itself
                let mut condition_statements = Vec::new();
                for stmt in cond {
                    condition_statements.push(self.resolve_stmt(*stmt)?);
                }
                // condition isnt included in the stack depth count
                let cond = self.arena.pop_or_error(self.unresolved_arena.get_unresolved_stmt_span(body))?;
                let cond_type = self.arena.get_type_id_of_expr(cond)?;
                self.arena.compare_types(cond_type, ElaboratedType::new_bool(0).type_id(), vec![
                    self.arena.get_expr_span(cond).clone(),
                ])?;

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
                                        let mut else_body_spans = Vec::new();
                                        for stmt in stmts {
                                            match stmt {
                                                UnresolvedTopLevelId::VariableDecl(unresolved_var_id) => else_body_spans
                                                    .push(self.unresolved_arena.get_unresolved_var_span(*unresolved_var_id)),
                                                UnresolvedTopLevelId::FunctionDecl(unresolved_func_id) => else_body_spans
                                                    .push(self.unresolved_arena.get_unresolved_func_span(*unresolved_func_id)),
                                                UnresolvedTopLevelId::Statement(unresolved_stmt_id) => else_body_spans
                                                    .push(self.unresolved_arena.get_unresolved_stmt_span(*unresolved_stmt_id)),
                                            }
                                        }
                                        return stack_analyzer_error!(
                                            StackAnalyzerErrorKind::MismatchingStackDepths(
                                                if_depth_after - if_depth_before,
                                                else_depth_after - if_depth_before,
                                            ),
                                            &self.arena,
                                            merge_spans(else_body_spans)
                                        );
                                    } else {
                                        panic!(
                                            "[internal error] else body is not a block scope (should already be resolved in \
                                             parser)"
                                        )
                                    }
                                }
                                Some(self.arena.alloc_stmt(Statement::Else(else_body), span.clone()))
                            },
                            UnresolvedStatement::ElseIf { cond: _, body: _, else_stmt: _ } => {
                                // Reset stack to pre-if state for elseif branch
                                self.arena.hfs_stack = stack_before_branches;
                                Some(self.resolve_stmt(else_stmt_id)?)
                            },
                            _ => {
                                panic!("[internal error] can't have other statements in else/else-if statement position")
                            },
                        }
                    },
                    None => {
                        // If no else, the if body must have net-zero stack effect
                        if if_depth_before != if_depth_after {
                            let if_body = self.arena.get_stmt(body.clone());
                            if let Statement::BlockScope(stmts, _) = if_body {
                                let mut if_body_spans = Vec::new();
                                for stmt in stmts {
                                    match stmt {
                                        TopLevelId::VariableDecl(var_id) =>
                                            if_body_spans.push(self.arena.get_var_span(*var_id).clone()),
                                        TopLevelId::FunctionDecl(func_id) =>
                                            if_body_spans.push(self.arena.get_function_span(*func_id).clone()),
                                        TopLevelId::Statement(stmt_id) =>
                                            if_body_spans.push(self.arena.get_stmt_span(*stmt_id).clone()),
                                    }
                                }
                                return stack_analyzer_error!(
                                    StackAnalyzerErrorKind::ExpectedNetZeroStackEffectIfStmt(if_depth_after - if_depth_before),
                                    &self.arena,
                                    merge_spans(if_body_spans)
                                );
                            } else {
                                panic!("[internal error] if body is not a block scope (should be resolved in parser)")
                            }
                        }
                        None
                    },
                };
                if matches!(unresolved_stmt, UnresolvedStatement::ElseIf { .. }) {
                    Ok(self.arena.alloc_stmt(Statement::ElseIf { condition: condition_statements, body, else_stmt }, span))
                } else {
                    Ok(self.arena.alloc_stmt(Statement::If { condition: condition_statements, body, else_stmt }, span))
                }
            },
            UnresolvedStatement::While { body, cond } => {
                let mut condition_statements = Vec::new();
                for stmt in cond {
                    condition_statements.push(self.resolve_stmt(stmt)?);
                }

                let cond = self.arena.pop_or_error(self.unresolved_arena.get_unresolved_stmt_span(body))?;
                // condition isnt included in the stack depth count since its popped when entering
                // each while loop (our stack state is left right after the condition is popped)
                let stack_depth_before = self.arena.hfs_stack.len();

                let cond_type = self.arena.get_type_id_of_expr(cond)?;
                self.arena.compare_types(cond_type, ElaboratedType::new_bool(0).type_id(), vec![
                    self.arena.get_expr_span(cond).clone(),
                ])?;

                // Analyze the body
                let body = self.resolve_stmt(body)?;

                // Enforce stack balance
                let stack_depth_after = self.arena.hfs_stack.len();
                if stack_depth_before != stack_depth_after {
                    if let Statement::BlockScope(stmts, _) = self.arena.get_stmt(body) {
                        let mut while_body_spans = Vec::new();
                        for stmt in stmts {
                            match stmt {
                                TopLevelId::VariableDecl(var_id) => while_body_spans.push(*self.arena.get_var_span(*var_id)),
                                TopLevelId::FunctionDecl(func_id) =>
                                    while_body_spans.push(*self.arena.get_function_span(*func_id)),
                                TopLevelId::Statement(stmt_id) => while_body_spans.push(*self.arena.get_stmt_span(*stmt_id)),
                            }
                        }
                        return stack_analyzer_error!(
                            StackAnalyzerErrorKind::ExpectedNetZeroStackEffectWhileLoop(stack_depth_after - stack_depth_before),
                            &self.arena,
                            merge_spans(while_body_spans)
                        );
                    } else {
                        panic!("[internal error] while body is not a block scope (should be resolved in parser)")
                    }
                }

                Ok(self.arena.alloc_stmt(Statement::While { cond: condition_statements, body }, span))
            },
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => {
                let stack_start = self.arena.hfs_stack.clone();

                for expr_id in unresolved_expr_ids {
                    self.resolve_expr(expr_id)?;
                }
                // when were resolving a stack block, we can consume the previous stack state
                // e.g: if we had:
                // @(1 2 3 4);
                // @(+ + + 2);
                //
                //
                // @(+)
                //
                // we consumed the 2 and the 3,
                // so to find what to push, we grab the initial stack state, and on a cloned version of the current stack,
                // we pop until they differ, so if we had [1, 2, 3] and [1, Add(2, 3)]
                // we would pop the 1 ending up with [Add(2, 3)]
                let mut expr_ids = Vec::<ExprId>::new();
                let (stack_change, consumed_count) = self.arena.get_stack_change(stack_start, self.arena.hfs_stack.clone());
                for &expr_id in &stack_change {
                    expr_ids.push(expr_id);
                }
                Ok(self.arena.alloc_stmt(Statement::StackBlock { expr_ids, consumed_count }, span))
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

                Ok(self.arena.alloc_stmt(Statement::BlockScope(top_level_ids, scope_kind), span))
            },
            UnresolvedStatement::Return => {
                // this is the case where we validate manually written 'return' statements
                // rather than the implicit return at the end of functions
                self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type(), span.clone())?;
                Ok(self.arena.alloc_stmt(Statement::Return, span))
            },
            UnresolvedStatement::Break => {
                if !self.scope_resolution_stack.is_in_while_loop_context() {
                    return stack_analyzer_error!(
                        StackAnalyzerErrorKind::FoundXOutsideWhileLoop(JumpKeyword::Break),
                        &self.arena,
                        self.unresolved_arena.get_unresolved_stmt_span(id)
                    );
                }
                Ok(self.arena.alloc_stmt(Statement::Break, span))
            },
            UnresolvedStatement::Continue => {
                if !self.scope_resolution_stack.is_in_while_loop_context() {
                    return stack_analyzer_error!(
                        StackAnalyzerErrorKind::FoundXOutsideWhileLoop(JumpKeyword::Continue),
                        &self.arena,
                        self.unresolved_arena.get_unresolved_stmt_span(id)
                    );
                }
                Ok(self.arena.alloc_stmt(Statement::Continue, span))
            },
            UnresolvedStatement::Empty => Ok(self.arena.alloc_stmt(Statement::Empty, span)),
            UnresolvedStatement::Assignment { identifier, is_move, deref_count } => {
                // FIXME: do stuff with deref_count
                let assign_span = self.unresolved_arena.get_unresolved_stmt_span(id);
                let value = if is_move {
                    self.arena.pop_or_error(self.unresolved_arena.get_unresolved_stmt_span(id))?
                } else {
                    self.arena.last_or_error(self.unresolved_arena.get_unresolved_stmt_span(id))?
                };

                let resolved_identifier = self.resolve_var_assignment_identifier(
                    identifier,
                    *self.arena.get_expr_provenance(value),
                    deref_count,
                    assign_span,
                )?;

                let value_type_id = self.arena.get_type_id_of_expr(value)?;
                let identifier_type_id = self.arena.get_type_id_of_identifier(resolved_identifier);
                self.arena.compare_types(value_type_id, identifier_type_id, vec![*self.arena.get_expr_span(value)])?;

                Ok(self.arena.alloc_stmt(Statement::Assignment { identifier: resolved_identifier, is_move, deref_count }, span))
            },
            UnresolvedStatement::FunctionCall { identifier, is_move } => {
                // just checks if we actually had a function and finds the identifier
                let func_id =
                    self.resolve_func_call_identifier(identifier, self.unresolved_arena.get_unresolved_stmt_span(id))?;
                let func_decl = self.arena.get_func(func_id).clone(); // make borrow checker happy

                let ElaboratedType::Tuple { type_ids: param_types, .. } = self.arena.get_type(func_decl.param_type) else {
                    panic!("[internal error] functions only recieve tuples at the moment.")
                };
                let mut arg_count = 0;
                let mut arg_types = Vec::new();
                let mut arg_expr_spans = Vec::new();
                let mut arg_exprs = Vec::new();
                for _ in 0..param_types.len() {
                    let arg_expr = self.arena.pop_or_error(self.unresolved_arena.get_unresolved_stmt_span(id))?;
                    arg_expr_spans.push(self.arena.get_expr_span(arg_expr).clone());
                    arg_types.push(self.arena.get_type_id_of_expr(arg_expr)?);
                    arg_exprs.push(arg_expr);
                    arg_count += 1;
                }
                // must reverse because we pop therefore we have our arguments backwards
                // from what the type was written as (this is the language semantics of a "stack view")
                arg_types.reverse();
                arg_expr_spans.reverse();
                let arg_type_id =
                    self.arena.alloc_type(ElaboratedType::Tuple { type_ids: arg_types, ptr_count: 0 }, Span::default());

                if !is_move {
                    // restore the stack
                    for arg_expr in arg_exprs {
                        self.arena.hfs_stack.push(arg_expr);
                    }
                }
                // first make sure calling this function is valid given the stack state
                self.arena.validate_func_call(func_decl.param_type, arg_type_id, arg_expr_spans)?;

                // now make sure the stack is updated based on the return type of the function
                let ElaboratedType::Tuple { type_ids: return_types, .. } = self.arena.get_type(func_decl.return_type) else {
                    panic!("[internal error] functions only return tuples at the moment.")
                };
                let mut return_values = Vec::new();
                for type_id in return_types.clone() {
                    return_values.push(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::ReturnValue(type_id),
                        ExprProvenance::RuntimeValue,
                        span.clone(), // TODO: (joao) some tokens are kinda not precise enough for error reporting.
                                      // you will wanna get more pinpointed tokens for better location info
                    ));
                }
                // function calls dont go to the stack
                Ok(self.arena.alloc_stmt(Statement::FunctionCall { arg_count, func_id, is_move, return_values }, span))
            },
            UnresolvedStatement::Else(_) => panic!("[internal error] else statements are not solved here"),
            UnresolvedStatement::ArrayAssignment { identifier, is_move, deref_count } => {
                let assign_span = self.unresolved_arena.get_unresolved_stmt_span(id);
                let (value, idx) = if is_move {
                    self.arena.pop2_or_error(self.unresolved_arena.get_unresolved_stmt_span(id))?
                } else {
                    self.arena.last2_or_error(self.unresolved_arena.get_unresolved_stmt_span(id))?
                };

                let resolved_identifier = self.resolve_var_assignment_identifier(
                    identifier,
                    *self.arena.get_expr_provenance(value),
                    deref_count,
                    assign_span,
                )?;

                let idx_type_id = self.arena.get_type_id_of_expr(idx)?;
                let value_type_id = self.arena.get_type_id_of_expr(value)?;
                let identifier_type_id = self.arena.get_type_id_of_identifier(resolved_identifier);

                let value_span = self.arena.get_expr_span(value);
                let idx_span = self.arena.get_expr_span(idx);

                if let ElaboratedType::Array { hfs_type, length, .. } = self.arena.get_type(identifier_type_id) {
                    self.arena.compare_types(value_type_id, *hfs_type, vec![value_span.clone()])?;
                    self.arena.compare_types(idx_type_id, ElaboratedType::new_int(0).type_id(), vec![idx_span.clone()])?;
                    match length {
                        Some(ArrayLength::Unresolved(_)) => panic!("[internal error] array length should be resolved by now"),
                        Some(ArrayLength::Resolved(length)) =>
                        // we don't need to check for the provenance of the length expression, that is done when it's resolved
                            if *self.arena.get_expr_provenance(idx) == ExprProvenance::CompiletimeValue {
                                self.check_bounds(&idx, length)?;
                            },
                        None => unimplemented!(),
                    }

                    self.arena.compare_types(value_type_id, *hfs_type, vec![self.arena.get_expr_span(value).clone()])?;
                } else {
                    return stack_analyzer_error!(
                        StackAnalyzerErrorKind::TypeMismatch(
                            ElaboratedType::Array { hfs_type: TypeId(usize::MAX), length: None, ptr_count: 0 }
                                .get_repr(&self.arena),
                            self.arena.get_type(identifier_type_id).get_repr(&self.arena),
                        ),
                        &self.arena,
                        self.unresolved_arena.get_unresolved_expr_span(identifier)
                    );
                }

                Ok(self.arena.alloc_stmt(
                    Statement::ArrayAssignment { identifier: resolved_identifier, is_move, deref_count, position: idx },
                    span,
                ))
            },
        }
    }

    fn resolve_var_assignment_identifier(
        &mut self,
        id: UnresolvedExprId,
        provenance: ExprProvenance,
        deref_count: usize,
        assign_span: Span,
    ) -> Result<Identifier, Box<dyn CompileError>> {
        // neither this or func_call identifier allocate an expression
        let span = self.unresolved_arena.get_unresolved_expr_span(id);
        match self.unresolved_arena.get_unresolved_expr(id) {
            UnresolvedExpression::Identifier(identifier) => {
                let identifier = self.scope_resolution_stack.find_identifier(&identifier, span.clone(), &self.arena)?;
                match identifier {
                    Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                        self.arena.curr_var_provenances[var_id.0] = provenance;
                        // note that solving an identifier updates the provenance
                        if deref_count > 0 {
                            let ptr_count = self.arena.get_type(self.arena.get_var(var_id).hfs_type).get_ptr_count();
                            if deref_count > ptr_count {
                                return stack_analyzer_error!(
                                    StackAnalyzerErrorKind::TooManyDereferences(deref_count, ptr_count),
                                    &self.arena,
                                    span
                                );
                            }
                        }
                        Ok(identifier)
                    },
                    Identifier::Function(_) =>
                        return stack_analyzer_error!(StackAnalyzerErrorKind::AssignValueToFunction, &self.arena, assign_span),
                }
            },
            _ => unreachable!("[internal error] you're assigning to something that isn't an identifier"),
        }
    }
    fn resolve_func_call_identifier(&mut self, id: UnresolvedExprId, assign_span: Span) -> Result<FuncId, Box<dyn CompileError>> {
        // dont allocate an expression for these cases
        let identifier = self.unresolved_arena.get_unresolved_expr(id);
        let span = self.unresolved_arena.get_unresolved_expr_span(id);
        let UnresolvedExpression::Identifier(identifier) = identifier else {
            panic!("[internal error] function call must have identifier")
        };
        let identifier = self.scope_resolution_stack.find_identifier(&identifier, span, &self.arena)?;
        match identifier {
            Identifier::GlobalVar(_) | Identifier::Variable(_) =>
                return stack_analyzer_error!(StackAnalyzerErrorKind::CallVariableAsFunction, &self.arena, assign_span),
            Identifier::Function(func_id) => {
                self.arena.curr_func_call_provenances[func_id.0] = ExprProvenance::RuntimeValue;
                // doing it this way because we dont really care about evaluating functions at
                // compile time for now (so we jut make it runtime)
                Ok(func_id)
            },
        }
    }

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> Result<ExprId, Box<dyn CompileError>> {
        let span = self.unresolved_arena.get_unresolved_expr_span(id);
        match self.unresolved_arena.get_unresolved_expr(id).clone() {
            UnresolvedExpression::Operation(unresolved_operation) => {
                let expr_id = self.resolve_operation(&unresolved_operation, span)?;
                self.arena.get_type_of_expr(expr_id)?;
                Ok(expr_id)
            },
            UnresolvedExpression::Identifier(identifier) => {
                // there should only be identifiers here that were inside the stack scope
                // @(1 2 var foo) // like this example
                let identifier = self.scope_resolution_stack.find_identifier(&identifier, span.clone(), &self.arena)?;
                Ok(self.arena.alloc_and_push_to_hfs_stack(
                    Expression::Identifier(identifier),
                    *self.arena.get_identifier_provenance(identifier),
                    span,
                ))
            },
            UnresolvedExpression::Literal(literal) =>
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Literal(literal), ExprProvenance::CompiletimeValue, span)),
            UnresolvedExpression::Tuple { expressions } => {
                // the tuple's type is formed recursively whenever someone wants it
                // by calling arena.get_type_of_expr(tuple_expr_id); (dont create it here)
                let mut expr_ids = Vec::<ExprId>::new();
                let mut tuple_provenance = ExprProvenance::CompiletimeValue;
                for expr_id in expressions {
                    let expr_id = self.resolve_expr(expr_id)?;
                    expr_ids.push(expr_id);
                    self.arena.pop_or_error(self.arena.get_expr_span(expr_id).clone())?;
                    // clear the stack after we pushed, so that the only thing left on the
                    // stack is the tuple itself, not its arguments (keeps the API agnostic)
                    // (we allocate by pushing and dont want to add a new API just for this to
                    // enforce this intended stack behavior)
                    if matches!(self.arena.get_expr_provenance(expr_id), ExprProvenance::RuntimeValue) {
                        // if any element is runtime, drop comptime from this tuple
                        tuple_provenance = ExprProvenance::RuntimeValue
                    }
                }
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Tuple { expressions: expr_ids }, tuple_provenance, span))
            },
            UnresolvedExpression::StackKeyword(name) => {
                self.perform_stack_keyword(&name, span)?;

                Ok(ExprId(usize::MAX)) // trust me bro
            },
        }
    }

    fn resolve_operation(&mut self, op: &UnresolvedOperation, span: Span) -> Result<ExprId, Box<dyn CompileError>> {
        match op {
            op if op.is_binary() => {
                let (lhs_expr, rhs_expr) = self.arena.pop2_or_error(span.clone())?;
                self.validate_binary_operands(lhs_expr, rhs_expr, op)?;
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
                        span,
                    )),
                    UnresolvedOperation::Sub => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Sub(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Mul => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Mul(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Div => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Div(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Mod => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Mod(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Or => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Or(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::And => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::And(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Equal => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Equal(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::NotEqual => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::NotEqual(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Less => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Less(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::LessEqual => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::LessEqual(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::Greater => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Greater(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::GreaterEqual => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::GreaterEqual(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    UnresolvedOperation::ArrayAccess => Ok(self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::ArrayAccess(lhs_expr, rhs_expr)),
                        provenance,
                        span,
                    )),
                    _ => unreachable!(),
                }
            },
            UnresolvedOperation::Not => {
                let expr = self.arena.pop_or_error(span.clone())?;
                self.validate_unary_operand(expr, op)?;
                let provenance = *self.arena.get_expr_provenance(expr);
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Operation(Operation::Not(expr)), provenance, span))
            },
            UnresolvedOperation::Dereference => {
                let expr = self.arena.pop_or_error(span.clone())?;
                self.validate_unary_operand(expr, op)?;
                let provenance = *self.arena.get_expr_provenance(expr);
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Operation(Operation::Dereference(expr)), provenance, span))
            },
            UnresolvedOperation::AddressOf => {
                let expr = self.arena.pop_or_error(span.clone())?;
                self.validate_unary_operand(expr, op)?;
                let provenance = *self.arena.get_expr_provenance(expr);
                Ok(self.arena.alloc_and_push_to_hfs_stack(Expression::Operation(Operation::AddressOf(expr)), provenance, span))
            },
            _ => unreachable!(),
        }
    }

    fn perform_stack_keyword(&mut self, name: &str, span: Span) -> Result<(), Box<dyn CompileError>> {
        match name {
            "@pop" => {
                self.arena.pop_or_error(span)?;
                Ok(())
            },
            "@pop_all" => {
                self.arena.hfs_stack.clear();
                Ok(())
            },
            "@dup" => {
                let id = self.arena.last_or_error(span)?;
                self.arena.hfs_stack.push(id);
                Ok(())
            },
            "@swap" => {
                let b_id = self.arena.pop_or_error(span)?;
                let a_id = self.arena.pop_or_error(span)?;
                self.arena.hfs_stack.push(b_id);
                self.arena.hfs_stack.push(a_id);
                Ok(())
            },
            "@over" => {
                let b_id = self.arena.pop_or_error(span)?;
                let a_id = self.arena.pop_or_error(span)?;
                self.arena.hfs_stack.push(a_id);
                self.arena.hfs_stack.push(b_id);
                self.arena.hfs_stack.push(a_id);
                Ok(())
            },
            "@rot" => {
                let c_id = self.arena.pop_or_error(span)?;
                let b_id = self.arena.pop_or_error(span)?;
                let a_id = self.arena.pop_or_error(span)?;
                self.arena.hfs_stack.push(b_id);
                self.arena.hfs_stack.push(c_id);
                self.arena.hfs_stack.push(a_id);
                Ok(())
            },
            "@rrot" => {
                let c_id = self.arena.pop_or_error(span)?;
                let b_id = self.arena.pop_or_error(span)?;
                let a_id = self.arena.pop_or_error(span)?;
                self.arena.hfs_stack.push(c_id);
                self.arena.hfs_stack.push(a_id);
                self.arena.hfs_stack.push(b_id);
                Ok(())
            },
            "@nip" => {
                let b_id = self.arena.pop_or_error(span)?;
                let _ = self.arena.pop_or_error(span)?;
                self.arena.hfs_stack.push(b_id);
                Ok(())
            },
            "@tuck" => {
                let b_id = self.arena.pop_or_error(span)?;
                let a_id = self.arena.pop_or_error(span)?;
                self.arena.hfs_stack.push(b_id);
                self.arena.hfs_stack.push(a_id);
                self.arena.hfs_stack.push(b_id);
                Ok(())
            },
            "@rev" => {
                while !self.arena.hfs_stack.is_empty() {
                    let id = self.arena.pop_or_error(span)?;
                    self.arena.hfs_stack.push(id);
                }
                Ok(())
            },
            "@depth" => {
                self.arena.alloc_and_push_to_hfs_stack(
                    Expression::Literal(Literal::Integer(self.arena.hfs_stack.len() as i32)),
                    ExprProvenance::RuntimeValue,
                    span,
                );
                Ok(())
            },

            "@print" => Ok(()),
            _ => {
                panic!("[internal error] invalid stack keyword")
            },
        }
    }

    fn validate_binary_operands(
        &mut self,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: &UnresolvedOperation,
    ) -> Result<(), Box<dyn CompileError>> {
        match op {
            UnresolvedOperation::Add
            | UnresolvedOperation::Sub
            | UnresolvedOperation::Mul
            | UnresolvedOperation::Div
            | UnresolvedOperation::Mod
            | UnresolvedOperation::Or
            | UnresolvedOperation::And
            | UnresolvedOperation::Equal
            | UnresolvedOperation::NotEqual
            | UnresolvedOperation::Less
            | UnresolvedOperation::LessEqual
            | UnresolvedOperation::Greater
            | UnresolvedOperation::GreaterEqual => {
                self.has_valid_types_binary(lhs_expr, rhs_expr, *op)?;
                Ok(self.has_same_types(lhs_expr, rhs_expr)?)
            },
            UnresolvedOperation::ArrayAccess => self.has_valid_types_binary(lhs_expr, rhs_expr, *op),
            _ => panic!("[internal error] unary operation being typechecked in binary context"),
        }
    }

    fn has_same_types(&mut self, lhs_expr: ExprId, rhs_expr: ExprId) -> Result<(), Box<dyn CompileError>> {
        let lhs_type = self.arena.get_type_id_of_expr(lhs_expr)?;
        let rhs_type = self.arena.get_type_id_of_expr(rhs_expr)?;
        Ok(self.arena.compare_types(lhs_type, rhs_type, vec![self.arena.get_expr_span(lhs_expr).clone()])?)
    }

    fn has_valid_types_binary(
        &mut self,
        lhs_expr: ExprId,
        rhs_expr: ExprId,
        op: UnresolvedOperation,
    ) -> Result<(), Box<dyn CompileError>> {
        let lhs_type = self.arena.get_type_id_of_expr(lhs_expr)?;
        let rhs_type = self.arena.get_type_id_of_expr(rhs_expr)?;
        match op {
            UnresolvedOperation::Less
            | UnresolvedOperation::LessEqual
            | UnresolvedOperation::Greater
            | UnresolvedOperation::GreaterEqual
            | UnresolvedOperation::Add
            | UnresolvedOperation::Sub
            | UnresolvedOperation::Mul
            | UnresolvedOperation::Div => {
                self.arena
                    .compare_types(lhs_type, ElaboratedType::new_int(0).type_id(), vec![
                        self.arena.get_expr_span(lhs_expr).clone(),
                    ])
                    .or(self.arena.compare_types(lhs_type, ElaboratedType::new_float(0).type_id(), vec![
                        self.arena.get_expr_span(lhs_expr).clone(),
                    ]))?;
                Ok(self
                    .arena
                    .compare_types(rhs_type, ElaboratedType::new_int(0).type_id(), vec![
                        self.arena.get_expr_span(rhs_expr).clone(),
                    ])
                    .or(self.arena.compare_types(rhs_type, ElaboratedType::new_float(0).type_id(), vec![
                        self.arena.get_expr_span(rhs_expr).clone(),
                    ]))?)
            },
            UnresolvedOperation::Mod => {
                self.arena.compare_types(lhs_type, ElaboratedType::new_int(0).type_id(), vec![
                    self.arena.get_expr_span(lhs_expr).clone(),
                ])?;
                Ok(self.arena.compare_types(rhs_type, ElaboratedType::new_int(0).type_id(), vec![
                    self.arena.get_expr_span(rhs_expr).clone(),
                ])?)
            },
            UnresolvedOperation::Or | UnresolvedOperation::And => {
                self.arena.compare_types(lhs_type, ElaboratedType::new_bool(0).type_id(), vec![
                    self.arena.get_expr_span(lhs_expr).clone(),
                ])?;
                Ok(self.arena.compare_types(rhs_type, ElaboratedType::new_bool(0).type_id(), vec![
                    self.arena.get_expr_span(rhs_expr).clone(),
                ])?)
            },
            UnresolvedOperation::NotEqual | UnresolvedOperation::Equal => Ok(()),
            UnresolvedOperation::ArrayAccess =>
                if let ElaboratedType::Array { length, .. } = self.arena.get_type(lhs_type) {
                    self.arena.compare_types(rhs_type, ElaboratedType::new_int(0).type_id(), vec![
                        self.arena.get_expr_span(rhs_expr).clone(),
                    ])?;
                    match length {
                        Some(ArrayLength::Unresolved(_)) => panic!("[internal error] array length should be resolved by now"),
                        Some(ArrayLength::Resolved(expr)) =>
                        // we don't need to check for the provenance of the length expression, that is done when it's resolved
                            if *self.arena.get_expr_provenance(rhs_expr) == ExprProvenance::CompiletimeValue {
                                self.check_bounds(&rhs_expr, expr)
                            } else {
                                Ok(()) // emit at runtime
                            },
                        None => unimplemented!(),
                    }
                } else {
                    stack_analyzer_error!(
                        StackAnalyzerErrorKind::TypeMismatch(
                            ElaboratedType::Array { hfs_type: TypeId(usize::MAX), length: None, ptr_count: 0 }
                                .get_repr(&self.arena),
                            self.arena.get_type(lhs_type).get_repr(&self.arena)
                        ),
                        &self.arena,
                        *self.arena.get_expr_span(lhs_expr)
                    )
                },
            _ => panic!("[internal error] unary operation being typechecked in binary context"),
        }
    }

    fn validate_unary_operand(&mut self, expr: ExprId, op: &UnresolvedOperation) -> Result<(), Box<dyn CompileError>> {
        let lhs_type = self.arena.get_type_id_of_expr(expr)?;
        match op {
            UnresolvedOperation::Not => Ok(self
                .arena
                .compare_types(lhs_type, ElaboratedType::new_bool(0).type_id(), vec![self.arena.get_expr_span(expr).clone()])?),
            UnresolvedOperation::AddressOf => Ok(()),
            UnresolvedOperation::Dereference =>
                if self.arena.get_type(lhs_type).get_ptr_count() > 0 {
                    Ok(())
                } else {
                    stack_analyzer_error!(
                        StackAnalyzerErrorKind::TooManyDereferences(1, self.arena.get_type(lhs_type).get_ptr_count()),
                        &self.arena,
                        *self.arena.get_expr_span(expr)
                    )
                },
            _ => panic!("[internal error] binary operation being typechecked in unary context"),
        }
    }

    fn elaborate(&mut self, unresolved_type: UnresolvedType) -> Result<ElaboratedType, Box<dyn CompileError>> {
        match unresolved_type {
            UnresolvedType::Int { ptr_count } => Ok(ElaboratedType::Int { ptr_count }),
            UnresolvedType::String { ptr_count } => Ok(ElaboratedType::String { ptr_count }),
            UnresolvedType::Bool { ptr_count } => Ok(ElaboratedType::Bool { ptr_count }),
            UnresolvedType::Float { ptr_count } => Ok(ElaboratedType::Float { ptr_count }),
            UnresolvedType::Tuple { type_ids, ptr_count } => Ok(ElaboratedType::Tuple { type_ids, ptr_count }),
            UnresolvedType::Array { hfs_type, length, ptr_count } => Ok(ElaboratedType::Array {
                hfs_type,
                length: match length {
                    // arrays stay unresolved at elaboration because resolving them
                    // requires knowing information about compile-time variables before elaborating types
                    Some(expr) => Some(ArrayLength::Unresolved(expr)),
                    None => None,
                },
                ptr_count,
            }),
        }
    }

    fn check_bounds(&self, index: &ExprId, length: &ExprId) -> Result<(), Box<dyn CompileError>> {
        match (self.arena.get_expr(*index), self.arena.get_expr(*length)) {
            (Expression::Literal(Literal::Integer(idx)), Expression::Literal(Literal::Integer(len))) =>
                if *idx >= 0 && *idx < *len {
                    Ok(())
                } else {
                    return stack_analyzer_error!(
                        StackAnalyzerErrorKind::IndexOutOfBounds(*idx, *len),
                        &self.arena,
                        *self.arena.get_expr_span(*index)
                    );
                },
            (_, _) => Ok(()),
        }
    }

    fn resolve_array(
        &mut self,
        hfs_type: TypeId,
        span: Span,
        error_fn: impl Fn(String) -> StackAnalyzerErrorKind + Copy,
    ) -> Result<TypeId, Box<dyn CompileError>> {
        match self.arena.get_type(hfs_type).clone() {
            ElaboratedType::Array { hfs_type, length: Some(ArrayLength::Unresolved(id)), ptr_count } => {
                let hfs_type = self.resolve_array(hfs_type, span.clone(), error_fn)?;

                let resolved_expr_id = self.resolve_expr(id)?;
                let length_span = self.arena.get_expr_span(resolved_expr_id).clone();
                self.arena.pop_or_error(length_span.clone())?;

                if *self.arena.get_expr_provenance(resolved_expr_id) != ExprProvenance::CompiletimeValue {
                    let name = self.arena.get_type(hfs_type).get_repr(&self.arena);
                    return stack_analyzer_error!(error_fn(name), &self.arena, length_span);
                }

                Ok(self.arena.alloc_type(
                    ElaboratedType::Array { hfs_type, length: Some(ArrayLength::Resolved(resolved_expr_id)), ptr_count },
                    span,
                ))
            },
            ElaboratedType::Tuple { type_ids, ptr_count } => {
                let mut resolved_ids = Vec::new();
                let mut changed = false;
                for id in type_ids.iter() {
                    let resolved = self.resolve_array(*id, span.clone(), error_fn)?;
                    changed |= resolved != *id;
                    resolved_ids.push(resolved);
                }
                if changed {
                    Ok(self.arena.alloc_type(ElaboratedType::Tuple { type_ids: resolved_ids, ptr_count }, span))
                } else {
                    Ok(hfs_type)
                }
            },
            _ => Ok(hfs_type),
        }
    }
}
