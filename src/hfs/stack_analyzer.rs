use std::{collections::HashMap, rc::Rc};

// Expression stack methods (used to finish building up the AST)
// we need to make sure expressions have been pushed throughout the program
// to do this, we keep track of which expressions have been pushed
// (aka, the contents of a `@(123 123 123)` stack block along the program
// note that if you just wanted to interpret the language, we could already be done.
// this is 'basically' interpreting, but only at the level of "is it in the stack or not"
// the goal is to make Henceforth a compiled language, therefore we do multiple stack passes
use super::*;

impl<'a> AstArena<'a> {
    // clears the stack and returns it to the user
    pub fn pop_entire_hfs_stack(&mut self) -> Vec<ExprId> {
        let temp = self.hfs_stack.clone();
        self.hfs_stack.clear();
        temp
    }
    // Stack methods (manage the hfs stack for operations)
    pub fn pop_or_error(&mut self, msg: &str) -> ExprId {
        // should start using our own error structs instead
        self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg))
    }
    pub fn last_or_error(&self, msg: &str) -> ExprId {
        // should start using our own error structs instead
        *self.hfs_stack.last().unwrap_or_else(|| panic!("{}", msg))
    }
    pub fn pop2_or_error(&mut self, msg: &str) -> (ExprId, ExprId) {
        // should start using our own error structs instead
        let rhs = self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg));
        let lhs = self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg));
        (lhs, rhs)
    }

    pub fn popn_or_error(&mut self, n: usize, msg: &str) -> Vec<ExprId> {
        let mut popped: Vec<ExprId> = (0..n).map(|_| self.hfs_stack.pop().unwrap_or_else(|| panic!("{}", msg))).collect();
        popped.reverse();
        popped
    }

    pub fn validate_return_stack(&mut self, return_type: TypeId) {
        let Type::Tuple(return_types) = self.get_type(return_type) else {
            panic!("[internal error] functions only return tuples at the moment.")
        };
        let return_types = return_types.clone();
        let expected_count = return_types.len();
        let actual_count = self.hfs_stack.len();
        if expected_count != actual_count {
            panic!("expected {} values on stack for return, found {}", expected_count, actual_count)
        }
        let back_of_stack = &self.hfs_stack.clone()[actual_count - expected_count..];

        for (expr_id, expected_type_id) in back_of_stack.iter().zip(return_types.iter()) {
            let actual_type_id = self.get_type_id_of_expr(*expr_id);
            if let Err(err) = self.validate_type(actual_type_id, *expected_type_id) {
                panic!("return value {}", err);
            }
        }
    }
    pub fn validate_func_call(&self, param_type: TypeId, arg_type: TypeId) {
        let Type::Tuple(_) = self.get_type(arg_type) else {
            panic!("expected tuple on stack before function call")
        };
        let Type::Tuple(_) = self.get_type(param_type) else {
            panic!("[internal error] function parameter type must be a tuple")
        };

        if let Err(err) = self.validate_type(arg_type, param_type) {
            panic!("function call argument {}", err);
        }
    }

    pub fn validate_type(&self, actual_type_id: TypeId, expected_type_id: TypeId) -> Result<(), String> {
        let actual_type = self.get_type(actual_type_id);
        let expected_type = self.get_type(expected_type_id);

        match (actual_type, expected_type) {
            (Type::Tuple(actual_types), Type::Tuple(expected_types)) => {
                if actual_types.len() != expected_types.len() {
                    return Err(format!(
                        "tuple length mismatch: expected {} elements, found {}",
                        expected_types.len(),
                        actual_types.len()
                    ));
                }
                // Recursively validate each element
                for (i, (&actual_elem_id, &expected_elem_id)) in actual_types.iter().zip(expected_types.iter()).enumerate() {
                    self.validate_type(actual_elem_id, expected_elem_id)
                        .map_err(|err| format!("in tuple element {}: {}", i, err))?;
                }
                Ok(())
            },
            (actual, expected) if actual == expected => Ok(()),
            (actual, expected) => Err(format!("type mismatch: expected '{:?}', found '{:?}'", expected, actual)),
        }
    }
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
        let mut arena = AstArena::new();
        arena.types.extend_from_slice(&unresolved.types[4..]);
        Self { arena, unresolved_arena: unresolved, scope_resolution_stack: ScopeStack::new(file_name) }
    }

    pub fn resolve(
        top_level: Vec<UnresolvedTopLevelId>,
        unresolved: UnresolvedAstArena<'a>,
        file_name: String,
    ) -> (Vec<TopLevelId>, AstArena<'a>, ScopeStack) {
        let mut stack_parser = StackAnalyzer::new(unresolved, file_name);
        let resolved_top_level = stack_parser.resolve_top_level(top_level);
        (resolved_top_level, stack_parser.arena, stack_parser.scope_resolution_stack)
    }

    fn resolve_top_level(&mut self, nodes: Vec<UnresolvedTopLevelId>) -> Vec<TopLevelId> {
        let mut resolved_nodes = Vec::<TopLevelId>::new();
        for node in nodes.clone() {
            let new_node = match node {
                UnresolvedTopLevelId::VariableDecl(id) => TopLevelId::VariableDecl(self.resolve_var_decl(id)),
                UnresolvedTopLevelId::FunctionDecl(id) => TopLevelId::FunctionDecl(self.resolve_func_decl(id)),
                UnresolvedTopLevelId::Statement(id) => TopLevelId::Statement(self.resolve_stmt(id)),
            };
            resolved_nodes.push(new_node);
        }
        resolved_nodes
    }

    fn resolve_var_decl(&mut self, id: UnresolvedVarId) -> VarId {
        let unresolved_var = self.unresolved_arena.get_unresolved_var(id);
        let token = self.unresolved_arena.get_unresolved_var_token(id);

        let var_id = self
            .arena
            .alloc_var(VarDeclaration { name: unresolved_var.name.clone(), hfs_type: unresolved_var.hfs_type.clone() }, token);
        self.scope_resolution_stack.push_variable(&unresolved_var.name, var_id);
        var_id
    }

    fn resolve_func_decl(&mut self, id: UnresolvedFuncId) -> FuncId {
        // FIXME: keep track of the current function declaration
        // and distribute the parameter_exprs to people that utilize the stack
        // that came from the function arguments
        // later map these ExprId to runtime values when we call the function
        // and keep remapping those values every call
        // so they can be used as keys to runtime values later because the body
        // holds these exprids and requests the runtime value using them
        // they work as unique identifiers for the parameters basically
        let token = self.unresolved_arena.get_unresolved_func_token(id);
        let (name, param_type, return_type, unresolved_body, parameter_exprs) = {
            let unresolved_func = &self.unresolved_arena.get_unresolved_func(id);
            // deconstruct parameter tuple into Vec<Expression::Parameter>
            let mut params = Vec::<ExprId>::new();
            if let Type::Tuple(param_types) = self.unresolved_arena.get_type(unresolved_func.param_type) {
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

        let func = FunctionDeclaration {
            name: name.clone(),
            param_type,
            return_type,
            body: self.arena.temporarily_get_next_stmt_id(),
            parameter_exprs,
        }; // needed for recursive functions AND to match the correct token

        //------------------------------------------------------------

        //------------------------------------------------------------
        // we push scopes so the body can solve identifiers
        let func_id = self.push_function_and_scope_and_alloc(&name, func, token);
        let body = self.resolve_stmt(unresolved_body);
        //------------------------------------------------------------
        // TODO: when you add the CFG you want every block to end at the same "end" block which is
        // just the end of the function unless its a return statement/break, etc (llvm has ret
        // instructions). that means its up to YOU in the CFG pass to validate the stack sizes and
        // while you are at it, maybe the types too. so you should use validate_return_stack()
        // yourself whenever you find out that we are "leaving" a function.
        // one REALLY good approach is to have every block always go to our "end" block. llvm will
        // likely optimize that away and simplifies stuff.
        self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type());
        self.scope_resolution_stack.pop();
        self.arena.pop_entire_hfs_stack(); // context should be reset after each function!
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
            UnresolvedStatement::If { body, else_stmt, cond } => {
                let stack_before_branches = self.arena.hfs_stack.clone();
                // actually adds the condition to the hfs_stack, because the stack analyzer only "sees" the if statement itself
                self.resolve_stmt(cond);

                // condition isnt included in the stack depth count
                let cond = self.arena.pop_or_error("expected boolean on stack for if statement");
                if !matches!(self.arena.get_type_of_expr(cond), Type::Bool) {
                    panic!("expected expression of type 'bool' in if statement condition")
                }

                // Analyze if body
                let if_depth_before = self.arena.hfs_stack.len();
                let body = self.resolve_stmt(body);
                let if_depth_after = self.arena.hfs_stack.len();

                self.arena.hfs_stack.push(cond);

                let else_stmt = match else_stmt {
                    Some(UnresolvedElseStmt::Else(stmt_id)) => {
                        // Reset stack to pre-if state for else branch
                        self.arena.hfs_stack = stack_before_branches;
                        let else_body = self.resolve_stmt(stmt_id);
                        let else_depth_after = self.arena.hfs_stack.len();

                        // Both branches must have same stack effect
                        if if_depth_after != else_depth_after {
                            panic!(
                                "if/else branches must have matching stack effects: if branch results in {} values, else branch \
                                 results in {}",
                                if_depth_after - if_depth_before,
                                else_depth_after - if_depth_before
                            );
                        }
                        Some(ElseStmt::Else(else_body))
                    },
                    Some(UnresolvedElseStmt::ElseIf(stmt_id)) => {
                        self.arena.hfs_stack = stack_before_branches;
                        Some(ElseStmt::ElseIf(self.resolve_stmt(stmt_id)))
                    },
                    None => {
                        // If no else, the if body must have net-zero stack effect
                        if if_depth_before != if_depth_after {
                            panic!(
                                "if statement must have net-zero stack effect, but changed stack by {}",
                                (if_depth_after as i32) - (if_depth_before as i32)
                            );
                        }
                        None
                    },
                };

                self.arena.alloc_stmt(Statement::If { cond, body, else_stmt }, token)
            },
            UnresolvedStatement::While { body, cond } => {
                // TODO: decide how while loops should work. consume on entry or not?
                self.resolve_stmt(cond);

                let cond = self.arena.pop_or_error("expected value on stack for while loop argument");
                // condition isnt included in the stack depth count since its popped when entering
                // each while loop (our stack state is left right after the condition is popped)
                let stack_depth_before = self.arena.hfs_stack.len();

                if !matches!(self.arena.get_type_of_expr(cond), Type::Bool) {
                    panic!("expected expression of type 'bool' in while loop condition")
                }

                // Analyze the body
                let body = self.resolve_stmt(body);

                // Enforce stack balance
                let stack_depth_after = self.arena.hfs_stack.len();
                if stack_depth_before != stack_depth_after {
                    panic!(
                        "while loop body must maintain stack balance: expected {} values on stack after loop body, found {}",
                        stack_depth_before, stack_depth_after
                    );
                }

                self.arena.alloc_stmt(Statement::While { cond, body }, token)
            },
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => {
                let stack_start = self.arena.hfs_stack.clone();

                for expr_id in unresolved_expr_ids {
                    self.resolve_expr(expr_id);
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
                self.arena.alloc_stmt(Statement::StackBlock(expr_ids), token)
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
                    temp
                } else {
                    self.resolve_top_level(unresolved_top_level_ids)
                };

                self.arena.alloc_stmt(Statement::BlockScope(top_level_ids, scope_kind), token)
            },
            UnresolvedStatement::Return => {
                self.arena.validate_return_stack(self.scope_resolution_stack.get_curr_func_return_type());
                self.arena.alloc_stmt(Statement::Return, token)
            },
            UnresolvedStatement::Break => {
                if !self.scope_resolution_stack.is_in_while_loop_context() {
                    panic!("found break statement outside while loop.")
                }
                self.arena.alloc_stmt(Statement::Break, token)
            },
            UnresolvedStatement::Continue => {
                if !self.scope_resolution_stack.is_in_while_loop_context() {
                    panic!("found continue statement outside while loop.")
                }
                self.arena.alloc_stmt(Statement::Continue, token)
            },
            UnresolvedStatement::Empty => self.arena.alloc_stmt(Statement::Empty, token),
            UnresolvedStatement::Assignment { identifier, is_move } => {
                let value = if is_move {
                    self.arena.pop_or_error("expected value in stack for move assignment statement.")
                } else {
                    self.arena.last_or_error("expected value in stack for copy assignment statement.")
                };
                let identifier = self.resolve_var_assignment_identifier(identifier, *self.arena.get_expr_provenance(value));
                self.arena.alloc_stmt(Statement::Assignment { value, identifier, is_move }, token)
            },
            UnresolvedStatement::FunctionCall { identifier, is_move } => {
                // just checks if we actually had a function and finds the identifier
                let func_id = self.resolve_func_call_identifier(identifier);
                let func_decl = self.arena.get_func(func_id).clone(); // make borrow checker happy

                let Type::Tuple(param_types) = self.arena.get_type(func_decl.param_type) else {
                    panic!("[internal error] functions only recieve tuples at the moment.")
                };
                let mut expressions = Vec::<ExprId>::new();
                let mut arg_types = Vec::<TypeId>::new();
                for _ in 0..param_types.len() {
                    let arg_expr = self.arena.pop_or_error("expected more elements on stack for function call");
                    arg_types.push(self.arena.get_type_id_of_expr(arg_expr));
                    expressions.push(arg_expr);
                }
                let arg_type_id = self.arena.alloc_type(Type::Tuple(arg_types), Token {
                    kind: TokenKind::LeftParen,
                    source_info: SourceInfo::new(0, 0, 0, "Arg Tuple(function call)"),
                });
                // first make sure calling this function is valid given the stack state
                self.arena.validate_func_call(func_decl.param_type, arg_type_id);

                // now make sure the stack is updated based on the return type of the function
                let Type::Tuple(return_types) = self.arena.get_type(func_decl.return_type) else {
                    panic!("[internal error] functions only return tuples at the moment.")
                };
                // function calls dont go to the stack
                self.arena.alloc_stmt(Statement::FunctionCall { args: expressions, identifier: func_id, is_move }, token)
            },
        }
    }

    fn resolve_var_assignment_identifier(&mut self, id: UnresolvedExprId, provenance: ExprProvenance) -> Identifier {
        // neither this or func_call identifier allocate an expression
        let token = self.unresolved_arena.get_unresolved_expr_token(id);
        match self.unresolved_arena.get_unresolved_expr(id) {
            UnresolvedExpression::Identifier(identifier) => {
                let identifier = self.scope_resolution_stack.find_identifier(&identifier);
                match identifier {
                    Identifier::GlobalVar(var_id) | Identifier::Variable(var_id) => {
                        self.arena.curr_var_provenances[var_id.0] = provenance;
                        // note that solving an identifier updates the provenance
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

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> ExprId {
        let token = self.unresolved_arena.get_unresolved_expr_token(id);
        match self.unresolved_arena.get_unresolved_expr(id).clone() {
            UnresolvedExpression::Operation(unresolved_operation) => self.resolve_operation(&unresolved_operation, token),
            UnresolvedExpression::Identifier(identifier) => {
                // there should only be identifiers here that were inside the stack scope
                // @(1 2 var foo) // like this example
                let identifier = self.scope_resolution_stack.find_identifier(&identifier);
                self.arena.alloc_and_push_to_hfs_stack(
                    Expression::Identifier(identifier),
                    *self.arena.get_identifier_provenance(identifier),
                    token,
                )
            },
            UnresolvedExpression::Literal(literal) =>
                self.arena.alloc_and_push_to_hfs_stack(Expression::Literal(literal), ExprProvenance::CompiletimeValue, token),
            UnresolvedExpression::Tuple { expressions } => {
                // the tuple's type is formed recursively whenever someone wants it
                // by calling arena.get_type_of_expr(tuple_expr_id); (dont create it here)
                let mut expr_ids = Vec::<ExprId>::new();
                let mut tuple_provenance = ExprProvenance::CompiletimeValue;
                for expr_id in expressions {
                    let expr_id = self.resolve_expr(expr_id);
                    expr_ids.push(expr_id);
                    self.arena.pop_or_error("[internal error] didnt push to the stack properly");
                    // clear the stack after we pushed, so that the only thing left on the
                    // stack is the tuple itself, not its arguments (keeps the API agnostic)
                    // (we allocate by pushing and dont want to add a new API just for this to
                    // enforce this intended stack behavior)
                    if matches!(self.arena.get_expr_provenance(expr_id), ExprProvenance::RuntimeValue) {
                        // if any element is runtime, drop comptime from this tuple
                        tuple_provenance = ExprProvenance::RuntimeValue
                    }
                }
                self.arena.alloc_and_push_to_hfs_stack(Expression::Tuple { expressions: expr_ids }, tuple_provenance, token)
            },
            UnresolvedExpression::StackKeyword(name) => {
                let kw_declaration = self.arena.get_stack_keyword_from_name(name.as_str());

                // Copy out the function pointer so we dont have mutable borrow while there is an immutable borrow
                // let effect = kw_declaration.type_effect;

                let args = match kw_declaration.expected_args_size {
                    Some(n) => self
                        .arena
                        .popn_or_error(n, format!("expected {} arguments for stack keyword {}", n, kw_declaration.name).as_str()),
                    None => self.arena.pop_entire_hfs_stack(), // for @pop_all
                };
                todo!();
                // // simulate stack
                // let simulated_stack: Vec<TypeId> = args.iter().map(|id| self.arena.get_type_id_of_expr(*id)).collect();
                // let return_values = effect(simulated_stack);
                // let mut return_value_ids = Vec::new();
                // return_values
                //     .iter()
                //     .for_each(|ret| {
                //         if let Expression::ReturnValue(id) = ret {
                //             let return_val_id = self
                //                 .arena
                //                 .alloc_and_push_to_hfs_stack(ret.clone(), ExprProvenance::RuntimeValue,
                //                                              self.arena.get_type_token(*id).clone());
                //             return_value_ids.push(return_val_id);
                //         } else {
                //             panic!("[internal error] stack keyword effect should return a Expression::ReturnValue")
                //         }
                // });
                //
                // let id = self.arena.alloc_and_push_to_hfs_stack(Expression::StackKeyword { name, args, return_values: return_value_ids },
                //                                                 ExprProvenance::RuntimeValue, token);
                // self.arena.pop_or_error("could not pop stack keyword");
                // id
            },
        }
    }

    fn resolve_operation(&mut self, op: &UnresolvedOperation, token: Token<'a>) -> ExprId {
        match op {
            op if op.is_binary() => {
                let (lhs_expr, rhs_expr) = self
                    .arena
                    .pop2_or_error(format!("expected at least 2 values in stack for binary operation '{:?}'", op).as_str());
                let provenance = if matches!(self.arena.get_expr_provenance(lhs_expr), ExprProvenance::CompiletimeValue)
                    && matches!(self.arena.get_expr_provenance(lhs_expr), ExprProvenance::CompiletimeValue)
                {
                    ExprProvenance::CompiletimeValue
                } else {
                    ExprProvenance::RuntimeValue
                };
                match op {
                    UnresolvedOperation::Add => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Add(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Sub => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Sub(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Mul => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Mul(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Div => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Div(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Mod => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Mod(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Or => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Or(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::And => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::And(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Equal => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Equal(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::NotEqual => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::NotEqual(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Less => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Less(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::LessEqual => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::LessEqual(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::Greater => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::Greater(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    UnresolvedOperation::GreaterEqual => self.arena.alloc_and_push_to_hfs_stack(
                        Expression::Operation(Operation::GreaterEqual(lhs_expr, rhs_expr)),
                        provenance,
                        token,
                    ),
                    _ => unreachable!(),
                }
            },
            UnresolvedOperation::Not => {
                let expr = self
                    .arena
                    .pop_or_error(format!("expected at least 1 value in stack for unary operation '{:?}'", op).as_str());
                let provenance = *self.arena.get_expr_provenance(expr);
                self.arena.alloc_and_push_to_hfs_stack(Expression::Operation(Operation::Not(expr)), provenance, token)
            },
            _ => panic!("missing semantic analysis for unary operation '{:?}'", op),
        }
    }
}
