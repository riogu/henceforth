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
}

// ============================================================================
// Stack Analyzer (2nd pass over the AST)
// this is used to finish up the AST we started in the parser pass
// we also need to solve identifiers here, otherwise we cant check 
// how function calls/assignments change the stack
// ============================================================================
pub struct StackAnalyzer<'a> {
    unresolved_arena: UnresolvedAstArena<'a>,
    arena: AstArena<'a>,
}

impl<'a> StackAnalyzer<'a> {
    pub fn new(unresolved: UnresolvedAstArena<'a>) -> Self {
        Self {
            unresolved_arena: unresolved,
            arena: AstArena::new(),
        }
    }

    pub fn resolve(top_level: Vec<UnresolvedTopLevelId>, unresolved: UnresolvedAstArena<'a>) -> (Vec<TopLevelId>, AstArena<'a>) {
        let mut stack_parser = StackAnalyzer::new(unresolved);

        let resolved_top_level = stack_parser.resolve_top_level(top_level);
        // Move token vectors directly - indices will match since we process in order
        stack_parser.arena.expr_tokens      = stack_parser.unresolved_arena.unresolved_expr_tokens;
        stack_parser.arena.stmt_tokens      = stack_parser.unresolved_arena.unresolved_stmt_tokens;
        stack_parser.arena.var_tokens       = stack_parser.unresolved_arena.unresolved_var_tokens;
        stack_parser.arena.function_tokens  = stack_parser.unresolved_arena.unresolved_function_tokens;

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
        // stays the same, just copy over
        let unresolved_var = &self.unresolved_arena.get_unresolved_var(id);
        self.arena.alloc_var(VarDeclaration { name: unresolved_var.name.clone(), hfs_type: unresolved_var.hfs_type.clone() })
    }

    fn resolve_func_decl(&mut self, id: UnresolvedFuncId) -> FuncId {
        let (name, param_type, return_type, body) = {
            let unresolved_func = &self.unresolved_arena.get_unresolved_func(id);
            (
                unresolved_func.name.clone(),
                unresolved_func.param_type.clone(),
                unresolved_func.return_type.clone(),
                self.resolve_stmt(unresolved_func.body),
            )
        };
        let func = FunctionDeclaration { name, param_type, return_type, body, };
        self.arena.alloc_function(func)
    }

    fn resolve_stmt(&mut self, id: UnresolvedStmtId) -> StmtId {
        match self.unresolved_arena.get_unresolved_stmt(id) {
            UnresolvedStatement::If { body, else_stmt } => todo!(),
            UnresolvedStatement::While { body } => todo!(),
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => todo!(),
            UnresolvedStatement::BlockScope(unresolved_top_level_ids) => todo!(),
            UnresolvedStatement::Return => todo!(),
            UnresolvedStatement::Break => todo!(),
            UnresolvedStatement::Continue => todo!(),
            UnresolvedStatement::Empty => todo!(),
            UnresolvedStatement::Assignment { identifier, is_move } => todo!(),
        }
        todo!()
    }

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> ExprId {
        match self.unresolved_arena.get_unresolved_expr(id) {
            UnresolvedExpression::Operation(unresolved_operation) => todo!(),
            UnresolvedExpression::Identifier(_) => todo!(),
            UnresolvedExpression::Literal(literal) => todo!(),
            UnresolvedExpression::FunctionCall { identifier } => todo!(),
            UnresolvedExpression::Tuple { expressions, variadic } => todo!(),
        }
        todo!()
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
        todo!()
    }

}
