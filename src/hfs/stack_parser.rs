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
// Stack Parser (2nd pass over the AST)
// ============================================================================
// this is used to finish up the AST we started in the first parser pass

// ============================================================================
// StackParser (second pass)
// ============================================================================

pub struct StackParser<'a> {
    unresolved_arena: UnresolvedAstArena<'a>,
    arena: AstArena<'a>,
}

impl<'a> StackParser<'a> {
    pub fn new(unresolved: UnresolvedAstArena<'a>) -> Self {
        Self {
            unresolved_arena: unresolved,
            arena: AstArena::new(),
        }
    }

    pub fn resolve(mut self, top_level: Vec<UnresolvedTopLevelId>) -> (Vec<TopLevelId>, AstArena<'a>) {
        // Move token vectors directly - indices will match since we process in order
        let resolved_top_level = self.resolve_top_level(top_level);
        self.arena.var_tokens = self.unresolved_arena.unresolved_var_tokens;
        self.arena.function_tokens = self.unresolved_arena.unresolved_function_tokens;
        self.arena.expr_tokens = self.unresolved_arena.unresolved_expr_tokens;
        self.arena.stmt_tokens = self.unresolved_arena.unresolved_stmt_tokens;
        (resolved_top_level, self.arena)
    }

    fn resolve_top_level(&mut self, nodes: Vec<UnresolvedTopLevelId>) -> Vec<TopLevelId> {
        let mut resolved_nodes = Vec::<TopLevelId>::new();
        for node in nodes {
            let new_node = match node {
                UnresolvedTopLevelId::VariableDecl(id) => TopLevelId::VariableDecl(self.resolve_var_decl(id)),
                UnresolvedTopLevelId::FunctionDecl(id) => TopLevelId::FunctionDecl(self.resolve_func_decl(id)),
                UnresolvedTopLevelId::Statement(id)    => TopLevelId::Statement(self.resolve_stmt(id))
            };
            resolved_nodes.push(new_node);
        }
        resolved_nodes
    }


    fn resolve_var_decl(&mut self, id: UnresolvedVarId) -> VarId {
        let unresolved_var = &self.unresolved_arena.unresolved_vars[id.0];
        
        let var = VarDeclaration {
            name: unresolved_var.name.clone(),
            hfs_type: unresolved_var.hfs_type.clone(),
        };
        self.arena.alloc_var(var)
    }

    fn resolve_func_decl(&mut self, id: UnresolvedFuncId) -> FuncId {
        let (name, param_type, return_type, body) = {
            let unresolved_func = &self.unresolved_arena.unresolved_functions[id.0];
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
        todo!()
    }

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> ExprId {
        todo!()
    }

    fn resolve_stack_block(&mut self, exprs: &[UnresolvedExprId]) -> StmtId {
        // TODO: Process expressions and manage hfs_stack
        todo!()
    }
}
