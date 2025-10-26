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
pub struct StackParser<'a> {
    unresolved: UnresolvedAstArena<'a>,
    resolved: AstArena<'a>,
}

impl<'a> StackParser<'a> {
    pub fn new(unresolved: UnresolvedAstArena<'a>) -> Self {
        Self {
            unresolved,
            resolved: AstArena::new(),
        }
    }

    pub fn resolve(mut self, top_level: Vec<UnresolvedTopLevelId>) -> (Vec<TopLevelId>, AstArena<'a>) {
        let resolved_top_level = self.resolve_top_level(top_level);
        (resolved_top_level, self.resolved)
    }

    fn resolve_top_level(&mut self, nodes: Vec<UnresolvedTopLevelId>) -> Vec<TopLevelId> {
        // TODO: You'll implement this
        todo!()
    }

    fn resolve_stmt(&mut self, id: UnresolvedStmtId) -> StmtId {
        // TODO: You'll implement this
        todo!()
    }

    fn resolve_expr(&mut self, id: UnresolvedExprId) -> ExprId {
        // TODO: You'll implement this
        todo!()
    }

    fn resolve_stack_block(&mut self, exprs: &[UnresolvedExprId]) -> StmtId {
        // TODO: Process expressions and manage hfs_stack
        todo!()
    }
}
