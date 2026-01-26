use crate::hfs::{
    builder::builder::{Builder, ControlFlowOps, FunctionOps, LoopOps, StackOps, VariableOps},
    AstArena, ExprId, Expression, ScopeKind, StackAnalyzer, Statement, StmtId, TopLevelId, TypeId,
};

pub struct StackAnalyzerBuilder {
    arena: AstArena,
    current_function: Option<FunctionContext>,
    stack_block_exprs: Vec<ExprId>,
    context_stack: Vec<BuilderContext>,
}

pub enum StmtOrExpr {
    Expr(Expression),
    Stmt(Statement),
}

struct FunctionContext {
    name: String,
    param_type: TypeId,
    return_type: TypeId,
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
        todo!()
    }

    fn push(self, elem: StmtOrExpr) -> Self {
        todo!()
    }

    fn push_many(self, elems: Vec<StmtOrExpr>) -> Self {
        todo!()
    }

    fn build(self) -> Self::Built {
        todo!()
    }
}

impl VariableOps for StackAnalyzerBuilder {
    fn variable(self, name: &str, typename: crate::hfs::Type) -> Self {
        todo!()
    }

    fn assign_to(self, name: &str, mode: super::builder::PassMode) -> Self {
        todo!()
    }
}
impl StackOps for StackAnalyzerBuilder {
    fn stack_block(self) -> Self {
        todo!()
    }

    fn end_stack_block(self, semicolon: bool) -> Self {
        todo!()
    }

    fn push_literal<T: Into<crate::hfs::Literal>>(self, lit: T) -> Self {
        todo!()
    }

    fn push_operation(self, op: super::builder::BuilderOperation) -> Self {
        todo!()
    }

    fn push_stack_keyword(self, keyword: &str, semicolon: bool) -> Self {
        todo!()
    }

    fn push_variable(self, name: &str) -> Self {
        todo!()
    }
}
impl FunctionOps for StackAnalyzerBuilder {
    fn args(self, args: Option<Vec<crate::hfs::Type>>) -> Self {
        todo!()
    }

    fn return_types(self, return_types: Option<Vec<crate::hfs::Type>>) -> Self {
        todo!()
    }

    fn func_with(self, name: &str, args: Option<Vec<crate::hfs::Type>>, return_type: Option<Vec<crate::hfs::Type>>) -> Self {
        todo!()
    }

    fn body(self) -> Self {
        todo!()
    }

    fn end_body(self) -> Self {
        todo!()
    }

    fn call_function(self, name: &str, mode: super::builder::PassMode) -> Self {
        todo!()
    }

    fn return_statement(self) -> Self {
        todo!()
    }
}
impl LoopOps for StackAnalyzerBuilder {
    fn while_loop(self) -> Self {
        todo!()
    }
}
impl ControlFlowOps for StackAnalyzerBuilder {
    fn if_statement(self) -> Self {
        todo!()
    }

    fn elif_statement(self) -> Self {
        todo!()
    }

    fn else_statement(self) -> Self {
        todo!()
    }
}
