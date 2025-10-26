use std::collections::HashMap;
use crate::hfs::ast::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// Scopes, used for solving symbols
pub enum ScopeKind {
    Global,
    Function, // func_name()::
    Block,    // 0::, 1::, etc
}

struct Scope {
    name: String,
    kind: ScopeKind,
    inner_count: usize, // for blocks within this frame
}

pub struct ScopeStack {
    scope_stack: Vec<Scope>,
    mangled_global_vars: HashMap<String, VarId>,
    mangled_locals: HashMap<String, VarId>,
    mangled_functions: HashMap<String, FuncId>,
}

impl ScopeStack {
    pub fn new(file_name: String) -> Self {
        Self {
            scope_stack: vec![Scope { name: file_name + "%", kind: ScopeKind::Global, inner_count: 0, }],
            mangled_global_vars: HashMap::new(),
            mangled_locals: HashMap::new(),
            mangled_functions: HashMap::new(),
        }
    }
    pub fn push_function_and_scope(&mut self, name: &str, func_id: FuncId) {
        let parent = self.scope_stack.last_mut().expect("[internal hfs error] couldn't push function, scopes were set up wrong.");
        let mangled_name = format!("{}{}::", parent.name, name);
        self.scope_stack.push(Scope { name: format!("{}()::", mangled_name), kind: ScopeKind::Function, inner_count: 0, });
        self.mangled_functions.insert(mangled_name, func_id);
    }
    pub fn push_block_scope(&mut self) {
        let parent = self.scope_stack.last_mut().expect("[internal hfs error] couldn't push block, scopes were set up wrong.");
        let block_name = format!("{}{}::", parent.name, parent.inner_count);
        parent.inner_count += 1;
        self.scope_stack.push(Scope { name: block_name, kind: ScopeKind::Block, inner_count: 0 });
    }
    pub fn push_variable(&mut self, name: &str, var_id: VarId) {
        let curr_stack = self.scope_stack.last().expect("[internal hfs error] scopes were set up wrong.");
        let mangled_name = format!("{}{}", curr_stack.name, name);
        match curr_stack.kind {
            ScopeKind::Global => self.mangled_global_vars.insert(mangled_name, var_id),
            ScopeKind::Function => self.mangled_locals.insert(mangled_name, var_id),
            ScopeKind::Block => self.mangled_locals.insert(mangled_name, var_id),
        };
    }
    pub fn pop(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }
    pub fn curr_scope_kind(&mut self) -> ScopeKind {
        self.scope_stack.last()
                        .expect("[internal hfs error] couldn't push block, scopes were set up wrong.")
                        .kind
    }
}
