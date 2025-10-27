use std::collections::HashMap;
use crate::hfs::ast::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// Scopes, used for solving symbols
pub enum ScopeKind {
    Global,
    Function, // func_name()::
    Block,    // 0::, 1::, etc
    WhileLoop,    // 0::, 1::, etc
    IfStmt,       // 0::, 1::, etc
    ElseStmt,     // 0::, 1::, etc
}

struct Scope {
    name: String,
    kind: ScopeKind,
    inner_count: usize, // for blocks within this frame
    curr_func_return_type: TypeId,
}

pub struct ScopeStack {
    scope_stack: Vec<Scope>,
    pub mangled_global_vars: HashMap<String, VarId>,
    pub mangled_locals: HashMap<String, VarId>,
    pub mangled_functions: HashMap<String, FuncId>,
}

impl ScopeStack {
    pub fn new(file_name: String) -> Self {
        Self {
            scope_stack: vec![Scope { name: file_name + "%", kind: ScopeKind::Global, inner_count: 0,curr_func_return_type : TypeId(0)}],
            mangled_global_vars: HashMap::new(),
            mangled_locals: HashMap::new(),
            mangled_functions: HashMap::new(),
        }
    }
    pub fn push_function_and_scope(&mut self, name: &str, func_id: FuncId, curr_func_return_type: TypeId) {
        let parent = self.scope_stack.last_mut().expect("[internal hfs error] couldn't push function, scopes were set up wrong.");
        let mangled_name = format!("{}{}()", parent.name, name);
        self.scope_stack.push(Scope { name: format!("{}::", mangled_name), kind: ScopeKind::Function, inner_count: 0, curr_func_return_type });
        self.mangled_functions.insert(mangled_name, func_id);
    }
    pub fn push_scope(&mut self, scope_kind: ScopeKind) {
        if scope_kind == ScopeKind::Function {
            panic!("[internal error] function scopes shouldn't be pushed with this method.");
        }
        let parent = self.scope_stack.last_mut().expect("[internal hfs error] couldn't push block, scopes were set up wrong.");
        let scope_name = format!("{}{}::", parent.name, parent.inner_count);
        parent.inner_count += 1;
        let curr_func_return_type = parent.curr_func_return_type;
        self.scope_stack.push(Scope { name: scope_name, kind: scope_kind, inner_count: 0 , curr_func_return_type});
    }
    pub fn push_variable(&mut self, name: &str, var_id: VarId) {
        let curr_stack = self.scope_stack.last().expect("[internal hfs error] scopes were set up wrong.");
        let mangled_name = format!("{}{}", curr_stack.name, name);
        match curr_stack.kind {
             ScopeKind::Global => self.mangled_global_vars.insert(mangled_name, var_id),
            ScopeKind::Function => self.mangled_locals.insert(mangled_name, var_id),
            ScopeKind::Block | ScopeKind::WhileLoop | ScopeKind::IfStmt | ScopeKind::ElseStmt => {
                self.mangled_locals.insert(mangled_name, var_id)
            }
        };
    }
    pub fn pop(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }
    pub fn curr_scope_kind(&self) -> ScopeKind {
        self.scope_stack.last().expect("[internal hfs error] no scope found").kind
    }
    pub fn get_curr_func_return_type(&self)  -> TypeId {
        self.scope_stack.last().expect("[internal hfs error] no scope found").curr_func_return_type
    }
    pub fn is_in_while_loop_context(&self) -> bool {
        for scope in self.scope_stack.iter().rev() {
            match scope.kind {
                ScopeKind::WhileLoop => return true,
                ScopeKind::Function => return false, // Stop at function boundary
                _ => continue, // Keep searching
            }
        }
        false // Not in any while loop
    }
    fn find_variable(&self, name: &str) -> (Option<VarId>, bool) {
        // Search from innermost to outermost scope until function boundary
        for scope in self.scope_stack.iter().rev() {
            let mangled_name = format!("{}{}", scope.name, name);
            // Check if variable exists at this scope
            if let Some(&var_id) = self.mangled_locals.get(&mangled_name) {
                return (Some(var_id), false);
            }
            // Stop at function boundary - don't look into outer functions
            if scope.kind == ScopeKind::Function {
                break;
            }
        }
        // Check globals
        let global_mangled_name = format!("{}{}", self.scope_stack[0].name, name);
        (self.mangled_global_vars.get(&global_mangled_name).copied(), true)
    }

    pub fn find_function(&self, name: &str) -> Option<FuncId> {
        // Search from innermost scope to outermost until function boundary
        for scope in self.scope_stack.iter().rev() {
            let mangled_name = format!("{}{}()", scope.name, name);
            
            if let Some(&func_id) = self.mangled_functions.get(&mangled_name) {
                return Some(func_id);
            }
            // Stop at function boundary - nested functions can't see outer nested functions
            if scope.kind == ScopeKind::Function {
                break;
            }
        }
        // Check globals
        let global_mangled_name = format!("{}{}()", self.scope_stack[0].name, name);
        self.mangled_functions.get(&global_mangled_name).copied()
    }

    pub fn find_identifier(&self, name: &str) -> Identifier {
        match self.find_variable(&name) {
            (None, _) => match self.find_function(&name) {
                Some(id) => Identifier::Function(id),
                None => panic!("use of undeclared identifier '{}'", name),
            },
            (Some(id), false) => Identifier::Variable(id),
            (Some(id), true) => Identifier::GlobalVar(id),
        }
    }
}
