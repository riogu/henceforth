use std::collections::HashMap;
use crate::hfs::scope_stack::*;
use crate::hfs::ast::*;
use crate::hfs::types::*;
use crate::hfs::token::*;

//---------------------------------------------------------------------------
// Runtime values
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Int(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
}
impl RuntimeValue {
    pub fn default(hfs_type: &Type) -> RuntimeValue {
        match hfs_type {
            Type::Int => RuntimeValue::Int(0),
            Type::String => RuntimeValue::String("".to_string()),
            Type::Bool => RuntimeValue::Float(0.0),
            Type::Float => RuntimeValue::Bool(false),
            Type::Tuple(type_ids) => RuntimeValue::Tuple(Vec::new()),
        }
    }
}
pub struct CallFrame {
    function_id: FuncId,
    locals: HashMap<VarId, RuntimeValue>,
}

//---------------------------------------------------------------------------
pub struct Interpreter<'a> {
    arena: &'a AstArena<'a>,
    globals: HashMap<VarId, RuntimeValue>, 
    call_stack: Vec<CallFrame>,
}
impl<'a> Interpreter<'a> {
    // utils 
    pub fn curr_call_frame(&self) -> &CallFrame {
        self.call_stack.last().expect("call stack shouldn't be empty")
    }
    pub fn curr_local_vars(&self) -> &HashMap<VarId, RuntimeValue>  {
        &self.call_stack.last().expect("call stack shouldn't be empty").locals
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(arena: &'a AstArena<'a>) -> Self {
        Self {
            arena,
            globals: HashMap::new(),
            call_stack: Vec::new()
        }
    }

    pub fn interpret(arena: &AstArena, scope_stack: &ScopeStack) {
        let mut interpreter = Interpreter::new(arena);
        // create all our globals at the start of our program
        for var_id in scope_stack.mangled_global_vars.values() {
            let default_val = RuntimeValue::default(interpreter.arena.get_type_of_var(*var_id));
            interpreter.globals.insert(*var_id, default_val);
        }
        if let Some(main) = scope_stack.find_function("main") {
            interpreter.call_declared_function(main, Vec::new());
        } else {
            panic!("this file has no 'main()' entrypoint, so it cannot be interpreted")
        }
    }

    fn interpret_var_decl(&mut self, var_id: VarId) {
        let call_frame = self.call_stack.last_mut().expect("[internal error] invalid call stack");
        call_frame.locals.insert(var_id, RuntimeValue::default(self.arena.get_type_of_var(var_id)));
    }
    fn call_declared_function(&mut self, func_id: FuncId, tuple_args: Vec<ExprId>) -> Vec<RuntimeValue> {
        let func = self.arena.get_func(func_id);
        self.interpret_stmt(func.body);
        todo!()
        // if we have nothing to return, we return an empty vector (will be iterated anyway)
    }

    fn interpret_stmt(&mut self, stmt_id: StmtId) -> Option<&RuntimeValue> {
        match self.arena.get_stmt(stmt_id) {
            Statement::If { cond, body, else_stmt } => {
                let RuntimeValue::Bool(val) = self.interpret_expr(*cond) else {
                    panic!("expected boolean value on stack for if statement condition")
                };
                None
            }
            Statement::While { cond, body } => {
                let RuntimeValue::Bool(val) = self.interpret_expr(*cond) else {
                    panic!("expected boolean value on stack for while loop condition")
                };
                None
            }
            Statement::StackBlock(expressions) => todo!(),
            Statement::BlockScope(top_level_nodes) => {
                todo!();
                None
            }
            Statement::Return => {
                todo!();
                None
            }
            Statement::Break => {
                todo!();
                None
            }
            Statement::Continue => {
                todo!();
                None
            }
            Statement::Assignment { value, identifier, is_move } => {
                todo!();
                None
            }
            Statement::Empty => None,
        }
    }

    fn interpret_expr(&mut self, expr_id: ExprId) -> &RuntimeValue {
        match self.arena.get_expr(expr_id) {
            Expression::Operation(op) => self.interpret_operation(op),
            Expression::Identifier(id) => self.interpret_identifier(id),
            Expression::Literal(lit) => self.interpret_literal(lit),
            Expression::FunctionCall { args, identifier, return_values } => {
                let return_values = self.call_declared_function(*identifier, args.clone());
                // RuntimeValue::Tuple(return_values);
                todo!()
            }
            Expression::Tuple { expressions, variadic } => {
                todo!() 
            }
            Expression::Parameter(type_id) => todo!(),
            Expression::ReturnValue(type_id) => todo!(),
        }
    }

    fn interpret_identifier(&mut self, id: &Identifier) -> &RuntimeValue {
        match id {
            Identifier::GlobalVar(var_id) => self.globals.get(var_id).expect("solved in stack analyzer."),
            Identifier::Variable(var_id) => self.curr_local_vars().get(var_id).expect("solved in stack analyzer"),
            Identifier::Function(func_id) => panic!("function identifiers shouldn't be interpreted")
        }
    }

    fn interpret_literal(&mut self, lit: &Literal) -> &RuntimeValue {
        match lit {
            Literal::Integer(i) => todo!(),
            Literal::Float(f) => todo!(),
            Literal::String(s) => todo!(),
            Literal::Bool(b) => todo!(),
        }
    }

    fn interpret_operation(&mut self, op: &Operation) -> &RuntimeValue {
        match op {
            Operation::Add(l, r) => todo!(),
            Operation::Sub(l, r) => todo!(),
            Operation::Mul(l, r) => todo!(),
            Operation::Div(l, r) => todo!(),
            Operation::Mod(l, r) => todo!(),
            Operation::Equal(l, r) => todo!(),
            Operation::NotEqual(l, r) => todo!(),
            Operation::Less(l, r) => todo!(),
            Operation::LessEqual(l, r) => todo!(),
            Operation::Greater(l, r) => todo!(),
            Operation::GreaterEqual(l, r) => todo!(),
            Operation::Or(l, r) => todo!(),
            Operation::And(l, r) => todo!(),
            Operation::Not(expr) => todo!(),
        }
    }
}
