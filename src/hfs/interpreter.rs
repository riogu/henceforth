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
    ReturnedStack(Vec<RuntimeValue>),
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
    func_id: FuncId,
    locals: HashMap<VarId, RuntimeValue>,
    return_stack: Vec<RuntimeValue>,
    should_return: bool,
    should_break: bool,
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

    pub fn curr_call_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().expect("call stack shouldn't be empty")
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

        self.call_stack.push(CallFrame { func_id, locals: HashMap::new(), return_stack: Vec::new(),
                                         should_break: false, should_return: false });

        let func = self.arena.get_func(func_id);
        self.interpret_stmt(func.body);

        // if we have nothing to return, we return an empty vector (will be iterated anyway)
        self.call_stack.pop().expect("pushed above").return_stack
    }

    fn interpret_stmt(&mut self, stmt_id: StmtId) {
        match self.arena.get_stmt(stmt_id) {
            Statement::If { cond, body, else_stmt } => {
                let RuntimeValue::Bool(if_cond) = self.interpret_expr(*cond) else {
                    panic!("expected boolean value on stack for if statement condition")
                };
                if self.curr_call_frame().should_break || self.curr_call_frame().should_return {
                    return;
                }
                if if_cond {
                    self.interpret_stmt(*body);
                } else if let Some(else_stmt) = else_stmt {
                    match else_stmt {
                        ElseStmt::ElseIf(stmt_id) | ElseStmt::Else(stmt_id) => self.interpret_stmt(*stmt_id),
                    }
                }
            }
            Statement::While { cond, body } => loop {
                let RuntimeValue::Bool(while_cond) = self.interpret_expr(*cond) else {
                    panic!("expected boolean value on stack for while loop condition")
                };
                if !while_cond || self.curr_call_frame().should_break || self.curr_call_frame().should_return {
                    self.curr_call_frame_mut().should_break = false;
                    break
                } 
                self.interpret_stmt(*body);
            },
            Statement::StackBlock(expressions) => {
                for expr_id in expressions {
                    let val = self.interpret_expr(*expr_id);
                    if let RuntimeValue::ReturnedStack(runtime_values) = val {
                        for val in runtime_values {
                            self.curr_call_frame_mut().return_stack.push(val);
                        }
                    } else {
                        self.curr_call_frame_mut().return_stack.push(val);
                    }
                }
                todo!()
            }
            Statement::BlockScope(top_level_nodes) => {
                for &top_level_id in top_level_nodes {
                    match top_level_id {
                        TopLevelId::VariableDecl(var_id) => self.interpret_var_decl(var_id),
                        TopLevelId::Statement(stmt_id) => {
                            match self.arena.get_stmt(stmt_id) {
                                Statement::Continue => return,
                                _ => self.interpret_stmt(stmt_id)
                            }
                        }
                        TopLevelId::FunctionDecl(func_id) => { /* do nothing, function declarations are already handled */ }
                    }
                }
                todo!();
            }
            // note that we already ensured all break/continue are valid,
            // so we can just interpret them
            Statement::Return => self.curr_call_frame_mut().should_return = true, 
            Statement::Break => self.curr_call_frame_mut().should_break = true,
            Statement::Continue => {} // do nothing
            Statement::Assignment { value, identifier, is_move } => {
                todo!();
            }
            Statement::Empty => {}
        }
    }

    fn interpret_expr(&mut self, expr_id: ExprId) -> RuntimeValue {
        match self.arena.get_expr(expr_id) {
            Expression::Operation(op) => self.interpret_operation(op),
            Expression::Identifier(id) => self.interpret_identifier(id),
            Expression::Literal(lit) => self.interpret_literal(lit),
            Expression::FunctionCall { args, identifier, ..} => {
                let return_values = self.call_declared_function(*identifier, args.clone());
                RuntimeValue::ReturnedStack(return_values)
            }
            Expression::Tuple { expressions, variadic } => {
                let mut tuple_values = Vec::<RuntimeValue>::new();
                for &expr_id in expressions {
                    tuple_values.push(self.interpret_expr(expr_id))
                }
                RuntimeValue::Tuple(tuple_values)
            }
            Expression::Parameter(type_id) => todo!(),
            Expression::ReturnValue(type_id) => todo!(),
        }
    }

    fn interpret_identifier(&mut self, id: &Identifier) -> RuntimeValue {
        match id { // pushing an identifier to the hfs_stack just copies its value
            Identifier::GlobalVar(var_id) => self.globals.get(var_id).expect("solved in stack analyzer.").clone(),
            Identifier::Variable(var_id) => self.curr_local_vars().get(var_id).expect("solved in stack analyzer").clone(),
            Identifier::Function(func_id) => panic!("function identifiers shouldn't be interpreted")
        }
    }

    fn interpret_literal(&mut self, lit: &Literal) -> RuntimeValue {
        match lit {
            Literal::Integer(i) => todo!(),
            Literal::Float(f) => todo!(),
            Literal::String(s) => todo!(),
            Literal::Bool(b) => todo!(),
        }
    }

    fn interpret_operation(&mut self, op: &Operation) -> RuntimeValue {
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
