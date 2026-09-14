// NOTE:
// this code isn't used anymore; it is kept here in case we want to compare the speed of direct AST
// interpretation against the IR interpreter later as an alternative interpreter implementation

// use std::collections::HashMap;
//
// use crate::hfs::{ast::*, scope_stack::*, token::*, types::*};
//
// //---------------------------------------------------------------------------
// // Runtime values
// #[derive(Debug, Clone, PartialEq)]
// pub enum RuntimeValue {
//     Integer(i32),
//     Float(f32),
//     String(String),
//     Bool(bool),
//     Tuple(Vec<RuntimeValue>),
// }
//
// impl RuntimeValue {
//     pub fn default(hfs_type: &Type) -> RuntimeValue {
//         match hfs_type {
//             Type::Int => RuntimeValue::Integer(0),
//             Type::String => RuntimeValue::String("".to_string()),
//             Type::Bool => RuntimeValue::Bool(false),
//             Type::Float => RuntimeValue::Float(0.0),
//             Type::Tuple(type_ids) => RuntimeValue::Tuple(Vec::new()),
//         }
//     }
// }
// #[derive(PartialEq, Clone, Copy)]
// enum CtrlFlowState {
//     Normal,
//     Return,
//     Break,
//     Continue,
// }
// pub struct CallFrame {
//     func_id: FuncId,
//     locals: HashMap<VarId, RuntimeValue>,
//     flow_state: CtrlFlowState,
//     pub expr_values: HashMap<ExprId, RuntimeValue>,
// }
//
// //---------------------------------------------------------------------------
// pub struct Interpreter<'a> {
//     arena: &'a AstArena,
//     globals: HashMap<VarId, RuntimeValue>,
//     call_stack: Vec<CallFrame>,
// }
// impl<'a> Interpreter<'a> {
//     // utils
//     pub fn curr_call_frame(&self) -> &CallFrame {
//         self.call_stack.last().expect("call stack shouldn't be empty")
//     }
//
//     pub fn curr_call_frame_mut(&mut self) -> &mut CallFrame {
//         self.call_stack.last_mut().expect("call stack shouldn't be empty")
//     }
//     pub fn curr_local_vars(&self) -> &HashMap<VarId, RuntimeValue> {
//         &self.call_stack.last().expect("call stack shouldn't be empty").locals
//     }
//     pub fn curr_local_vars_mut(&mut self) -> &mut HashMap<VarId, RuntimeValue> {
//         &mut self.call_stack.last_mut().expect("call stack shouldn't be empty").locals
//     }
// }
//
// impl<'a> Interpreter<'a> {
//     pub fn new(arena: &'a AstArena) -> Self {
//         Self { arena, globals: HashMap::new(), call_stack: Vec::new() }
//     }
//
//     pub fn interpret(arena: &AstArena, scope_stack: &ScopeStack) {
//         let mut interpreter = Interpreter::new(arena);
//         // create all our globals at the start of our program
//         for var_id in scope_stack.mangled_global_vars.values() {
//             let default_val = RuntimeValue::default(interpreter.arena.get_type_of_var(*var_id));
//             interpreter.globals.insert(*var_id, default_val);
//         }
//         if let Some(main) = scope_stack.find_function("main") {
//             interpreter.call_declared_function(main, Vec::new());
//         } else {
//             panic!("this file has no 'main()' entrypoint, so it cannot be interpreted (consider compiling it instead)")
//         }
//     }
//
//     fn interpret_var_decl(&mut self, var_id: VarId) {
//         let call_frame = self.call_stack.last_mut().expect("[internal error] invalid call stack");
//         call_frame.locals.insert(var_id, RuntimeValue::default(self.arena.get_type_of_var(var_id)));
//     }
//
//     fn call_declared_function(&mut self, func_id: FuncId, tuple_args: Vec<RuntimeValue>) -> Vec<RuntimeValue> {
//         self.call_stack.push(CallFrame {
//             func_id,
//             locals: HashMap::new(),
//             flow_state: CtrlFlowState::Normal,
//             expr_values: HashMap::new(),
//         });
//
//         let func = self.arena.get_func(func_id);
//         self.interpret_stmt(func.body);
//
//         todo!()
//         // if we have nothing to return, we return an empty vector (will be iterated anyway)
//         // self.call_stack.pop().expect("pushed above").local_hfs_stack
//     }
//
//     fn interpret_stmt(&mut self, stmt_id: StmtId) {
//         match self.arena.get_stmt(stmt_id) {
//             Statement::If { cond_stack_block: cond, body, else_stmt } => {
//                 let RuntimeValue::Bool(if_cond) = self.interpret_expr(*cond) else {
//                     panic!("expected boolean value on stack for if statement condition")
//                 };
//                 if self.curr_call_frame().flow_state != CtrlFlowState::Normal {
//                     return;
//                 }
//                 if if_cond {
//                     self.interpret_stmt(*body);
//                     match self.curr_call_frame().flow_state {
//                         CtrlFlowState::Normal => {},
//                         CtrlFlowState::Return | CtrlFlowState::Break | CtrlFlowState::Continue => {
//                             return;
//                         },
//                     }
//                 } else if let Some(else_stmt) = else_stmt {
//                     // TODO: this was from before the refactor
//                     // match else_stmt {
//                     //     ElseStmt::ElseIf(stmt_id) | ElseStmt::Else(stmt_id) => self.interpret_stmt(*stmt_id),
//                     // }
//                 }
//             },
//             Statement::While { cond, body } => loop {
//                 // TODO: decide the semantics of while loops
//                 let RuntimeValue::Bool(while_cond) = self.interpret_expr(*cond) else {
//                     panic!("expected boolean value on stack for while loop condition")
//                 };
//                 if !while_cond {
//                     return;
//                 }
//                 self.interpret_stmt(*body);
//                 match self.curr_call_frame().flow_state {
//                     CtrlFlowState::Return => return,
//                     CtrlFlowState::Break => {
//                         self.curr_call_frame_mut().flow_state = CtrlFlowState::Normal;
//                         break;
//                     },
//                     CtrlFlowState::Continue => {
//                         self.curr_call_frame_mut().flow_state = CtrlFlowState::Normal;
//                         continue;
//                     },
//                     CtrlFlowState::Normal => { /* do nothing */ },
//                 }
//             },
//             Statement::StackBlock(expressions) => {
//                 for expr_id in expressions {
//                     let val = self.interpret_expr(*expr_id);
//                     todo!("make stack merges actually do anything");
//                     // self.curr_call_frame_mut().local_hfs_stack.push(val);
//                 }
//             },
//             Statement::BlockScope(top_level_nodes, scope_kind) => {
//                 for &top_level_id in top_level_nodes {
//                     match top_level_id {
//                         TopLevelId::VariableDecl(var_id) => self.interpret_var_decl(var_id),
//                         TopLevelId::Statement(stmt_id) => self.interpret_stmt(stmt_id),
//                         TopLevelId::FunctionDecl(func_id) => { /* do nothing, function declarations are handled already */ },
//                     }
//                     match self.curr_call_frame().flow_state {
//                         CtrlFlowState::Normal => {}, // funny code lol
//                         CtrlFlowState::Return | CtrlFlowState::Break | CtrlFlowState::Continue => {
//                             return;
//                         },
//                     }
//                 }
//             },
//             Statement::Return => self.curr_call_frame_mut().flow_state = CtrlFlowState::Return,
//             Statement::Break => self.curr_call_frame_mut().flow_state = CtrlFlowState::Break,
//             Statement::Continue => self.curr_call_frame_mut().flow_state = CtrlFlowState::Continue,
//             Statement::Assignment { value, identifier, is_move } => {
//                 let new_value = self.interpret_expr(*value);
//                 match identifier {
//                     Identifier::GlobalVar(var_id) => self.globals.insert(*var_id, new_value),
//                     Identifier::Variable(var_id) => self.curr_local_vars_mut().insert(*var_id, new_value),
//                     Identifier::Function(func_id) => {
//                         panic!("[internal error] functions aren't assignable (fix StackAnalyzer)")
//                     },
//                 };
//             },
//             Statement::Empty => {},
//             Statement::FunctionCall { args, func_id: identifier, is_move } => {
//                 let args: Vec<RuntimeValue> = args.iter().map(|arg| self.interpret_expr(arg.clone())).collect();
//                 let return_values = self.call_declared_function(*identifier, args.clone());
//                 for val in return_values {
//                     // merge returned tuples into the caller's stack
//                     todo!("make stack merges actually do anything");
//                     // self.curr_call_frame_mut().local_hfs_stack.push(val);
//                 }
//             },
//             Statement::ElseIf { cond_stack_block: cond, body, else_stmt } => todo!(),
//             Statement::Else(stmt_id) => todo!(),
//         }
//     }
//
//     fn interpret_expr(&mut self, expr_id: ExprId) -> RuntimeValue {
//         // FIXME: we need to use ExprProvenance here:
//         // we need to first check if a runtime value already exists, this commonly happens anyways
//         // by ExprId that are used multiple times (which is quite common)
//         // and we also should convert the compile time values to runtimevalues directly.
//         // dont forget to add a "get or generate" logic at the start here
//         match self.arena.get_expr(expr_id) {
//             Expression::Operation(op) => self.interpret_operation(op),
//             Expression::Identifier(id) => self.interpret_identifier(id),
//             Expression::Literal(lit) => self.interpret_literal(lit),
//             Expression::Tuple { expressions } => {
//                 let mut tuple_values = Vec::<RuntimeValue>::new();
//                 for &expr_id in expressions {
//                     tuple_values.push(self.interpret_expr(expr_id))
//                 }
//                 RuntimeValue::Tuple(tuple_values)
//             },
//             Expression::Parameter { index, type_id } => {
//                 // Parameters should be pre-populated in the call frame when the function is called
//                 // This node shouldn't be encountered during interpretation if resolved properly
//                 panic!("Parameter nodes should be replaced by semantic analyzer")
//             },
//             Expression::StackKeyword { .. } => todo!(),
//         }
//     }
//
//     fn interpret_identifier(&mut self, id: &Identifier) -> RuntimeValue {
//         match id {
//             // pushing an identifier to the hfs_stack just copies its value
//             Identifier::GlobalVar(var_id) => self.globals.get(var_id).expect("solved in stack analyzer.").clone(),
//             Identifier::Variable(var_id) => self.curr_local_vars().get(var_id).expect("solved in stack analyzer").clone(),
//             Identifier::Function(func_id) => {
//                 panic!("function identifiers shouldn't be interpreted")
//             },
//         }
//     }
//
//     fn interpret_literal(&mut self, lit: &Literal) -> RuntimeValue {
//         match lit {
//             Literal::Integer(i) => RuntimeValue::Integer(*i),
//             Literal::Float(f) => RuntimeValue::Float(*f),
//             Literal::String(s) => RuntimeValue::String(s.to_string()),
//             Literal::Bool(b) => RuntimeValue::Bool(*b),
//         }
//     }
//
//     fn interpret_operation(&mut self, op: &Operation) -> RuntimeValue {
//         match op {
//             Operation::Add(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Integer(a + b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a + b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a as f32 + b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Float(a + b as f32),
//                     (RuntimeValue::String(a), RuntimeValue::String(b)) => RuntimeValue::String(format!("{}{}", a, b)),
//                     _ => panic!("invalid operands for addition"),
//                 }
//             },
//             Operation::Sub(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Integer(a - b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a - b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a as f32 - b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Float(a - b as f32),
//                     _ => panic!("invalid operands for subtraction"),
//                 }
//             },
//             Operation::Mul(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Integer(a * b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a * b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a as f32 * b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Float(a * b as f32),
//                     _ => panic!("invalid operands for multiplication"),
//                 }
//             },
//             Operation::Div(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => {
//                         if b == 0 {
//                             panic!("division by zero");
//                         }
//                         RuntimeValue::Integer(a / b)
//                     },
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
//                         if b == 0.0 {
//                             panic!("division by zero");
//                         }
//                         RuntimeValue::Float(a / b)
//                     },
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => {
//                         if b == 0.0 {
//                             panic!("division by zero");
//                         }
//                         RuntimeValue::Float(a as f32 / b)
//                     },
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => {
//                         if b == 0 {
//                             panic!("division by zero");
//                         }
//                         RuntimeValue::Float(a / b as f32)
//                     },
//                     _ => panic!("invalid operands for division"),
//                 }
//             },
//             Operation::Mod(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => {
//                         if b == 0 {
//                             panic!("modulo by zero");
//                         }
//                         RuntimeValue::Integer(a % b)
//                     },
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
//                         if b == 0.0 {
//                             panic!("modulo by zero");
//                         }
//                         RuntimeValue::Float(a % b)
//                     },
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => {
//                         if b == 0.0 {
//                             panic!("modulo by zero");
//                         }
//                         RuntimeValue::Float(a as f32 % b)
//                     },
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => {
//                         if b == 0 {
//                             panic!("modulo by zero")
//                         }
//                         RuntimeValue::Float(a % b as f32)
//                     },
//                     _ => panic!("invalid operands for modulo"),
//                 }
//             },
//             Operation::Equal(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 RuntimeValue::Bool(left == right)
//             },
//             Operation::NotEqual(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 RuntimeValue::Bool(left != right)
//             },
//             Operation::Less(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a < b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a < b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) < b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a < (b as f32)),
//                     _ => panic!("invalid operands for less than comparison"),
//                 }
//             },
//             Operation::LessEqual(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a <= b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a <= b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) <= b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a <= (b as f32)),
//                     _ => panic!("invalid operands for '<=' comparison"),
//                 }
//             },
//             Operation::Greater(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a > b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a > b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) > b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a > (b as f32)),
//                     _ => panic!("invalid operands for greater than comparison"),
//                 }
//             },
//             Operation::GreaterEqual(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a >= b),
//                     (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a >= b),
//                     (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) >= b),
//                     (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a >= (b as f32)),
//                     _ => panic!("invalid operands for greater than or equal comparison"),
//                 }
//             },
//             Operation::Or(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => RuntimeValue::Bool(a || b),
//                     _ => panic!("invalid operands for logical OR (expected booleans)"),
//                 }
//             },
//             Operation::And(l, r) => {
//                 let left = self.interpret_expr(*l);
//                 let right = self.interpret_expr(*r);
//                 match (left, right) {
//                     (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => RuntimeValue::Bool(a && b),
//                     _ => panic!("invalid operands for logical AND (expected booleans)"),
//                 }
//             },
//             Operation::Not(expr) => {
//                 let value = self.interpret_expr(*expr);
//                 match value {
//                     RuntimeValue::Bool(b) => RuntimeValue::Bool(!b),
//                     _ => panic!("invalid operand for logical NOT (expected boolean)"),
//                 }
//             },
//         }
//     }
// }
