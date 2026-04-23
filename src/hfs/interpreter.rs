use std::collections::HashMap;

use slotmap::Key;

use crate::hfs::{ast::*, hfs_mir::*, scope_stack::*, token::*, IrArena};

//---------------------------------------------------------------------------
// Runtime values
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
    Address(InstId),
}

impl RuntimeValue {
    pub fn default(hfs_type: &Type) -> RuntimeValue {
        match hfs_type {
            Type::Int { .. } => RuntimeValue::Integer(0),
            Type::String { .. } => RuntimeValue::String("".to_string()),
            Type::Bool { .. } => RuntimeValue::Bool(false),
            Type::Float { .. } => RuntimeValue::Float(0.0),
            Type::Tuple { .. } => RuntimeValue::Tuple(Vec::new()),
            Type::Array { hfs_type, length } => todo!(),
        }
    }
}
pub struct CallFrame {
    func_id: IrFuncId,
    inst_values: HashMap<InstId, RuntimeValue>,
    return_stack: Vec<RuntimeValue>,
}

//---------------------------------------------------------------------------
pub struct Interpreter {
    arena: IrArena,
    globals: HashMap<GlobalIrVarId, RuntimeValue>,
    call_stack: Vec<CallFrame>,
    disable_cache: bool, // NOTE: not really used yet (but we should probably)
    prev_block_id: BlockId,
    curr_block_id: BlockId,

    memory: HashMap<InstId, RuntimeValue>,
}
impl Interpreter {
    // utils
    pub fn curr_call_frame(&self) -> &CallFrame { self.call_stack.last().expect("call stack shouldn't be empty") }

    pub fn curr_call_frame_mut(&mut self) -> &mut CallFrame { self.call_stack.last_mut().expect("call stack shouldn't be empty") }
}
impl Interpreter {
    pub fn new(arena: IrArena) -> Self {
        Self {
            arena,
            globals: HashMap::new(),
            call_stack: Vec::new(),
            disable_cache: false,
            prev_block_id: BlockId::null(),
            curr_block_id: BlockId::null(),
            memory: HashMap::new(),
        }
    }

    pub fn interpret(arena: IrArena, top_level_insts: Vec<IrTopLevelId>, scope_stack: ScopeStack) {
        let mut interpreter = Interpreter::new(arena);
        for inst_id in top_level_insts {
            match inst_id {
                IrTopLevelId::GlobalVarDecl(ir_var_id) => {
                    let default_val = RuntimeValue::default(interpreter.arena.get_type_of_var(ir_var_id));
                    interpreter.globals.insert(ir_var_id, default_val);
                },
                IrTopLevelId::FunctionDecl(_) => { /* do nothing, declarations dont matter for interpreting */ },
            }
        }

        if let Some(main) = scope_stack.find_function("main") {
            // get the CfgFunction version of main (not the old AST function)
            let main = interpreter.arena.func_id_map[&main];
            interpreter.call_declared_function(main, Vec::new());
        } else {
            panic!("this file has no 'main()' entrypoint, so it cannot be interpreted")
        }
    }

    /* example of MIR
    fn func: (i32 i32 i32) -> (i32 i32) {
        start_function:
            branch 1 < 2.0, if_body_0, else_if_cond_0;
            if_body_0:
                jump if_end_0;
            else_if_cond_0:
                %0 = 1 + 3;
                %1 = %0 * 5;
                branch -420 < 5, else_if_body_0, else_if_cond_1;
                else_if_body_0:
                    jump if_end_0;
            else_if_cond_1:
                stack becomes:
                branch -3 < 5, else_if_body_1, else_body_0;
                else_if_body_1:
                    jump if_end_0;
            else_body_0:
                jump if_end_0;
            if_end_0:
                jump end_function;
        end_function:
            return;
    }
    fn main: () -> () {
        call func, (1 2 3);
    }
    */
    fn call_declared_function(&mut self, func_id: IrFuncId, args: Vec<RuntimeValue>) -> Vec<RuntimeValue> {
        let func = self.arena.get_func(func_id);
        if func.name == "print" {
            for arg in &args {
                match arg {
                    RuntimeValue::Integer(v) => print!("{}", v),
                    RuntimeValue::Float(v) => print!("{}", v),
                    RuntimeValue::String(v) => print!("{}", v),
                    RuntimeValue::Bool(v) => print!("{}", v),
                    RuntimeValue::Tuple(runtime_values) => {
                        dbg!(runtime_values);
                    },
                    RuntimeValue::Address(inst_id) => print!("{:?}", inst_id),
                }
            }
        }

        // bind all the parameters before interpreting the function
        let mut inst_values = HashMap::new();
        for (param_id, arg_val) in func.parameter_insts.iter().zip(args) {
            inst_values.insert(*param_id, arg_val);
        }
        self.call_stack.push(CallFrame { func_id, inst_values, return_stack: vec![] });

        self.interpret_block(func.entry_block);

        self.call_stack.pop().expect("[internal error] wrong scope management while calling function").return_stack
    }
    pub fn interpret_block(&mut self, block_id: BlockId) {
        let mut next_block = Some(block_id);

        while let Some(block_id) = next_block {
            self.prev_block_id = self.curr_block_id;
            self.curr_block_id = block_id;
            let block = self.arena.get_block(block_id);
            let term = block.terminator;

            for inst_id in block.instructions.clone() {
                // Invalidate cached values for this block's instructions
                // so that loads and operations are re-evaluated on each visit.
                if !matches!(self.arena.get_inst(inst_id), Instruction::Parameter { .. } | Instruction::ReturnValue { .. }) {
                    // parameters should only be interpreted once at the start of a block
                    // return values are bound by function calls which means we never wanna invalidate them from the cache
                    // as each function call overwrites them correctly
                    self.curr_call_frame_mut().inst_values.remove(&inst_id);
                }

                let val = self.interpret_instruction(inst_id);
                self.curr_call_frame_mut().inst_values.insert(inst_id, val);
            }
            if let Some(terminator) = term {
                next_block = self.interpret_terminator(terminator);
            } else {
                panic!(
                    // TODO: joao please add a nicer print here with info about the broken block
                    "[internal error] found block with no terminator",
                )
            }
        }
    }
    pub fn interpret_instruction(&mut self, inst_id: InstId) -> RuntimeValue {
        // NOTE: i need to be careful wether we want to reuse already generated instids, or
        // actually generate them here. i can't tell if we wont lose side-effects by always caching
        // i added a disable_cache variable for cases where you know you need to reinterpret this InstId
        // (i want to make it work as a sentinel later)
        if !self.disable_cache && self.curr_call_frame().inst_values.contains_key(&inst_id) {
            return self.curr_call_frame().inst_values[&inst_id].clone();
        }

        match self.arena.get_inst(inst_id) {
            Instruction::Parameter { .. } => {
                // parameters should always hit the cache (because they are bound at the start of
                // the function
                panic!(
                    "[internal error] found unbound 'Instruction::Parameter'. a parameter should be bound to a value before \
                     being interpreted",
                )
            },
            Instruction::ReturnValue { .. } => {
                panic!(
                    "[internal error] found unbound 'Instruction::ReturnValue'. a ReturnValue should be bound to a value before \
                     being interpreted"
                )
            },
            Instruction::FunctionCall { source_info: _, args, func_id, is_move: _, return_values } => {
                let mut arg_values = Vec::new();
                let func_id = func_id.clone();
                let return_values = return_values.clone();
                for inst_id in args.clone() {
                    arg_values.push(self.interpret_instruction(inst_id));
                }
                let runtime_return_values = self.call_declared_function(func_id, arg_values);
                for (retval_inst, runtime_retval) in return_values.iter().zip(runtime_return_values.clone()) {
                    self.curr_call_frame_mut().inst_values.insert(*retval_inst, runtime_retval);
                }
                // NOTE: we return a tuple with the runtime values just for a consistent API
                // in reality, no one is directly using this tuple at all in the logic of the interpreter
                // or knowing how functions work (where return values are implicit and you cant
                // capture them). maybe one day we might want to allow capturing them
                RuntimeValue::Tuple(runtime_return_values)
            },
            Instruction::Phi { source_info: _, incoming } =>
                if let Some(inst_id) = incoming.get(&self.prev_block_id) {
                    self.interpret_instruction(*inst_id)
                } else {
                    panic!("[internal error] reached phi without going through one of its predecessor blocks")
                },
            Instruction::Tuple { source_info: _, instructions } => {
                let mut runtime_values = Vec::<RuntimeValue>::new();
                for inst_id in instructions.clone() {
                    runtime_values.push(self.interpret_instruction(inst_id));
                }
                RuntimeValue::Tuple(runtime_values)
            },
            Instruction::Operation { source_info: _, op } => self.interpret_operation(*op),
            Instruction::Literal { source_info: _, literal } => match literal {
                // NOTE: we are not interning strings or literals at all right now
                // it might be a good idea to do this later for performance
                Literal::Integer(val) => RuntimeValue::Integer(*val),
                Literal::Float(val) => RuntimeValue::Float(*val),
                Literal::String(val) => RuntimeValue::String(val.clone()),
                Literal::Bool(val) => RuntimeValue::Bool(*val),
            },
            Instruction::LoadElement { source_info: _, index: _, tuple: _ } => {
                todo!("[internal error] we aren't currently using Instruction::LoadElement for anything yet")
            },

            Instruction::Store { address, value, .. } => {
                // address is an InstId whose value is an Address(target)
                let RuntimeValue::Address(target) = self.curr_call_frame().inst_values[&address] else {
                    panic!("[internal error] store to non-address")
                };
                let val = self.curr_call_frame().inst_values[&value].clone();
                // Store the value AT that address
                self.memory.insert(target, val.clone());
                val
                // NOTE: we should never actually use the value of a store for anything...
                // there is no real representation of the value of a store. and if everything went
                // well we should never need it either
            },

            Instruction::Load { address, .. } => {
                let RuntimeValue::Address(target) = self.curr_call_frame().inst_values[&address] else {
                    panic!("[internal error] load from non-address")
                };
                self.memory[&target].clone()
            },
            Instruction::GlobalAlloca(..) | Instruction::Alloca { .. } => {
                // The alloca itself is just an address. store a placeholder
                // that we can load/store to. Use the InstId as the "address".
                RuntimeValue::Address(inst_id)
            },
        }
    }
    pub fn interpret_operation(&mut self, op: IrOperation) -> RuntimeValue {
        match op {
            IrOperation::Add(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Integer(a + b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a + b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a as f32 + b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Float(a + b as f32),
                    (RuntimeValue::String(a), RuntimeValue::String(b)) => RuntimeValue::String(format!("{}{}", a, b)),
                    _ => panic!("invalid operands for addition"),
                }
            },
            IrOperation::Sub(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Integer(a - b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a - b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a as f32 - b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Float(a - b as f32),
                    _ => panic!("invalid operands for subtraction"),
                }
            },
            IrOperation::Mul(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Integer(a * b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a * b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Float(a as f32 * b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Float(a * b as f32),
                    _ => panic!("invalid operands for multiplication"),
                }
            },
            IrOperation::Div(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => {
                        if b == 0 {
                            panic!("division by zero");
                        }
                        RuntimeValue::Integer(a / b)
                    },
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
                        if b == 0.0 {
                            panic!("division by zero");
                        }
                        RuntimeValue::Float(a / b)
                    },
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => {
                        if b == 0.0 {
                            panic!("division by zero");
                        }
                        RuntimeValue::Float(a as f32 / b)
                    },
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => {
                        if b == 0 {
                            panic!("division by zero");
                        }
                        RuntimeValue::Float(a / b as f32)
                    },
                    _ => panic!("invalid operands for division"),
                }
            },
            IrOperation::Mod(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => {
                        if b == 0 {
                            panic!("modulo by zero");
                        }
                        RuntimeValue::Integer(a % b)
                    },
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => {
                        if b == 0.0 {
                            panic!("modulo by zero");
                        }
                        RuntimeValue::Float(a % b)
                    },
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => {
                        if b == 0.0 {
                            panic!("modulo by zero");
                        }
                        RuntimeValue::Float(a as f32 % b)
                    },
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => {
                        if b == 0 {
                            panic!("modulo by zero")
                        }
                        RuntimeValue::Float(a % b as f32)
                    },
                    _ => panic!("invalid operands for modulo"),
                }
            },
            IrOperation::Equal(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                RuntimeValue::Bool(left == right)
            },
            IrOperation::NotEqual(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                RuntimeValue::Bool(left != right)
            },
            IrOperation::Less(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a < b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a < b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) < b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a < (b as f32)),
                    _ => panic!("invalid operands for less than comparison"),
                }
            },
            IrOperation::LessEqual(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a <= b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a <= b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) <= b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a <= (b as f32)),
                    _ => panic!("invalid operands for '<=' comparison"),
                }
            },
            IrOperation::Greater(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a > b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a > b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) > b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a > (b as f32)),
                    _ => panic!("invalid operands for greater than comparison"),
                }
            },
            IrOperation::GreaterEqual(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Integer(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a >= b),
                    (RuntimeValue::Float(a), RuntimeValue::Float(b)) => RuntimeValue::Bool(a >= b),
                    (RuntimeValue::Integer(a), RuntimeValue::Float(b)) => RuntimeValue::Bool((a as f32) >= b),
                    (RuntimeValue::Float(a), RuntimeValue::Integer(b)) => RuntimeValue::Bool(a >= (b as f32)),
                    _ => panic!("invalid operands for greater than or equal comparison"),
                }
            },
            IrOperation::Or(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => RuntimeValue::Bool(a || b),
                    _ => panic!("invalid operands for logical OR (expected booleans)"),
                }
            },
            IrOperation::And(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => RuntimeValue::Bool(a && b),
                    _ => panic!("invalid operands for logical AND (expected booleans)"),
                }
            },
            IrOperation::Not(inst_id) => {
                let value = self.interpret_instruction(inst_id);
                match value {
                    RuntimeValue::Bool(b) => RuntimeValue::Bool(!b),
                    _ => panic!("invalid operand for logical NOT (expected boolean)"),
                }
            },
        }
    }
    // Returns `Some(block_id)` to continue to, or `None` to stop (return)
    pub fn interpret_terminator(&mut self, term_id: TermInstId) -> Option<BlockId> {
        match self.arena.get_term(term_id) {
            TerminatorInst::Return { source_info: _, return_tuple } => {
                if let RuntimeValue::Tuple(return_stack) = self.interpret_instruction(*return_tuple) {
                    self.curr_call_frame_mut().return_stack = return_stack;
                } else {
                    panic!("[internal error] expected a RuntimeValue::Tuple from TerminatorInst::Return")
                }
                None
            },
            TerminatorInst::Branch { source_info: _, cond, true_block, false_block } => {
                let false_block = false_block.clone();
                let true_block = true_block.clone();
                if let RuntimeValue::Bool(cond) = self.interpret_instruction(*cond) {
                    Some(if cond { true_block } else { false_block })
                } else {
                    panic!("[internal error] expected 'RuntimeValue::Bool' in TerminatorInst::Branch condition value")
                }
            },
            TerminatorInst::Jump { source_info: _, target } => Some(*target),
            TerminatorInst::Unreachable => panic!("[internal error] reached 'Unreachable' instruction while interpreting"),
        }
    }
}
