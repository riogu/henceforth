use std::collections::HashMap;

use crate::hfs::{
    ast::*,
    cfg_analyzer::*,
    hfs_mir::*,
    scope_stack::{self, *},
    token::*,
    types::*,
};

//---------------------------------------------------------------------------
// Runtime values
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
}

impl RuntimeValue {
    pub fn default(hfs_type: &Type) -> RuntimeValue {
        match hfs_type {
            Type::Int { .. } => RuntimeValue::Integer(0),
            Type::String { .. } => RuntimeValue::String("".to_string()),
            Type::Bool { .. } => RuntimeValue::Bool(false),
            Type::Float { .. } => RuntimeValue::Float(0.0),
            Type::Tuple { .. } => RuntimeValue::Tuple(Vec::new()),
        }
    }
}
pub struct CallFrame {
    func_id: IrFuncId,
    inst_values: HashMap<InstId, RuntimeValue>,
    return_stack: Vec<RuntimeValue>,
}
// struct Sentinel<'a, T> {
//     saved_value: T,
//     value: &'a T,
// }
// // TODO: actually learn how traits work and make a Sentinel trait
// // also probably make it require bools only for now, but maybe generalize it later
//
// impl<'a, T> Sentinel<'a, T>
// where
//     T: Clone,
// {
//     pub fn new(value: &'a T) -> Sentinel<'a, T> {
//         Sentinel { saved_value: value.clone(), value }
//     }
// }
//
//---------------------------------------------------------------------------
pub struct Interpreter {
    arena: IrArena,
    globals: HashMap<GlobalIrVarId, RuntimeValue>,
    call_stack: Vec<CallFrame>,
    disable_cache: bool, // NOTE: not really used yet (but we should probably)
    curr_block_id: BlockId,
}
impl Interpreter {
    // utils
    pub fn curr_call_frame(&self) -> &CallFrame {
        self.call_stack.last().expect("call stack shouldn't be empty")
    }

    pub fn curr_call_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().expect("call stack shouldn't be empty")
    }
}
impl Interpreter {
    pub fn new(arena: IrArena) -> Self {
        Self { arena, globals: HashMap::new(), call_stack: Vec::new(), disable_cache: false, curr_block_id: BlockId(0) }
    }

    pub fn interpret(arena: IrArena, top_level_insts: Vec<CfgTopLevelId>, scope_stack: ScopeStack) {
        let mut interpreter = Interpreter::new(arena);
        for inst_id in top_level_insts {
            match inst_id {
                CfgTopLevelId::GlobalVarDecl(ir_var_id) => {
                    let default_val = RuntimeValue::default(interpreter.arena.get_type_of_var(ir_var_id));
                    interpreter.globals.insert(ir_var_id, default_val);
                },
                CfgTopLevelId::FunctionDecl(ir_func_id) => { /* do nothing, declarations dont matter for interpreting */ },
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
        self.curr_block_id = block_id;
        let block = self.arena.get_block(block_id);
        let term = block.terminator;
        for inst_id in block.instructions.clone() {
            self.interpret_instruction(inst_id);
        }
        if let Some(terminator) = term {
            self.interpret_terminator(terminator);
        } else {
            // TODO: joao please add a nicer print here with info about the broken block
            panic!(
                "[internal error] found block with no terminator in: '{}'",
                self.arena.get_func(self.curr_call_frame().func_id).get_repr(&self.arena)
            )
            // panic!("[internal error] found block with no terminator: '{}'", self.arena.get_block(block_id).get_repr(&self.arena))
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

        match self.arena.get_instruction(inst_id) {
            Instruction::Parameter { source_info, index, type_id } => {
                // parameters should always hit the cache (because they are bound at the start of
                // the function
                panic!(
                    "[internal error] found unbound 'Instruction::Parameter'. a parameter should be bound to a value before \
                     being interpreted\n {}",
                    self.arena.get_func(self.curr_call_frame().func_id).get_repr(&self.arena)
                )
            },
            Instruction::ReturnValue { source_info, type_id } => {
                panic!(
                    "[internal error] found unbound 'Instruction::ReturnValue'. a ReturnValue should be bound to a value before \
                     being interpreted"
                )
            },
            Instruction::FunctionCall { source_info, args, func_id, is_move, return_values } => {
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
            Instruction::Phi { source_info, incoming } =>
                if let Some(inst_id) = incoming.get(&self.curr_block_id) {
                    self.interpret_instruction(*inst_id)
                } else {
                    panic!("[internal error] reached phi without going through one of its predecessor blocks")
                },
            Instruction::StackKeyword { source_info, name, args } =>
            // NOTE: in my opinion, StackKeywords shouldnt exist in MIR. we should create MIR that
            // is the result of each StackKeyword in cfg_analyzer instead. if you do this,
            // eliminate this instruction
                todo!("joao implement this however you like."),
            Instruction::Tuple { source_info, instructions } => {
                let mut runtime_values = Vec::<RuntimeValue>::new();
                for inst_id in instructions.clone() {
                    runtime_values.push(self.interpret_instruction(inst_id));
                }
                RuntimeValue::Tuple(runtime_values)
            },
            Instruction::Operation(source_info, cfg_operation) => self.interpret_operation(*cfg_operation),
            Instruction::Literal(source_info, literal) => match literal {
                // NOTE: we are not interning strings or literals at all right now
                // it might be a good idea to do this later for performance
                Literal::Integer(val) => RuntimeValue::Integer(*val),
                Literal::Float(val) => RuntimeValue::Float(*val),
                Literal::String(val) => RuntimeValue::String(val.clone()),
                Literal::Bool(val) => RuntimeValue::Bool(*val),
            },
            Instruction::LoadElement { source_info, index, tuple } =>
                todo!("[internal error] we aren't currently using Instruction::LoadElement for anything yet"),
            Instruction::Load { source_info, address, type_id } => todo!(),
            Instruction::Store { source_info, address, value } => todo!(),
            Instruction::Alloca { source_info, type_id } => todo!(),
            Instruction::GlobalAlloca(global_ir_var_id) => todo!(),
        }
    }
    pub fn interpret_operation(&mut self, op: CfgOperation) -> RuntimeValue {
        match op {
            CfgOperation::Add(l, r) => {
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
            CfgOperation::Sub(l, r) => {
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
            CfgOperation::Mul(l, r) => {
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
            CfgOperation::Div(l, r) => {
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
            CfgOperation::Mod(l, r) => {
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
            CfgOperation::Equal(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                RuntimeValue::Bool(left == right)
            },
            CfgOperation::NotEqual(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                RuntimeValue::Bool(left != right)
            },
            CfgOperation::Less(l, r) => {
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
            CfgOperation::LessEqual(l, r) => {
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
            CfgOperation::Greater(l, r) => {
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
            CfgOperation::GreaterEqual(l, r) => {
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
            CfgOperation::Or(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => RuntimeValue::Bool(a || b),
                    _ => panic!("invalid operands for logical OR (expected booleans)"),
                }
            },
            CfgOperation::And(l, r) => {
                let left = self.interpret_instruction(l);
                let right = self.interpret_instruction(r);
                match (left, right) {
                    (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => RuntimeValue::Bool(a && b),
                    _ => panic!("invalid operands for logical AND (expected booleans)"),
                }
            },
            CfgOperation::Not(inst_id) => {
                let value = self.interpret_instruction(inst_id);
                match value {
                    RuntimeValue::Bool(b) => RuntimeValue::Bool(!b),
                    _ => panic!("invalid operand for logical NOT (expected boolean)"),
                }
            },
        }
    }
    pub fn interpret_terminator(&mut self, term_id: TermInstId) {
        match self.arena.get_terminator_instruction(term_id) {
            TerminatorInst::Return(source_info, inst_id) => {
                if let RuntimeValue::Tuple(return_stack) = self.interpret_instruction(*inst_id) {
                    self.curr_call_frame_mut().return_stack = return_stack;
                } else {
                    panic!("[internal error] expected a RuntimeValue::Tuple from TerminatorInst::Return")
                }
            },
            TerminatorInst::Branch { source_info, cond, true_block, false_block } => {
                let false_block = false_block.clone();
                let true_block = true_block.clone();
                if let RuntimeValue::Bool(cond) = self.interpret_instruction(*cond) {
                    if cond {
                        self.interpret_block(true_block);
                    } else {
                        self.interpret_block(false_block);
                    }
                } else {
                    panic!("[internal error] expected 'RuntimeValue::Bool' in TerminatorInst::Branch condition value")
                }
            },
            TerminatorInst::Jump(source_info, block_id) => self.interpret_block(*block_id),
            TerminatorInst::Unreachable => panic!("[internal error] reached 'Unreachable' instruction while interpreting"),
        }
    }
}
