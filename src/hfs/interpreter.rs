use std::collections::HashMap;

use slotmap::Key;

use crate::hfs::{Builtin, IrArena, IrType, find_builtin, hfs_ir::*, scope_stack::*, token::*};

//---------------------------------------------------------------------------
// Runtime values
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    Integer(i32),
    Float(f32),
    String(String),
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
    // homogeneous and GEP-indexable at runtime; tuples use LoadElement instead
    Array(Vec<RuntimeValue>),
    // InstId of the alloca/global it points into, plus a path of indices into it; GEP appends to the path
    Address(InstId, Vec<usize>),
}

impl RuntimeValue {
    pub fn default(hfs_type: &IrType, arena: &IrArena) -> RuntimeValue {
        match hfs_type {
            IrType::Int { .. } => RuntimeValue::Integer(0),
            IrType::String { .. } => RuntimeValue::String("".to_string()),
            IrType::Bool { .. } => RuntimeValue::Bool(false),
            IrType::Float { .. } => RuntimeValue::Float(0.0),
            // tuple fields aren't defaulted either, always fully written before read
            IrType::Tuple { .. } => RuntimeValue::Tuple(Vec::new()),
            IrType::Array { hfs_type, length, .. } => {
                // elements are written one at a time via GEP + Store, so this needs the right shape
                // upfront. decayed arrays have no length to build that shape from, but they're always
                // overwritten by a whole-array store before anything indexes into them, so an empty
                // placeholder is fine here
                let len = match length {
                    Some(length_inst) => match arena.get_inst(*length_inst) {
                        Instruction::Literal { literal: Literal::Integer(n), .. } => *n as usize,
                        _ => panic!("[internal error] array length must be a compile-time integer literal"),
                    },
                    None => 0,
                };
                let elem_type = arena.get_type(*hfs_type).clone();
                RuntimeValue::Array(vec![RuntimeValue::default(&elem_type, arena); len])
            },
        }
    }
}

// walks an aggregate by a GEP index path
fn navigate<'a>(value: &'a RuntimeValue, path: &[usize]) -> &'a RuntimeValue {
    match path {
        [] => value,
        [first, rest @ ..] => match value {
            RuntimeValue::Array(elems) => navigate(&elems[*first], rest),
            _ => panic!("[internal error] tried to index into a non-array runtime value"),
        },
    }
}

fn navigate_mut<'a>(value: &'a mut RuntimeValue, path: &[usize]) -> &'a mut RuntimeValue {
    match path {
        [] => value,
        [first, rest @ ..] => match value {
            RuntimeValue::Array(elems) => navigate_mut(&mut elems[*first], rest),
            _ => panic!("[internal error] tried to index into a non-array runtime value"),
        },
    }
}

fn print_runtime_value(value: &RuntimeValue) {
    match value {
        RuntimeValue::Integer(v) => print!("{}", v),
        RuntimeValue::Float(v) => print!("{}", v),
        RuntimeValue::String(v) => print!("{}", v),
        RuntimeValue::Bool(v) => print!("{}", v),
        RuntimeValue::Array(elems) | RuntimeValue::Tuple(elems) => {
            print!("[");
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 {
                    print!(", ");
                }
                print_runtime_value(elem);
            }
            print!("]");
        },
        RuntimeValue::Address(inst_id, path) => print!("{:?}{:?}", inst_id, path),
    }
}

// flushes stdout first, in case a prompt was just printed without a trailing newline
fn read_input_line() -> String {
    use std::io::Write;
    let _ = std::io::stdout().flush();
    let mut line = String::new();
    std::io::stdin().read_line(&mut line).expect("[internal error] failed to read from stdin");
    line
}

fn call_builtin(builtin: Builtin, args: Vec<RuntimeValue>) -> Vec<RuntimeValue> {
    match builtin {
        Builtin::Print => {
            for arg in &args {
                print_runtime_value(arg);
            }
            vec![]
        },
        // there's no error-propagation path from a builtin back into the language yet, so
        // malformed input just becomes 0/0.0 instead of crashing the interpreter
        Builtin::InputInt => vec![RuntimeValue::Integer(read_input_line().trim().parse().unwrap_or(0))],
        Builtin::InputFloat => vec![RuntimeValue::Float(read_input_line().trim().parse().unwrap_or(0.0))],
        Builtin::InputStr => vec![RuntimeValue::String(read_input_line().trim_end_matches(['\n', '\r']).to_string())],
    }
}

pub struct CallFrame {
    _func_id: IrFuncId,
    inst_values: HashMap<InstId, RuntimeValue>,
    return_stack: Vec<RuntimeValue>,
    // per-invocation. interpret_block recurses into a callee's blocks for a FunctionCall, so these
    // can't live on Interpreter itself, or a callee's block visits stomp on the caller's
    prev_block_id: BlockId,
    curr_block_id: BlockId,
}

//---------------------------------------------------------------------------
pub struct Interpreter {
    arena: IrArena,
    globals: HashMap<GlobalIrVarId, RuntimeValue>,
    call_stack: Vec<CallFrame>,
    disable_cache: bool, // NOTE: not really used yet (but we should probably)

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
            memory: HashMap::new(),
        }
    }

    pub fn interpret(arena: IrArena, top_level_insts: Vec<IrTopLevelId>, scope_stack: ScopeStack) {
        let mut interpreter = Interpreter::new(arena);
        for inst_id in top_level_insts {
            match inst_id {
                IrTopLevelId::GlobalVarDecl(ir_var_id) => {
                    let var_type = interpreter.arena.get_type_of_var(ir_var_id).clone();
                    let default_val = RuntimeValue::default(&var_type, &interpreter.arena);
                    interpreter.globals.insert(ir_var_id, default_val);
                },
                IrTopLevelId::FunctionDecl(_) => { /* do nothing, declarations dont matter for interpreting */ },
            }
        }

        if let Some(_) = scope_stack.find_function("main") {
            // get the CfgFunction version of main (not the old AST function)
            if let Some((main, _)) = interpreter.arena.functions.iter().find(|func| func.1.name == "main") {
                interpreter.call_declared_function(main, Vec::new());
            };
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
        if let Some(builtin) = find_builtin(&func.name).map(|spec| spec.builtin) {
            return call_builtin(builtin, args);
        }

        // bind all the parameters before interpreting the function
        let mut inst_values = HashMap::new();
        for (param_id, arg_val) in func.parameter_insts.iter().zip(args) {
            inst_values.insert(*param_id, arg_val);
        }
        self.call_stack.push(CallFrame {
            _func_id: func_id,
            inst_values,
            return_stack: vec![],
            prev_block_id: BlockId::null(),
            curr_block_id: BlockId::null(),
        });

        self.interpret_block(func.entry_block);

        self.call_stack.pop().expect("[internal error] wrong scope management while calling function").return_stack
    }
    pub fn interpret_block(&mut self, block_id: BlockId) {
        let mut next_block = Some(block_id);

        while let Some(block_id) = next_block {
            self.curr_call_frame_mut().prev_block_id = self.curr_call_frame().curr_block_id;
            self.curr_call_frame_mut().curr_block_id = block_id;
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
            Instruction::FunctionCall { span: _, args, func_id, is_move: _, return_values } => {
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
            Instruction::Phi { span: _, incoming } =>
                if let Some(inst_id) = incoming.get(&self.curr_call_frame().prev_block_id) {
                    self.interpret_instruction(*inst_id)
                } else {
                    panic!("[internal error] reached phi without going through one of its predecessor blocks")
                },
            Instruction::Tuple { span: _, instructions } => {
                let mut runtime_values = Vec::<RuntimeValue>::new();
                for inst_id in instructions.clone() {
                    runtime_values.push(self.interpret_instruction(inst_id));
                }
                RuntimeValue::Tuple(runtime_values)
            },
            Instruction::Operation { span: _, op } => self.interpret_operation(*op),
            Instruction::Literal { span: _, literal } => match literal {
                // NOTE: we are not interning strings or literals at all right now
                // it might be a good idea to do this later for performance
                Literal::Integer(val) => RuntimeValue::Integer(*val),
                Literal::Float(val) => RuntimeValue::Float(*val),
                Literal::String(val) => RuntimeValue::String(val.clone()),
                Literal::Bool(val) => RuntimeValue::Bool(*val),
            },
            Instruction::LoadElement { span: _, index: _, tuple: _ } => {
                todo!("[internal error] we aren't currently using Instruction::LoadElement for anything yet")
            },

            Instruction::Store { address, value, .. } => {
                // address is an InstId whose value is an Address(target, path)
                let RuntimeValue::Address(target, path) = self.curr_call_frame().inst_values[&address].clone() else {
                    panic!("[internal error] store to non-address")
                };
                let val = self.curr_call_frame().inst_values[&value].clone();
                let slot = self.memory.get_mut(&target).expect("[internal error] store to unallocated memory");
                *navigate_mut(slot, &path) = val.clone();
                val
                // NOTE: we should never actually use the value of a store for anything...
                // there is no real representation of the value of a store. and if everything went
                // well we should never need it either
            },

            Instruction::Load { address, .. } => {
                let RuntimeValue::Address(target, path) = self.curr_call_frame().inst_values[&address].clone() else {
                    panic!("[internal error] load from non-address")
                };
                navigate(&self.memory[&target], &path).clone()
            },
            Instruction::GetElementPtr { address, indexes, .. } => {
                let address = *address;
                let indexes = indexes.clone();
                let RuntimeValue::Address(target, mut path) = self.curr_call_frame().inst_values[&address].clone() else {
                    panic!("[internal error] gep base is not an address")
                };
                for idx_inst in indexes {
                    let RuntimeValue::Integer(i) = self.interpret_instruction(idx_inst) else {
                        panic!("[internal error] gep index must be an integer")
                    };
                    if i < 0 {
                        panic!("array index out of bounds: index is negative ({})", i);
                    }
                    path.push(i as usize);
                }
                RuntimeValue::Address(target, path)
            },
            Instruction::Alloca { type_id, .. } => {
                let ty = self.arena.get_type(*type_id).clone();
                let default_val = RuntimeValue::default(&ty, &self.arena);
                self.memory.insert(inst_id, default_val);
                RuntimeValue::Address(inst_id, vec![])
            },
            Instruction::GlobalAlloca(global_var_id) => {
                let ty = self.arena.get_type_of_var(*global_var_id).clone();
                let default_val = RuntimeValue::default(&ty, &self.arena);
                self.memory.insert(inst_id, default_val);
                RuntimeValue::Address(inst_id, vec![])
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
            TerminatorInst::Return { span: _, return_tuple } => {
                if let RuntimeValue::Tuple(return_stack) = self.interpret_instruction(*return_tuple) {
                    self.curr_call_frame_mut().return_stack = return_stack;
                } else {
                    panic!("[internal error] expected a RuntimeValue::Tuple from TerminatorInst::Return")
                }
                None
            },
            TerminatorInst::Branch { span: _, cond, true_block, false_block } => {
                let false_block = false_block.clone();
                let true_block = true_block.clone();
                if let RuntimeValue::Bool(cond) = self.interpret_instruction(*cond) {
                    Some(if cond { true_block } else { false_block })
                } else {
                    panic!("[internal error] expected 'RuntimeValue::Bool' in TerminatorInst::Branch condition value")
                }
            },
            TerminatorInst::Jump { span: _, target } => Some(*target),
            TerminatorInst::Unreachable => panic!("[internal error] reached 'Unreachable' instruction while interpreting"),
        }
    }
}
