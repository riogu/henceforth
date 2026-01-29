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
            Type::Int => RuntimeValue::Integer(0),
            Type::String => RuntimeValue::String("".to_string()),
            Type::Bool => RuntimeValue::Bool(false),
            Type::Float => RuntimeValue::Float(0.0),
            Type::Tuple(type_ids) => RuntimeValue::Tuple(Vec::new()),
        }
    }
}
pub struct CallFrame {
    func_id: IrFuncId,
    inst_values: HashMap<InstId, RuntimeValue>,
}

//---------------------------------------------------------------------------
pub struct Interpreter {
    arena: IrArena,
    globals: HashMap<IrVarId, RuntimeValue>,
    call_stack: Vec<CallFrame>,
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
        Self { arena, globals: HashMap::new(), call_stack: Vec::new() }
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

    fn call_declared_function(&mut self, func_id: IrFuncId, tuple_args: Vec<RuntimeValue>) -> Vec<RuntimeValue> {
        self.call_stack.push(CallFrame { func_id, inst_values: HashMap::new() });
        let func = self.arena.get_func(func_id);
        self.interpret_block(func.entry_block);
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
        todo!()
    }
    pub fn interpret_block(&mut self, block_id: BlockId) {
        let block = self.arena.get_block(block_id);
        for inst_ir in &block.instructions {}
    }
    pub fn interpret_instruction(&mut self, inst_id: InstId) {
        // TODO: add a cache to check if this InstId has been interpreted before
        // and just return the computed value if yes
        match self.arena.get_instruction(inst_id) {
            Instruction::Parameter { source_info, index, type_id } => todo!(),
            Instruction::FunctionCall { source_info, args, func_id, is_move } => todo!(),
            Instruction::Phi { source_info, incoming } => todo!(),
            Instruction::StackKeyword { source_info, name, args } => todo!(),
            Instruction::Tuple { source_info, instructions } => todo!(),
            Instruction::Operation(source_info, cfg_operation) => self.interpret_operation(*cfg_operation),
            Instruction::Literal(source_info, literal) => todo!(),
            Instruction::LoadElement { source_info, index, tuple } => todo!(),
        }
    }
    pub fn interpret_terminator(&mut self, term_id: TermInstId) {
        match self.arena.get_terminator_instruction(term_id) {
            TerminatorInst::Return(source_info, inst_id) => todo!(),
            TerminatorInst::Branch { source_info, cond, true_block, false_block } => todo!(),
            TerminatorInst::Jump(source_info, block_id) => todo!(),
            TerminatorInst::Unreachable => todo!(),
        }
    }
    pub fn interpret_operation(&mut self, op: CfgOperation) {
        match op {
            CfgOperation::Add(inst_id, inst_id1) => todo!(),
            CfgOperation::Sub(inst_id, inst_id1) => todo!(),
            CfgOperation::Mul(inst_id, inst_id1) => todo!(),
            CfgOperation::Div(inst_id, inst_id1) => todo!(),
            CfgOperation::Mod(inst_id, inst_id1) => todo!(),
            CfgOperation::Equal(inst_id, inst_id1) => todo!(),
            CfgOperation::NotEqual(inst_id, inst_id1) => todo!(),
            CfgOperation::Less(inst_id, inst_id1) => todo!(),
            CfgOperation::LessEqual(inst_id, inst_id1) => todo!(),
            CfgOperation::Greater(inst_id, inst_id1) => todo!(),
            CfgOperation::GreaterEqual(inst_id, inst_id1) => todo!(),
            CfgOperation::Or(inst_id, inst_id1) => todo!(),
            CfgOperation::And(inst_id, inst_id1) => todo!(),
            CfgOperation::Not(inst_id) => todo!(),
        }
    }
}
