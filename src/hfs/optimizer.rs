use crate::hfs::{DefUseInfo, IrArena, IrFuncId};

// these are the basic traits and APIs our passes/pipelines must meet
//
pub trait IrPass {
    fn new() -> Box<Self>
    where Self: Sized;
    fn name(&self) -> &str;
    fn run(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool; // returns true if changed
}
pub trait CleanupPass: IrPass {}
pub trait OptPass: IrPass {
    // opt passes usually require some form of cleanup
    fn get_cleanup_passes(&self) -> &[Box<dyn CleanupPass>];
    fn run_cleanup(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut any_changed = false;
        for cleanup_pass in self.get_cleanup_passes() {
            any_changed |= cleanup_pass.run(arena, func_id);
        }
        any_changed
    }
}
pub trait OptPipeline {
    fn new() -> Self;
    fn name(&self) -> &str;
    fn run_on_function(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool; // returns true if changed
    fn get_opt_passes(&mut self) -> &mut [Box<dyn OptPass>];

    // default run method if none is provided
    fn run(&mut self, arena: &mut IrArena) -> bool {
        // run all passes in the pipeline at once, for each function
        let mut any_changed: bool = false;
        for func_id in arena.functions.clone().keys() {
            for opt_pass in self.get_opt_passes() {
                any_changed |= opt_pass.run(arena, func_id)
            }
        }
        any_changed
    }
}
// ======================================================================================
// Cleanup passes
// ======================================================================================

struct RemoveStaleInstIds;
impl CleanupPass for RemoveStaleInstIds {}
impl IrPass for RemoveStaleInstIds {
    fn new() -> Box<Self> { Box::new(RemoveStaleInstIds) }
    fn name(&self) -> &str { "RemoveStaleInstIds: clean all blocks that have stale InstIds" }
    fn run(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut any_changed = false;
        for block_id in arena.get_blocks_in(func_id) {
            let block = &mut arena.blocks[block_id];
            let len_before = block.instructions.len();

            block.instructions.retain(|id| arena.instructions.contains_key(*id));

            any_changed |= block.instructions.len() != len_before;
        }
        any_changed
    }
    // SlotMap will return none if this instruction no longer exists, and we want to remove those
}

// ======================================================================================
// DeadCodeElimination implementation
// ======================================================================================

struct DeadCodeElimination {
    cleanup_passes: Vec<Box<dyn CleanupPass>>,
}
impl OptPass for DeadCodeElimination {
    fn get_cleanup_passes(&self) -> &[Box<dyn CleanupPass>] { &self.cleanup_passes }
}
impl IrPass for DeadCodeElimination {
    fn new() -> Box<Self> { Box::new(DeadCodeElimination { cleanup_passes: vec![RemoveStaleInstIds::new()] }) }
    fn name(&self) -> &str { "DeadCodeElimination: remove unused and unreachable code" }
    fn run(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut def_use = DefUseInfo::compute(arena, func_id);
        let mut changed = false;
        // start by marking all useless instructions
        let mut worklist = Vec::new();
        for block_id in arena.get_blocks_in(func_id) {
            for inst_id in arena.get_block(block_id).instructions.clone() {
                if def_use.users_of(inst_id).is_empty() && !arena.get_inst(inst_id).has_side_effects() {
                    worklist.push(inst_id) // this is marked as useless and will be deleted later
                }
            }
        }
        // iterate over all useless instructions and find what instructions became useless as well
        while let Some(inst_id) = worklist.pop() {
            let Some(inst) = arena.try_get_inst(inst_id) else {
                continue; // already deleted, don't add again
            };
            for operand in inst.get_operands() {
                let Some(operand_inst) = arena.try_get_inst(operand) else {
                    continue; // already deleted, don't add again
                };
                // when an instruction is marked as useless, we want to check if it was the only
                // user of its operands, so that we may also delete those as well
                def_use.remove_user(operand, inst_id);
                if def_use.users_of(operand).is_empty() && !operand_inst.has_side_effects() {
                    // by removing this user, there is no one else using this instruction
                    // therefore it is now useless as well
                    worklist.push(operand);
                }
            }
            arena.remove_inst(inst_id);
            changed = true;
        }
        changed | self.run_cleanup(arena, func_id)
    }
}

// ======================================================================================
//  O0 OptPipeline
// ======================================================================================

pub struct O0 {
    opt_passes: Vec<Box<dyn OptPass>>,
}
impl OptPipeline for O0 {
    fn new() -> Self { O0 { opt_passes: vec![DeadCodeElimination::new()] } }
    fn name(&self) -> &str { "-O0: Basic Optimizations" }
    fn run_on_function(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut any_changed: bool = false;
        for pass in self.opt_passes.iter_mut() {
            any_changed |= pass.run(arena, func_id);
        }
        any_changed
    }
    fn get_opt_passes(&mut self) -> &mut [Box<dyn OptPass>] { &mut self.opt_passes }
}
