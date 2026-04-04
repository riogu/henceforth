use std::collections::HashSet;

use crate::hfs::{BlockId, InstId, IrArena, IrFuncId};

// these are the basic traits and APIs our passes/pipelines must meet
//
pub trait IrPass {
    fn new() -> Box<Self>
    where Self: Sized;
    fn name(&self) -> &str;
    fn run(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool; // returns true if changed
}
pub trait CleanupPass: IrPass {}
pub trait OptPass: IrPass {
    // opt passes usually require some form of cleanup
    fn get_cleanup_passes(&mut self) -> &mut [Box<dyn CleanupPass>];
    fn run_cleanup(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
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

struct RemoveStaleInstIds {
    any_changed: bool,
    visited: HashSet<BlockId>,
}
impl CleanupPass for RemoveStaleInstIds {}
impl IrPass for RemoveStaleInstIds {
    fn new() -> Box<Self> { Box::new(RemoveStaleInstIds { any_changed: false, visited: HashSet::new() }) }
    fn name(&self) -> &str { "RemoveStaleInstIds: clean all blocks that have stale InstIds" }
    fn run(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let entry_block_id = arena.get_func(func_id).entry_block;
        self.clean_block(arena, entry_block_id);
        self.any_changed
    }
}
impl RemoveStaleInstIds {
    fn clean_block(&mut self, arena: &mut IrArena, block_id: BlockId) {
        if !self.visited.insert(block_id) {
            return; // dont revisit blocks we've seen before
        }
        let insts = arena.get_block(block_id).instructions.clone();
        let len_before = arena.get_block(block_id).instructions.len();

        arena.get_block_mut(block_id).instructions = insts
            .into_iter()
            .filter(|inst_id: &InstId| {
                // SlotMap will return none if this instruction no longer exists
                // we want to remove all cases of that
                arena.try_get_inst(*inst_id).is_some()
            })
            .collect();
        // if we removed anything then this pass caused changes
        self.any_changed |= len_before != arena.get_block_mut(block_id).instructions.len();

        for successor in arena.get_block(block_id).successors.clone() {
            self.clean_block(arena, successor);
        }
    }
}

// ======================================================================================
// DeadCodeElimination implementation
// ======================================================================================

struct DeadCodeElimination {
    cleanup_passes: Vec<Box<dyn CleanupPass>>,
}
impl OptPass for DeadCodeElimination {
    fn get_cleanup_passes(&mut self) -> &mut [Box<dyn CleanupPass>] { &mut self.cleanup_passes }
}
impl IrPass for DeadCodeElimination {
    fn new() -> Box<Self> { Box::new(DeadCodeElimination { cleanup_passes: vec![RemoveStaleInstIds::new()] }) }
    fn name(&self) -> &str { "DeadCodeElimination: remove unused and unreachable code" }
    fn run(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        /*
        1. walk all instructions
        2. if inst has no users AND inst has no side effects:
             add to worklist
        3. while worklist is not empty:
             remove inst from worklist
             for each operand of inst:
               remove (inst, idx) from operand's user list
               if operand now has no users and no side effects:
                 add operand to worklist
             delete inst from arena
        4. cleanup stale IDs from blocks  */

        let mut worklist = Vec::new();
        for inst_id in arena.instructions.keys() {
            if arena.users_of(inst_id).is_empty() && !arena.get_inst(inst_id).has_side_effects() {
                // this is marked as useless
                worklist.push(inst_id)
            }
        }
        for inst_id in worklist {
            // when an instruction is ready for deletion, we have to also consider
            // the effect this
        }
        self.run_cleanup(arena, func_id)
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
