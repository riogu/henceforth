use std::collections::{HashMap, HashSet};

use indexmap::IndexMap;
use slotmap::Key;

use crate::hfs::{BlockId, DefUseInfo, DominatorTree, InstId, InstOrTermId, Instruction, IrArena, IrFuncId};

// these are the basic traits and APIs our passes/pipelines must meet
pub trait IrPass {
    fn new() -> Box<Self>
    where Self: Sized;
    fn name(&self) -> &str;
    fn run(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool; // returns true if changed
}
pub trait OptPipeline {
    fn new() -> Self;
    fn name(&self) -> &str;
    fn run_on_function(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool; // returns true if changed
    fn get_opt_passes(&mut self) -> &mut [Box<dyn IrPass>];

    // default run method if none is provided
    fn run(&mut self, arena: &mut IrArena) -> bool {
        // run all passes in the pipeline at once, for each function
        let mut any_changed: bool = false;
        for func_id in arena.functions.clone().keys() {
            for opt_pass in self.get_opt_passes() {
                any_changed |= opt_pass.run(arena, func_id)
            }
            // if arena.get_func(func_id).name == "foo" {
            //     let dom = DominatorTree::compute(arena, func_id);
            //     dbg!(dom.immediate_dominators);
            //     dbg!(dom.dominance_frontiers);
            // }
        }
        any_changed
    }
}
// ======================================================================================
//  O0 OptPipeline
// ======================================================================================
macro_rules! passes { ($($pass:ty),+ $(,)?) => { vec![$(<$pass>::new()),+] }; }
pub struct O0 {
    opt_passes: Vec<Box<dyn IrPass>>,
}
impl OptPipeline for O0 {
    fn new() -> Self { O0 { opt_passes: passes!(Mem2Reg, DeadCodeElimination) } }
    fn name(&self) -> &str { "-O0: Basic Optimizations" }
    fn run_on_function(&mut self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut any_changed: bool = false;
        for pass in self.opt_passes.iter_mut() {
            any_changed |= pass.run(arena, func_id);
        }
        any_changed
    }
    fn get_opt_passes(&mut self) -> &mut [Box<dyn IrPass>] { &mut self.opt_passes }
}
// ======================================================================================
// Cleanup passes
// ======================================================================================

pub struct RemoveStaleInstIds;
impl IrPass for RemoveStaleInstIds {
    fn new() -> Box<Self> { Box::new(Self) }
    fn name(&self) -> &str { "RemoveStaleInstIds: clean all blocks that have stale InstIds" }
    fn run(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut any_changed = false;
        for block_id in arena.get_blocks_in(func_id) {
            let mut instructions = arena.get_block(block_id).instructions.clone();
            let len_before = instructions.len();

            // cleanup all the instructions that were removed
            instructions.retain(|inst_id| arena.inst_is_valid(*inst_id));

            any_changed |= instructions.len() != len_before;
            arena.get_block_mut(block_id).instructions = instructions;
        }
        any_changed
    }
}

// ======================================================================================
// DeadCodeElimination implementation
// ======================================================================================

pub struct DeadCodeElimination;

impl IrPass for DeadCodeElimination {
    fn new() -> Box<Self> { Box::new(Self) }
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
            arena.invalidate_inst(inst_id);
            changed = true;
        } // run RemoveStaleInstIds for cleanup
        changed | RemoveStaleInstIds::new().run(arena, func_id)
    }
}

// ======================================================================================
// Mem2Reg implementation (SSA construction for alloca instruction replacement)
// Algorithm is based on Engineering a Compiler (Chap 9.3.3 and 9.3.4), although
// it has some changes to focus on removing alloca/load/store pairs specifically
// ======================================================================================
pub struct Mem2Reg;
impl IrPass for Mem2Reg {
    fn new() -> Box<Self> { Box::new(Self) }
    fn name(&self) -> &str { "Mem2Reg: replace alloca/load/store with ssa values and phis" }
    fn run(&self, arena: &mut IrArena, func_id: IrFuncId) -> bool {
        let mut def_use = DefUseInfo::compute(arena, func_id);
        let dom_tree = DominatorTree::compute(arena, func_id);
        // 1. find all promotable allocas
        let promotable_allocas = Mem2Reg::find_promotable_allocas(arena, &def_use, func_id);

        // 2. for each alloca: compute iterated dominance frontier, insert phis
        // The rename phase uses this to know which alloca's stack it pushes the curr phi to
        let mut phi_to_alloca = HashMap::<InstId, InstId>::new();
        for alloca in &promotable_allocas {
            let inserted_phis = Mem2Reg::phi_insertion(arena, &dom_tree, alloca, func_id);
            for phi in inserted_phis {
                phi_to_alloca.insert(phi, *alloca);
            }
        }
        // 3. walk dominator tree once, renaming all allocas simultaneously:
        //    - maintain a stack of current values per alloca
        //    - replace loads, delete stores, fill in phi operands
        Mem2Reg::rename_variables(
            arena,
            &phi_to_alloca,
            &promotable_allocas,
            &dom_tree,
            &mut def_use,
            &mut HashMap::<InstId, Vec<InstId>>::new(),
            arena.get_func(func_id).entry_block,
        );
        for alloca in promotable_allocas {
            arena.invalidate_inst(alloca); // invalidate allocas for cleanup after
        }
        // 4. delete all the dead allocas, stores, loads
        RemoveStaleInstIds::new().run(arena, func_id)
        // if this pass does anything, then RemoveStaleInstIds will have stuff to cleanup
        // therefore checking if it did anything is enough to know if this pass did work
    }
}
// phase 1 of SSA construction algorithm (also known as Mem2Reg)
impl Mem2Reg {
    // compute iterated dominance frontier, insert phis
    fn phi_insertion(arena: &mut IrArena, dom_tree: &DominatorTree, alloca: &InstId, func_id: IrFuncId) -> Vec<InstId> {
        // worklist begins with all the blocks that store to this alloca
        let mut worklist = Vec::new();
        for block_id in arena.get_blocks_in(func_id) {
            for inst_id in &arena.get_block(block_id).instructions {
                if let Instruction::Store { address, .. } = arena.get_inst(*inst_id)
                    && address == alloca
                {
                    worklist.push(block_id); // this block stores (redefines) this alloca
                }
            }
        }
        // for every store to an alloca address, we need to place a phi on the dominance frontier
        // of that store's block.
        // in turn, these blocks now also need phis, which from the algorithm's perspective is the
        // same as a new definition of the variable. this means we also need a phi at the dominance
        // frontier of these blocks.
        let mut needs_phi = HashSet::<BlockId>::new();
        while let Some(curr_block) = worklist.pop() {
            for df_block in dom_tree.dom_frontier(curr_block) {
                if !needs_phi.contains(df_block) {
                    needs_phi.insert(*df_block);
                    worklist.push(*df_block); // df_block now has a phi, so it has a new definition
                }
            }
        }
        let source_info = arena.get_inst(*alloca).get_source_info();
        let mut inserted_phis = Vec::new();
        for block_id in needs_phi {
            let predecessors = arena.get_block(block_id).predecessors.clone();
            let incoming: IndexMap<BlockId, InstId> =
                predecessors.into_iter().map(|pred| (pred, InstId::null() /* placeholder to be filled in later */)).collect();
            inserted_phis
                .push(arena.alloc_inst_at_start_for(Instruction::Phi { source_info: source_info.clone(), incoming }, block_id));
        }
        inserted_phis
    }

    fn find_promotable_allocas(arena: &IrArena, def_use: &DefUseInfo, func_id: IrFuncId) -> HashSet<InstId> {
        let mut promotable_allocas = HashSet::new();
        for block_id in arena.get_blocks_in(func_id) {
            for inst_id in &arena.get_block(block_id).instructions {
                if Mem2Reg::is_promotable_alloca(arena, &def_use, *inst_id) {
                    promotable_allocas.insert(*inst_id);
                }
            }
        }
        promotable_allocas
    }
    fn is_promotable_alloca(arena: &IrArena, def_use: &DefUseInfo, alloca: InstId) -> bool {
        if !matches!(arena.get_inst(alloca), Instruction::Alloca { .. }) {
            return false; // needs to be an alloca
        }
        for (user_id, _) in def_use.users_of(alloca) {
            let user = match user_id {
                InstOrTermId::InstId(inst_id) => arena.get_inst(*inst_id),
                InstOrTermId::TermInstId(_) => panic!("[internal error] alloca shouldn't be used by a terminator operand"),
            };
            match user {
                // promotable allocas are allocas that are:
                // - only used directly by loads or is the address operand of a store instruction
                // - never have their address taken (no pointer escapes).
                // - It can't show up in function arguments or be stored into another alloca'd variable
                Instruction::Load { address, .. } if *address == alloca => continue,
                Instruction::Store { address, .. } if *address == alloca => continue,
                _ => return false,
            }
        }
        true
    }
}

// phase 2 of SSA construction algorithm
impl Mem2Reg {
    // the idea is to walk the dominator tree top-down,
    // maintaining a stack of what is the current SSA value for each alloca.
    // its like interpreting the code but we rewrite the IR instead
    fn rename_variables(
        arena: &mut IrArena,
        phi_to_alloca: &HashMap<InstId, InstId>,
        promotable_allocas: &HashSet<InstId>,
        dom_tree: &DominatorTree,
        def_use: &mut DefUseInfo,
        alloca_stacks: &mut HashMap<InstId, Vec<InstId>>,
        block_id: BlockId,
    ) {
        // // this maps allocas to a stack of the current SSA value for it
        for inst_id in arena.get_block(block_id).instructions.clone() {
            match arena.get_inst(inst_id) {
                Instruction::Phi { .. } =>
                    if let Some(alloca) = phi_to_alloca.get(&inst_id) {
                        // this means we found a phi meant to replace our alloca
                        // this phi represents what the value of that alloca is at this point.
                        alloca_stacks.entry(*alloca).or_default().push(inst_id)
                    },
                Instruction::Load { address, .. } =>
                    if promotable_allocas.contains(address) {
                        // we found a load from one of the allocas we are replacing.
                        // all uses of this loaded value should be replaced with the latest value in
                        // our alloca value tracking stack
                        let latest_def = *alloca_stacks[address].last().expect("[internal error] found no value for alloca");
                        def_use.replace_all_uses_with(inst_id, latest_def, arena);
                        arena.invalidate_inst(inst_id); // invalidate to cleanup later
                    },
                Instruction::Store { address, value, .. } =>
                    if let Some(alloca) = promotable_allocas.get(address) {
                        // we found a store targeting a promotable alloca.
                        // this means we want to "interpret" this store by adding the value to our
                        // alloca stack, and then using that later.
                        alloca_stacks.entry(*alloca).or_default().push(*value);
                        arena.invalidate_inst(inst_id); // invalidate to cleanup later
                    },
                _ => {},
            }
        }
        for successor in arena.get_block(block_id).successors.clone() {
            for inst_id in arena.get_block(successor).instructions.clone() {
                // For each successor block, if it has a phi for one of our allocas,
                // fill in the phi operand for the current block's edge with the top of the alloca's stack.
                if let Some(alloca) = phi_to_alloca.get(&inst_id) {
                    if let Instruction::Phi { incoming, .. } = arena.get_inst_mut(inst_id) {
                        let latest_def = *alloca_stacks[alloca].last().expect("[internal error] found no value for alloca");
                        incoming.entry(block_id).insert_entry(latest_def);
                    }
                }
            }
        }
        // recursively continue renaming in dominator tree successor order
        for dom_successor in dom_tree.successors(block_id) {
            // store the state the stack had before we went into a successor block
            let snapshot: HashMap<InstId, usize> = alloca_stacks.iter().map(|(k, v)| (*k, v.len())).collect();
            Mem2Reg::rename_variables(arena, phi_to_alloca, promotable_allocas, dom_tree, def_use, alloca_stacks, *dom_successor);
            // restore the alloca_stacks back to how it was before the successor changed it
            for (alloca, len) in snapshot {
                alloca_stacks.get_mut(&alloca).unwrap().truncate(len);
            }
        }
    }
}
