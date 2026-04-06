use core::panic;
use std::collections::{HashMap, HashSet};

use slotmap::Key;

use crate::hfs::{BlockId, InstId, InstOrTermId, Instruction, IrArena, IrFuncId, TermInstId};

// ============================================================================
// Optimizer related code, Dominance frontiers, DefUseInfo, etc
// ============================================================================
pub struct DefUseInfo {
    // where usize is the index of the operand this instruction is used in
    // for example
    // %6 = add %2, %5
    // we would map %5 to (%6, 2), because it is used on the rhs of the add
    pub users: HashMap<InstId, Vec<(InstOrTermId, usize)>>,
}
impl DefUseInfo {
    pub fn compute(arena: &IrArena, func_id: IrFuncId) -> Self {
        let mut def_use = Self { users: HashMap::new() };
        for block_id in arena.get_blocks_in(func_id) {
            let block = arena.get_block(block_id);
            for inst_id in block.instructions.clone() {
                // iterate every instruction, check what is in its operands and add the current inst to
                // the operand's user list (because this instruction is a user of that operand's value)
                for (op_idx, operand_id) in arena.get_inst(inst_id).get_operands().iter().enumerate() {
                    def_use.add_user(*operand_id, inst_id, op_idx);
                }
            }
            let term_id = block.terminator.expect("[internal error] found block with no terminator while computing DefUseInfo");
            let terminator = arena.get_term(term_id);
            for (op_idx, operand_id) in terminator.get_operands().iter().enumerate() {
                def_use.add_user(*operand_id, term_id, op_idx);
            }
        }
        def_use
    }
    pub fn users_of(&self, inst_id: InstId) -> &[(InstOrTermId, usize)] { self.users.get(&inst_id).map_or(&[], |v| v.as_slice()) }

    pub fn add_user(&mut self, def: InstId, user: impl Into<InstOrTermId>, operand_idx: usize) {
        self.users.entry(def).or_default().push((user.into(), operand_idx));
    }
    pub fn remove_user(&mut self, def: InstId, removed_user: impl Into<InstOrTermId>) {
        // we need this often as DefUseInfo gets stale frequently when stuff is deleted or changed
        let removed_user = removed_user.into();
        if let Some(users) = self.users.get_mut(&def) {
            users.retain(|(user, _)| *user != removed_user);
        }
    }
    pub fn replace_all_uses_with(&mut self, old_id: InstId, new_id: InstId, ir_arena: &mut IrArena) {
        for (user, op_idx) in self.users_of(old_id) {
            match user {
                InstOrTermId::InstId(inst_id) => ir_arena.get_inst_mut(*inst_id).replace_operands(new_id, *op_idx),
                InstOrTermId::TermInstId(term_inst_id) => ir_arena.get_term_mut(*term_inst_id).replace_operand(new_id),
            }
        }
        // replace old entry
        if let Some(users) = self.users.remove(&old_id) {
            self.users.entry(new_id).or_default().extend(users);
        }
    }
    pub fn remove_phi_operand(&mut self, arena: &mut IrArena, phi: InstId, remove_block_id: BlockId) {
        let Instruction::Phi { incoming, .. } = arena.get_inst_mut(phi) else {
            panic!("[internal error] expected phi in remove_phi_operand")
        };
        if incoming.shift_remove(&remove_block_id).is_none() {
            panic!("[internal error] tried removing block from phi that wasn't an operand")
        }
        // we have to update the operand index for the phi operands because when we remove one
        // operand, the indexes might have changed
        let phi_id: InstOrTermId = phi.into();
        for (new_op_idx, (_, inst_id)) in incoming.iter().enumerate() {
            let users = self.users.get_mut(inst_id).expect("[internal error] expected phi operand to have at least 1 user");
            for (user, op_idx) in users.iter_mut() {
                if *user == phi_id {
                    *op_idx = new_op_idx;
                }
            }
        }
    }
}
impl IrArena {
    // used to signal cleanup is needed later
    pub fn invalidate_inst(&mut self, inst_id: InstId) -> Option<Instruction> { self.instructions.remove(inst_id) }
    pub fn inst_is_valid(&self, inst_id: InstId) -> bool { self.instructions.contains_key(inst_id) }
}

// ============================================================================
// DominatorTree
// ============================================================================
impl IrArena {
    pub fn postorder(&self, func_id: IrFuncId) -> Vec<BlockId> {
        /* Only record a block after all its successors have been recorded. So leaves of the CFG
           (blocks with no successors, or whose successors were already visited) get recorded
           first, and the entry block gets recorded last.
              entry
              /   \
             A     B
             |     |
             C     D
              \   /
                E
            postorder = [E, C, A, D, B, entry]
        */
        let mut worklist = Vec::new();
        let mut visited = HashSet::new();
        let entry = self.get_func(func_id).entry_block;
        fn visit_block(block_id: BlockId, arena: &IrArena, worklist: &mut Vec<BlockId>, visited: &mut HashSet<BlockId>) {
            if !visited.insert(block_id) {
                return; // don't visit blocks twice
            }
            for successor in arena.get_block(block_id).successors.clone() {
                visit_block(successor, arena, worklist, visited);
            }
            worklist.push(block_id);
        }
        visit_block(entry, self, &mut worklist, &mut visited);
        worklist
    }
    pub fn reverse_postorder(&self, func_id: IrFuncId) -> Vec<BlockId> {
        /* In reverse-postorder iteration, a node is visited before any of its successor nodes has
           been visited, except when the successor is reached by a back edge.
              entry
              /   \
             A     B
             |     |
             C     D
              \   /
                E
           reverse_postorder = [entry, B, D, A, C, E]
        */
        let mut postorder = self.postorder(func_id);
        postorder.reverse();
        postorder
    }
}
#[derive(Default)]
pub struct DominatorTree {
    immediate_dominators: HashMap<BlockId, BlockId>,
    dominance_frontiers: HashMap<BlockId, Vec<BlockId>>,
    idom_successors: HashMap<BlockId, Vec<BlockId>>,
}

impl DominatorTree {
    /*
     This is an implemention of Cooper-Harvey-Kennedy (A Simple, Fast Dominance Algorithm, 2006)
     It can be read at: https://www.researchgate.net/publication/2569680_A_Simple_Fast_Dominance_Algorithm

     Below is a table that exemplifies the expected result of the dominance calculation
     ──────────────────────────────────────────────────────────────────────────────────
         B0          n   │ DOM       │ IDOM │  DF
         │           B0  │ {0}       │  —   │  —
         v           B1  │ {0,1}     │  0   │  1
         B1 <─────┐  B2  │ {0,1,2}   │  1   │  3
        ╱  ╲      │  B3  │ {0,1,3}   │  1   │  1
       v    v     │  B4  │ {0,1,3,4} │  3   │  —
      B2    B5    │  B5  │ {0,1,5}   │  1   │  3
       │   ╱  ╲   │  B6  │ {0,1,5,6} │  5   │  7
       │  v    v  │  B7  │ {0,1,5,7} │  5   │  3
       │ B6    B8 │  B8  │ {0,1,5,8} │  5   │  7
       │  │    │  │
       │  v    │  │  Dom(n) = {n} ∪ ( ∩ [m ∈ preds(n)] Dom(m) )
       │  B7 <─┘  │  reverse_postorder = [B0, B1, B5, B8, B6, B7, B2, B3, B4]
       v  │       │
      B3 <┘       │
       │          │
       v          │
      B4 ─────────┘
     ──────────────────────────────────────────────────────────────────────────────────
    */
    pub fn compute(arena: &IrArena, func_id: IrFuncId) -> Self {
        // 1. compute reverse postorder
        // 2. compute immediate dominators
        // 3. compute dominance frontiers
        let immediate_dominators = DominatorTree::compute_immediate_dominators(arena, func_id);
        // build a map of successors for easier traversal later
        let mut idom_successors = HashMap::<BlockId, Vec<BlockId>>::new();
        for (block, idom) in &immediate_dominators {
            if block != idom {
                idom_successors.entry(*idom).or_default().push(*block);
            } // skip entry which is its own idom
        }
        let dominance_frontiers = DominatorTree::compute_dominance_frontiers(arena, func_id, &immediate_dominators);
        Self { immediate_dominators, dominance_frontiers, idom_successors }
    }
    fn compute_immediate_dominators(arena: &IrArena, func_id: IrFuncId) -> HashMap<BlockId, BlockId> {
        let mut immediate_dominators = HashMap::<BlockId, BlockId>::new();
        let mut po_indexes = HashMap::<BlockId, usize>::new();
        let mut postorder = arena.postorder(func_id);

        let start_block = postorder.pop().expect("[internal error] computed dominance for empty set of blocks");
        immediate_dominators.insert(start_block, start_block);
        po_indexes.insert(start_block, postorder.len());

        for (po_idx, block) in postorder.iter().enumerate() {
            // initialize the immediate_dominators array
            immediate_dominators.insert(*block, BlockId::null());
            po_indexes.insert(*block, po_idx);
        }
        postorder.reverse(); // turn it into reverse_postorder

        let mut changed = true;
        while changed {
            changed = false; // note that start node isnt in the postorder array
            for curr_block in &postorder {
                let predecessors = &arena.get_block(*curr_block).predecessors;
                let mut new_idom = *predecessors
                    .iter() // pick any processed predecessor of curr_block that was processed
                    .find(|&pred| !immediate_dominators[pred].is_null())
                    .expect("[internal error] must have at least one processed predecessor");
                for predecessor in &arena.get_block(*curr_block).predecessors {
                    if *predecessor == new_idom {
                        continue; // skip the picked element, new idom
                    }
                    if !immediate_dominators[predecessor].is_null() {
                        // if the immediate_dominators was already calculated for the predecessor
                        // then we found a second valid predecessor for intersect()
                        new_idom = DominatorTree::intersect(&mut immediate_dominators, &po_indexes, *predecessor, new_idom);
                    }
                }
                if immediate_dominators[curr_block] != new_idom {
                    immediate_dominators.insert(*curr_block, new_idom);
                    changed = true;
                }
            }
        }
        immediate_dominators
    }
    // Instead of intersecting sets, we walk two fingers up the idom tree until they meet.
    // The finger that's "deeper" (higher RPO number, meaning further from entry) walks up via
    // its immediate dominator. They must eventually meet because both paths lead to entry.
    fn intersect(
        immediate_dominators: &mut HashMap<BlockId, BlockId>,
        po_indexes: &HashMap<BlockId, usize>,
        block1: BlockId,
        block2: BlockId,
    ) -> BlockId {
        let mut finger1 = (po_indexes[&block1], block1);
        let mut finger2 = (po_indexes[&block2], block2);
        // Note that the comparison finger1.0 < finger2.0 uses RPO position, not the BlockId.
        while finger1 != finger2 {
            // (check the graph and table shown above for an example)
            // A lower RPO position means closer to entry, meaning higher in the dominator tree.
            while finger1.0 < finger2.0 {
                // This means the algorithm always moves the deeper finger upward until both
                // fingers point to the same block, which is the nearest common dominator.
                let idom = immediate_dominators[&finger1.1];
                finger1 = (po_indexes[&idom], idom);
            }

            while finger2.0 < finger1.0 {
                // move the other finger up too until we go to where finger1 is at
                let idom = immediate_dominators[&finger2.1];
                finger2 = (po_indexes[&idom], idom);
            }
        }
        finger1.1
    }
    fn compute_dominance_frontiers(
        arena: &IrArena,
        func_id: IrFuncId,
        immediate_dominators: &HashMap<BlockId, BlockId>,
    ) -> HashMap<BlockId, Vec<BlockId>> {
        // the dominance frontier of a block is the (usually one)
        // block that is one CFG edge beyond the region that this blocks dominates.
        // (with some extra conditions that are required for correct usage of this region)

        let mut dominance_frontiers = HashMap::<BlockId, Vec<BlockId>>::new();
        for curr_block in arena.get_blocks_in(func_id) {
            let predecessors = arena.get_block(curr_block).predecessors.clone();
            if predecessors.len() >= 2 {
                // for every join point (block with 2+ predecessors), walk up from each predecessor
                // until you hit the join point's immediate dominator.
                for pred in predecessors {
                    let mut runner = pred;
                    while runner != immediate_dominators[&curr_block] {
                        // Every block passed through on that backwards walk has this join point in
                        // its dominance frontier.
                        // This is because that block dominates the predecessor but doesn't
                        // strictly dominate the join point itself, which is the same as saying
                        // that its dominance "ends" there.
                        dominance_frontiers.entry(runner).or_default().push(curr_block);
                        runner = immediate_dominators[&runner];
                    }
                }
            }
        }
        dominance_frontiers
    }

    // Does block1 dominate block2?
    pub fn dominates(&self, block1: BlockId, block2: BlockId) -> bool {
        // walk up from block2 via idom until we hit block1 or entry
        self.iter_idom(block2).any(|idom| idom == block1)
    }
    /// The immediate dominator of a block
    pub fn idom(&self, block: BlockId) -> BlockId { self.immediate_dominators[&block] }
    /// The dominance frontier of a block
    pub fn dom_frontier(&self, block: BlockId) -> &[BlockId] {
        self.dominance_frontiers.get(&block).map_or(&[], |v| v.as_slice())
    }
    pub fn successors(&self, block_id: BlockId) -> &[BlockId] {
        self.idom_successors.get(&block_id).map_or(&[], |v| v.as_slice())
    }
}
// dom tree iterators
pub struct IdomIter<'a> {
    tree: &'a DominatorTree,
    curr: Option<BlockId>,
}
impl<'a> Iterator for IdomIter<'a> {
    type Item = BlockId;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr?;
        let parent = self.tree.immediate_dominators[&curr];
        if parent == curr {
            // reached entry, stop
            // entry is the only block with itself as an immediate dominator
            self.curr = None;
        } else {
            self.curr = Some(parent);
        }
        Some(curr)
    }
}
impl DominatorTree {
    pub fn iter_idom(&self, block: BlockId) -> IdomIter<'_> { IdomIter { tree: self, curr: Some(block) } }
}
