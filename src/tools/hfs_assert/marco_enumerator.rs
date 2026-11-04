use std::time::{Duration, Instant};

use z3::{Params, SatResult, Solver, ast::Bool};

use crate::{hfs::error::CompileError, tools::hfs_assert::stack_assist::TrackedConstraint};

#[derive(Debug)]
pub struct MarcoResult {
    pub mcses: Vec<Vec<usize>>,
    pub muses: Vec<Vec<usize>>,
    pub msses: Vec<Vec<usize>>,
}

pub struct MarcoEnumerator {
    pub theory_solver: Solver,
    pub map_solver: Solver,
    pub constraints: Vec<TrackedConstraint>,
    pub selectors: Vec<Bool>,
}

// MARCO is a method for enumerating MCSes and MUSes from a set of constraint
//
// Marco combines two solvers:
// - A theory solver that checks whether a selected set of constraints is satisfiable
// - A map solver that explores subsets of constraints using selector variables
//
// For each subset selected by the map solver:
// - If the result of the theory solver is UNSAT, it is shrunk to an MUS. We can then block all supersets of that MUS in the map solver.
// - If the result is SAT, it is grown to an MSS, and its complement is an MCS. We can then block all subsets of that MSS in the map solver.
//
// The process continues iteratively until no more explorable subsets are found by the map solver.
impl MarcoEnumerator {
    pub fn new(theory_solver: Solver, constraints: Vec<TrackedConstraint>) -> Self {
        let selectors: Vec<Bool> = (0..constraints.len()).map(|i| Bool::new_const(format!("selector_{i}"))).collect();

        let map_solver = Solver::new();
        let mut p = Params::new();
        p.set_u32("smt.phase_selection", 1);
        map_solver.set_params(&p);

        Self { theory_solver, map_solver, constraints, selectors }
    }

    pub fn enumerate(&self) -> Result<MarcoResult, Box<dyn CompileError>> {
        let mut result = MarcoResult { mcses: vec![], muses: vec![], msses: vec![] };

        loop {
            match self.map_solver.check() {
                SatResult::Unsat => break,
                SatResult::Sat => {},
                SatResult::Unknown => todo!(),
            }

            let model = self.map_solver.get_model().unwrap();

            let subset: Vec<usize> = self
                .selectors
                .iter()
                .enumerate()
                .filter_map(|(i, selector)| {
                    // include i if model evaluates selector to true
                    let value = model.eval(selector, true)?;

                    if value.as_bool() == Some(true) { Some(i) } else { None }
                })
                .collect();

            match self.check_subset(&subset) {
                SatResult::Unsat => {
                    let mus = self.shrink(&subset)?;
                    result.muses.push(mus.clone());
                    self.block_mus(&mus);
                },
                SatResult::Sat => {
                    let mss = self.grow(&subset)?;
                    let mcs: Vec<usize> = (0..self.constraints.len()).filter(|i| !mss.contains(i)).collect();
                    result.msses.push(mss.clone());
                    result.mcses.push(mcs.clone());
                    self.block_mss(&mss);
                },
                SatResult::Unknown => todo!(),
            }
        }

        Ok(result)
    }

    fn assumptions_for(&self, subset: &[usize]) -> Vec<Bool> {
        let mut selected = vec![false; self.constraints.len()];
        for &i in subset {
            selected[i] = true;
        }
        self.constraints.iter().zip(selected).map(|(c, on)| if on { c.literal.clone() } else { c.literal.not() }).collect()
    }

    fn check_subset(&self, subset: &[usize]) -> SatResult {
        self.theory_solver.check_assumptions(&self.assumptions_for(subset))
    }

    fn shrink(&self, subset: &[usize]) -> Result<Vec<usize>, Box<dyn CompileError>> {
        // we use z3 to find a smaller core before running the deletion loop
        let mut mus = subset.to_vec();
        loop {
            if self.check_subset(&mus) != SatResult::Unsat {
                break;
            }
            let core = self.theory_solver.get_unsat_core();
            let next: Vec<usize> = mus.iter().copied().filter(|&i| core.contains(&self.constraints[i].literal)).collect();
            if next.len() == mus.len() || self.check_subset(&next) != SatResult::Unsat {
                break;
            }
            mus = next;
        }

        let mut i = 0;

        while i < mus.len() {
            let mut candidate = mus.clone();
            candidate.remove(i);

            match self.check_subset(&candidate) {
                SatResult::Unsat => {
                    mus = candidate;
                },
                SatResult::Sat => {
                    i += 1;
                },
                SatResult::Unknown => {
                    todo!()
                },
            }
        }

        Ok(mus)
    }

    fn grow(&self, subset: &[usize]) -> Result<Vec<usize>, Box<dyn CompileError>> {
        let mut mss = subset.to_vec();

        for i in 0..self.constraints.len() {
            if mss.contains(&i) {
                continue;
            }

            let mut candidate = mss.clone();
            candidate.push(i);

            match self.check_subset(&candidate) {
                SatResult::Sat => {
                    mss = candidate;
                },
                SatResult::Unsat => {},
                SatResult::Unknown => {
                    todo!()
                },
            }
        }

        Ok(mss)
    }

    fn block_mus(&self, mus: &[usize]) {
        if mus.is_empty() {
            self.map_solver.assert(&Bool::from_bool(false));
            return;
        }

        let clause: Vec<Bool> = mus.iter().map(|&i| self.selectors[i].not()).collect();

        self.map_solver.assert(&Bool::or(&clause.iter().collect::<Vec<_>>()));
    }

    fn block_mss(&self, mss: &[usize]) {
        let clause: Vec<Bool> =
            (0..self.constraints.len()).filter(|i| !mss.contains(i)).map(|i| self.selectors[i].clone()).collect();

        if clause.is_empty() {
            self.map_solver.assert(&Bool::from_bool(false));
            return;
        }

        self.map_solver.assert(&Bool::or(&clause.iter().collect::<Vec<_>>()));
    }
}

#[cfg(test)]
mod tests {
    use crate::{hfs::Span, tools::hfs_assert::stack_assist::ConstraintKind};

    use super::*;
    use z3::ast::Int;

    fn make_constraint(solver: &Solver, name: &str, constraint: Bool) -> TrackedConstraint {
        let literal = Bool::new_const(name);
        solver.assert(&literal.implies(&constraint));

        TrackedConstraint { literal, span: Span::default(), kind: ConstraintKind::StackOperation }
    }

    #[test]
    fn test_simple_contradiction() {
        let solver = Solver::new();
        let x = Int::new_const("x");

        let constraints =
            vec![make_constraint(&solver, "c0", x.gt(Int::from_i64(5))), make_constraint(&solver, "c1", x.lt(Int::from_i64(3)))];

        let result = MarcoEnumerator::new(solver, constraints).enumerate().unwrap();

        assert_eq!(result.muses, vec![vec![0, 1]]);
        assert_eq!(result.mcses.len(), 2);
        assert_eq!(result.msses.len(), 2);
    }

    #[test]
    fn test_independent_contradictions() {
        let solver = Solver::new();

        let x = Int::new_const("x");
        let y = Int::new_const("y");

        let constraints = vec![
            make_constraint(&solver, "c0", x.gt(Int::from_i64(5))),
            make_constraint(&solver, "c1", x.lt(Int::from_i64(3))),
            make_constraint(&solver, "c2", y.gt(Int::from_i64(10))),
            make_constraint(&solver, "c3", y.lt(Int::from_i64(8))),
        ];

        let result = MarcoEnumerator::new(solver, constraints).enumerate().unwrap();

        let mut muses = result.muses.clone();
        muses.sort();

        assert_eq!(muses, vec![vec![0, 1], vec![2, 3]]);
        assert_eq!(result.mcses.len(), 4);
        assert_eq!(result.msses.len(), 4);
    }

    #[test]
    fn test_fully_satisfiable() {
        let solver = Solver::new();

        let x = Int::new_const("x");
        let y = Int::new_const("y");

        let constraints =
            vec![make_constraint(&solver, "c0", x.gt(Int::from_i64(5))), make_constraint(&solver, "c1", y.gt(Int::from_i64(10)))];

        let result = MarcoEnumerator::new(solver, constraints).enumerate().unwrap();

        assert!(result.muses.is_empty());
        assert_eq!(result.msses, vec![vec![0, 1]]);
        assert_eq!(result.mcses, vec![vec![]]);
    }
}
