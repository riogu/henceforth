// Rendel & Ostermann-style invertible syntax for HFS MIR
//
// We define a set of syntax descriptors that are seen as
// the source of truth, and are simultaneously a printer and a parser
//
// Paper: https://dl.acm.org/doi/10.1145/2088456.1863525
//
// This implementation solves the problem of having a separate parser and printer, which can become
// out of sync.
//
// We think of parsers and printers as inverse functions of each other, going from concrete to
// abstract syntax, and vice-versa. We can then define a trait (Syntax trait) which both the parser
// and the printer implement, which defines their specific behavior. However, just this isn't
// enough, since if we were to compose two parsers (e.g. load and store), they would return
// different ASTs, with no common structure, making it hard to compose them. To fix this problem, we
// add two extra layers, flat representation and ADT, and define functions (Isos) that transform
// between them (we have to specify both sides, but since these correspond to the internal
// representation, it doesn't compromise the invertibility as much). Now, instead of composing the
// parsers themselves, we compose the Isos
//
// Our pipeline looks like this now:
//
// Text -> Flat representation -> ADT -> AST
//
// Using those Isos and the Syntax trait, we define syntax descriptors, which define the specifics
// of both the textual representation and the AST at the same time, by being generic over a Syntax.
// Note that we never specify if we are applying or unapplying the Isos in the descriptors, that is
// handled by the Syntax
//
// Additionally, since this IR can have forward references to registers and blocks, we need a pass
// that collects names

// Partial isomorphisms (Iso)
// Represents a partial invertible function, that is:
// - a function that has two directions that are inverses of each other
// - either direction can fail, returning None
// - a round-trip `unapply(apply(x))` will return x iff neither direction fails

use std::collections::HashMap;

use crate::hfs::{BlockId, InstId, TypeId};

pub struct NameMap {
    pub inst_to_name: HashMap<InstId, String>,
    pub name_to_inst: HashMap<String, InstId>,
    pub block_to_name: HashMap<BlockId, String>,
    pub name_to_block: HashMap<String, BlockId>,
    pub type_to_name: HashMap<TypeId, String>,
    pub name_to_type: HashMap<String, TypeId>,
}

pub struct Iso<A, B> {
    pub apply: Box<dyn Fn(A) -> Option<B> + 'static>,
    pub unapply: Box<dyn Fn(B) -> Option<A> + 'static>,
}

impl<A: 'static, B: 'static> Iso<A, B> {
    fn new(apply: impl Fn(A) -> Option<B> + 'static, unapply: impl Fn(B) -> Option<A> + 'static) -> Self {
        Self { apply: Box::new(apply), unapply: Box::new(unapply) }
    }

    // given two isomorphisms:
    // A <-> B and B <-> C
    // we compose them such that the result is an isomorphism A <-> C
    // if one of the directions fails for one of the isomorphisms, the resulting will fail too
    pub fn compose<C: 'static>(self, other: Iso<B, C>) -> Iso<A, C> {
        let apply_ab = self.apply;
        let unapply_ab = self.unapply;
        let apply_bc = other.apply;
        let unapply_bc = other.unapply;
        Iso {
            apply: Box::new(move |a| (apply_ab)(a).and_then(|b| (apply_bc)(b))),
            unapply: Box::new(move |c| (unapply_bc)(c).and_then(|b| (unapply_ab)(b))),
        }
    }
}

// Syntax interface (Syntax trait)
// The Syntax trait allows us to have a generic interface for both parsing and pretty-printing, without actually caring which one is which.
// Then, each syntax descriptor (shared grammar), will take a type that implements Syntax and perform its specific behavior
pub trait Syntax {
    type Output<A: 'static>;

    // Equivalent to the IsoFunctor typeclass defined in the paper.
    // Transforms an Output<A> into an Output<B> using an Iso<A, B>.
    // For the printer: unapply the iso (B -> A) then run the inner printer.
    // For the parser: run the inner parser then apply the iso (A -> B).
    fn iso<A: 'static, B: 'static>(&self, iso: Iso<A, B>, inner: Self::Output<A>) -> Self::Output<B>;

    // matches a single character
    fn token(&self) -> Self::Output<char>;

    // Equivalent to the ProductFunctor typeclass.
    // Represents a combinator that performs sequential composition
    // For the printer: print A then print B and concatenate
    // For the parser: parse A then parse B from the remaining input
    fn product<A: 'static, B: 'static>(&self, left: Self::Output<A>, right: Self::Output<B>) -> Self::Output<(A, B)>;

    // Equivalent to the Alternative typeclass.
    // Represents the <|> combinator that tries the left side, and falls back to the right side if None is returned
    fn alt<A: Clone>(&self, left: Self::Output<A>, right: Self::Output<A>) -> Self::Output<A>;

    // The following methods are derived solely from these three, which are sufficient for a full bidirectional parser and pretty printer

    fn empty<A>(&self) -> Self::Output<A>;

    fn keyword(&self, s: &'static str) -> Self::Output<()>;

    fn literal_str(&self, s: &'static str) -> Self::Output<()>;

    fn many<A>(&self, item: Self::Output<A>) -> Self::Output<Vec<A>>;

    fn sep_by<A>(&self, item: Self::Output<A>, sep: &'static str) -> Self::Output<Vec<A>>;

    fn whitespace(&self) -> Self::Output<()>;

    fn opt_whitespace(&self) -> Self::Output<()>;

    fn newline(&self) -> Self::Output<()>;

    fn uint(&self) -> Self::Output<usize>;
}

// Printer implementation
//
// Implementation of the Syntax Trait for the printer, this corresponds to unapplying an isomorphism

pub struct Printer;

pub struct PrinterSyntax<A: 'static>(pub Box<dyn Fn(A) -> Option<String>>);

impl Syntax for Printer {
    type Output<A: 'static> = PrinterSyntax<A>;

    fn iso<A: 'static, B: 'static>(&self, iso: Iso<A, B>, inner: Self::Output<A>) -> Self::Output<B> {
        PrinterSyntax(Box::new(move |b| {
            let a = (iso.unapply)(b)?;
            (inner.0)(a)
        }))
    }

    fn token(&self) -> PrinterSyntax<char> {
        PrinterSyntax(Box::new(|c| Some(c.to_string())))
    }

    fn product<A, B>(&self, left: Self::Output<A>, right: Self::Output<B>) -> Self::Output<(A, B)> {
        PrinterSyntax(Box::new(move |(a, b)| {
            let ls = (left.0)(a)?;
            let rs = (right.0)(b)?;
            Some(ls + &rs)
        }))
    }

    fn alt<A: Clone>(&self, left: Self::Output<A>, right: Self::Output<A>) -> Self::Output<A> {
        PrinterSyntax(Box::new(move |a: A| (left.0)(a.clone()).or_else(|| right.0(a))))
    }

    fn empty<A>(&self) -> Self::Output<A> {
        PrinterSyntax(Box::new(|_| None))
    }

    fn keyword(&self, s: &'static str) -> Self::Output<()> {
        self.literal_str(s)
    }

    fn literal_str(&self, s: &'static str) -> PrinterSyntax<()> {
        PrinterSyntax(Box::new(move |()| Some(s.to_string())))
    }

    fn many<A>(&self, item: Self::Output<A>) -> Self::Output<Vec<A>> {
        PrinterSyntax(Box::new(move |xs: Vec<A>| {
            let mut out = String::new();
            for x in xs {
                out += &(item.0)(x)?;
            }
            Some(out)
        }))
    }

    fn sep_by<A>(&self, item: PrinterSyntax<A>, sep: &'static str) -> PrinterSyntax<Vec<A>> {
        PrinterSyntax(Box::new(move |xs: Vec<A>| {
            let parts: Option<Vec<String>> = xs.into_iter().map(|x| (item.0)(x)).collect();
            Some(parts?.join(sep))
        }))
    }

    fn whitespace(&self) -> PrinterSyntax<()> {
        PrinterSyntax(Box::new(|()| Some(" ".into())))
    }
    fn opt_whitespace(&self) -> PrinterSyntax<()> {
        PrinterSyntax(Box::new(|()| Some("".into())))
    }
    fn newline(&self) -> PrinterSyntax<()> {
        PrinterSyntax(Box::new(|()| Some("\n".into())))
    }

    fn uint(&self) -> PrinterSyntax<usize> {
        PrinterSyntax(Box::new(|n| Some(n.to_string())))
    }
}

// Parser implementation
//
// Implementation of the Syntax Trait for the parser, this corresponds to applying an isomorphism
// A combinatorial parser works by trying to consume a prefix and returning the rest (`&str -> Option<(A, &str)>`)

pub struct IrParser;
pub struct IrParserSyntax<A>(pub Box<dyn Fn(&str) -> Option<(A, &str)>>);

impl Syntax for IrParser {
    type Output<A: 'static> = IrParserSyntax<A>;

    fn iso<A: 'static, B: 'static>(&self, iso: Iso<A, B>, inner: Self::Output<A>) -> Self::Output<B> {
        IrParserSyntax(Box::new(move |input| {
            // Parses the input, then applies the isomorphism and returns the result + the remaining string
            let (a, rest) = (inner.0)(input)?;
            let b = (iso.apply)(a)?;
            Some((b, rest))
        }))
    }

    fn product<A: 'static, B: 'static>(&self, left: Self::Output<A>, right: Self::Output<B>) -> Self::Output<(A, B)> {
        IrParserSyntax(Box::new(move |input| {
            let (a, rest) = (left.0)(input)?;
            let (b, rest2) = (right.0)(rest)?;
            Some(((a, b), rest2))
        }))
    }

    fn alt<A: Clone + 'static>(&self, left: Self::Output<A>, right: Self::Output<A>) -> Self::Output<A> {
        IrParserSyntax(Box::new(move |input| (left.0)(input).or_else(|| (right.0)(input))))
    }

    fn empty<A: 'static>(&self) -> Self::Output<A> {
        IrParserSyntax(Box::new(|_| None))
    }

    fn literal_str(&self, s: &'static str) -> Self::Output<()> {
        IrParserSyntax(Box::new(move |input| input.strip_prefix(s).map(|rest| ((), rest))))
    }

    fn many<A: 'static>(&self, item: Self::Output<A>) -> Self::Output<Vec<A>> {
        IrParserSyntax(Box::new(move |mut input| {
            let mut results = Vec::new();
            while let Some((x, rest)) = (item.0)(input) {
                results.push(x);
                input = rest;
            }
            Some((results, input))
        }))
    }

    fn sep_by<A: 'static>(&self, item: Self::Output<A>, sep: &'static str) -> Self::Output<Vec<A>> {
        IrParserSyntax(Box::new(move |input| {
            let Some((first, mut rest)) = (item.0)(input) else {
                return Some((vec![], input));
            };
            let mut results = vec![first];
            while let Some(after_sep) = rest.strip_prefix(sep) {
                if let Some((x, r)) = (item.0)(after_sep) {
                    results.push(x);
                    rest = r;
                } else {
                    break;
                }
            }
            Some((results, rest))
        }))
    }

    fn whitespace(&self) -> Self::Output<()> {
        IrParserSyntax(Box::new(|input| {
            let trimmed = input.trim_start_matches([' ', '\t']);
            if trimmed.len() < input.len() {
                Some(((), trimmed))
            } else {
                None
            }
        }))
    }

    fn opt_whitespace(&self) -> Self::Output<()> {
        IrParserSyntax(Box::new(|input| Some(((), input.trim_start_matches([' ', '\t'])))))
    }

    fn newline(&self) -> Self::Output<()> {
        IrParserSyntax(Box::new(|input| input.strip_prefix('\n').or_else(|| input.strip_prefix("\r\n")).map(|r| ((), r))))
    }

    fn uint(&self) -> Self::Output<usize> {
        IrParserSyntax(Box::new(|input| {
            let end = input.find(|c: char| !c.is_ascii_digit()).unwrap_or(input.len());
            if end == 0 {
                return None;
            }
            let n = input[..end].parse().ok()?;
            Some((n, &input[end..]))
        }))
    }

    fn token(&self) -> Self::Output<char> {
        IrParserSyntax(Box::new(|input| {
            let mut chars = input.chars();
            let c = chars.next()?;
            Some((c, chars.as_str()))
        }))
    }

    fn keyword(&self, s: &'static str) -> Self::Output<()> {
        self.literal_str(s)
    }
}

// Isos for IR types
//
// These isomorphisms are bijections between algebraic data types and their "flat" representations. Note that these don't go from String to IrArena
mod isos {
    use indexmap::IndexMap;

    use super::*;
    use crate::hfs::{BlockId, InstId, Instruction, IrFuncId, IrOperation, Literal, SourceInfo, TerminatorInst, TypeId};

    pub fn literal_integer() -> Iso<i32, Literal> {
        Iso::new(|n| Some(Literal::Integer(n)), |lit| if let Literal::Integer(n) = lit { Some(n) } else { None })
    }
    pub fn literal_float() -> Iso<f32, Literal> {
        Iso::new(|f| Some(Literal::Float(f)), |lit| if let Literal::Float(f) = lit { Some(f) } else { None })
    }
    pub fn literal_bool() -> Iso<bool, Literal> {
        Iso::new(|b| Some(Literal::Bool(b)), |lit| if let Literal::Bool(b) = lit { Some(b) } else { None })
    }
    pub fn literal_string() -> Iso<String, Literal> {
        Iso::new(|s| Some(Literal::String(s)), |lit| if let Literal::String(s) = lit { Some(s) } else { None })
    }

    pub fn op_add() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Add(a, b)),
            |op| {
                if let IrOperation::Add(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_sub() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Sub(a, b)),
            |op| {
                if let IrOperation::Sub(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_mul() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Mul(a, b)),
            |op| {
                if let IrOperation::Mul(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_div() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Div(a, b)),
            |op| {
                if let IrOperation::Div(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_mod() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Mod(a, b)),
            |op| {
                if let IrOperation::Mod(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_eq() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Equal(a, b)),
            |op| {
                if let IrOperation::Equal(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_neq() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::NotEqual(a, b)),
            |op| {
                if let IrOperation::NotEqual(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_gt() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Greater(a, b)),
            |op| {
                if let IrOperation::Greater(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_gte() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::GreaterEqual(a, b)),
            |op| {
                if let IrOperation::GreaterEqual(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_lt() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Less(a, b)),
            |op| {
                if let IrOperation::Less(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_lte() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::LessEqual(a, b)),
            |op| {
                if let IrOperation::LessEqual(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_and() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::And(a, b)),
            |op| {
                if let IrOperation::And(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_or() -> Iso<(InstId, InstId), IrOperation> {
        Iso::new(
            |(a, b)| Some(IrOperation::Or(a, b)),
            |op| {
                if let IrOperation::Or(a, b) = op {
                    Some((a, b))
                } else {
                    None
                }
            },
        )
    }

    pub fn op_not() -> Iso<InstId, IrOperation> {
        Iso::new(|a| Some(IrOperation::Not(a)), |op| if let IrOperation::Not(a) = op { Some(a) } else { None })
    }

    pub fn inst_id(names: &'static NameMap) -> Iso<String, InstId> {
        let name_to_inst = names.name_to_inst.clone();
        let inst_to_name = names.inst_to_name.clone();
        Iso::new(move |name| name_to_inst.get(&name).copied(), move |id| inst_to_name.get(&id).cloned())
    }

    pub fn block_id(names: &NameMap) -> Iso<String, BlockId> {
        let name_to_block = names.name_to_block.clone();
        let block_to_name = names.block_to_name.clone();
        Iso::new(move |name| name_to_block.get(&name).copied(), move |id| block_to_name.get(&id).cloned())
    }

    pub fn term_jump() -> Iso<BlockId, TerminatorInst> {
        use crate::hfs::SourceInfo;
        Iso::new(
            |b| Some(TerminatorInst::Jump { source_info: SourceInfo::default(), target: b }),
            |term| {
                if let TerminatorInst::Jump { source_info: _, target: b } = term {
                    Some(b)
                } else {
                    None
                }
            },
        )
    }

    pub fn term_return() -> Iso<InstId, TerminatorInst> {
        use crate::hfs::SourceInfo;
        Iso::new(
            |id| Some(TerminatorInst::Return { source_info: SourceInfo::default(), return_tuple: id }),
            |term| {
                if let TerminatorInst::Return { source_info: _, return_tuple: id } = term {
                    Some(id)
                } else {
                    None
                }
            },
        )
    }
    pub fn term_branch() -> Iso<(InstId, BlockId, BlockId), TerminatorInst> {
        use crate::hfs::SourceInfo;
        Iso::new(
            |(cond, t, f)| {
                Some(TerminatorInst::Branch { source_info: SourceInfo::default(), cond, true_block: t, false_block: f })
            },
            |term| {
                if let TerminatorInst::Branch { cond, true_block, false_block, .. } = term {
                    Some((cond, true_block, false_block))
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_load() -> Iso<InstId, Instruction> {
        Iso::new(
            |address| Some(Instruction::Load { source_info: SourceInfo::default(), address, type_id: TypeId::default() }),
            |inst| {
                if let Instruction::Load { address, .. } = inst {
                    Some(address)
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_store() -> Iso<(InstId, InstId), Instruction> {
        Iso::new(
            |(value, address)| Some(Instruction::Store { source_info: SourceInfo::default(), address, value }),
            |inst| {
                if let Instruction::Store { address, value, .. } = inst {
                    Some((value, address))
                } else {
                    None
                }
            },
        )
    }
    pub fn inst_alloca() -> Iso<TypeId, Instruction> {
        Iso::new(
            |type_id| Some(Instruction::Alloca { source_info: SourceInfo::default(), type_id }),
            |inst| {
                if let Instruction::Alloca { type_id, .. } = inst {
                    Some(type_id)
                } else {
                    None
                }
            },
        )
    }
    pub fn inst_parameter() -> Iso<(usize, TypeId), Instruction> {
        Iso::new(
            |(index, type_id)| Some(Instruction::Parameter { source_info: SourceInfo::default(), index, type_id }),
            |inst| {
                if let Instruction::Parameter { index, type_id, .. } = inst {
                    Some((index, type_id))
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_return_value() -> Iso<TypeId, Instruction> {
        Iso::new(
            |type_id| Some(Instruction::ReturnValue { source_info: SourceInfo::default(), type_id }),
            |inst| {
                if let Instruction::ReturnValue { type_id, .. } = inst {
                    Some(type_id)
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_function_call() -> Iso<(IrFuncId, Vec<InstId>), Instruction> {
        Iso::new(
            |(func_id, args)| {
                Some(Instruction::FunctionCall {
                    source_info: SourceInfo::default(),
                    func_id,
                    args,
                    // NOTE: ignoring is_move and return_values should be fine as long as we don't test the stack directly at the IR level (if we do this we are stupid)
                    is_move: false,
                    return_values: vec![],
                })
            },
            |inst| {
                if let Instruction::FunctionCall { func_id, args, .. } = inst {
                    Some((func_id, args))
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_phi() -> Iso<Vec<(BlockId, InstId)>, Instruction> {
        Iso::new(
            |pairs: Vec<(BlockId, InstId)>| {
                Some(Instruction::Phi {
                    source_info: SourceInfo::default(),
                    incoming: pairs.into_iter().collect::<IndexMap<_, _>>(),
                })
            },
            |inst| {
                if let Instruction::Phi { incoming, .. } = inst {
                    Some(incoming.into_iter().collect())
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_tuple() -> Iso<Vec<InstId>, Instruction> {
        Iso::new(
            |instructions| Some(Instruction::Tuple { source_info: SourceInfo::default(), instructions }),
            |inst| {
                if let Instruction::Tuple { instructions, .. } = inst {
                    Some(instructions)
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_operation() -> Iso<IrOperation, Instruction> {
        Iso::new(
            |op| Some(Instruction::Operation { source_info: SourceInfo::default(), op }),
            |inst| {
                if let Instruction::Operation { op, .. } = inst {
                    Some(op)
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_literal() -> Iso<Literal, Instruction> {
        Iso::new(
            |literal| Some(Instruction::Literal { source_info: SourceInfo::default(), literal }),
            |inst| {
                if let Instruction::Literal { literal, .. } = inst {
                    Some(literal)
                } else {
                    None
                }
            },
        )
    }
    pub fn inst_load_element() -> Iso<(usize, InstId), Instruction> {
        Iso::new(
            |(index, tuple)| Some(Instruction::LoadElement { source_info: SourceInfo::default(), index, tuple }),
            |inst| {
                if let Instruction::LoadElement { index, tuple, .. } = inst {
                    Some((index, tuple))
                } else {
                    None
                }
            },
        )
    }

    pub fn type_id(names: &NameMap) -> Iso<String, TypeId> {
        let name_to_type = names.name_to_type.clone();
        let type_to_name = names.type_to_name.clone();
        Iso::new(move |name| name_to_type.get(&name).copied(), move |id| type_to_name.get(&id).cloned())
    }
}

// Syntax descriptors
//
// This is the part that bridges text and AST, in an abstract way that doesn't require knowledge about the printer or parser, with help of the Isos
