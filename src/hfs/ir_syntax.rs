// Rendel & Ostermann-style invertible syntax for HFS MIR
//
// We define a set of syntax descriptors that are seen as
// the source of truth, and are simultaneously a printer and a parser
//
// Paper: https://dl.acm.org/doi/10.1145/2088456.1863525

// Partial isomorphisms (Iso)
// Represents a partial invertible function, that is:
// - a function that has two directions that are inverses of each other
// - either direction can fail, returning None
// - a round-trip `unapply(apply(x))` will return x iff neither direction fails

use std::collections::HashMap;

use crate::hfs::{
    BlockId, GlobalIrVarDeclaration, GlobalIrVarId, InstId, Instruction, IrArena, IrFuncId, IrFunction, IrOperation,
    IrTopLevelId, Literal, SourceInfo, TermInstId, TerminatorInst, Type, TypeId, VarId,
};

pub struct NameMap {
    pub inst_to_name: HashMap<InstId, String>,
    pub name_to_inst: HashMap<String, InstId>,
    pub block_to_name: HashMap<BlockId, String>,
    pub name_to_block: HashMap<String, BlockId>,
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
    use crate::hfs::{BlockId, InstId, IrOperation, Literal, TerminatorInst};

    use super::*;

    // Literal ---
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
}
