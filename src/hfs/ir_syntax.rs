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
// Text <-> Flat representation <-> ADT <-> AST
//
// Using those Isos and the Syntax trait, we define syntax descriptors, which define the specifics
// of both the textual representation and the AST at the same time, by being generic over a Syntax.
// Note that we never specify if we are applying or unapplying the Isos in the descriptors, that is
// handled by the Syntax
//
// Additionally, since this IR can have forward references to registers and blocks, we need a pass
// that collects names

use std::collections::HashMap;

use crate::hfs::{
    BlockId, InstId, Instruction, IrArena, IrFuncId, IrFunction, Literal, SourceInfo, TerminatorInst, Type, TypeId,
};

#[derive(Default)]
pub struct NameMap {
    pub inst_to_name: HashMap<InstId, usize>,
    pub name_to_inst: HashMap<usize, InstId>,
    pub block_to_name: HashMap<BlockId, String>,
    pub name_to_block: HashMap<String, BlockId>,
    pub type_to_name: HashMap<TypeId, String>,
    pub name_to_type: HashMap<String, TypeId>,
    pub func_to_name: HashMap<IrFuncId, String>,
    pub name_to_func: HashMap<String, IrFuncId>,
    pub unmangled_to_block: HashMap<String, BlockId>,
}

// We need a raw block and raw function using instructions instead of IDs to abstract the top-level descriptors from the arena building
#[derive(Debug)]
struct RawBlock {
    id: BlockId,
    insts: Vec<RawInst>,
    term: TerminatorInst,
}

#[derive(Debug, Clone)]
enum RawInst {
    Named(InstId, TypeId, Instruction),
    Unnamed(Instruction),
}

struct RawFunction {
    name: String,
    param_type: TypeId,
    return_type: TypeId,
    blocks: Vec<RawBlock>,
}

// Partial isomorphisms (Iso)
// Represents a partial invertible function, that is:
// - a function that has two directions that are inverses of each other
// - either direction can fail, returning None
// - a round-trip `unapply(apply(x))` will return x iff neither direction fails
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

    fn sep_by<A>(&self, item: Self::Output<A>, sep: Self::Output<()>) -> Self::Output<Vec<A>>;

    fn whitespace(&self) -> Self::Output<()>;

    fn opt_whitespace(&self) -> Self::Output<()>;

    fn newline(&self) -> Self::Output<()>;

    fn many_newlines(&self) -> Self::Output<()> {
        self.iso(Iso::new(|_: Vec<()>| Some(()), |()| Some(vec![()])), self.many(self.newline()))
    }

    fn uint(&self) -> Self::Output<usize>;

    fn integer(&self) -> Self::Output<i32>;

    fn float(&self) -> Self::Output<f32>;

    fn string(&self) -> Self::Output<String>;

    // Helpers to make code nicer (not actually necessary)
    fn constant<A: Clone + PartialEq + 'static>(&self, keyword: &'static str, value: A) -> Self::Output<A> {
        let v1 = value.clone();
        let v2 = value;
        self.iso(Iso::new(move |_| Some(v1.clone()), move |v| if v == v2 { Some(()) } else { None }), self.literal_str(keyword))
    }

    fn choice<A: Clone + 'static>(&self, items: Vec<Self::Output<A>>) -> Self::Output<A> {
        items.into_iter().reduce(|a, b| self.alt(a, b)).unwrap_or_else(|| self.empty())
    }

    fn ignore_left<A: 'static>(&self, left: Self::Output<()>, right: Self::Output<A>) -> Self::Output<A> {
        self.iso(Iso::new(|(_, b)| Some(b), |b| Some(((), b))), self.product(left, right))
    }

    fn ignore_right<A: 'static>(&self, left: Self::Output<A>, right: Self::Output<()>) -> Self::Output<A> {
        self.iso(Iso::new(|(a, _)| Some(a), |a| Some((a, ()))), self.product(left, right))
    }

    fn lexeme<A: 'static>(&self, p: Self::Output<A>) -> Self::Output<A> { self.ignore_left(self.whitespace(), p) }

    fn inst_name(&self, names: &NameMap) -> Self::Output<InstId> {
        self.iso(iso::inst_id(names), self.ignore_left(self.literal_str("%"), self.uint()))
    }

    fn identifier(&self) -> Self::Output<String>;

    fn type_name(&self, names: &NameMap) -> Self::Output<TypeId> { self.iso(iso::type_id(names), self.identifier()) }

    fn func_name(&self, names: &NameMap) -> Self::Output<IrFuncId> { self.iso(iso::func_id(names), self.identifier()) }

    fn block_name(&self, names: &NameMap) -> Self::Output<BlockId> { self.iso(iso::block_id(names), self.identifier()) }

    fn symbol(&self, s: &'static str) -> Self::Output<()> {
        self.ignore_left(self.opt_whitespace(), self.ignore_left(self.literal_str(s), self.opt_whitespace()))
    }

    fn between<A: 'static>(&self, (left, right): (&'static str, &'static str), inner: Self::Output<A>) -> Self::Output<A> {
        self.ignore_left(self.symbol(left), self.ignore_right(inner, self.literal_str(right)))
    }

    fn product3<A: 'static, B: 'static, C: 'static>(
        &self,
        a: Self::Output<A>,
        b: Self::Output<B>,
        c: Self::Output<C>,
    ) -> Self::Output<(A, B, C)> {
        self.iso(Iso::new(|((a, b), c)| Some((a, b, c)), |(a, b, c)| Some(((a, b), c))), self.product(self.product(a, b), c))
    }

    fn operator(&self, s: &'static str) -> Self::Output<()> {
        self.ignore_left(self.whitespace(), self.ignore_right(self.literal_str(s), self.whitespace()))
    }

    fn indent(&self) -> Self::Output<()> { self.opt_whitespace() }
    fn double_indent(&self) -> Self::Output<()> { self.opt_whitespace() }
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

    fn token(&self) -> PrinterSyntax<char> { PrinterSyntax(Box::new(|c| Some(c.to_string()))) }

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

    fn empty<A>(&self) -> Self::Output<A> { PrinterSyntax(Box::new(|_| None)) }

    fn keyword(&self, s: &'static str) -> PrinterSyntax<()> { PrinterSyntax(Box::new(move |()| Some(format!("{} ", s)))) }

    fn literal_str(&self, s: &'static str) -> PrinterSyntax<()> { PrinterSyntax(Box::new(move |()| Some(s.to_string()))) }

    fn many<A>(&self, item: Self::Output<A>) -> Self::Output<Vec<A>> {
        PrinterSyntax(Box::new(move |xs: Vec<A>| {
            let mut out = String::new();
            for x in xs {
                out += &(item.0)(x)?;
            }
            Some(out)
        }))
    }

    fn sep_by<A: 'static>(&self, item: PrinterSyntax<A>, sep: PrinterSyntax<()>) -> PrinterSyntax<Vec<A>> {
        PrinterSyntax(Box::new(move |xs: Vec<A>| {
            let mut out = String::new();

            for (i, x) in xs.into_iter().enumerate() {
                if i > 0 {
                    out.push_str(&(sep.0)(())?);
                }
                out.push_str(&(item.0)(x)?);
            }

            Some(out)
        }))
    }

    fn whitespace(&self) -> PrinterSyntax<()> { PrinterSyntax(Box::new(|()| Some(" ".into()))) }
    fn opt_whitespace(&self) -> PrinterSyntax<()> { PrinterSyntax(Box::new(|()| Some("".into()))) }
    fn newline(&self) -> PrinterSyntax<()> { PrinterSyntax(Box::new(|()| Some("\n".into()))) }

    fn uint(&self) -> PrinterSyntax<usize> { PrinterSyntax(Box::new(|n| Some(n.to_string()))) }
    fn integer(&self) -> PrinterSyntax<i32> { PrinterSyntax(Box::new(|n| Some(n.to_string()))) }

    fn float(&self) -> PrinterSyntax<f32> { PrinterSyntax(Box::new(|n| Some(n.to_string()))) }

    fn string(&self) -> PrinterSyntax<String> {
        PrinterSyntax(Box::new(|s| {
            let escaped =
                s.replace('\\', "\\\\").replace('"', "\\\"").replace('\n', "\\n").replace('\t', "\\t").replace('\r', "\\r");
            Some(format!("\"{}\"", escaped))
        }))
    }

    fn identifier(&self) -> PrinterSyntax<String> { PrinterSyntax(Box::new(|s| Some(s))) }

    fn indent(&self) -> PrinterSyntax<()> { PrinterSyntax(Box::new(|()| Some("\t".into()))) }
    fn double_indent(&self) -> PrinterSyntax<()> { PrinterSyntax(Box::new(|()| Some("\t\t".into()))) }
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
        IrParserSyntax(Box::new(move |input| {
            let snapshot = input;

            if let Some(res) = (left.0)(input) {
                return Some(res);
            }

            (right.0)(snapshot)
        }))
    }

    fn empty<A: 'static>(&self) -> Self::Output<A> { IrParserSyntax(Box::new(|_| None)) }

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

    fn sep_by<A: 'static>(&self, item: Self::Output<A>, sep: Self::Output<()>) -> Self::Output<Vec<A>> {
        IrParserSyntax(Box::new(move |input| {
            let Some((first, mut rest)) = (item.0)(input) else {
                return Some((vec![], input));
            };

            let mut results = vec![first];

            loop {
                let before = rest;

                let Some((_, after_sep)) = (sep.0)(rest) else {
                    break;
                };

                if let Some((x, new_rest)) = (item.0)(after_sep) {
                    results.push(x);
                    rest = new_rest;
                } else {
                    break;
                }

                if rest == before {
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

    fn token(&self) -> Self::Output<char> {
        IrParserSyntax(Box::new(|input| {
            let mut chars = input.chars();
            let c = chars.next()?;
            Some((c, chars.as_str()))
        }))
    }

    fn keyword(&self, s: &'static str) -> IrParserSyntax<()> {
        self.ignore_left(self.opt_whitespace(), self.ignore_right(self.literal_str(s), self.opt_whitespace()))
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
    fn integer(&self) -> Self::Output<i32> {
        IrParserSyntax(Box::new(|input| {
            let start = if input.starts_with('-') { 1 } else { 0 };
            let end = input[start..].find(|c: char| !c.is_ascii_digit()).unwrap_or(input[start..].len()) + start;
            if end == start {
                return None;
            }
            let n = input[..end].parse().ok()?;
            Some((n, &input[end..]))
        }))
    }

    fn float(&self) -> Self::Output<f32> {
        IrParserSyntax(Box::new(|input| {
            let mut i = 0;
            if input.starts_with('-') {
                i += 1;
            }

            // parse integer part
            let start_digits = i;
            while let Some(c) = input[i..].chars().next() {
                if c.is_ascii_digit() {
                    i += c.len_utf8();
                } else {
                    break;
                }
            }

            if i == start_digits {
                return None;
            }

            if !input[i..].starts_with('.') {
                return None;
            }
            i += 1;

            let frac_start = i;
            while let Some(c) = input[i..].chars().next() {
                if c.is_ascii_digit() {
                    i += c.len_utf8();
                } else {
                    break;
                }
            }

            if i == frac_start {
                return None;
            }

            let n: f32 = input[..i].parse().ok()?;
            Some((n, &input[i..]))
        }))
    }

    fn string(&self) -> Self::Output<String> {
        IrParserSyntax(Box::new(|input| {
            let input = input.strip_prefix('"')?;
            let mut result = String::new();
            let mut chars = input.char_indices();
            loop {
                let (i, c) = chars.next()?;
                match c {
                    '"' => return Some((result, &input[i + 1..])),
                    '\\' => match chars.next()?.1 {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        _ => return None,
                    },
                    c => result.push(c),
                }
            }
        }))
    }

    fn identifier(&self) -> Self::Output<String> {
        IrParserSyntax(Box::new(|input| {
            let end = input
                .char_indices()
                .take_while(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
                .map(|(i, _)| i)
                .last()
                .map(|i| i + 1)
                .unwrap_or(0);
            if end == 0 {
                return None;
            }
            Some((input[..end].to_string(), &input[end..]))
        }))
    }
}

// Isos for IR types
//
// These isomorphisms are bijections between algebraic data types and their "flat" representations. Note that these don't go from String to IrArena
mod iso {

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

    pub fn inst_id(names: &NameMap) -> Iso<usize, InstId> {
        let name_to_inst = names.name_to_inst.clone();
        let inst_to_name = names.inst_to_name.clone();
        Iso::new(move |name| name_to_inst.get(&name).copied(), move |id| inst_to_name.get(&id).cloned())
    }

    pub fn block_id(names: &NameMap) -> Iso<String, BlockId> {
        let unmangled_to_block = names.unmangled_to_block.clone();
        let block_to_name = names.block_to_name.clone();
        Iso::new(move |name| unmangled_to_block.get(&name).copied(), move |id| block_to_name.get(&id).cloned())
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
    pub fn inst_parameter() -> Iso<(TypeId, usize), Instruction> {
        Iso::new(
            |(type_id, index)| Some(Instruction::Parameter { source_info: SourceInfo::default(), index, type_id }),
            |inst| {
                if let Instruction::Parameter { index, type_id, .. } = inst {
                    Some((type_id, index))
                } else {
                    None
                }
            },
        )
    }

    pub fn inst_retval() -> Iso<TypeId, Instruction> {
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

    pub fn inst_fncall() -> Iso<(IrFuncId, Vec<InstId>), Instruction> {
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

    pub fn func_id(names: &NameMap) -> Iso<String, IrFuncId> {
        let name_to_func = names.name_to_func.clone();
        let func_to_name = names.func_to_name.clone();

        Iso::new(move |name| name_to_func.get(&name).copied(), move |id| func_to_name.get(&id).cloned())
    }
}

// Syntax descriptions
//
// This is the part that bridges text and AST, in an abstract way that doesn't require knowledge about the printer or parser, with help of the Isos
fn syntax_literal<S: Syntax>(s: &S) -> S::Output<Instruction> {
    let integer = s.iso(iso::literal_integer(), s.integer());
    let float = s.iso(iso::literal_float(), s.float());
    let string = s.iso(iso::literal_string(), s.string());
    let bool = s.iso(iso::literal_bool(), s.alt(s.constant("true", true), s.constant("false", false)));

    s.iso(iso::inst_literal(), s.choice(vec![float, integer, string, bool]))
}

fn syntax_load<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(iso::inst_load(), s.ignore_left(s.keyword("load"), s.inst_name(names)))
}

fn syntax_store<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(
        iso::inst_store(),
        s.ignore_left(s.keyword("store"), s.product(s.inst_name(names), s.ignore_left(s.symbol(","), s.inst_name(names)))),
    )
}

fn syntax_parameter<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(
        iso::inst_parameter(),
        s.product(s.ignore_right(s.type_name(names), s.whitespace()), s.ignore_left(s.literal_str("%arg"), s.uint())),
    )
}

fn syntax_alloca<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(iso::inst_alloca(), s.ignore_left(s.keyword("alloca"), s.type_name(names)))
}
fn syntax_retval<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(iso::inst_retval(), s.ignore_left(s.keyword("retval"), s.type_name(names)))
}
fn syntax_fncall<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(
        iso::inst_fncall(),
        s.ignore_left(
            s.keyword("call"),
            s.product(s.func_name(names), s.between(("(", ")"), s.sep_by(s.inst_name(names), s.symbol(",")))),
        ),
    )
}
fn syntax_phi<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(
        iso::inst_phi(),
        s.ignore_left(
            s.keyword("phi"),
            s.sep_by(
                s.between(
                    ("[", "]"),
                    s.product(
                        s.block_name(names),
                        s.ignore_left(s.ignore_right(s.literal_str(":"), s.whitespace()), s.inst_name(names)),
                    ),
                ),
                s.symbol(","),
            ),
        ),
    )
}
fn syntax_tuple<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(iso::inst_tuple(), s.between(("(", ")"), s.sep_by(s.inst_name(names), s.symbol(","))))
}
fn syntax_binop<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    let op = s.choice(vec![
        s.iso(iso::op_add(), s.product(s.inst_name(names), s.ignore_left(s.operator("+"), s.inst_name(names)))),
        s.iso(iso::op_sub(), s.product(s.inst_name(names), s.ignore_left(s.operator("-"), s.inst_name(names)))),
        s.iso(iso::op_mul(), s.product(s.inst_name(names), s.ignore_left(s.operator("*"), s.inst_name(names)))),
        s.iso(iso::op_div(), s.product(s.inst_name(names), s.ignore_left(s.operator("/"), s.inst_name(names)))),
        s.iso(iso::op_mod(), s.product(s.inst_name(names), s.ignore_left(s.operator("%"), s.inst_name(names)))),
        s.iso(iso::op_eq(), s.product(s.inst_name(names), s.ignore_left(s.operator("=="), s.inst_name(names)))),
        s.iso(iso::op_neq(), s.product(s.inst_name(names), s.ignore_left(s.operator("!="), s.inst_name(names)))),
        s.iso(iso::op_gt(), s.product(s.inst_name(names), s.ignore_left(s.operator(">"), s.inst_name(names)))),
        s.iso(iso::op_gte(), s.product(s.inst_name(names), s.ignore_left(s.operator(">="), s.inst_name(names)))),
        s.iso(iso::op_lt(), s.product(s.inst_name(names), s.ignore_left(s.operator("<"), s.inst_name(names)))),
        s.iso(iso::op_lte(), s.product(s.inst_name(names), s.ignore_left(s.operator("<="), s.inst_name(names)))),
        s.iso(iso::op_or(), s.product(s.inst_name(names), s.ignore_left(s.operator("||"), s.inst_name(names)))),
        s.iso(iso::op_and(), s.product(s.inst_name(names), s.ignore_left(s.operator("&&"), s.inst_name(names)))),
    ]);
    s.iso(iso::inst_operation(), op)
}

fn syntax_unop<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    let op = s.iso(iso::op_not(), s.ignore_left(s.symbol("!"), s.inst_name(names)));
    s.iso(iso::inst_operation(), op)
}
fn syntax_load_element<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.iso(
        iso::inst_load_element(),
        s.ignore_left(s.keyword("load_element"), s.product(s.ignore_right(s.uint(), s.whitespace()), s.inst_name(names))),
    )
}
fn syntax_inst<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Instruction> {
    s.choice(vec![
        syntax_load_element(s, names),
        syntax_load(s, names),
        syntax_store(s, names),
        syntax_alloca(s, names),
        syntax_binop(s, names),
        syntax_unop(s, names),
        syntax_parameter(s, names),
        syntax_retval(s, names),
        syntax_fncall(s, names),
        syntax_phi(s, names),
        syntax_tuple(s, names),
        syntax_literal(s),
    ])
}

pub(crate) fn syntax_named_inst<S: Syntax>(s: &S, names: &NameMap) -> S::Output<(InstId, TypeId, Instruction)> {
    s.product3(
        s.ignore_right(s.inst_name(names), s.operator("=")),
        s.ignore_right(s.type_name(names), s.whitespace()),
        syntax_inst(s, names),
    )
}

fn syntax_return<S: Syntax>(s: &S, names: &NameMap) -> S::Output<TerminatorInst> {
    s.iso(iso::term_return(), s.ignore_left(s.keyword("return"), s.inst_name(names)))
}

fn syntax_branch<S: Syntax>(s: &S, names: &NameMap) -> S::Output<TerminatorInst> {
    s.iso(
        iso::term_branch(),
        s.ignore_left(
            s.keyword("branch"),
            s.product3(
                s.ignore_right(s.inst_name(names), s.whitespace()),
                s.ignore_left(s.symbol(","), s.block_name(names)),
                s.ignore_left(s.whitespace(), s.block_name(names)),
            ),
        ),
    )
}

fn syntax_jump<S: Syntax>(s: &S, names: &NameMap) -> S::Output<TerminatorInst> {
    s.iso(iso::term_jump(), s.ignore_left(s.keyword("jump"), s.block_name(names)))
}
pub(crate) fn syntax_term<S: Syntax>(s: &S, names: &NameMap) -> S::Output<TerminatorInst> {
    s.choice(vec![syntax_return(s, names), syntax_branch(s, names), syntax_jump(s, names)])
}

fn syntax_raw_inst<S: Syntax>(s: &S, names: &NameMap) -> S::Output<RawInst> {
    let named = s.iso(
        Iso::new(
            |(id, type_id, inst)| Some(RawInst::Named(id, type_id, inst)),
            |ri| {
                if let RawInst::Named(id, type_id, inst) = ri {
                    Some((id, type_id, inst))
                } else {
                    None
                }
            },
        ),
        syntax_named_inst(s, names),
    );
    let unnamed = s.iso(
        Iso::new(
            |inst| Some(RawInst::Unnamed(inst)),
            |ri| {
                if let RawInst::Unnamed(inst) = ri {
                    Some(inst)
                } else {
                    None
                }
            },
        ),
        syntax_store(s, names),
    );
    s.alt(named, unnamed)
}

fn syntax_raw_block<S: Syntax>(s: &S, names: &NameMap) -> S::Output<RawBlock> {
    let iso = Iso::new(|(id, insts, term)| Some(RawBlock { id, insts, term }), |rb| Some((rb.id, rb.insts, rb.term)));
    s.iso(
        iso,
        // s.product3(
        //     s.ignore_right(s.ignore_left(s.indent(), s.block_name(names)), s.ignore_right(s.symbol(":"), s.newline())),
        //     s.sep_by(
        //         s.ignore_right(s.ignore_left(s.double_indent(), syntax_named_inst(s, names)), s.newline()),
        //         s.opt_whitespace(),
        //     ),
        //     s.ignore_right(s.ignore_left(s.double_indent(), syntax_term(s, names)), s.newline()),
        // ),
        s.product3(
            s.ignore_right(s.block_name(names), s.ignore_right(s.literal_str(":"), s.newline())),
            s.sep_by(s.ignore_right(syntax_raw_inst(s, names), s.newline()), s.opt_whitespace()),
            s.ignore_right(syntax_term(s, names), s.newline()),
        ),
    )
}

fn syntax_raw_function<S: Syntax>(s: &S, names: &NameMap) -> S::Output<RawFunction> {
    let iso = Iso::new(
        |((name, param_type, return_type), blocks)| Some(RawFunction { name, param_type, return_type, blocks }),
        |rf| Some(((rf.name, rf.param_type, rf.return_type), rf.blocks)),
    );
    s.iso(
        iso,
        s.product(
            s.ignore_left(
                s.keyword("fn"),
                s.product3(
                    s.identifier(),
                    s.ignore_left(s.symbol(":"), s.type_name(names)),
                    s.ignore_left(s.operator("->"), s.ignore_right(s.type_name(names), s.whitespace())),
                ),
            ),
            s.between(
                ("{", "}"),
                s.ignore_left(s.newline(), s.ignore_right(s.many(syntax_raw_block(s, names)), s.opt_whitespace())),
            ),
        ),
    )
}

fn syntax_top_level<S: Syntax>(s: &S, names: &NameMap) -> S::Output<Vec<RawFunction>> {
    s.ignore_left(s.opt_whitespace(), s.sep_by(syntax_raw_function(s, names), s.ignore_left(s.newline(), s.opt_whitespace())))
}

// Name collection pass
//
// Simple scan over the text for functions, blocks and instructions
fn collect_names(input: &str, arena: &mut IrArena, names: &mut NameMap) {
    let placeholder_inst = || Instruction::Literal { source_info: SourceInfo::default(), literal: Literal::Integer(0) };
    let placeholder_type = TypeId(0);

    let mut current_func: Option<IrFuncId> = None;

    for line in input.lines() {
        let line = line.trim();

        if line.is_empty() {
            continue;
        }

        // collect func_ids
        if let Some(rest) = line.strip_prefix("fn ") {
            let name = rest.split_whitespace().next().unwrap_or("").trim_end_matches(':').to_string();
            let func_id = arena.alloc_function(IrFunction {
                source_info: SourceInfo::default(),
                name: name.clone(),
                param_type: placeholder_type,
                return_type: placeholder_type,
                parameter_insts: vec![],
                entry_block: BlockId::default(),
            });
            names.func_to_name.insert(func_id, name.clone());
            names.name_to_func.insert(name, func_id);
            current_func = Some(func_id);
            continue;
        }

        // collect block_ids
        if let Some(name) = line.strip_suffix(':') {
            let name = name.trim().to_string();
            if let Some(func_id) = current_func {
                let block_id = arena.alloc_block(&name, func_id);
                let actual_name = arena.get_block(block_id).name.clone();
                names.block_to_name.insert(block_id, actual_name.clone());
                names.name_to_block.insert(actual_name, block_id);
                names.unmangled_to_block.insert(name, block_id);
            }
            continue;
        }

        // collect inst_ids
        if let Some(rest) = line.strip_prefix('%') {
            if let Some(num_str) = rest.split(|c: char| !c.is_ascii_digit()).next() {
                if let Ok(num) = num_str.parse::<usize>() {
                    let inst_id = arena.instructions.insert(placeholder_inst());
                    names.inst_to_name.insert(inst_id, num);
                    names.name_to_inst.insert(num, inst_id);
                }
            }
            continue;
        }

        // collect type_ids
        for (i, typ) in arena.types.iter().enumerate() {
            let type_id = TypeId(i);
            if names.name_to_type.contains_key(&names.type_to_name.get(&type_id).cloned().unwrap_or_default()) {
                continue;
            }
            if let Type::Tuple { type_ids, .. } = typ {
                let inner: Vec<String> = type_ids
                    .iter()
                    .map(|id| names.type_to_name.get(id).cloned().unwrap_or_else(|| format!("t{}", id.0)))
                    .collect();
                let name = format!("({})", inner.join(" "));
                names.type_to_name.insert(type_id, name.clone());
                names.name_to_type.insert(name, type_id);
            }
        }
    }
}

fn collect_parameter_insts(func_id: IrFuncId, arena: &mut IrArena) {
    let entry_block = arena.get_func(func_id).entry_block;
    let insts = arena.get_block(entry_block).instructions.clone();
    let params: Vec<InstId> =
        insts.iter().filter(|&&id| matches!(arena.get_inst(id), Instruction::Parameter { .. })).copied().collect();
    arena.get_func_mut(func_id).parameter_insts = params;
}

fn parse_function<'a>(input: &'a str, names: &NameMap, arena: &mut IrArena) -> Option<(&'a str, IrFuncId)> {
    let parser = IrParser;

    let syntax = syntax_raw_function(&parser, names);

    let result = (syntax.0)(input);

    let (raw, rest) = match result {
        Some(v) => v,
        None => {
            return None;
        },
    };

    let func_id = match names.name_to_func.get(&raw.name) {
        Some(id) => *id,
        None => {
            return None;
        },
    };

    {
        let func = arena.get_func_mut(func_id);
        func.param_type = raw.param_type;
        func.return_type = raw.return_type;
    }

    let mut entry_block = None;

    for raw_block in raw.blocks {
        if entry_block.is_none() {
            entry_block = Some(raw_block.id);
        }

        for raw_inst in raw_block.insts {
            match raw_inst {
                RawInst::Named(_, _, inst) | RawInst::Unnamed(inst) => {
                    arena.alloc_inst_for(inst, raw_block.id);
                },
            }
        }

        arena.alloc_terminator_for(raw_block.term, raw_block.id);
    }

    let entry_block = match entry_block {
        Some(b) => b,
        None => {
            return None;
        },
    };

    arena.get_func_mut(func_id).entry_block = entry_block;

    collect_parameter_insts(func_id, arena);

    Some((rest, func_id))
}

// Top-level parser and printer functions
pub fn parse(input: &str, arena: &mut IrArena) -> Option<Vec<IrFuncId>> {
    let mut names = NameMap {
        inst_to_name: HashMap::new(),
        name_to_inst: HashMap::new(),
        block_to_name: HashMap::new(),
        name_to_block: HashMap::new(),
        type_to_name: HashMap::new(),
        name_to_type: HashMap::new(),
        func_to_name: HashMap::new(),
        name_to_func: HashMap::new(),
        unmangled_to_block: HashMap::new(),
    };

    names.type_to_name.insert(TypeId(0), "i32".to_string());
    names.name_to_type.insert("i32".to_string(), TypeId(0));
    names.type_to_name.insert(TypeId(1), "f32".to_string());
    names.name_to_type.insert("f32".to_string(), TypeId(1));
    names.type_to_name.insert(TypeId(2), "bool".to_string());
    names.name_to_type.insert("bool".to_string(), TypeId(2));
    names.type_to_name.insert(TypeId(3), "str".to_string());
    names.name_to_type.insert("str".to_string(), TypeId(3));

    collect_names(input, arena, &mut names);
    let mut func_ids = vec![];
    let mut rest = input;
    loop {
        rest = rest.trim_start_matches([' ', '\t', '\n', '\r']);
        if rest.is_empty() {
            break;
        }
        let result = parse_function(rest, &names, arena);
        let (remaining, func_id) = result?;
        func_ids.push(func_id);
        rest = remaining;
    }
    Some(func_ids)
}

pub fn print(func_ids: &[IrFuncId], arena: &IrArena) -> Option<String> {
    let mut names = NameMap {
        inst_to_name: HashMap::new(),
        name_to_inst: HashMap::new(),
        block_to_name: HashMap::new(),
        name_to_block: HashMap::new(),
        type_to_name: HashMap::new(),
        name_to_type: HashMap::new(),
        func_to_name: HashMap::new(),
        name_to_func: HashMap::new(),
        unmangled_to_block: HashMap::new(),
    };

    names.type_to_name.insert(TypeId(0), "i32".to_string());
    names.name_to_type.insert("i32".to_string(), TypeId(0));
    names.type_to_name.insert(TypeId(1), "f32".to_string());
    names.name_to_type.insert("f32".to_string(), TypeId(1));
    names.type_to_name.insert(TypeId(2), "bool".to_string());
    names.name_to_type.insert("bool".to_string(), TypeId(2));
    names.type_to_name.insert(TypeId(3), "str".to_string());
    names.name_to_type.insert("str".to_string(), TypeId(3));
    for (i, typ) in arena.types.iter().enumerate() {
        let type_id = TypeId(i);
        if names.type_to_name.contains_key(&type_id) {
            continue;
        }
        let name = match typ {
            Type::Tuple { type_ids, .. } => {
                let inner: Vec<String> = type_ids
                    .iter()
                    .map(|id| names.type_to_name.get(id).cloned().unwrap_or_else(|| format!("{}", id.0)))
                    .collect();
                format!("({})", inner.join(" "))
            },
            _ => continue,
        };
        names.type_to_name.insert(type_id, name.clone());
        names.name_to_type.insert(name, type_id);
    }

    let mut inst_counter = 0usize;
    for func_id in func_ids {
        let func = arena.get_func(*func_id);
        names.func_to_name.insert(*func_id, func.name.clone());
        names.name_to_func.insert(func.name.clone(), *func_id);

        for block_id in arena.get_blocks_in(*func_id) {
            let block = arena.get_block(block_id);
            names.block_to_name.insert(block_id, block.name.clone());
            names.name_to_block.insert(block.name.clone(), block_id);
            names.unmangled_to_block.insert(block.name.clone(), block_id);

            for inst_id in &block.instructions {
                names.inst_to_name.insert(*inst_id, inst_counter);
                names.name_to_inst.insert(inst_counter, *inst_id);
                inst_counter += 1;
            }
        }
    }

    let mut raw_functions = Vec::new();
    for func_id in func_ids {
        let (func_name, param_type, return_type) = {
            let func = arena.get_func(*func_id);
            (func.name.clone(), func.param_type, func.return_type)
        };

        let block_ids = arena.compute_reverse_postorder(*func_id);
        let mut blocks = Vec::new();
        for block_id in &block_ids {
            let inst_ids: Vec<InstId> = arena.get_block(*block_id).instructions.clone();
            let terminator = arena.get_block(*block_id).terminator;

            let mut type_ids = Vec::new();
            for inst_id in &inst_ids {
                type_ids.push(arena.get_type_id_of_inst_no_alloc(*inst_id).unwrap_or_default());
            }

            let mut insts = Vec::new();
            for inst_id in &inst_ids {
                let inst = arena.get_inst(*inst_id).clone();
                let raw = match &inst {
                    Instruction::Store { .. } => RawInst::Unnamed(inst),
                    _ => {
                        let type_id = arena.get_type_id_of_inst_no_alloc(*inst_id).unwrap_or_default();
                        RawInst::Named(*inst_id, type_id, inst)
                    },
                };
                insts.push(raw);
            }

            let term = match terminator {
                Some(id) => arena.get_term(id).clone(),
                None => unimplemented!("TODO: add <no terminator>"),
            };

            blocks.push(RawBlock { id: *block_id, insts, term });
        }

        raw_functions.push(RawFunction { name: func_name, param_type, return_type, blocks });
    }
    let printer = Printer;
    let syntax = syntax_top_level(&printer, &names);

    (syntax.0)(raw_functions)
}
