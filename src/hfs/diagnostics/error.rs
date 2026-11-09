use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    fs,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::Span;

pub struct Label {
    pub span: Span,
    pub message: Option<String>,
}

pub struct Snippet {
    pub path: PathBuf,
    pub labels: Vec<Label>,
}

pub struct Hint {
    pub message: String,
    pub snippets: Vec<Snippet>,
}

#[macro_export]
macro_rules! impl_display_for_error {
    ($ty:ty) => {
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $crate::hfs::error::CompileError::render(self))
            }
        }
    };
}

pub trait CompileError: Display + Debug {
    fn get_debug_info(&self) -> DebugInfo;
    fn message(&self) -> String;
    fn snippets(&self) -> Vec<Snippet>;
    fn render(&self) -> ColoredString {
        let mut out = format!("{}", self.header());
        for s in self.snippets() {
            out += &render_snippet(&s, true);
        }
        for h in self.hints() {
            out += &format!("\n{} {}", "hint:".cyan().bold(), h.message);
            for s in &h.snippets {
                out += &render_snippet(s, false);
            }
        }
        out += &self.debug_info().to_string();
        ColoredString::from(out)
    }
    fn hints(&self) -> Vec<Hint> {
        vec![]
    }
    fn current_function(&self) -> Option<String> {
        None
    }
    fn header(&self) -> ColoredString {
        match self.current_function() {
            Some(func) => format!("{} {} {}: {}", "error:".red().bold(), "in".blue(), func, self.message().bold()).into(),
            None => format!("{} {}", "error:".red().bold(), self.message().bold()).into(),
        }
    }

    fn debug_info(&self) -> ColoredString {
        #[cfg(debug_assertions)]
        return ColoredString::from(format!(
            "\n\nDebug info:\n\tprogram crashed at [{} @ {}:{}]\n\nInternal dump:\n{}",
            self.get_debug_info().compiler_file,
            self.get_debug_info().compiler_line,
            self.get_debug_info().compiler_column,
            self.get_debug_info().internal_dump
        ));
        #[cfg(not(debug_assertions))]
        return ColoredString::from("");
    }
}

#[derive(Debug, Default, Clone)]
pub struct DiagnosticInfo {
    pub path: PathBuf,
    pub eof_pos: Span,
    pub current_function: RefCell<Option<String>>,
}

impl DiagnosticInfo {
    pub fn new(path: PathBuf, eof_pos: Span) -> Self {
        Self { path, eof_pos, current_function: RefCell::new(None) }
    }
}

pub fn number_length(n: usize) -> usize {
    n.to_string().len()
}

#[derive(Debug, Default, Clone)]
pub struct DebugInfo {
    pub compiler_file: &'static str,
    pub compiler_line: u32,
    pub compiler_column: u32,
    pub internal_dump: String,
}

pub trait Dumpable {
    type Arena;
    fn dump(&self, arena: &Self::Arena) -> ColoredString;
}

fn display_col(_line: &str, col: usize) -> usize { col - 1 }
fn display_width(line: &str) -> usize {
    line.replace('\t', "    ").chars().count()
}

const MAX_WIDTH: usize = 100; 

pub fn render_snippet(s: &Snippet, show_location: bool) -> String {
    let src = match fs::read_to_string(&s.path) {
        Ok(src) => src,
        Err(e) => return format!("\n<source unavailable: {e}>"),
    };
    let lines: Vec<&str> = src.lines().collect();

    let mut labels: Vec<&Label> = s.labels.iter().collect();
    labels.sort_by_key(|l| (l.span.start.line, l.span.start.col));
    let Some(first) = labels.first() else { return String::new() };

    let gutter = number_length(labels.iter().map(|l| l.span.end.line).max().unwrap());
    let pad = " ".repeat(gutter);
    let bar = "|".blue().bold();
    let mut out = String::new();

    if show_location {
        out += &format!("\n{pad}{} {}:{}:{}", "-->".blue(), s.path.display(), first.span.start.line, first.span.start.col);
    }
    out += &format!("\n{pad} {bar}");

    let print_line = |out: &mut String, n: usize| {
        let text = lines.get(n - 1).copied().unwrap_or("");
        let num = format!("{:>gutter$}", n).blue().bold();
        *out += &format!("\n{num} {bar} {}", text.replace('\t', "    "));
    };

    let mut last_printed = 0usize;
    let mut i = 0;
    while i < labels.len() {
        let line_no = labels[i].span.start.line;
        let mut j = i;
        while j < labels.len() && labels[j].span.start.line == line_no { j += 1; }
        let group = &labels[i..j];
        i = j;

        if last_printed != 0 && line_no > last_printed + 1 {
            out += &format!("\n{}", "...".blue().bold());
        }
        if line_no > last_printed {
            print_line(&mut out, line_no);
            last_printed = line_no;
        }
        let line = lines.get(line_no - 1).copied().unwrap_or("");

        let mut row: Vec<char> = Vec::new();
        for l in group {
            let offset = display_col(line, l.span.start.col);
            let width = if l.span.is_multiline() {
                display_width(line) - offset
            } else {
                display_col(line, l.span.end.col) - offset
            }
            .max(1);
            if row.len() < offset + width { row.resize(offset + width, ' '); }
            for c in &mut row[offset..offset + width] { *c = '^'; }
        }
        let row: String = row.into_iter().collect();
        out += &format!("\n{pad} {bar} {}", row.red().bold());

        let msgs: Vec<(usize, &str)> = group
            .iter()
            .filter_map(|l| l.message.as_deref().map(|m| (display_col(line, l.span.start.col), m)))
            .collect();
        for k in (0..msgs.len()).rev() {
            let (col, m) = msgs[k];
            let budget = MAX_WIDTH.saturating_sub(gutter + 3 + m.chars().count());
            let col = col.min(budget);
            let mut row = vec![' '; col];
            for &(c, _) in &msgs[..k] {
                if c < col { row[c] = '|'; }
            }
            let row: String = row.into_iter().collect();
            out += &format!("\n{pad} {bar} {}", format!("{row}{m}").red().bold());
        }

        let end_line = group.iter().map(|l| l.span.end.line).max().unwrap();
        for n in (last_printed + 1)..=end_line {
            print_line(&mut out, n);
            last_printed = n;
        }
    }

    out += &format!("\n{pad} {bar}");
    out
}

