use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    fs,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::Span;
const MAX_WIDTH: usize = 100;

pub struct Label {
    pub span: Span,
    pub message: Option<String>,
}

pub enum DiagnosticKind {
    Error,
    Hint(HintKind),
}

pub struct Snippet {
    pub path: PathBuf,
    pub labels: Vec<Label>,
    gutter: usize,
    pad: String,
    bar: ColoredString,
}

impl Snippet {
    pub fn new(path: PathBuf, labels: Vec<Label>) -> Self {
        let gutter = number_length(labels.iter().map(|l| l.span.end.line).max().unwrap());
        let pad = " ".repeat(gutter);
        let bar = "|".blue().bold();
        Self { path, labels, gutter, pad, bar }
    }

    fn blank_line(&self) -> String {
        format!("\n{} {}", self.pad, self.bar)
    }

    fn location_line(&self, first: &Label) -> String {
        format!("\n{}{} {}:{}:{}", self.pad, "-->".blue(), self.path.display(), first.span.start.line, first.span.start.col)
    }

    fn source_line(&self, lines: &[&str], n: usize, labels: &[&Label], kind: &DiagnosticKind) -> String {
        let text = lines.get(n - 1).copied().unwrap_or("");
        let text = match kind {
            DiagnosticKind::Error => text.replace('\t', "    "),
            DiagnosticKind::Hint(_) => self.highlight_text(text, n, labels, kind),
        };

        let num = format!("{:>width$}", n, width = self.gutter).blue().bold();

        format!("\n{num} {} {text}", self.bar)
    }

    fn labels_on_line<'a>(labels: &[&'a Label], line: usize) -> Vec<&'a Label> {
        labels.iter().copied().filter(|l| l.span.start.line <= line && l.span.end.line >= line).collect()
    }

    fn underline_character(&self, kind: &DiagnosticKind) -> char {
        match kind {
            DiagnosticKind::Error => '^',
            DiagnosticKind::Hint(hint_kind) => match hint_kind {
                HintKind::Addition => '+',
                HintKind::Removal => '-',
                HintKind::Modification => '~',
            },
        }
    }

    fn color_underline(&self, underline: String, kind: &DiagnosticKind) -> ColoredString {
        match kind {
            DiagnosticKind::Error => underline.red(),
            DiagnosticKind::Hint(hint_kind) => match hint_kind {
                HintKind::Addition => underline.green(),
                HintKind::Removal => underline.red(),
                HintKind::Modification => underline.yellow(),
            },
        }
    }

    fn underline(&self, line: &str, group: &[&Label], kind: &DiagnosticKind) -> String {
        let mut row: Vec<char> = Vec::new();
        let char = self.underline_character(kind);
        for l in group {
            let offset = display_col(line, l.span.start.col);
            let width = if l.span.is_multiline() {
                display_width(line) - offset
            } else {
                display_col(line, l.span.end.col) - offset
            }
            .max(1);
            if row.len() < offset + width {
                row.resize(offset + width, ' ');
            }
            for c in &mut row[offset..offset + width] {
                *c = char;
            }
        }
        let row: String = row.into_iter().collect();
        format!("\n{} {} {}", self.pad, self.bar, self.color_underline(row, kind).bold())
    }

    fn highlight_text(&self, line: &str, line_no: usize, labels: &[&Label], kind: &DiagnosticKind) -> String {
        let text = line.replace('\t', "    ");
        let chars: Vec<char> = text.chars().collect();

        let mut highlighted = vec![false; chars.len()];

        for label in labels {
            let start = if line_no == label.span.start.line {
                display_col(line, label.span.start.col)
            } else {
                0
            };

            let end = if line_no == label.span.end.line {
                display_col(line, label.span.end.col)
            } else {
                chars.len()
            };

            let start = start.min(chars.len());
            let end = end.min(chars.len());

            for i in start..end {
                highlighted[i] = true;
            }
        }

        let mut out = String::new();
        let mut i = 0;

        while i < chars.len() {
            let state = highlighted[i];
            let mut j = i + 1;

            while j < chars.len() && highlighted[j] == state {
                j += 1;
            }

            let segment: String = chars[i..j].iter().collect();

            if state {
                out += &self.color_underline(segment, kind).bold().to_string();
            } else {
                out += &segment;
            }

            i = j;
        }

        out
    }

    fn message_rows(&self, line: &str, group: &[&Label], kind: &DiagnosticKind) -> String {
        let mut out = String::new();
        let msgs: Vec<(usize, &str)> =
            group.iter().filter_map(|l| l.message.as_deref().map(|m| (display_col(line, l.span.start.col), m))).collect();
        for k in (0..msgs.len()).rev() {
            let (col, m) = msgs[k];
            let budget = MAX_WIDTH.saturating_sub(self.gutter + 3 + m.chars().count());
            let col = col.min(budget);
            let mut row = vec![' '; col];
            for &(c, _) in &msgs[..k] {
                if c < col {
                    row[c] = '|';
                }
            }
            let row: String = row.into_iter().collect();
            out += &format!("\n{} {} {}", self.pad, self.bar, self.color_underline(format!("{row}{m}"), kind).bold());
        }
        out
    }

    pub fn render(&self, show_location: bool, kind: DiagnosticKind) -> String {
        let src = match fs::read_to_string(&self.path) {
            Ok(src) => src,
            Err(e) => return format!("\n<source unavailable: {e}>"),
        };
        let lines: Vec<&str> = src.lines().collect();
        let labels = self.sorted_labels();
        let Some(first) = labels.first() else {
            return String::new();
        };

        let mut out = String::new();
        if show_location {
            out += &self.location_line(first);
        }
        out += &self.blank_line();

        let mut last_printed = 0usize;
        for group in labels.chunk_by(|a, b| a.span.start.line == b.span.start.line) {
            let line_no = group[0].span.start.line;
            if last_printed != 0 && line_no > last_printed + 1 {
                out += &format!("\n{}", "...".blue().bold());
            }
            if line_no > last_printed {
                let active = Self::labels_on_line(&labels, line_no);

                out += &self.source_line(&lines, line_no, &active, &kind);
                last_printed = line_no;
            }
            let line = lines.get(line_no - 1).copied().unwrap_or("");
            out += &self.underline(line, group, &kind);
            out += &self.message_rows(line, group, &kind);

            let end_line = group.iter().map(|l| l.span.end.line).max().unwrap();
            for n in (last_printed + 1)..=end_line {
                let active = Self::labels_on_line(&labels, line_no);

                out += &self.source_line(&lines, n, &active, &kind);
                last_printed = n;
            }
        }
        out + &self.blank_line()
    }

    fn sorted_labels(&self) -> Vec<&Label> {
        let mut labels: Vec<&Label> = self.labels.iter().collect();
        labels.sort_by_key(|l| (l.span.start.line, l.span.start.col));
        labels
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HintKind {
    Addition,
    Removal,
    Modification,
}

pub struct Hint {
    pub kind: HintKind,
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
    fn get_span(&self) -> Span;
    fn get_line(&self) -> usize {
        return self.get_span().start.line;
    }
    fn message(&self) -> String;
    fn snippets(&self) -> Vec<Snippet>;
    fn render(&self) -> ColoredString {
        let mut out = format!("{}", self.header());
        for s in self.snippets() {
            out += &s.render(true, DiagnosticKind::Error);
        }
        for h in self.hints() {
            out += &format!("\n{} {}", "hint:".cyan().bold(), h.message);
            for s in &h.snippets {
                out += &s.render(false, DiagnosticKind::Hint(h.kind));
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

fn display_col(_line: &str, col: usize) -> usize {
    col - 1
}
fn display_width(line: &str) -> usize {
    line.replace('\t', "    ").chars().count()
}
