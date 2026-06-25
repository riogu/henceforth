use std::{
    fmt::{Debug, Display},
    fs,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::Span;

pub trait CompileError: Display + Debug {
    fn get_span(&self) -> Span;
    fn get_path(&self) -> PathBuf;
    fn message(&self) -> (String, String);
    fn header(&self) -> ColoredString { format!("{} {}", "error:".red().bold(), self.message().0.bold()).into() }
    fn location(&self) -> ColoredString {
        format!(
            "{}{} {}:{}:{}",
            " ".repeat(number_length(self.get_span().start.line)),
            "-->".blue(),
            self.get_path().to_str().unwrap(),
            self.get_span().start.line,
            self.get_span().start.col
        )
        .into()
    }

    fn source_code(&self) -> Result<colored::ColoredString, Box<dyn std::error::Error>> {
        let source = fs::read_to_string(&self.get_path()).map_err(|e| format!("Could not read source file: {}", e))?;

        let lines: Vec<String> = source
            .lines()
            .skip(self.get_span().start.line - 1)
            .take(self.get_span().end.line - self.get_span().start.line + 1)
            .map(|line| line.replace("\t", "    "))
            .collect();
        let error_pointer_size = if self.get_span().is_multiline() {
            let line_end = lines.first().expect("[internal error] get_span() must include at least one line").len();
            line_end - self.get_span().start.col + 1
        } else {
            self.get_span().end.col - self.get_span().start.col
        };
        let mut error_pointer = " ".repeat(self.get_span().start.col - 1);
        error_pointer.push_str(format!("{} {}", "^".repeat(error_pointer_size), self.message().1).as_str());
        let mut error_msg = format!(
            "{} {}\n{} {} {}\n{} {} {}\n",
            " ".repeat(number_length(self.get_span().end.line)),
            "|".blue().bold(),
            self.get_span().start.line.to_string().blue().bold(),
            "|".blue().bold(),
            lines.first().unwrap(),
            " ".repeat(number_length(self.get_span().end.line)),
            "|".blue().bold(),
            error_pointer.red().bold()
        );

        error_msg.push_str(
            &lines[1..]
                .iter()
                .enumerate()
                .map(|(i, line)| {
                    format!("{} {} {}", (self.get_span().start.line + 1 + i).to_string().blue().bold(), "|".blue().bold(), line)
                })
                .collect::<Vec<_>>()
                .join("\n"),
        );

        error_msg.push_str(&format!("\n{} {}", " ".repeat(number_length(self.get_span().end.line)), "|".blue().bold()));

        Ok(error_msg.into())
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

    fn get_debug_info(&self) -> DebugInfo;

    fn get_line(&self) -> usize { return self.get_span().start.line; }
}

#[derive(Debug, Default, Clone)]
pub struct DiagnosticInfo {
    pub path: PathBuf,
    pub eof_pos: Span,
}

impl DiagnosticInfo {
    pub fn new(path: PathBuf, eof_pos: Span) -> Self { Self { path, eof_pos } }
}

pub fn number_length(n: usize) -> usize { n.to_string().len() }

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
