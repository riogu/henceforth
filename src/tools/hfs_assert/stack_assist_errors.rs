use std::{fmt::Display, path::PathBuf};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    Span,
    error::{CompileError, DebugInfo, number_length},
};

#[derive(Debug)]
pub enum StackAssistErrorKind {
    NoPostconditionFound,
    AssertionFailed,
}

#[derive(Debug)]
pub struct StackAssistError {
    pub kind: StackAssistErrorKind,
    pub path: PathBuf,
    pub span: Vec<Span>,
    pub debug_info: DebugInfo,
}

impl Display for StackAssistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}

#[macro_export]
macro_rules! stack_assist_error {
    ($kind:expr, $assistant:expr, $span:expr) => {
        Err(Box::new(StackAssistError {
            kind: $kind,
            path: $assistant.diagnostic_info.path.clone(),
            span: $span,
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: String::new(),
            },
        }))
    };
}

impl CompileError for StackAssistError {
    fn get_span(&self) -> Span {
        match self.span.first() {
            Some(span) => *span,
            None => Span::default(),
        }
    }

    fn get_path(&self) -> PathBuf {
        self.path.clone()
    }

    fn message(&self) -> (String, String) {
        match self.kind {
            StackAssistErrorKind::NoPostconditionFound => {
                (format!("no postcondition found for this stack environment with a precondition"), String::new())
            },
            StackAssistErrorKind::AssertionFailed => {
                ("stack assertion failed".to_string(), "these expressions conflict with the post condition".to_string())
            },
        }
    }

    fn get_debug_info(&self) -> DebugInfo {
        self.debug_info.clone()
    }

    fn current_function(&self) -> Option<String> {
        None
    }

    fn header(&self) -> ColoredString {
        match self.current_function() {
            Some(func) => format!("{} {} {}: {}", "error:".red().bold(), "in".blue(), func, self.message().0.bold()).into(),
            None => format!("{} {}", "error:".red().bold(), self.message().0.bold()).into(),
        }
    }

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
        let source = std::fs::read_to_string(&self.path).map_err(|e| format!("Could not read source file: {}", e))?;

        match self.kind {
            StackAssistErrorKind::AssertionFailed => self.render_multiple_pointers(&source),
            _ => self.render_single_pointer(&source),
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

    fn get_line(&self) -> usize {
        return self.get_span().start.line;
    }
}

impl StackAssistError {
    fn render_single_pointer(&self, source: &str) -> Result<ColoredString, Box<dyn std::error::Error>> {
        let span = self.get_span();

        let lines: Vec<String> = source
            .lines()
            .skip(span.start.line - 1)
            .take(span.end.line - span.start.line + 1)
            .map(|line| line.replace("\t", "    "))
            .collect();

        let error_pointer_size = if span.is_multiline() {
            let line_end = lines.first().expect("[internal error] get_span() must include at least one line").len();

            line_end - span.start.col + 1
        } else {
            span.end.col - span.start.col
        };

        let mut error_pointer = " ".repeat(span.start.col - 1);
        error_pointer.push_str(format!("{} {}", "^".repeat(error_pointer_size), self.message().1).as_str());

        let line_number_width = number_length(span.end.line);

        let mut error_msg = format!(
            "{} {} {}\n{} {} {}",
            " ".repeat(line_number_width),
            "|".blue().bold(),
            "",
            span.start.line.to_string().blue().bold(),
            "|".blue().bold(),
            lines.first().expect("[internal error] span must include at least one line"),
        );

        error_msg.push_str(&format!("\n{} {} {}", " ".repeat(line_number_width), "|".blue().bold(), error_pointer.red().bold()));

        for (i, line) in lines.iter().enumerate().skip(1) {
            error_msg.push_str(&format!("\n{} {} {}", (span.start.line + i).to_string().blue().bold(), "|".blue().bold(), line));
        }

        error_msg.push_str(&format!("\n{} {}", " ".repeat(line_number_width), "|".blue().bold()));

        Ok(error_msg.into())
    }

    fn render_multiple_pointers(&self, source: &str) -> Result<ColoredString, Box<dyn std::error::Error>> {
        if self.span.is_empty() {
            return Ok(ColoredString::from(""));
        }

        let mut spans = self.span.clone();
        spans.sort_by_key(|span| (span.start.line, span.start.col));

        let source_lines: Vec<&str> = source.lines().collect();

        let first_line = spans.first().unwrap().start.line;
        let last_line = spans.last().unwrap().end.line;

        let line_number_width = number_length(last_line);
        let mut output = String::new();

        for line_number in first_line..=last_line {
            let line = source_lines.get(line_number - 1).ok_or("Span points outside source file")?;

            let displayed_line = line.replace("\t", "    ");

            output.push_str(&format!(
                "\n{:>width$} {} {}",
                line_number.to_string().blue().bold(),
                "|".blue().bold(),
                displayed_line,
                width = line_number_width,
            ));

            let line_spans: Vec<&Span> =
                spans.iter().filter(|span| span.start.line <= line_number && span.end.line >= line_number).collect();

            for span in line_spans {
                let start_col = if span.start.line == line_number {
                    span.start.col
                } else {
                    1
                };

                let end_col = if span.end.line == line_number {
                    span.end.col
                } else {
                    line.len() + 1
                };

                let pointer_size = end_col.saturating_sub(start_col).max(1);

                let is_last_pointer = span.start == spans.last().unwrap().start;

                let annotation = if is_last_pointer {
                    format!(" {}", self.message().1).red().bold()
                } else {
                    ColoredString::from("")
                };

                output.push_str(&format!(
                    "\n{} {} {}{}",
                    " ".repeat(line_number_width),
                    "|".blue().bold(),
                    format!("{}{}", " ".repeat(start_col.saturating_sub(1)), "^".repeat(pointer_size)).red().bold(),
                    annotation
                ));
            }
        }

        output.push_str(&format!("\n{} {}", " ".repeat(line_number_width), "|".blue().bold()));

        Ok(output.into())
    }
}
