use std::{
    error::Error,
    fmt::{format, Display},
    fs,
    path::PathBuf,
};

use colored::{ColoredString, Colorize};

use crate::hfs::{
    error::{number_length, CompileError, DebugInfo, Dumpable},
    SourceInfo, TokenKind, Type, UnresolvedAstArena, UnresolvedExpression, UnresolvedFunctionDeclaration, UnresolvedOperation,
    UnresolvedStatement, UnresolvedTopLevelId, UnresolvedVarDeclaration,
};

#[derive(Debug)]
pub enum Expectable {
    AnyToken,
    Token(TokenKind),
    Identifier,
    StackKeyword,
    Type,
    VariableDecl,
    FunctionDecl,
    Statement,
    StackExpression,
    StackOperation,
}

impl Display for Expectable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expectable::Token(token_kind) => write!(f, "'{}'", token_kind),
            Expectable::Identifier => write!(f, "identifier"),
            Expectable::StackKeyword => write!(f, "stack keyword"),
            Expectable::Type => write!(f, "type"),
            Expectable::VariableDecl => write!(f, "variable declaration"),
            Expectable::FunctionDecl => write!(f, "function declaration"),
            Expectable::Statement => write!(f, "statement"),
            Expectable::AnyToken => write!(f, "token"),
            Expectable::StackExpression => write!(f, "stack expression"),
            Expectable::StackOperation => write!(f, "stack operation"),
        }
    }
}

#[derive(Debug)]
pub enum ParserErrorKind {
    ExpectedButFound(Vec<Expectable>, Option<TokenKind>),
}

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub path: PathBuf,
    pub source_info: SourceInfo,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! parser_error {
    ($kind:expr, $arena:expr, $source_info:expr) => {
        Err(Box::new(ParserError {
            kind: $kind,
            path: $arena.diagnostic_info.path.clone(),
            source_info: ParserError::merge_source_info($source_info),
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: ParserError::dump_ast($arena),
            },
        }))
    };
}

impl ParserError {
    pub fn merge_source_info(mut source_infos: Vec<SourceInfo>) -> SourceInfo {
        source_infos.sort();
        let first = source_infos.first().unwrap();
        let last = source_infos.last().unwrap();

        SourceInfo {
            line_number: first.line_number,
            line_offset: first.line_offset,
            token_width: last.line_offset + last.token_width - first.line_offset,
        }
    }
    pub fn dump_ast(arena: &UnresolvedAstArena) -> String {
        let functions =
            arena.unresolved_functions.iter().map(|func| func.dump(arena).to_string()).collect::<Vec<String>>().join("\n");
        let variables = arena.unresolved_vars.iter().map(|var| var.dump(arena).to_string()).collect::<Vec<String>>().join("\n");
        format!("{}\n\n{}", functions, variables)
    }
}

impl CompileError for ParserError {
    fn message(&self) -> (String, String) {
        match &self.kind {
            ParserErrorKind::ExpectedButFound(expected, found) => match found {
                Some(found) => {
                    let expected_repr = format!(
                        "{}",
                        expected.iter().map(|expected| format!("{}", expected)).collect::<Vec<String>>().join(" or ")
                    );
                    (format!("expected {}, found '{}'", expected_repr, found), String::new())
                },
                None => {
                    let expected_repr = format!(
                        "{}",
                        expected.iter().map(|expected| format!("{}", expected)).collect::<Vec<String>>().join(" or ")
                    );
                    (format!("expected {}", expected_repr), String::new())
                },
            },
        }
    }

    fn header(&self) -> ColoredString {
        format!("{} {}", "error:".red().bold(), self.message().0.bold()).into()
    }

    fn location(&self) -> ColoredString {
        format!(
            "{}{} {}:{}:{}",
            " ".repeat(number_length(self.source_info.line_number)),
            "-->".blue(),
            self.path.to_str().unwrap(),
            self.source_info.line_number,
            self.source_info.line_offset
        )
        .into()
    }

    fn source_code(&self) -> Result<ColoredString, Box<dyn Error>> {
        let source = fs::read_to_string(&self.path).map_err(|e| format!("Could not read source file: {}", e))?;

        let line = source
            .lines()
            .nth(self.source_info.line_number - 1)
            .ok_or_else(|| format!("Line {} not found in file", self.source_info.line_number))?
            .replace("\t", "    ");

        let mut error_pointer = " ".repeat(self.source_info.line_offset - 1);
        error_pointer.push_str(format!("{} {}", "^".repeat(self.source_info.token_width), self.message().1).as_str());
        return Ok(ColoredString::from(format!(
            "{} {}\n{} {} {}\n{} {} {}",
            " ".repeat(number_length(self.source_info.line_number)),
            "|".blue().bold(),
            self.source_info.line_number.to_string().blue().bold(),
            "|".blue().bold(),
            line,
            " ".repeat(number_length(self.source_info.line_number)),
            "|".blue().bold(),
            error_pointer.red().bold()
        )));
    }

    fn debug_info(&self) -> ColoredString {
        #[cfg(debug_assertions)]
        return ColoredString::from(format!(
            "\n\nDebug info:\n\tprogram crashed at [{} @ {}:{}]\n\nInternal dump:\n{}",
            self.debug_info.compiler_file,
            self.debug_info.compiler_line,
            self.debug_info.compiler_column,
            self.debug_info.internal_dump
        ));

        #[cfg(not(debug_assertions))]
        return ColoredString::from("");
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source_code = match self.source_code() {
            Ok(source_code) => source_code,
            Err(e) => ColoredString::from(format!("<source unavailable: {}>", e)),
        };
        write!(f, "{}\n{}\n{}{}", self.header(), self.location(), source_code, self.debug_info())
    }
}

fn indent(s: &ColoredString) -> ColoredString {
    ColoredString::from(s.trim_start_matches('\n').replace('\n', "\n\t"))
}

fn indent_list(items: &[ColoredString]) -> ColoredString {
    ColoredString::from(items.iter().map(|s| format!("\t{}", indent(s))).collect::<Vec<String>>().join(",\n"))
}

impl Dumpable for UnresolvedFunctionDeclaration {
    type Arena = UnresolvedAstArena;
    fn dump(&self, arena: &UnresolvedAstArena) -> ColoredString {
        ColoredString::from(format!(
            "{}\n\tParameter type: {}\n\tReturn type: {}\n\tBody: {}",
            format!("Function {}:", self.name).bold().red(),
            indent(&arena.get_type(self.param_type).dump(arena)),
            indent(&arena.get_type(self.return_type).dump(arena)),
            indent(&arena.get_unresolved_stmt(self.body).dump(arena))
        ))
    }
}

impl Dumpable for UnresolvedVarDeclaration {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        ColoredString::from(format!(
            "{}\n\tType: {}",
            format!("Variable {}:", self.name).bold().red(),
            indent(&arena.get_type(self.hfs_type).dump(arena))
        ))
    }
}

impl Dumpable for UnresolvedOperation {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            UnresolvedOperation::Add => ColoredString::from(format!("Add")),
            UnresolvedOperation::Sub => ColoredString::from(format!("Sub")),
            UnresolvedOperation::Mul => ColoredString::from(format!("Mul")),
            UnresolvedOperation::Div => ColoredString::from(format!("Div")),
            UnresolvedOperation::Mod => ColoredString::from(format!("Mod")),
            UnresolvedOperation::Or => ColoredString::from(format!("Or")),
            UnresolvedOperation::And => ColoredString::from(format!("And")),
            UnresolvedOperation::Equal => ColoredString::from(format!("Equal")),
            UnresolvedOperation::NotEqual => ColoredString::from(format!("NotEqual")),
            UnresolvedOperation::Less => ColoredString::from(format!("Less")),
            UnresolvedOperation::LessEqual => ColoredString::from(format!("LessEqual")),
            UnresolvedOperation::Greater => ColoredString::from(format!("Greater")),
            UnresolvedOperation::GreaterEqual => ColoredString::from(format!("GreaterEqual")),
            UnresolvedOperation::Not => ColoredString::from(format!("Not")),
            UnresolvedOperation::AddressOf => ColoredString::from(format!("AdressOf")),
            UnresolvedOperation::Dereference => ColoredString::from(format!("Dereference")),
        }
    }
}

impl Dumpable for UnresolvedExpression {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            UnresolvedExpression::Operation(unresolved_operation) =>
                ColoredString::from(format!("Operation:\n\t{}", indent(&unresolved_operation.dump(arena)))),
            UnresolvedExpression::Identifier(name) => ColoredString::from(format!("Identifier ({name})")),
            UnresolvedExpression::Literal(literal) => ColoredString::from(format!("Literal: {}", literal)),
            UnresolvedExpression::Tuple { expressions } => {
                let items: Vec<ColoredString> =
                    expressions.iter().map(|expr| arena.get_unresolved_expr(*expr).dump(arena)).collect();
                ColoredString::from(format!("Tuple:\n\t[\n{}\n\t]", indent_list(&items)))
            },
            UnresolvedExpression::StackKeyword(name) => ColoredString::from(format!("Stack Keyword ({name})")),
        }
    }
}

impl Dumpable for UnresolvedTopLevelId {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            UnresolvedTopLevelId::VariableDecl(unresolved_var_id) => arena.get_unresolved_var(*unresolved_var_id).dump(arena),
            UnresolvedTopLevelId::FunctionDecl(unresolved_func_id) => arena.get_unresolved_func(*unresolved_func_id).dump(arena),
            UnresolvedTopLevelId::Statement(unresolved_stmt_id) => arena.get_unresolved_stmt(*unresolved_stmt_id).dump(arena),
        }
    }
}

impl Dumpable for UnresolvedStatement {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            UnresolvedStatement::ElseIf { cond, body, else_stmt } => ColoredString::from(format!(
                "\nElse-If Statement:\n\tCondition: {}\n\tBody: {}{}",
                indent(&arena.get_unresolved_stmt(*cond).dump(arena)),
                indent(&arena.get_unresolved_stmt(*body).dump(arena)),
                match else_stmt {
                    Some(stmt) => format!("\n\tElse Statement: {}", indent(&arena.get_unresolved_stmt(*stmt).dump(arena))),
                    None => format!(""),
                }
            )),
            UnresolvedStatement::Else(unresolved_stmt_id) => ColoredString::from(format!(
                "\nElse Statement:\n\tBody: {}",
                indent(&arena.get_unresolved_stmt(*unresolved_stmt_id).dump(arena)),
            )),
            UnresolvedStatement::If { cond, body, else_stmt } => ColoredString::from(format!(
                "\nIf Statement:\n\tCondition: {}\n\tBody: {}{}",
                indent(&arena.get_unresolved_stmt(*cond).dump(arena)),
                indent(&arena.get_unresolved_stmt(*body).dump(arena)),
                match else_stmt {
                    Some(stmt) => format!("\n\tElse Statement: {}", indent(&arena.get_unresolved_stmt(*stmt).dump(arena))),
                    None => format!(""),
                }
            )),
            UnresolvedStatement::While { cond, body } => ColoredString::from(format!(
                "\nWhile Loop:\n\tCondition: {}\n\tBody: {}",
                indent(&arena.get_unresolved_stmt(*cond).dump(arena)),
                indent(&arena.get_unresolved_stmt(*body).dump(arena)),
            )),
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => {
                let items: Vec<ColoredString> =
                    unresolved_expr_ids.iter().map(|id| arena.get_unresolved_expr(*id).dump(arena)).collect();
                ColoredString::from(format!("\nStack Block:\n\tExpressions:\n\t[\n\t{}\n\t]", indent_list(&items)))
            },
            UnresolvedStatement::BlockScope(unresolved_top_level_ids, scope_kind) => {
                let items: Vec<ColoredString> = unresolved_top_level_ids.iter().map(|id| id.dump(arena)).collect();
                ColoredString::from(format!("\nBlock Scope ({:?}):\n\t[\n\t{}\n\t]", scope_kind, indent_list(&items)))
            },
            UnresolvedStatement::Return => ColoredString::from("Return"),
            UnresolvedStatement::Break => ColoredString::from("Break"),
            UnresolvedStatement::Continue => ColoredString::from("Continue"),
            UnresolvedStatement::Empty => ColoredString::from("Empty"),
            UnresolvedStatement::Assignment { identifier, is_move, deref_count } => ColoredString::from(format!(
                "\nAssignment:\n\tIdentifier: {}\n\tIs Move: {}\n\tDeref Count: {}",
                indent(&arena.get_unresolved_expr(*identifier).dump(arena)),
                is_move,
                deref_count
            )),
            UnresolvedStatement::FunctionCall { identifier, is_move } => ColoredString::from(format!(
                "\nFunction Call:\n\tIdentifier: {}\n\tIs Move: {}",
                indent(&arena.get_unresolved_expr(*identifier).dump(arena)),
                is_move,
            )),
        }
    }
}

impl Dumpable for Type {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        ColoredString::from(format!("{}", self.get_repr_unresolved(arena))).yellow()
    }
}
