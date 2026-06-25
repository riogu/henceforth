use std::{error::Error, fmt::Display, fs, path::PathBuf};

use colored::{ColoredString, Colorize, CustomColor};

use crate::hfs::{
    Span, TokenKind, Type, UnresolvedAstArena, UnresolvedExpression, UnresolvedFunctionDeclaration, UnresolvedOperation,
    UnresolvedStatement, UnresolvedTopLevelId, UnresolvedType, UnresolvedVarDeclaration,
    error::{CompileError, DebugInfo, Dumpable, number_length},
};

#[derive(Debug)]
pub enum Expectable {
    AnyToken,
    Token(TokenKind),
    Literal,
    IntegerLiteral,
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
            Expectable::Literal => write!(f, "literal"),
            Expectable::IntegerLiteral => write!(f, "integer literal"),
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
    pub span: Span,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! parser_error {
    ($kind:expr, $arena:expr, $span:expr) => {
        Err(Box::new(ParserError {
            kind: $kind,
            path: $arena.diagnostic_info.path.clone(),
            span: $span,
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
    pub fn dump_ast(arena: &UnresolvedAstArena) -> String {
        let functions =
            arena.unresolved_functions.iter().map(|func| func.dump(arena).to_string()).collect::<Vec<String>>().join("\n");
        format!("{}", functions)
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

    fn get_path(&self) -> PathBuf { self.path.clone() }
    fn get_debug_info(&self) -> DebugInfo { self.debug_info.clone() }
    fn get_span(&self) -> Span { self.span }
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

fn indent(s: &ColoredString) -> ColoredString { ColoredString::from(s.trim_start_matches('\n').replace('\n', "\n\t")) }

fn indent_list(items: &[ColoredString]) -> ColoredString {
    ColoredString::from(items.iter().map(|s| format!("\t{}", indent(s))).collect::<Vec<String>>().join(",\n"))
}

impl Dumpable for UnresolvedFunctionDeclaration {
    type Arena = UnresolvedAstArena;
    fn dump(&self, arena: &UnresolvedAstArena) -> ColoredString {
        ColoredString::from(format!(
            "{}\n\t{} {}\n\t{} {}\n\t{} {}",
            format!("Function {}:", self.name).bold().red(),
            "Parameter type:".blue(),
            indent(&arena.get_type(self.param_type).dump(arena)).yellow(),
            "Return type:".blue(),
            indent(&arena.get_type(self.return_type).dump(arena)).yellow(),
            "Body:".blue(),
            indent(&arena.get_unresolved_stmt(self.body).dump(arena))
        ))
    }
}

impl Dumpable for UnresolvedVarDeclaration {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        ColoredString::from(format!(
            "{}\n\t{} {}",
            format!("Variable {}:", self.name).bold().red(),
            "Type:".blue(),
            indent(&arena.get_type(self.hfs_type).dump(arena)).yellow()
        ))
    }
}

impl Dumpable for UnresolvedOperation {
    type Arena = UnresolvedAstArena;

    fn dump(&self, _arena: &Self::Arena) -> ColoredString {
        let op = match self {
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
            UnresolvedOperation::ArrayAccess => ColoredString::from(format!("ArrayAccess")),
        };
        op.yellow()
    }
}

impl Dumpable for UnresolvedExpression {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            UnresolvedExpression::Operation(unresolved_operation) => ColoredString::from(format!(
                "{}\n\t{}",
                "Operation:".red().bold(),
                indent(&unresolved_operation.dump(arena)).yellow()
            )),
            UnresolvedExpression::Identifier(name) =>
                ColoredString::from(format!("{} {}", "Identifier".red().bold(), format!("({name})").green())),
            UnresolvedExpression::Literal(literal) =>
                ColoredString::from(format!("{} {}", "Literal: ".red().bold(), format!("{}", literal).green())),
            UnresolvedExpression::Tuple { expressions } => {
                let items: Vec<ColoredString> =
                    expressions.iter().map(|expr| arena.get_unresolved_expr(*expr).dump(arena)).collect();
                ColoredString::from(format!(
                    "{}\n\t{}\n{}\n\t{}",
                    "Tuple:".red(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&items),
                    "]".custom_color(CustomColor::new(129, 137, 150))
                ))
            },
            UnresolvedExpression::StackKeyword(name) =>
                ColoredString::from(format!("{} {}", "Stack Keyword".red().bold(), format!("({name})").green())),
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
                "\n{}\n\t{} {}\n\t{} {}{}",
                "Else-If Statement:".red().bold(),
                "Condition:".blue(),
                cond.iter()
                    .map(|stmt| indent(&arena.get_unresolved_stmt(*stmt).dump(arena)).to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
                "Body:".blue(),
                indent(&arena.get_unresolved_stmt(*body).dump(arena)),
                match else_stmt {
                    Some(stmt) =>
                        format!("\n\t{} {}", "Next Branch:".blue(), indent(&arena.get_unresolved_stmt(*stmt).dump(arena))),
                    None => format!(""),
                }
            )),
            UnresolvedStatement::Else(unresolved_stmt_id) => ColoredString::from(format!(
                "\n{}\n\t{} {}",
                "Else Statement:".red().bold(),
                "Body:".blue(),
                indent(&arena.get_unresolved_stmt(*unresolved_stmt_id).dump(arena)),
            )),
            UnresolvedStatement::If { cond, body, else_stmt } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}{}",
                "If Statement:".red().bold(),
                "Condition:".blue(),
                cond.iter()
                    .map(|stmt| indent(&arena.get_unresolved_stmt(*stmt).dump(arena)).to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
                "Body:".blue(),
                indent(&arena.get_unresolved_stmt(*body).dump(arena)),
                match else_stmt {
                    Some(stmt) =>
                        format!("\n\t{} {}", "Next Branch:".blue(), indent(&arena.get_unresolved_stmt(*stmt).dump(arena))),
                    None => format!(""),
                }
            )),
            UnresolvedStatement::While { cond, body } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}",
                "While Loop:".red().bold(),
                "Condition:".blue(),
                cond.iter()
                    .map(|stmt| indent(&arena.get_unresolved_stmt(*stmt).dump(arena)).to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
                "Body:".blue(),
                indent(&arena.get_unresolved_stmt(*body).dump(arena)),
            )),
            UnresolvedStatement::StackBlock(unresolved_expr_ids) => {
                let items: Vec<ColoredString> =
                    unresolved_expr_ids.iter().map(|id| arena.get_unresolved_expr(*id).dump(arena)).collect();
                ColoredString::from(format!(
                    "\n{}\n\t{}\n\t{}\n\t{}\n\t{}",
                    "Stack Block:".red().bold(),
                    "Expressions:".blue(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&items),
                    "]".custom_color(CustomColor::new(129, 137, 150)),
                ))
            },
            UnresolvedStatement::BlockScope(unresolved_top_level_ids, scope_kind) => {
                let items: Vec<ColoredString> = unresolved_top_level_ids.iter().map(|id| id.dump(arena)).collect();
                ColoredString::from(format!(
                    "\n{} {}:\n\t{}\n\t{}\n\t{}",
                    "Block Scope".red().bold(),
                    format!("({:?})", scope_kind).yellow(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&items),
                    "]".custom_color(CustomColor::new(129, 137, 150)),
                ))
            },
            UnresolvedStatement::Return => "Return".red().bold(),
            UnresolvedStatement::Break => "Break".red().bold(),
            UnresolvedStatement::Continue => "Continue".red().bold(),
            UnresolvedStatement::Empty => format!("Empty").red().bold(),
            UnresolvedStatement::Assignment { identifier, is_move, deref_count } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}\n\t{} {}",
                "Assignment:".red().bold(),
                "Identifier:".blue(),
                indent(&arena.get_unresolved_expr(*identifier).dump(arena)),
                "Is Move:".blue(),
                format!("{}", is_move).green(),
                "Deref Count:".blue(),
                format!("{}", deref_count).green(),
            )),
            UnresolvedStatement::ArrayAssignment { identifier, is_move, deref_count } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}\n\t{} {}",
                "Assignment:".red().bold(),
                "Identifier:".blue(),
                indent(&arena.get_unresolved_expr(*identifier).dump(arena)),
                "Is Move:".blue(),
                format!("{}", is_move).green(),
                "Deref Count:".blue(),
                format!("{}", deref_count).green(),
            )),
            UnresolvedStatement::FunctionCall { identifier, is_move } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}",
                "Function Call:".red().bold(),
                "Identifier:".blue(),
                indent(&arena.get_unresolved_expr(*identifier).dump(arena)),
                "Is Move:".blue(),
                format!("{}", is_move).green(),
            )),
        }
    }
}

impl Dumpable for UnresolvedType {
    type Arena = UnresolvedAstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString { ColoredString::from(format!("{}", self.get_repr(arena))) }
}
