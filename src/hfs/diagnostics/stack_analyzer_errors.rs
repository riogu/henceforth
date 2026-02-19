use std::{fmt::Display, fs, path::PathBuf};

use colored::{ColoredString, Colorize, CustomColor};

use crate::hfs::{
    error::{number_length, CompileError, DebugInfo, Dumpable},
    AstArena, Expression, FunctionDeclaration, Identifier, Operation, SourceInfo, Statement, TopLevelId, Type, VarDeclaration,
};

#[derive(Debug)]
pub enum JumpKeyword {
    Break,
    Continue,
    Return,
}

impl Display for JumpKeyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpKeyword::Break => write!(f, "break"),
            JumpKeyword::Continue => write!(f, "continue"),
            JumpKeyword::Return => write!(f, "return"),
        }
    }
}

#[derive(Debug)]
pub enum StackAnalyzerErrorKind {
    StackUnderflow,
    ExpectedItemOnStack,
    IncorrectNumberReturnValues(usize, usize),
    TypeMismatchReturnValues(Type, Type),
    TypeMismatch(Type, Type),
    IncorrectTupleLength(usize, usize),
    IncorrectPointerCount(usize, usize),
    MismatchingStackDepths(usize, usize),
    ExpectedNetZeroStackEffectIfStmt(usize),
    ExpectedNetZeroStackEffectWhileLoop(usize),
    FoundXOutsideWhileLoop(JumpKeyword),
    AssignValueToFunction,
    TooManyDereferences(usize, usize),
    CallVariableAsFunction,
    UseOfUndeclaredIdentifier(String),
}

#[derive(Debug)]
pub struct StackAnalyzerError {
    pub kind: StackAnalyzerErrorKind,
    pub path: PathBuf,
    pub source_info: SourceInfo,
    pub debug_info: DebugInfo,
}

#[macro_export]
macro_rules! stack_analyzer_error {
    ($kind:expr, $arena:expr, $source_info:expr) => {
        Err(Box::new(StackAnalyzerError {
            kind: $kind,
            path: $arena.diagnostic_info.path.clone(),
            source_info: StackAnalyzerError::merge_source_info($source_info),
            debug_info: $crate::hfs::diagnostics::error::DebugInfo {
                compiler_file: file!(),
                compiler_line: line!(),
                compiler_column: column!(),
                internal_dump: StackAnalyzerError::dump_ast($arena),
            },
        }))
    };
}
impl StackAnalyzerError {
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

    pub fn dump_ast(arena: &AstArena) -> String {
        let functions = arena.functions.iter().map(|func| func.dump(arena).to_string()).collect::<Vec<String>>().join("\n");
        format!("{}", functions)
    }
}

impl CompileError for StackAnalyzerError {
    fn message(&self) -> (String, String) {
        match &self.kind {
            StackAnalyzerErrorKind::StackUnderflow =>
                (String::from("stack underflow"), String::from("stack underflow occurred here")),
            StackAnalyzerErrorKind::ExpectedItemOnStack => (String::from("expected item on stack"), String::new()),
            StackAnalyzerErrorKind::IncorrectNumberReturnValues(expected, actual) =>
                (format!("expected {} values on stack for return, found {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::TypeMismatchReturnValues(expected, actual) =>
                (format!("expected {} on stack for return, found {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::TypeMismatch(expected, actual) =>
                (format!("expected {}, found {}", expected, actual), format!("found {}", actual)),
            StackAnalyzerErrorKind::IncorrectTupleLength(expected, actual) =>
                (format!("expected a tuple of size {}, found a tuple of size {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::IncorrectPointerCount(expected, actual) =>
                (format!("expected a pointer count of {}, found a pointer count of {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::MismatchingStackDepths(expected, actual) =>
                (format!("expected a stack depth of {}, found a stack depth of {}", expected, actual), String::new()),
            StackAnalyzerErrorKind::ExpectedNetZeroStackEffectIfStmt(found) => (
                format!("expected a net-zero stack effect on all branches, found a stack depth difference of {}", found),
                String::new(),
            ),
            StackAnalyzerErrorKind::ExpectedNetZeroStackEffectWhileLoop(found) => (
                format!("expected while loop to maintain a net-zero stack effect, found a stack depth difference of {}", found),
                String::new(),
            ),
            StackAnalyzerErrorKind::FoundXOutsideWhileLoop(jump_keyword) =>
                (format!("found {} outside while loop", jump_keyword), format!("found {}", jump_keyword)),
            StackAnalyzerErrorKind::AssignValueToFunction => (format!("cannot assign value to a function"), String::new()),
            StackAnalyzerErrorKind::TooManyDereferences(actual, max) =>
                (format!("cannot dereference {} times", actual), format!("type only has {} levels of indirection", max)),
            StackAnalyzerErrorKind::CallVariableAsFunction => (format!("cannot call variable as a function"), String::new()),
            StackAnalyzerErrorKind::UseOfUndeclaredIdentifier(name) =>
                (format!("use of undeclared identifier '{}'", name), format!("undeclared identifier")),
        }
    }

    fn header(&self) -> colored::ColoredString {
        format!("{} {}", "error:".red().bold(), self.message().0.bold()).into()
    }

    fn location(&self) -> colored::ColoredString {
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

    fn source_code(&self) -> Result<colored::ColoredString, Box<dyn std::error::Error>> {
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

impl Display for StackAnalyzerError {
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

impl Dumpable for FunctionDeclaration {
    type Arena = AstArena;
    fn dump(&self, arena: &AstArena) -> ColoredString {
        let items: Vec<ColoredString> = self.parameter_exprs.iter().map(|expr| arena.get_expr(*expr).dump(arena)).collect();
        ColoredString::from(format!(
            "{}\n\t{} {}\n\t{} {}\n\t{} {}\n\t{}\n\t{}\n\t{}\n\t{}",
            format!("Function {}:", self.name).bold().red(),
            "Parameter type:".blue(),
            indent(&arena.get_type(self.param_type).dump_resolved(arena)).yellow(),
            "Return type:".blue(),
            indent(&arena.get_type(self.return_type).dump_resolved(arena)).yellow(),
            "Body:".blue(),
            indent(&arena.get_stmt(self.body).dump(arena)),
            "Parameter Exprs:".blue(),
            "[".custom_color(CustomColor::new(129, 137, 150)),
            indent_list(&items),
            "]".custom_color(CustomColor::new(129, 137, 150))
        ))
    }
}

impl Dumpable for VarDeclaration {
    type Arena = AstArena;
    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        ColoredString::from(format!(
            "{}\n\t{} {}",
            format!("Variable {}:", self.name).bold().red(),
            "Type:".blue(),
            indent(&arena.get_type(self.hfs_type).dump_resolved(arena)).yellow()
        ))
    }
}

impl Dumpable for Operation {
    type Arena = AstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        let op = match self {
            Operation::Add(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Add".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Sub(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Sub".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Mul(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Mul".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Div(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Div".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Mod(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Mod".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Or(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Or".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::And(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "And".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Equal(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Equal".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::NotEqual(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "NotEqual".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Less(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Less".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::LessEqual(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "LessEqual".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Greater(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "Greater".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::GreaterEqual(x, y) => ColoredString::from(format!(
                "{}{}{}{}{}{}",
                "GreaterEqual".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ", ".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*y).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Not(x) => ColoredString::from(format!(
                "{}{}{}{}",
                "Not".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::AddressOf(x) => ColoredString::from(format!(
                "{}{}{}{}",
                "AdressOf".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
            Operation::Dereference(x) => ColoredString::from(format!(
                "{}{}{}{}",
                "Dereference".yellow(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                arena.get_expr(*x).dump(arena),
                ")".custom_color(CustomColor::new(129, 137, 150))
            )),
        };
        op
    }
}

impl Dumpable for Identifier {
    type Arena = AstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            Identifier::GlobalVar(var_id) => ColoredString::from(format!(
                "{} {}{}{}",
                format!("{}", arena.get_var(*var_id).name).green(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                "Global Variable".yellow(),
                ")".custom_color(CustomColor::new(129, 137, 150)),
            )),
            Identifier::Variable(var_id) => ColoredString::from(format!(
                "{} {}{}{}",
                format!("{}", arena.get_var(*var_id).name).green(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                "Variable".yellow(),
                ")".custom_color(CustomColor::new(129, 137, 150)),
            )),

            Identifier::Function(func_id) => ColoredString::from(format!(
                "{} {}{}{}",
                format!("{}", arena.get_func(*func_id).name).green(),
                "(".custom_color(CustomColor::new(129, 137, 150)),
                "Variable".yellow(),
                ")".custom_color(CustomColor::new(129, 137, 150)),
            )),
        }
    }
}

impl Dumpable for Expression {
    type Arena = AstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            Expression::Operation(operation) =>
                ColoredString::from(format!("{}\n\t{}", "Operation:".red().bold(), indent(&operation.dump(arena)).yellow())),
            Expression::Identifier(name) => ColoredString::from(format!(
                "{} {}",
                "Identifier".red().bold(),
                format!(
                    "{}{}{}",
                    "(".custom_color(CustomColor::new(129, 137, 150)),
                    name.dump(arena),
                    ")".custom_color(CustomColor::new(129, 137, 150)),
                )
            )),
            Expression::Literal(literal) =>
                ColoredString::from(format!("{} {}", "Literal: ".red().bold(), format!("{}", literal).green())),
            Expression::Tuple { expressions } => {
                let items: Vec<ColoredString> = expressions.iter().map(|expr| arena.get_expr(*expr).dump(arena)).collect();
                ColoredString::from(format!(
                    "{}\n\t{}\n{}\n\t{}",
                    "Tuple:".red(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&items),
                    "]".custom_color(CustomColor::new(129, 137, 150))
                ))
            },
            Expression::StackKeyword(kw) => {
                let param_exprs: Vec<ColoredString> =
                    kw.parameter_exprs.iter().map(|expr| arena.get_expr(*expr).dump(arena)).collect();
                let return_values: Vec<ColoredString> =
                    kw.return_values.iter().map(|expr| arena.get_expr(*expr).dump(arena)).collect();
                ColoredString::from(format!(
                    "{}\n\t{} {}\n\t{} {}\n\t{}\n\t{}\n\t{}\n\t{}\n\t{}\n\t{}\n\t{}\n\t{}",
                    format!("Function {}:", kw.name).bold().red(),
                    "Parameter type:".blue(),
                    indent(&arena.get_type(kw.param_type).dump_resolved(arena)).yellow(),
                    "Return type:".blue(),
                    indent(&arena.get_type(kw.return_type).dump_resolved(arena)).yellow(),
                    "Parameter Exprs:".blue(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&param_exprs),
                    "]".custom_color(CustomColor::new(129, 137, 150)),
                    "Return Values:".blue(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&param_exprs),
                    "]".custom_color(CustomColor::new(129, 137, 150))
                ))
            },
            Expression::Parameter { index, type_id } => ColoredString::from(format!(
                "{}\n\t{} {}\n\t{} {}",
                "Parameter:".red().bold(),
                "Index:".blue(),
                format!("{}", index).green(),
                "Type:".blue(),
                arena.get_type(*type_id).dump_resolved(arena).yellow()
            )),
            Expression::ReturnValue(type_id) => ColoredString::from(format!(
                "{}\n\t{} {}",
                "Return Value:".red().bold(),
                "Type:".blue(),
                arena.get_type(*type_id).dump_resolved(arena).yellow()
            )),
        }
    }
}

impl Dumpable for TopLevelId {
    type Arena = AstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            TopLevelId::VariableDecl(unresolved_var_id) => arena.get_var(*unresolved_var_id).dump(arena),
            TopLevelId::FunctionDecl(unresolved_func_id) => arena.get_func(*unresolved_func_id).dump(arena),
            TopLevelId::Statement(unresolved_stmt_id) => arena.get_stmt(*unresolved_stmt_id).dump(arena),
        }
    }
}

impl Dumpable for Statement {
    type Arena = AstArena;

    fn dump(&self, arena: &Self::Arena) -> ColoredString {
        match self {
            Statement::ElseIf { cond_stack_block, body, else_stmt } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}{}",
                "Else-If Statement:".red().bold(),
                "Condition:".blue(),
                indent(&arena.get_stmt(*cond_stack_block).dump(arena)),
                "Body:".blue(),
                indent(&arena.get_stmt(*body).dump(arena)),
                match else_stmt {
                    Some(stmt) => format!("\n\t{} {}", "Next Branch:".blue(), indent(&arena.get_stmt(*stmt).dump(arena))),
                    None => format!(""),
                }
            )),
            Statement::Else(unresolved_stmt_id) => ColoredString::from(format!(
                "\n{}\n\t{} {}",
                "Else Statement:".red().bold(),
                "Body:".blue(),
                indent(&arena.get_stmt(*unresolved_stmt_id).dump(arena)),
            )),
            Statement::If { cond_stack_block, body, else_stmt } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}{}",
                "If Statement:".red().bold(),
                "Condition:".blue(),
                indent(&arena.get_stmt(*cond_stack_block).dump(arena)),
                "Body:".blue(),
                indent(&arena.get_stmt(*body).dump(arena)),
                match else_stmt {
                    Some(stmt) => format!("\n\t{} {}", "Next Branch:".blue(), indent(&arena.get_stmt(*stmt).dump(arena))),
                    None => format!(""),
                }
            )),
            Statement::While { cond, body } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}",
                "While Loop:".red().bold(),
                "Condition:".blue(),
                indent(&arena.get_expr(*cond).dump(arena)),
                "Body:".blue(),
                indent(&arena.get_stmt(*body).dump(arena)),
            )),
            Statement::StackBlock(unresolved_expr_ids) => {
                let items: Vec<ColoredString> = unresolved_expr_ids.iter().map(|id| arena.get_expr(*id).dump(arena)).collect();
                ColoredString::from(format!(
                    "\n{}\n\t{}\n\t{}\n\t{}\n\t{}",
                    "Stack Block:".red().bold(),
                    "Expressions:".blue(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&items),
                    "]".custom_color(CustomColor::new(129, 137, 150)),
                ))
            },
            Statement::BlockScope(unresolved_top_level_ids, scope_kind) => {
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
            Statement::Return => "Return".red().bold(),
            Statement::Break => "Break".red().bold(),
            Statement::Continue => "Continue".red().bold(),
            Statement::Empty => format!("Empty").red().bold(),
            Statement::Assignment { identifier, is_move, deref_count } => ColoredString::from(format!(
                "\n{}\n\t{} {}\n\t{} {}\n\t{} {}",
                "Assignment:".red().bold(),
                "Identifier:".blue(),
                indent(&identifier.dump(arena)),
                "Is Move:".blue(),
                format!("{}", is_move).green(),
                "Deref Count:".blue(),
                format!("{}", deref_count).green(),
            )),
            Statement::FunctionCall { is_move, arg_count, func_id, return_values } => {
                let items: Vec<ColoredString> = return_values.iter().map(|id| arena.get_expr(*id).dump(arena)).collect();
                ColoredString::from(format!(
                    "\n{}\n\t{} {}\n\t{} {}\n\t{} {}\n\t{}\n\t{}\n\t{}\n\t{}",
                    "Function Call:".red().bold(),
                    "Identifier:".blue(),
                    indent(&format!("{}", arena.get_func(*func_id).name).green()),
                    "Is Move:".blue(),
                    format!("{}", is_move).green(),
                    "Arg Count:".blue(),
                    format!("{}", arg_count).green(),
                    "Return Values:".blue(),
                    "[".custom_color(CustomColor::new(129, 137, 150)),
                    indent_list(&items),
                    "]".custom_color(CustomColor::new(129, 137, 150)),
                ))
            },
        }
    }
}

impl Type {
    fn dump_resolved(&self, arena: &AstArena) -> ColoredString {
        ColoredString::from(format!("{}", self.get_repr_resolved(arena)))
    }
}
