use std::{collections::HashMap, path::PathBuf};

use crate::hfscheck::hfscheck::{HfsRegex, Test, TestInput};

pub struct DiagnosticParser {}

pub struct DiagnosticTest {
    pub path: PathBuf,
    pub is_error: bool,
    pub message_check: Box<dyn Fn(String) -> bool + Send + Sync>,
    pub diagnostic_line_check: Box<dyn Fn(usize) -> bool + Send + Sync>,
}

impl Test for DiagnosticTest {
    fn check(&self, input: TestInput) -> bool {
        if let TestInput::Diagnostic(message, line) = input {
            (self.message_check)(message) && (self.diagnostic_line_check)(line)
        } else {
            false
        }
    }

    fn path(&self) -> &PathBuf { &self.path }

    fn is_error(&self) -> bool {
        self.is_error
    }
}

impl DiagnosticParser {
    pub fn generate_test(path: &'_ PathBuf, line_number: usize, diagnostic: String, is_error: bool) -> DiagnosticTest {
        let mut chars_iter = diagnostic.chars().peekable();
        let mut diagnostic_line_check: Box<dyn Fn(usize) -> bool + Send + Sync> = Box::new(move |l| l == line_number);
        let mut bindings = HashMap::new();
        let mut message_check: Box<dyn Fn(String) -> bool + Send + Sync> = Box::new(|_| true);
        while let Some(char) = chars_iter.next() {
            match char {
                ' ' | '\t' | '\n' => continue,
                '"' => {
                    let mut message = String::new();
                    while let Some(char) = chars_iter.next_if(|c| c != &'"') {
                        message.push(char);
                    }
                    chars_iter.next();
                    let local_bindings;
                    (message_check, local_bindings) = HfsRegex::parse(message, &bindings);
                    bindings.extend(local_bindings);
                },
                '@' =>
                    if let Some(_) = chars_iter.next_if_eq(&'[') {
                        let mut error_position = String::new();
                        while let Some(char) = chars_iter.next_if(|c| c != &']') {
                            error_position.push(char);
                        }
                        chars_iter.next();
                        diagnostic_line_check = DiagnosticParser::generate_diagnostic_line_check(error_position, line_number);
                    },
                c => panic!("found {c} in {path:?}"),
            }
        }

        return DiagnosticTest { path: path.clone(), message_check, diagnostic_line_check, is_error };
    }

    fn generate_diagnostic_line_check(diagnostic_position: String, line_number: usize) -> Box<dyn Fn(usize) -> bool + Send + Sync> {
        match diagnostic_position.chars().nth(0) {
            Some('^') => match &diagnostic_position[1..] {
                "*" => Box::new(move |pos| pos < line_number),
                offset => match offset.parse::<usize>() {
                    Ok(offset) => Box::new(move |pos| pos == line_number - offset),
                    Err(_) => panic!("invalid token for diagnostic line offset"),
                },
            },
            Some('v') => match &diagnostic_position[1..] {
                "*" => Box::new(move |pos| pos > line_number),
                offset => match offset.parse::<usize>() {
                    Ok(offset) => Box::new(move |pos| pos == line_number + offset),
                    Err(_) => panic!("invalid token for diagnostic line offset"),
                },
            },
            Some('.') => Box::new(move |pos| pos == line_number),
            Some(_) => panic!("invalid token for diagnostic line positioning"),
            None => Box::new(move |pos| pos == line_number),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::hfscheck::diagnostic_parser::DiagnosticParser;

    #[test]
    fn test_empty_diagnostic_matches_any_message() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 1, String::new(), true);
        assert!((test.message_check)(String::from("any message")));
        assert!((test.message_check)(String::from("literally any message")));
        assert!((test.message_check)(String::new()));
    }

    #[test]
    fn test_empty_diagnostic_matches_exact_line() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 1, String::new(), true);
        assert!((test.diagnostic_line_check)(1));
        assert!(!(test.diagnostic_line_check)(3));
        assert!(!(test.diagnostic_line_check)(5));
    }

    #[test]
    fn test_upstar_matches_any_before() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 50, String::from("@[^*]"), true);
        assert!((test.diagnostic_line_check)(30));
        assert!((test.diagnostic_line_check)(1));
        assert!((test.diagnostic_line_check)(15));
        assert!(!(test.diagnostic_line_check)(50));
        assert!(!(test.diagnostic_line_check)(70));
    }

    #[test]
    fn test_downstar_matches_any_after() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 50, String::from("@[v*]"), true);
        assert!((test.diagnostic_line_check)(60));
        assert!((test.diagnostic_line_check)(70));
        assert!((test.diagnostic_line_check)(150));
        assert!(!(test.diagnostic_line_check)(50));
        assert!(!(test.diagnostic_line_check)(10));
    }

    #[test]
    fn test_downn_matches_n_after() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 50, String::from("@[v5]"), true);
        assert!((test.diagnostic_line_check)(55));
    }

    #[test]
    fn test_upn_matches_n_before() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 50, String::from("@[^5]"), true);
        assert!((test.diagnostic_line_check)(45));
    }

    #[test]
    fn test_dot_matches_exact_line() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 50, String::from("@[.]"), true);
        assert!((test.diagnostic_line_check)(50));
    }

    #[test]
    fn test_empty_pos_matches_exact_line() {
        let path = PathBuf::new();
        let test = DiagnosticParser::generate_test(&path, 50, String::from("@[]"), true);
        assert!((test.diagnostic_line_check)(50));
    }
}
