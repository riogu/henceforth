use std::{fs, path::PathBuf};

use crate::hfscheck::error_parser::{ErrorParser, Test};

#[derive(Debug, PartialEq)]
pub enum Assertion<'a> {
    Error(&'a PathBuf, usize, String),
}

pub fn find_assertions(path: &'_ PathBuf) -> Vec<Assertion<'_>> {
    let file_lines = fs::read_to_string(path).expect("Could not read file.").lines().map(String::from).collect::<Vec<String>>();
    let mut assertions: Vec<Assertion> = Vec::new();
    for (line_number, line_string) in file_lines.iter().enumerate() {
        let mut chars_iter = line_string.chars().peekable();
        let mut line_column = 1;
        while let Some(ch) = chars_iter.next() {
            match ch {
                '/' => {
                    line_column += 1;
                    if let Some(_) = chars_iter.next_if_eq(&'/') {
                        line_column += 1;
                        if let Some(char) = chars_iter.next() {
                            match char {
                                '?' => assertions.push(Assertion::Error(
                                    path,
                                    line_number + 1,
                                    line_string[line_column..].to_string(),
                                )),
                                _ => {},
                            }
                        }
                    }
                },
                _ => line_column += 1,
            }
        }
    }
    return assertions;
}

pub fn generate_tests(path: &PathBuf) -> Vec<Test> {
    let assertions = find_assertions(path);
    let mut tests = Vec::new();
    for assertion in assertions {
        match assertion {
            Assertion::Error(path, line, error) => tests.push(ErrorParser::generate_test(path, line, error)),
        }
    }
    return tests;
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::hfscheck::hfscheck::{find_assertions, Assertion};

    #[test]
    fn test_finds_error() {
        let path = PathBuf::from("tests/failure_tests/lexer/lexer1.hfs");
        let assertions = find_assertions(&path);

        assert_eq!(assertions.len(), 1);
        assert_eq!(assertions[0], Assertion::Error(&path, 1, String::from(" \"unexpected character\" ")));
    }

    #[test]
    fn test_no_errors() {
        let path = PathBuf::from("tests/compile_tests/operations.hfs");
        let assertions = find_assertions(&path);
        assert_eq!(assertions.len(), 0);
    }
}
