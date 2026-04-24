use std::{fs, path::PathBuf};

use crate::hfscheck::error_parser::{ErrorParser, Test};

#[derive(Debug, PartialEq)]
pub enum Assertion<'a> {
    Error(&'a PathBuf, usize, String),
}

pub enum HfsRegex {
    Text(String),
    SingleChar,
    Wildcard,
    Range(char, char),
    List(Vec<char>),
}

impl HfsRegex {
    fn new(pattern: &str) -> Self {
        match pattern {
            "*" => HfsRegex::Wildcard,
            "." => HfsRegex::SingleChar,
            text if text.starts_with('[') && text.ends_with(']') => {
                let inner = &text[1..text.len() - 1];
                let mut chars = inner.chars();
                match (chars.next(), chars.next(), chars.next()) {
                    (Some(a), Some('-'), Some(b)) => HfsRegex::Range(a, b),
                    _ => panic!("invalid range syntax"),
                }
            },
            text if text.starts_with('{') && text.ends_with('}') => {
                let inner = &text[1..text.len() - 1];
                let chars: Vec<char> = inner.split(',').map(|s| s.chars().next().expect("empty list item")).collect();
                HfsRegex::List(chars)
            },
            text => HfsRegex::Text(text.to_string()),
        }
    }

    fn matches(&self, msg: &str) -> Option<usize> {
        match self {
            HfsRegex::Text(text) => msg.find(text).map(|i| i + text.len()),
            HfsRegex::SingleChar => Some(1),
            HfsRegex::Wildcard => Some(0),
            HfsRegex::Range(a, b) => msg.chars().position(|c| c >= *a && c <= *b).map(|i| i + 1),
            HfsRegex::List(items) => msg.chars().position(|c| items.contains(&c)).map(|i| i + 1),
        }
    }
    pub fn parse(message: String) -> Box<dyn Fn(String) -> bool + Send + Sync> {
        let mut patterns: Vec<HfsRegex> = Vec::new();
        let mut chars = message.chars().peekable();
        while let Some(ch) = chars.next() {
            match ch {
                ' ' => continue,
                '[' => {
                    let mut inner = String::new();
                    while let Some(c) = chars.next_if(|c| c != &']') {
                        inner.push(c);
                    }
                    chars.next();
                    patterns.push(HfsRegex::new(&format!("[{}]", inner)));
                },
                '{' => {
                    let mut inner = String::new();
                    while let Some(c) = chars.next_if(|c| c != &'}') {
                        inner.push(c);
                    }
                    chars.next();
                    patterns.push(HfsRegex::new(&format!("{{{}}}", inner)));
                },
                '*' => patterns.push(HfsRegex::new("*")),
                '.' => patterns.push(HfsRegex::new(".")),
                c => {
                    let mut text = String::from(c);
                    while let Some(c) = chars.next_if(|c| !matches!(c, ' ' | '[' | '{' | '*' | '.')) {
                        text.push(c);
                    }
                    patterns.push(HfsRegex::new(&text));
                },
            }
        }
        Box::new(move |msg| {
            let mut last_found_pos = 0;
            for pattern in &patterns {
                match pattern.matches(&msg[last_found_pos..]) {
                    Some(pos) => last_found_pos += pos,
                    None => return false,
                }
            }
            true
        })
    }
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
                                '?' => {
                                    let rest = &line_string[line_column..].trim_start_matches(' ');
                                    let (kind, msg) = rest.split_once(' ').unwrap_or(("", ""));
                                    match kind {
                                        "ERROR" => assertions.push(Assertion::Error(path, line_number + 1, msg.to_string())),
                                        _ => {},
                                    }
                                },
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

    use crate::hfscheck::hfscheck::{find_assertions, Assertion, HfsRegex};

    #[test]
    fn test_finds_error() {
        let path = PathBuf::from("tests/failure_tests/lexer/lexer1.hfs");
        let assertions = find_assertions(&path);

        assert_eq!(assertions.len(), 1);
        assert_eq!(assertions[0], Assertion::Error(&path, 1, String::from("\"unexpected character\" ")));
    }

    #[test]
    fn test_no_errors() {
        let path = PathBuf::from("tests/compile_tests/operations.hfs");
        let assertions = find_assertions(&path);
        assert_eq!(assertions.len(), 0);
    }

    #[test]
    fn test_regex_empty_matches_any() {
        let regex = HfsRegex::parse(String::from(""));
        assert!(regex(String::from("any message")));
        assert!(regex(String::from("any message works for this")));
        assert!(regex(String::new()));
    }

    #[test]
    fn test_regex_text_works_fuzzily() {
        let regex = HfsRegex::parse(String::from("error when stuff"));
        assert!(regex(String::from("error when performing stuff")));
        assert!(regex(String::from("error when doing stuff")));
        assert!(regex(String::from("you got an error when stuff")));
        assert!(regex(String::from("error when stuff went wrong")));
        assert!(!regex(String::from("error")));
    }

    #[test]
    fn test_regex_wildcard_matches_as_much_as_necessary() {
        let regex = HfsRegex::parse(String::from("error when * stuff"));
        assert!(regex(String::from("error when performing stuff")));
        assert!(regex(String::from("error when doing stuff")));
        assert!(!regex(String::from("error when doing things")));
        assert!(regex(String::from("error when stuff")));
    }

    #[test]
    fn test_regex_anychar_matches_a_single_character() {
        let regex = HfsRegex::parse(String::from(". error"));
        assert!(regex(String::from("a error")));
        assert!(regex(String::from("b error")));
        assert!(!regex(String::from("error")));
    }

    #[test]
    fn test_regex_range_matches_only_in_range() {
        let regex = HfsRegex::parse(String::from("[0-9]"));
        assert!(regex(String::from("0")));
        assert!(regex(String::from("9")));
        assert!(regex(String::from("5")));
        assert!(!regex(String::from("e")));
    }
    #[test]
    fn test_regex_list_matches_only_members() {
        let regex = HfsRegex::parse(String::from("{a,b,c}"));
        assert!(regex(String::from("a")));
        assert!(regex(String::from("b")));
        assert!(regex(String::from("c")));
        assert!(!regex(String::from("d")));
    }
}
