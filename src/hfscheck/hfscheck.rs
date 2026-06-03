use std::{fs, path::PathBuf};

use crate::{
    hfs::{OptPipeline, O0},
    hfscheck::{error_parser::ErrorParser, ir_check::IrCheck},
};

#[derive(Debug, PartialEq, Clone)]
pub enum CheckKind {
    Plain(String),
    Block(String),
    Next(String),
    Same(String),
    Not(String),
    Count(usize, String),
}

#[derive(Debug, PartialEq)]
pub enum Assertion<'a> {
    Error(&'a PathBuf, usize, String),
    CheckFn { path: &'a PathBuf, name: String, checks: Vec<CheckKind>, pipelines: Vec<String> },
}

#[derive(Debug)]
pub enum HfsRegex {
    Text(String),
    SingleChar,
    Wildcard,
    Range(char, char),
    List(Vec<char>),
}

pub enum TestInput {
    Error(String, usize),
    Ir(String),
}

pub trait Test {
    fn check(&self, input: TestInput) -> bool;
    fn path(&self) -> &PathBuf;
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
            text if text.starts_with('<') && text.ends_with('>') => {
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
                '[' =>
                    if chars.next_if(|c| c == &'[').is_some() {
                        let mut inner = String::new();
                        while let Some(c) = chars.next_if(|c| c != &']') {
                            inner.push(c);
                        }
                        chars.next();
                        chars.next();
                        todo!("[[]] variable handling")
                    },

                '{' =>
                    if chars.next_if(|c| c == &'{').is_some() {
                        let inner_chars = {
                            let mut inner = String::new();
                            loop {
                                match chars.next() {
                                    Some('}') if chars.peek() == Some(&'}') => {
                                        chars.next();
                                        break;
                                    },
                                    Some(c) => inner.push(c),
                                    None => break,
                                }
                            }
                            inner
                        };
                        let mut inner = inner_chars.chars().peekable();
                        while let Some(ch) = inner.next() {
                            match ch {
                                ' ' => continue,
                                '*' => patterns.push(HfsRegex::Wildcard),
                                '.' => patterns.push(HfsRegex::SingleChar),
                                '[' => {
                                    let mut range = String::new();
                                    while let Some(c) = inner.next_if(|c| c != &']') {
                                        range.push(c);
                                    }
                                    inner.next();
                                    patterns.push(HfsRegex::new(&format!("[{}]", range)));
                                },
                                '<' => {
                                    let mut list = String::new();
                                    while let Some(c) = inner.next_if(|c| c != &'>') {
                                        list.push(c);
                                    }
                                    inner.next();
                                    patterns.push(HfsRegex::new(&format!("<{}>", list)));
                                },
                                c => {
                                    let mut text = String::from(c);
                                    while let Some(c) = inner.next_if(|c| !matches!(c, ' ' | '[' | '*' | '.')) {
                                        text.push(c);
                                    }
                                    patterns.push(HfsRegex::new(&text));
                                },
                            }
                        }
                    },
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

pub fn find_assertions<'a>(path: &'a PathBuf, text: String) -> Vec<Assertion<'a>> {
    let comment_lines = text.lines().map(String::from).collect::<Vec<String>>();
    let mut assertions: Vec<Assertion> = Vec::new();
    let mut curr_check_fn = None;
    let mut curr_opt_pipelines: Vec<String> = Vec::new();
    for (line_number, line_string) in comment_lines.iter().enumerate() {
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
                                        "CHECK" => {
                                            let (check_kind, msg) = msg.split_once(' ').unwrap_or((msg, ""));
                                            let kind = match check_kind {
                                                "FN" => {
                                                    if let Some(pending) = curr_check_fn.take() {
                                                        assertions.push(pending);
                                                    }

                                                    curr_check_fn = Some(Assertion::CheckFn {
                                                        path,
                                                        name: msg.trim_matches('"').to_string(),
                                                        checks: Vec::new(),
                                                        pipelines: curr_opt_pipelines.clone(),
                                                    });
                                                    continue;
                                                },
                                                "BLOCK" => CheckKind::Block(msg.trim_matches('"').trim().to_string()),
                                                "NEXT" => CheckKind::Next(msg.trim_matches('"').trim().to_string()),
                                                "SAME" => CheckKind::Same(msg.trim_matches('"').trim().to_string()),
                                                "NOT" => CheckKind::Not(msg.trim_matches('"').trim().to_string()),
                                                "COUNT" => {
                                                    let (n, rest) = msg.split_once(' ').unwrap_or(("0", ""));
                                                    CheckKind::Count(
                                                        n.parse().unwrap_or(0),
                                                        rest.trim_matches('"').trim().to_string(),
                                                    )
                                                },
                                                _ => CheckKind::Plain(check_kind.trim_matches('"').trim().to_string()),
                                            };
                                            if let Some(Assertion::CheckFn { checks, .. }) = curr_check_fn.as_mut() {
                                                checks.push(kind);
                                            }
                                        },
                                        "OPT" => {
                                            curr_opt_pipelines.push(msg.to_string());
                                        },
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
    if let Some(pending) = curr_check_fn.take() {
        assertions.push(pending);
    }

    return assertions;
}

pub fn generate_tests(path: &PathBuf) -> Vec<Box<dyn Test + Send + Sync>> {
    let assertions = find_assertions(path, fs::read_to_string(path).expect("Could not read file."));
    let mut tests: Vec<Box<dyn Test + Send + Sync>> = Vec::new();
    for assertion in assertions {
        match assertion {
            Assertion::Error(path, line, error) => tests.push(Box::new(ErrorParser::generate_test(path, line, error))),
            Assertion::CheckFn { path, name, checks, pipelines } => {
                let mut parsed_pipelines: Vec<(Box<dyn OptPipeline + Send + Sync>, bool)> =
                    pipelines.iter().map(IrCheck::parse_pipeline).collect();
                if parsed_pipelines.len() == 0 {
                    parsed_pipelines = vec![(Box::new(O0::new()), false)]
                }
                tests.push(Box::new(IrCheck::generate_test(path, name, checks, parsed_pipelines)));
            },
        }
    }
    tests
}

#[cfg(test)]
mod assertion_tests {
    use std::{fs, path::PathBuf};

    use crate::hfscheck::hfscheck::{find_assertions, Assertion, CheckKind};

    #[test]
    fn test_finds_error() {
        let path = PathBuf::from("tests/failure_tests/lexer/lexer1.hfs");
        let assertions = find_assertions(&path, fs::read_to_string(&path).expect("Could not read file."));

        assert_eq!(assertions.len(), 1);
        assert_eq!(assertions[0], Assertion::Error(&path, 1, String::from("\"unexpected character\" ")));
    }

    #[test]
    fn test_no_errors() {
        let path = PathBuf::from("tests/compile_tests/operations.hfs");
        let assertions = find_assertions(&path, fs::read_to_string(&path).expect("Could not read file."));
        assert_eq!(assertions.len(), 0);
    }

    #[test]
    fn test_finds_empty_checkfn() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, name, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(name, "test");
        assert!(checks.is_empty());
    }

    #[test]
    fn test_two_checkfns_finds_two_checks() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK FN \"other_test\"".to_string());
        assert_eq!(assertions.len(), 2);
    }
    #[test]
    fn test_two_checkfns_finds_different_checks_inside() {
        let path = PathBuf::new();
        let assertions = find_assertions(
            &path,
            "//? CHECK FN \"test\"\n//? CHECK COUNT 1 \"test\"\n//? CHECK FN \"other_test\"\n//? CHECK \"other_test\"\n"
                .to_string(),
        );
        assert_eq!(assertions.len(), 2);
        let Assertion::CheckFn { name: name1, checks: checks1, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        let Assertion::CheckFn { name: name2, checks: checks2, .. } = &assertions[1] else {
            panic!("expected CheckFn")
        };
        assert_eq!(name1, "test");
        assert_eq!(checks1.len(), 1);
        assert_eq!(name2, "other_test");
        assert_eq!(checks2.len(), 1);
        assert!(matches!(&checks1[0], CheckKind::Count(1, s) if s == "test"));
        assert!(matches!(&checks2[0], CheckKind::Plain(s) if s == "other_test"));
    }

    #[test]
    fn test_no_checkfn_finds_no_checks() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK \"test\"".to_string());
        assert_eq!(assertions.len(), 0);
    }

    #[test]
    fn test_finds_check_plain() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(checks.len(), 1);
        assert!(matches!(&checks[0], CheckKind::Plain(s) if s == "test"));
    }

    #[test]
    fn test_finds_check_next() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK NEXT \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(checks.len(), 1);
        assert!(matches!(&checks[0], CheckKind::Next(s) if s == "test"));
    }

    #[test]
    fn test_finds_check_same() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK SAME \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(checks.len(), 1);
        assert!(matches!(&checks[0], CheckKind::Same(s) if s == "test"));
    }

    #[test]
    fn test_finds_check_count() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK COUNT 5 \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(checks.len(), 1);
        assert!(matches!(&checks[0], CheckKind::Count(5, s) if s == "test"));
    }

    #[test]
    fn test_finds_check_block() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK BLOCK \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(checks.len(), 1);
        assert!(matches!(&checks[0], CheckKind::Block(s) if s == "test"));
    }

    #[test]
    fn test_finds_check_not() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? CHECK FN \"test\"\n//? CHECK NOT \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { checks, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(checks.len(), 1);
        assert!(matches!(&checks[0], CheckKind::Not(s) if s == "test"));
    }

    #[test]
    fn test_finds_opt() {
        let path = PathBuf::new();
        let assertions = find_assertions(&path, "//? OPT -O0 -iterative\n//? CHECK FN \"test\"".to_string());
        assert_eq!(assertions.len(), 1);
        let Assertion::CheckFn { pipelines, .. } = &assertions[0] else {
            panic!("expected CheckFn")
        };
        assert_eq!(pipelines.len(), 1);
        assert_eq!(&pipelines[0], "-O0 -iterative");
    }
}

#[cfg(test)]
mod regex_tests {
    use crate::hfscheck::hfscheck::HfsRegex;

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
        let regex = HfsRegex::parse(String::from("error when {{*}} stuff"));
        assert!(regex(String::from("error when performing stuff")));
        assert!(regex(String::from("error when doing stuff")));
        assert!(!regex(String::from("error when doing things")));
        assert!(regex(String::from("error when stuff")));
    }

    #[test]
    fn test_regex_anychar_matches_a_single_character() {
        let regex = HfsRegex::parse(String::from("{{.}} error"));
        assert!(regex(String::from("a error")));
        assert!(regex(String::from("b error")));
        assert!(!regex(String::from("error")));
    }

    #[test]
    fn test_regex_range_matches_only_in_range() {
        let regex = HfsRegex::parse(String::from("{{[0-9]}}"));
        assert!(regex(String::from("0")));
        assert!(regex(String::from("9")));
        assert!(regex(String::from("5")));
        assert!(!regex(String::from("e")));
    }
    #[test]
    fn test_regex_list_matches_only_members() {
        let regex = HfsRegex::parse(String::from("{{<a,b,c>}}"));
        assert!(regex(String::from("a")));
        assert!(regex(String::from("b")));
        assert!(regex(String::from("c")));
        assert!(!regex(String::from("d")));
    }
}
