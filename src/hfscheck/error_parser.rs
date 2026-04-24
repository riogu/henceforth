use std::path::PathBuf;

use crate::hfscheck::hfscheck::HfsRegex;

pub struct ErrorParser {}

pub struct Test {
    pub path: PathBuf,
    pub message_check: Box<dyn Fn(String) -> bool + Send + Sync>,
    pub error_line_check: Box<dyn Fn(usize) -> bool + Send + Sync>,
}

impl Test {
    pub fn check(&self, message: String, line: usize) -> bool { (self.message_check)(message) && (self.error_line_check)(line) }
}

impl ErrorParser {
    pub fn generate_test(path: &'_ PathBuf, line_number: usize, error: String) -> Test {
        let mut chars_iter = error.chars().peekable();
        let mut error_line_check: Box<dyn Fn(usize) -> bool + Send + Sync> = Box::new(move |l| l == line_number);
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
                    message_check = HfsRegex::parse(message);
                },
                '@' =>
                    if let Some(_) = chars_iter.next_if_eq(&'[') {
                        let mut error_position = String::new();
                        while let Some(char) = chars_iter.next_if(|c| c != &']') {
                            error_position.push(char);
                        }
                        chars_iter.next();
                        error_line_check = ErrorParser::generate_error_line_check(error_position, line_number);
                    },
                c => panic!("found {c} in {path:?}"),
            }
        }

        return Test { path: path.clone(), message_check, error_line_check };
    }

    fn generate_error_line_check(error_position: String, line_number: usize) -> Box<dyn Fn(usize) -> bool + Send + Sync> {
        match error_position.chars().nth(0) {
            Some('^') => match &error_position[1..] {
                "*" => Box::new(move |pos| pos < line_number),
                offset => match offset.parse::<usize>() {
                    Ok(offset) => Box::new(move |pos| pos == line_number - offset),
                    Err(_) => panic!("invalid token for error line offset"),
                },
            },
            Some('v') => match &error_position[1..] {
                "*" => Box::new(move |pos| pos > line_number),
                offset => match offset.parse::<usize>() {
                    Ok(offset) => Box::new(move |pos| pos == line_number + offset),
                    Err(_) => panic!("invalid token for error line offset"),
                },
            },
            Some('.') => Box::new(move |pos| pos == line_number),
            Some(_) => panic!("invalid token for error line positioning"),
            None => Box::new(move |pos| pos == line_number),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::hfscheck::error_parser::ErrorParser;

    #[test]
    fn test_empty_error_matches_any_message() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 1, String::new());
        assert!((test.message_check)(String::from("any message")));
        assert!((test.message_check)(String::from("literally any message")));
        assert!((test.message_check)(String::new()));
    }

    #[test]
    fn test_empty_error_matches_exact_line() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 1, String::new());
        assert!((test.error_line_check)(1));
        assert!(!(test.error_line_check)(3));
        assert!(!(test.error_line_check)(5));
    }

    #[test]
    fn test_upstar_matches_any_before() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 50, String::from("@[^*]"));
        assert!((test.error_line_check)(30));
        assert!((test.error_line_check)(1));
        assert!((test.error_line_check)(15));
        assert!(!(test.error_line_check)(50));
        assert!(!(test.error_line_check)(70));
    }

    #[test]
    fn test_downstar_matches_any_after() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 50, String::from("@[v*]"));
        assert!((test.error_line_check)(60));
        assert!((test.error_line_check)(70));
        assert!((test.error_line_check)(150));
        assert!(!(test.error_line_check)(50));
        assert!(!(test.error_line_check)(10));
    }

    #[test]
    fn test_downn_matches_n_after() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 50, String::from("@[v5]"));
        assert!((test.error_line_check)(55));
    }

    #[test]
    fn test_upn_matches_n_before() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 50, String::from("@[^5]"));
        assert!((test.error_line_check)(45));
    }

    #[test]
    fn test_dot_matches_exact_line() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 50, String::from("@[.]"));
        assert!((test.error_line_check)(50));
    }

    #[test]
    fn test_empty_pos_matches_exact_line() {
        let path = PathBuf::new();
        let test = ErrorParser::generate_test(&path, 50, String::from("@[]"));
        assert!((test.error_line_check)(50));
    }
}
