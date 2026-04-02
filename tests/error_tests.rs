use std::{error::Error, fs, io};

use henceforth::{
    hfs::utils::{run_until, Phase},
    hfscheck::{error_parser::Test, hfscheck::generate_tests},
};
use libtest_mimic::{Arguments, Failed, Trial};

const FAILURE_TESTS_FOLDERS: &[(&str, Phase)] = &[
    ("tests/failure_tests/lexer", Phase::Lexer),
    ("tests/failure_tests/stack_analyzer", Phase::StackAnalyzer),
    ("tests/failure_tests/parser", Phase::Parser),
];

fn generate_test_function(test: Test, phase: Phase) -> Box<dyn Fn() -> Result<(), Failed> + Send + Sync> {
    Box::new(move || {
        if let Err(e) = run_until(test.path.to_str().unwrap(), phase) {
            if test.check(e.message().0, e.get_line()) {
                Ok(())
            } else {
                Err(format!("different error found {}", e).into())
            }
        } else {
            Err("no error found".into())
        }
    })
}

fn get_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    let mut trials = Vec::new();
    for folder in FAILURE_TESTS_FOLDERS {
        let entries = fs::read_dir(folder.0)?.map(|res| res.map(|e| e.path())).collect::<Result<Vec<_>, io::Error>>()?;
        for entry in entries {
            let tests = generate_tests(&entry);
            for test in tests {
                let trial = generate_test_function(test, folder.1);
                trials.push(Trial::test(entry.to_str().unwrap(), trial));
            }
        }
    }
    return Ok(trials);
}

fn main() {
    let args = Arguments::from_args();
    let tests = get_tests();
    match tests {
        Ok(tests) => libtest_mimic::run(&args, tests).exit(),
        Err(e) => panic!("{}", e),
    };
}
