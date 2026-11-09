use std::{error::Error, fs, io};

use henceforth::{
    hfs::utils::{Phase, run_until},
    hfscheck::hfscheck::{Test, TestInput, generate_tests},
};
use libtest_mimic::{Arguments, Failed, Trial};

const FAILURE_TESTS_FOLDERS: &[(&str, Phase)] = &[
    ("tests/failure_tests/lexer", Phase::Lexer),
    ("tests/failure_tests/stack_analyzer", Phase::StackAnalyzer),
    ("tests/failure_tests/parser", Phase::Parser),
    ("tests/failure_tests/stack_assist", Phase::StackAssist),
];

fn generate_test_function(test: Box<dyn Test + Send + Sync>, phase: Phase) -> Box<dyn Fn() -> Result<(), Failed> + Send + Sync> {
    Box::new(move || {
        if let Err(e) = run_until(test.path().to_str().unwrap(), phase, None) {
            if test.is_error() {
                if test.check(TestInput::Diagnostic(e.message(), e.get_line())) {
                    Ok(())
                } else {
                    Err(format!("different diagnostic found {}", e).into())
                }
            } else {
                let hints = e.hints();
                let hint = match hints.first() {
                    Some(first) => first,
                    None => panic!("[internal error] no hints found"),
                };
                if test.check(TestInput::Diagnostic(hint.message.clone(), e.get_line())) {
                    Ok(())
                } else {
                    Err(format!("different diagnostic found {}", e).into())
                }
            }
        } else {
            Err("no diagnostic found".into())
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
                let name = format!("{}:{}", if test.is_error() { "ERROR" } else { "HINT" }, entry.to_str().unwrap());
                let trial = generate_test_function(test, folder.1);
                trials.push(Trial::test(name, trial));
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
