use std::{error::Error, fs, io};

use henceforth::{
    hfs::utils::{Phase, run_until},
    hfscheck::hfscheck::{Test, generate_tests},
};
use libtest_mimic::{Arguments, Failed, Trial};

const COMPILE_TEST_FOLDERS: &[&str] = &["tests/compile_tests"];

fn generate_test_function(test: Box<dyn Test + Send + Sync>, phase: Phase) -> Box<dyn Fn() -> Result<(), Failed> + Send + Sync> {
    Box::new(move || {
        if let Ok(_) = run_until(test.path().to_str().unwrap(), phase, None) {
            Ok(())
        } else {
            Err("did not compile".into())
        }
    })
}

fn get_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    let mut trials = Vec::new();
    for folder in COMPILE_TEST_FOLDERS {
        let entries = fs::read_dir(folder)?.map(|res| res.map(|e| e.path())).collect::<Result<Vec<_>, io::Error>>()?;
        dbg!(&entries);
        for entry in entries {
            let tests = generate_tests(&entry);
            for test in tests {
                let trial = generate_test_function(test, Phase::Interpreter);
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
