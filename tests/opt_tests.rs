use std::{error::Error, fs, io, path::PathBuf, rc::Rc};

use henceforth::{
    hfs::{error::DiagnosticInfo, parse, print, IrArena, IrFuncId, SourceInfo},
    hfscheck::{
        hfscheck::{generate_ir_tests, Test, TestInput},
        ir_check::IrTest,
    },
};
use libtest_mimic::{Arguments, Failed, Trial};

const OPT_TESTS_FOLDERS: &[&str] = &["tests/opt_tests"];

fn run_with_opts(test: &mut IrTest) -> (IrArena, Vec<IrFuncId>) {
    let contents = fs::read_to_string(test.path.clone()).expect("Could not read file");

    let mut arena = IrArena::new(Rc::new(DiagnosticInfo::new(PathBuf::new(), SourceInfo::new(0, 0, 0))));
    if let Some(funcs) = parse(&contents, &mut arena) {
        for (pipeline, iterative) in test.settings.pipelines.iter_mut() {
            if *iterative {
                pipeline.run_iteratively(&mut arena);
            } else {
                pipeline.run(&mut arena);
            }
        }
        (arena, funcs)
    } else {
        panic!("could not parse")
    }
}

pub fn generate_test_function(test: Box<IrTest>) -> Box<dyn Fn() -> Result<(), Failed> + Send + Sync> {
    let test = std::sync::Mutex::new(test);
    Box::new(move || {
        let mut test = test.lock().unwrap();
        let (arena, funcs) = run_with_opts(&mut test);
        let input = TestInput::Ir(print(&funcs, &arena).unwrap());
        if test.check(input) {
            Ok(())
        } else {
            Err("invalid assertions".into())
        }
    })
}

fn get_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    let mut trials = Vec::new();
    for folder in OPT_TESTS_FOLDERS {
        let entries = fs::read_dir(folder)?.map(|res| res.map(|e| e.path())).collect::<Result<Vec<_>, io::Error>>()?;
        for entry in entries {
            let tests = generate_ir_tests(&entry);
            for test in tests {
                let trial = generate_test_function(test);
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
