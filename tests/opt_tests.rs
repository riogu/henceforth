use std::{error::Error, fs, io, path::PathBuf, rc::Rc};

use henceforth::{
    hfs::{error::DiagnosticInfo, parse, prettify_ir, print, IrArena, IrFuncId, SourceInfo},
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
        if test.settings.dump {
            if let Some(dump) = print(&funcs, &arena) {
                println!("Before optimizations:\n{}", prettify_ir(dump));
            }
        }
        for (pipeline, iterative) in test.settings.pipelines.iter_mut() {
            println!("Running pipeline {}", pipeline.name());
            if *iterative {
                pipeline.run_iteratively(&mut arena);
            } else {
                pipeline.run(&mut arena);
            }
        }
        if test.settings.dump {
            if let Some(dump) = print(&funcs, &arena) {
                println!("After optimizations:\n{}", prettify_ir(dump));
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
        let optimized_output = print(&funcs, &arena).unwrap();
        let input = TestInput::Ir(optimized_output.clone());
        if test.check(input) {
            Ok(())
        } else {
            Err(format!("invalid assertions\noptimized output:\n{}", prettify_ir(optimized_output)).into())
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
                let name = entry.to_str().unwrap().to_string() + ":" + &test.func_name;
                let trial = generate_test_function(test);
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
