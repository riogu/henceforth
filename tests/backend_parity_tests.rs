use std::{env, error::Error, fs, path::PathBuf, process::Command};

use libtest_mimic::{Arguments, Failed, Trial};

const FIXTURE_FOLDER: &str = "tests/compile_tests";

const SKIP: &[&str] = &["builtin_input", "print_and_input_str", "tictactoe-user-input", "new_whiles"];

const KNOWN_GAPS: &[&str] = &["bubble_sort"];

fn henceforth_bin() -> &'static str { env!("CARGO_BIN_EXE_henceforth") }

fn run(fixture: &str, args: &[&str]) -> Result<String, String> {
    let output = Command::new(henceforth_bin())
        .args(args)
        .output()
        .map_err(|e| format!("{fixture}: failed to spawn henceforth: {e}"))?;
    if !output.status.success() {
        return Err(format!("{fixture}: {} exited with {}: {}", args.join(" "), output.status, String::from_utf8_lossy(&output.stderr)));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn check_parity(fixture_path: PathBuf) -> Result<(), Failed> {
    let fixture = fixture_path.to_str().expect("fixture path must be utf-8");
    let interpret_out = run(fixture, &[fixture, "--backend", "interpret"])?;

    let binary = env::temp_dir().join(format!("hfs_parity_{}_{}", fixture_path.file_stem().unwrap().to_str().unwrap(), std::process::id()));
    let binary = binary.to_str().expect("temp path must be utf-8");
    run(fixture, &[fixture, "--backend", "cranelift", "-o", binary])?;
    let cranelift_out =
        Command::new(binary).output().map_err(|e| format!("{fixture}: failed to run compiled binary: {e}"))?;
    let _ = fs::remove_file(binary);
    let cranelift_stdout = String::from_utf8_lossy(&cranelift_out.stdout).into_owned();

    if interpret_out != cranelift_stdout {
        return Err(format!("{fixture}: backends disagree\n  interpret: {interpret_out:?}\n  cranelift: {cranelift_stdout:?}").into());
    }
    Ok(())
}

fn get_tests() -> Result<Vec<Trial>, Box<dyn Error>> {
    let mut trials = Vec::new();
    for entry in fs::read_dir(FIXTURE_FOLDER)? {
        let path = entry?.path();
        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else { continue };
        if SKIP.contains(&stem) || KNOWN_GAPS.contains(&stem) {
            continue;
        }
        let name = path.to_str().expect("fixture path must be utf-8").to_string();
        trials.push(Trial::test(name, move || check_parity(path)));
    }
    Ok(trials)
}

fn main() {
    let args = Arguments::from_args();
    match get_tests() {
        Ok(tests) => libtest_mimic::run(&args, tests).exit(),
        Err(e) => panic!("{e}"),
    }
}
