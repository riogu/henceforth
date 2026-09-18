use std::{path::PathBuf, process::Command};

fn henceforth_bin() -> &'static str { env!("CARGO_BIN_EXE_henceforth") }

fn write_fixture(name: &str, source: &str) -> PathBuf {
    let path = std::env::temp_dir().join(format!("hfs_reftest_{name}_{}.hfs", std::process::id()));
    std::fs::write(&path, source).unwrap_or_else(|e| panic!("failed to write fixture {name}: {e}"));
    path
}

fn run_both_backends(name: &str, source: &str) -> (String, String, i32) {
    let fixture = write_fixture(name, source);

    let interpret_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "interpret"])
        .output()
        .expect("failed to run the interpreter backend");
    assert!(interpret_out.status.success(), "{name}: interpreter backend failed: {interpret_out:?}");

    let binary = std::env::temp_dir().join(format!("hfs_reftest_{name}_{}_bin", std::process::id()));
    let compile_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "cranelift", "-o", binary.to_str().unwrap()])
        .output()
        .expect("failed to run the cranelift backend");
    assert!(compile_out.status.success(), "{name}: cranelift compile failed: {compile_out:?}");

    let run_out = Command::new(&binary).output().unwrap_or_else(|e| panic!("{name}: failed to run compiled binary: {e}"));

    let _ = std::fs::remove_file(&fixture);
    let _ = std::fs::remove_file(&binary);

    (
        String::from_utf8_lossy(&interpret_out.stdout).into_owned(),
        String::from_utf8_lossy(&run_out.stdout).into_owned(),
        run_out.status.code().unwrap_or(-1),
    )
}

#[test]
fn array_mutated_through_reference_without_return() {
    let source = r#"
fn bubble_sort: ([]i32 i32) -> () {
    let N: i32; &= N;
    let arr: [N]i32; &= arr;
    let i: i32; @(0) &= i;

    while @(i N !=) {
        let j: i32; @(0) &= j;
        while @(j 4 i - !=) {
            if @(arr j [] arr j 1 + [] >) {
                let tmp: i32; @(arr j []) &= tmp;
                @(arr j 1 + [] j) [&]= arr;
                @(tmp j 1 +) [&]= arr;
            }
            @(j 1 +) &= j;
        }
        @(i 1 +) &= i;
    }
}

fn main: () -> () {
    let arr: [5]i32;
    @(5 0) [&]= arr;
    @(3 1) [&]= arr;
    @(4 2) [&]= arr;
    @(1 3) [&]= arr;
    @(2 4) [&]= arr;

    @(arr 5) &> bubble_sort;

    @(arr 0 []) &> print;
    @(arr 1 []) &> print;
    @(arr 2 []) &> print;
    @(arr 3 []) &> print;
    @(arr 4 []) &> print;
}
"#;
    let (interpret_stdout, cranelift_stdout, exit_code) = run_both_backends("mutate_without_return", source);
    assert_eq!(exit_code, 0);
    assert_eq!(interpret_stdout, "12345");
    assert_eq!(cranelift_stdout, "12345");
}

#[test]
fn discarding_a_forwarded_array_reference_does_not_crash() {
    let source = r#"
fn identity: ([]i32 i32) -> ([]i32) {
    @pop;
    @();
}

fn main: () -> () {
    let src: [3]i32;
    @(1 0) [&]= src;
    @(src 3) &> identity;
    @pop;
    @(src 0 []) &> print;
}
"#;
    let (interpret_stdout, cranelift_stdout, exit_code) = run_both_backends("discard_forwarded_ref", source);
    assert_eq!(exit_code, 0);
    assert_eq!(interpret_stdout, "1");
    assert_eq!(cranelift_stdout, "1");
}

#[test]
fn move_assignment_aliases_the_same_memory() {
    let source = r#"
fn main: () -> () {
    let arr: [3]i32;
    @(9 0) [&]= arr;
    @(0 1) [&]= arr;
    @(0 2) [&]= arr;
    let arr2: [3]i32; @(arr) &= arr2;
    @(5 1) [&]= arr2;
    @(arr 1 []) &> print;
}
"#;
    let (interpret_stdout, cranelift_stdout, exit_code) = run_both_backends("alias_shared_memory", source);
    assert_eq!(exit_code, 0);
    assert_eq!(interpret_stdout, "5");
    assert_eq!(cranelift_stdout, "5");
}

#[test]
fn nested_array_with_variable_sized_dimension_shared_across_functions_is_a_clear_error() {
    let source = r#"
let M: i32; let N: i32;

fn touches_row: ([M][N]i32) -> () {
    @(0 [] 0 []) &> print;
}

fn main: () -> () {
    @(2) &= M; @(3) &= N;
    let arena: [M][N]i32;
    @([[1 2 3] [4 5 6]]) &= arena;
    @(arena) &> touches_row;
}
"#;
    let fixture = write_fixture("nested_symbolic_dim_shared", source);
    let binary = std::env::temp_dir().join(format!("hfs_reftest_nested_symbolic_dim_shared_{}_bin", std::process::id()));
    let compile_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "cranelift", "-o", binary.to_str().unwrap()])
        .output()
        .expect("failed to run the cranelift backend");
    let _ = std::fs::remove_file(&fixture);
    let _ = std::fs::remove_file(&binary);

    assert!(!compile_out.status.success(), "expected a variable-sized array dimension shared across functions to be rejected");
    let stderr = String::from_utf8_lossy(&compile_out.stderr);
    assert!(
        stderr.contains("sized by a variable rather than a literal") || stderr.contains("known at compile time"),
        "expected the array-sizing panic, got: {stderr}"
    );
}
