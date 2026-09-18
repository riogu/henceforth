use std::process::Command;

fn henceforth_bin() -> &'static str { env!("CARGO_BIN_EXE_henceforth") }

fn write_fixture(name: &str, source: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(format!("hfs_printtest_{name}_{}.hfs", std::process::id()));
    std::fs::write(&path, source).unwrap_or_else(|e| panic!("failed to write fixture {name}: {e}"));
    path
}

#[test]
fn printing_an_array_with_a_folded_length_matches_both_backends() {
    let source = r#"
fn main: () -> () {
    let m: i32; @(5) &= m;
    let arr: [m]i32;
    @(9 0) [&]= arr;
    @(8 1) [&]= arr;
    @(7 2) [&]= arr;
    @(6 3) [&]= arr;
    @(5 4) [&]= arr;
    @(arr) &> print;
}
"#;
    let fixture = write_fixture("folded_length", source);

    let interpret_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "interpret"])
        .output()
        .expect("failed to run the interpreter backend");
    assert!(interpret_out.status.success(), "interpreter backend failed: {interpret_out:?}");

    let binary = std::env::temp_dir().join(format!("hfs_printtest_folded_length_{}_bin", std::process::id()));
    let compile_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "cranelift", "-o", binary.to_str().unwrap()])
        .output()
        .expect("failed to run the cranelift backend");
    assert!(compile_out.status.success(), "cranelift compile failed: {compile_out:?}");
    let run_out = Command::new(&binary).output().expect("failed to run compiled binary");

    let _ = std::fs::remove_file(&fixture);
    let _ = std::fs::remove_file(&binary);

    let interpret_stdout = String::from_utf8_lossy(&interpret_out.stdout);
    let cranelift_stdout = String::from_utf8_lossy(&run_out.stdout);
    assert_eq!(interpret_stdout, "[9, 8, 7, 6, 5]");
    assert_eq!(cranelift_stdout, "[9, 8, 7, 6, 5]");
}

#[test]
fn printing_a_decayed_parameter_directly_is_a_compile_error() {
    let source = r#"
fn show: ([]i32) -> () {
    &> print;
}

fn main: () -> () {
    let arr: [3]i32;
    @(1 0) [&]= arr;
    @(2 1) [&]= arr;
    @(3 2) [&]= arr;
    @(arr) &> show;
}
"#;
    let fixture = write_fixture("decayed_param", source);
    let binary = std::env::temp_dir().join(format!("hfs_printtest_decayed_param_{}_bin", std::process::id()));
    let compile_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "cranelift", "-o", binary.to_str().unwrap()])
        .output()
        .expect("failed to run the cranelift backend");
    let _ = std::fs::remove_file(&fixture);
    let _ = std::fs::remove_file(&binary);

    assert!(!compile_out.status.success(), "expected compilation to fail for a print of unknown length");
    let stderr = String::from_utf8_lossy(&compile_out.stderr);
    assert!(stderr.contains("unknown length"), "expected the unknown-length panic, got: {stderr}");
}
