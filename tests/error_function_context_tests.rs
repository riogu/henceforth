use std::process::Command;

fn henceforth_bin() -> &'static str { env!("CARGO_BIN_EXE_henceforth") }

fn write_fixture(name: &str, source: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(format!("hfs_errfunctest_{name}_{}.hfs", std::process::id()));
    std::fs::write(&path, source).unwrap_or_else(|e| panic!("failed to write fixture {name}: {e}"));
    path
}

#[test]
fn stack_analyzer_error_names_the_function_it_happened_in() {
    let source = r#"
fn add: (i32 i32) -> (i32) {
    @(+ "oops");
}

fn main: () -> () {
    @(1 2) &> add &> print;
}
"#;
    let fixture = write_fixture("stack_analyzer", source);
    let output = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "interpret"])
        .output()
        .expect("failed to run the interpreter backend");
    let _ = std::fs::remove_file(&fixture);

    assert!(!output.status.success(), "expected a type error");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("in add: (i32, i32) -> (i32)"), "expected the function name in the error, got: {stderr}");
}

#[test]
fn lexer_error_has_no_function_context() {
    let source = "fn main: () -> () {\n    /* never closes\n";
    let fixture = write_fixture("lexer", source);
    let output = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "interpret"])
        .output()
        .expect("failed to run the interpreter backend");
    let _ = std::fs::remove_file(&fixture);

    assert!(!output.status.success(), "expected an unterminated-comment error");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("unexpected EOF"), "expected the unterminated-comment panic, got: {stderr}");
    assert!(!stderr.contains(" in "), "a lexer error has no function context, got: {stderr}");
}
