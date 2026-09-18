use std::process::Command;

fn henceforth_bin() -> &'static str { env!("CARGO_BIN_EXE_henceforth") }

fn write_fixture(name: &str, source: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(format!("hfs_externtest_{name}_{}.hfs", std::process::id()));
    std::fs::write(&path, source).unwrap_or_else(|e| panic!("failed to write fixture {name}: {e}"));
    path
}

const SOURCE: &str = r#"
extern fn rand: () -> (i32);
extern fn srand: (i32) -> ();

fn main: () -> () {
    @(1) &> srand;
    @() &> rand &> print;
}
"#;

#[test]
fn cranelift_calls_a_declared_extern_function() {
    let fixture = write_fixture("cranelift_ok", SOURCE);
    let binary = std::env::temp_dir().join(format!("hfs_externtest_cranelift_ok_{}_bin", std::process::id()));
    let compile_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "cranelift", "-o", binary.to_str().unwrap()])
        .output()
        .expect("failed to run the cranelift backend");
    assert!(compile_out.status.success(), "cranelift compile failed: {compile_out:?}");

    let run_out = Command::new(&binary).output().expect("failed to run compiled binary");
    let _ = std::fs::remove_file(&fixture);
    let _ = std::fs::remove_file(&binary);

    // glibc's rand(), seeded with srand(1), always returns 1804289383 as its first value.
    assert_eq!(String::from_utf8_lossy(&run_out.stdout), "1804289383");
}

#[test]
fn interpreter_refuses_to_call_an_extern_function() {
    let fixture = write_fixture("interpret_refuses", SOURCE);
    let run_out = Command::new(henceforth_bin())
        .args([fixture.to_str().unwrap(), "--backend", "interpret"])
        .output()
        .expect("failed to run the interpreter backend");
    let _ = std::fs::remove_file(&fixture);

    assert!(!run_out.status.success(), "expected the interpreter to refuse an extern function call");
    let stderr = String::from_utf8_lossy(&run_out.stderr);
    assert!(stderr.contains("doesn't support extern functions"), "expected the extern-fn panic, got: {stderr}");
}
