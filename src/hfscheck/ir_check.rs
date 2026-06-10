use std::path::PathBuf;

use crate::{
    hfs::{CleanCFG, DeadCodeElimination, IrPass, Mem2Reg, OptPipeline, RemoveStaleIR, O0},
    hfscheck::hfscheck::{CheckKind, HfsRegex, Test, TestInput},
};

pub struct IrCheck {}

pub struct OptSettings {
    pub pipelines: Vec<(Box<dyn OptPipeline + Send + Sync>, bool)>,
    pub dump: bool,
}

pub struct IrTest {
    pub func_name: String,
    pub settings: OptSettings,
    pub path: PathBuf,
    pub code_check: Box<dyn Fn(Vec<String>) -> bool + Send + Sync>,
}

impl Test for IrTest {
    fn check(&self, input: TestInput) -> bool {
        if let TestInput::Ir(code) = input {
            (self.code_check)(code.lines().map(String::from).collect())
        } else {
            false
        }
    }

    fn path(&self) -> &PathBuf { &self.path }
}

pub struct CustomPipeline {
    passes: Vec<Box<dyn IrPass + Send + Sync>>,
}

impl CustomPipeline {
    pub fn new(passes: Vec<Box<dyn IrPass + Send + Sync>>) -> Self { Self { passes } }
}

impl OptPipeline for CustomPipeline {
    fn name(&self) -> &str { "Custom pipeline for testing purposes" }
    fn get_opt_passes(&self) -> &[Box<dyn IrPass + Send + Sync>] { &self.passes }
}

impl IrCheck {
    pub fn find_function(content: &str, name: &str) -> Option<usize> {
        content.lines().position(|line| line.contains(&format!("fn {}", name)))
    }

    pub fn generate_test(
        path: &PathBuf,
        name: String,
        checks: Vec<CheckKind>,
        pipelines: Vec<(Box<dyn OptPipeline + Send + Sync>, bool)>,
        dump: bool,
    ) -> IrTest {
        let path_clone = path.clone();
        let name_clone = name.clone();
        let code_check: Box<dyn Fn(Vec<String>) -> bool + Send + Sync> = Box::new(move |lines: Vec<String>| {
            let current_content = lines.join("\n");
            let fn_line = match IrCheck::find_function(&current_content, &name_clone) {
                Some(idx) => idx,
                None => {
                    eprintln!("[ERROR] function {} not found in optimized output for {:?}", name_clone, path_clone);
                    return false;
                },
            };
            let mut previous = fn_line;
            for check in &checks {
                match check {
                    CheckKind::Plain(regex) => {
                        let regex = HfsRegex::parse(regex.clone());

                        let matched = lines[previous..].iter().position(|line| regex(line.clone()));
                        if matched.is_none() {
                            return false;
                        } else {
                            previous = previous + matched.unwrap();
                        }
                    },
                    CheckKind::Not(regex) => {
                        let regex = HfsRegex::parse(regex.clone());

                        let matched = lines[previous..].iter().position(|line| regex(line.clone()));
                        if matched.is_some() {
                            return false;
                        }
                    },
                    CheckKind::Block(regex) => {
                        let regex = HfsRegex::parse(regex.clone());

                        let matched = lines[previous..].iter().position(|line| {
                            let t = line.trim().trim_end_matches(':');
                            line.trim().ends_with(':') && regex(t.to_string())
                        });
                        if matched.is_none() {
                            return false;
                        } else {
                            previous = previous + matched.unwrap();
                        }
                    },
                    CheckKind::Next(regex) => {
                        let regex = HfsRegex::parse(regex.clone());

                        if !regex(lines[previous + 1].clone()) {
                            return false;
                        }
                    },
                    CheckKind::Same(regex) => {
                        let regex = HfsRegex::parse(regex.clone());

                        if !regex(lines[previous].clone()) {
                            return false;
                        }
                    },
                    CheckKind::Count(count, regex) => {
                        let regex = HfsRegex::parse(regex.clone());

                        let mut matched_count = 0;
                        let mut last_match = None;
                        for (i, line) in lines[previous..].iter().enumerate() {
                            if regex(line.clone()) {
                                matched_count += 1;
                                last_match = Some(previous + i);
                            }
                        }
                        if matched_count != *count {
                            return false;
                        }
                        previous = last_match.unwrap();
                    },
                }
            }
            true
        });

        IrTest { settings: OptSettings { pipelines, dump }, path: path.clone(), code_check, func_name: name }
    }

    pub fn parse_pipeline(pipeline: &String) -> (Box<dyn OptPipeline + Send + Sync>, bool, bool) {
        let mut chars = pipeline.chars().peekable();
        let mut iterative = false;
        let mut dump = false;
        let mut opts: Vec<Box<dyn IrPass + Send + Sync>> = Vec::new();
        let mut pipeline: Option<Box<dyn OptPipeline + Send + Sync>> = None;
        while let Some(ch) = chars.next() {
            match ch {
                ' ' => continue,
                '[' => {
                    let mut inner = Vec::new();
                    while let Some(c) = chars.next_if(|c| c != &']') {
                        inner.push(c);
                    }
                    let inner_str: String = inner.iter().collect();
                    let parsed_opts: Vec<&str> = inner_str.split(',').map(|p| p.trim()).filter(|p| !p.is_empty()).collect();
                    for opt in parsed_opts {
                        match opt {
                            "DCE" => opts.push(Box::new(DeadCodeElimination)),
                            "CleanCFG" => opts.push(Box::new(CleanCFG)),
                            "Mem2Reg" => opts.push(Box::new(Mem2Reg)),
                            "RemoveStaleIR" => opts.push(Box::new(RemoveStaleIR)),
                            opt => panic!("invalid optimization: {opt}"),
                        }
                    }
                    chars.next();
                },
                '-' => {
                    let mut inner = Vec::new();
                    while let Some(c) = chars.next_if(|c| c != &' ') {
                        inner.push(c);
                    }
                    let inner_str: &str = &inner.iter().collect::<String>();
                    match inner_str {
                        "iterative" => iterative = true,
                        "dump" => dump = true,
                        "O0" => pipeline = Some(Box::new(O0::new())),
                        "O1" => unimplemented!("-O1 not implemented yet"),
                        "O2" => unimplemented!("-O2 not implemented yet"),
                        "O3" => unimplemented!("-O3 not implemented yet"),
                        flag => panic!("invalid flag: {flag}"),
                    }
                    chars.next();
                },
                c => panic!("found {c} in opt pipeline"),
            }
        }
        if let Some(pipeline) = pipeline {
            return (pipeline, iterative, dump);
        } else {
            pipeline = Some(Box::new(CustomPipeline::new(opts)));
            return (pipeline.expect("expected pipeline to be created"), iterative, dump);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{
        hfs::O0,
        hfscheck::{
            hfscheck::{CheckKind, Test, TestInput},
            ir_check::IrCheck,
        },
    };

    #[test]
    fn test_iterative_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("-iterative"));
        assert!(matches!(pipeline, (_, true, _)))
    }

    #[test]
    fn test_dump_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("-dump"));
        assert!(matches!(pipeline, (_, _, true)))
    }

    #[test]
    fn test_o0_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("-O0"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["Mem2Reg", "DeadCodeElimination"]);
    }

    #[test]
    #[should_panic]
    fn test_o1_is_parsed() { IrCheck::parse_pipeline(&String::from("-O1")); }

    #[test]
    #[should_panic]
    fn test_o2_is_parsed() { IrCheck::parse_pipeline(&String::from("-O3")); }

    #[test]
    #[should_panic]
    fn test_o3_is_parsed() { IrCheck::parse_pipeline(&String::from("-O3")); }

    #[test]
    fn test_cleancfg_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("[CleanCFG]"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["CleanCFG"]);
    }

    #[test]
    fn test_dce_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("[DCE]"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["DeadCodeElimination"]);
    }

    #[test]
    fn test_mem2reg_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("[Mem2Reg]"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["Mem2Reg"]);
    }

    #[test]
    fn test_removestaleir_is_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("[RemoveStaleIR]"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["RemoveStaleIR"]);
    }

    #[test]
    fn test_multiple_opts_are_parsed() {
        let pipeline = IrCheck::parse_pipeline(&String::from("[RemoveStaleIR, DCE, Mem2Reg, CleanCFG]"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["RemoveStaleIR", "DeadCodeElimination", "Mem2Reg", "CleanCFG"]);
    }

    #[test]
    fn test_multiple_opts_are_parsed_and_iterative() {
        let pipeline = IrCheck::parse_pipeline(&String::from("[RemoveStaleIR, DCE, Mem2Reg, CleanCFG] -iterative"));
        assert_eq!(pipeline.1, true);
        assert_eq!(pipeline.0.get_pass_names(), vec!["RemoveStaleIR", "DeadCodeElimination", "Mem2Reg", "CleanCFG"]);
    }

    #[test]
    fn test_precedence_o0_wins() {
        let pipeline = IrCheck::parse_pipeline(&String::from("-O0 [RemoveStaleIR, DCE, Mem2Reg, CleanCFG]"));
        assert_eq!(pipeline.1, false);
        assert_eq!(pipeline.0.get_pass_names(), vec!["Mem2Reg", "DeadCodeElimination"]);
    }

    #[test]
    fn test_checks_match_only_below_fn() {
        let message = "fn other: () -> () {\nphi [...]\n}\nfn main: () -> () {\nbranch %0, a, b\n}";
        let path = PathBuf::new();
        let plain = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Plain(String::from("phi"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let not = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Not(String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let next = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Next(String::from("phi"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let same = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Same(String::from("phi"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let count = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Count(1, String::from("phi"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let block = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Block(String::from("phi"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(!plain.check(TestInput::Ir(message.to_string())));
        assert!(!not.check(TestInput::Ir(message.to_string())));
        assert!(!next.check(TestInput::Ir(message.to_string())));
        assert!(!same.check(TestInput::Ir(message.to_string())));
        assert!(!count.check(TestInput::Ir(message.to_string())));
        assert!(!block.check(TestInput::Ir(message.to_string())));
    }

    #[test]
    fn test_check_plain_matches_below_last() {
        let message = "fn main: () -> () {\nbranch %0, a, b;\n%2 = i32 %arg0\n}";
        let path = PathBuf::new();
        let test = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Plain(String::from("i32 %arg{{[0-9]}}")), CheckKind::Plain(String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(!test.check(TestInput::Ir(message.to_string())));
    }

    #[test]
    fn test_check_not_matches_below_last() {
        let message = "fn main: () -> () {\nbranch %0, a, b;\n%2 = i32 %arg0\n}";
        let path = PathBuf::new();
        let test = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Plain(String::from("i32 %arg{{[0-9]}}")), CheckKind::Not(String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(test.check(TestInput::Ir(message.to_string())));
    }

    #[test]
    fn test_check_block_only_checks_blocks() {
        let message1 = "fn main: () -> () {\nlabel:      \n}";
        let message2 = "fn main: () -> () {\nlabel      \n}";
        let path = PathBuf::new();
        let test1 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Block(String::from("label"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let test2 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Block(String::from("label"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(test1.check(TestInput::Ir(message1.to_string())));
        assert!(!test2.check(TestInput::Ir(message2.to_string())));
    }

    #[test]
    fn test_check_next_only_checks_next() {
        let message1 = "fn main: () -> () {\nbranch %0, a, b\n}";
        let message2 = "fn main: () -> () {\nphi [a, b]\nbranch %0, a, b\n}";
        let path = PathBuf::new();
        let test1 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Next(String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let test2 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Next(String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(test1.check(TestInput::Ir(message1.to_string())));
        assert!(!test2.check(TestInput::Ir(message2.to_string())));
    }

    #[test]
    fn test_check_count_only_matches_exact_count() {
        let message1 = "fn main: () -> () {\nbranch %0, a, b\n}";
        let message2 = "fn main: () -> () {\nbranch %0, a, c\nbranch %0, a, b\n}";
        let message3 = "fn main: () -> () {\nbranch %0, a, c\nbranch %0, a, b\nbranch %1, a, c\n}";
        let path = PathBuf::new();
        let test1 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Count(2, String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let test2 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Count(2, String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let test3 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Count(2, String::from("branch"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(!test1.check(TestInput::Ir(message1.to_string())));
        assert!(test2.check(TestInput::Ir(message2.to_string())));
        assert!(!test3.check(TestInput::Ir(message3.to_string())));
    }
    #[test]
    fn test_check_same_only_checks_same_line() {
        let message1 = "fn main: () -> () {}";
        let message2 = "fn other: () -> () {\nmain\n}";
        let path = PathBuf::new();
        let test1 = IrCheck::generate_test(
            &path,
            String::from("main"),
            vec![CheckKind::Same(String::from("main"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        let test2 = IrCheck::generate_test(
            &path,
            String::from("other"),
            vec![CheckKind::Same(String::from("main"))],
            vec![(Box::new(O0::new()), false)],
            false,
        );
        assert!(test1.check(TestInput::Ir(message1.to_string())));
        assert!(!test2.check(TestInput::Ir(message2.to_string())));
    }
}
