#[cfg(test)]
mod tests {
    use henceforth::hfs::{
        ast::Type,
        builder::builder::{Builder, BuilderOperation, ControlFlowOps, FunctionOps, LoopOps, PassMode, StackOps, VariableOps},
        lexer_builder::TokenSequence,
        utils::{run_until, Phase},
    };

    use henceforth::hfs::{Token, TokenKind};

    pub fn tokenize_file_into_kinds(name: &str) -> Vec<TokenKind> {
        run_until(name, Phase::Lexer)
            .expect("compilation failed")
            .as_any()
            .downcast_ref::<Vec<Token>>()
            .expect("Expected Vec<Token> from Lexer")
            .clone()
            .into_iter()
            .map(|token| token.kind)
            .collect()
    }

    #[test]
    fn test_simple_main() {
        let tokens = tokenize_file_into_kinds("test/simple_main.hfs");
        let expected = TokenSequence::new().func_with("main", None, None).body().end_body().build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_function_with_lots_of_arguments() {
        let tokens = tokenize_file_into_kinds("test/function_with_lots_of_arguments.hfs");
        let expected = TokenSequence::new()
            .func_with(
                "func_with_lots_of_arguments",
                Some(vec![Type::new_int(0), Type::new_float(0), Type::new_string(0), Type::new_bool(0)]),
                Some(vec![Type::new_int(0), Type::new_float(0), Type::new_bool(0), Type::new_string(0)]),
            )
            .body()
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(5)
            .push_literal(5.0)
            .push_literal(false)
            .push_literal("test")
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_function_with_no_arguments() {
        let tokens = tokenize_file_into_kinds("test/function_with_no_arguments.hfs");
        let expected = TokenSequence::new()
            .func_with("no_args", None, Some(vec![Type::new_int(0)]))
            .body()
            .stack_block()
            .push_literal(4)
            .end_stack_block(true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_function_with_no_return_type() {
        let tokens = tokenize_file_into_kinds("test/function_with_no_return_type.hfs");
        let expected = TokenSequence::new()
            .func_with("no_return_type", Some(vec![Type::new_int(0)]), None)
            .body()
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_variable_declarations() {
        let tokens = tokenize_file_into_kinds("test/variable_declarations.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::new_int(0))
            .variable("b", Type::new_float(0))
            .variable("c", Type::new_string(0))
            .variable("d", Type::new_bool(0))
            .end_body()
            .build();

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_copy_and_move() {
        let tokens = tokenize_file_into_kinds("test/copy_and_move.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .variable("copy", Type::new_int(0))
            .variable("move", Type::new_int(0))
            .stack_block()
            .push_literal(5)
            .end_stack_block(false)
            .assign_to("copy", PassMode::Copy)
            .assign_to("move", PassMode::Move)
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_operations() {
        let tokens = tokenize_file_into_kinds("test/operations.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(1)
            .push_operation(BuilderOperation::Add)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Divide)
            .push_literal(2)
            .push_operation(BuilderOperation::Multiply)
            .push_literal(2)
            .push_operation(BuilderOperation::Modulo)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal(false)
            .end_stack_block(true)
            .push_stack_keyword("@dup", true)
            .stack_block()
            .push_operation(BuilderOperation::Or)
            .push_operation(BuilderOperation::Not)
            .push_literal(true)
            .push_operation(BuilderOperation::And)
            .end_stack_block(true)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_while_loop() {
        let tokens = tokenize_file_into_kinds("test/while_loop.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .variable("a", Type::new_int(0))
            .variable("b", Type::new_int(0))
            .stack_block()
            .push_literal(100)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_literal(0)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .while_loop()
            .stack_block()
            .push_variable("a")
            .push_literal(0)
            .push_operation(BuilderOperation::GreaterThan)
            .push_variable("b")
            .push_literal(200)
            .push_operation(BuilderOperation::LessThan)
            .push_operation(BuilderOperation::And)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_variable("a")
            .push_literal(1)
            .push_operation(BuilderOperation::Subtract)
            .end_stack_block(false)
            .assign_to("a", PassMode::Move)
            .stack_block()
            .push_variable("b")
            .push_literal(2)
            .push_operation(BuilderOperation::Add)
            .end_stack_block(false)
            .assign_to("b", PassMode::Move)
            .end_body()
            .return_statement()
            .end_body()
            .build();
        assert_eq!(tokens, expected)
    }

    #[test]
    fn test_simple_if_else() {
        let tokens = tokenize_file_into_kinds("test/simple_if_else.hfs");
        let expected = TokenSequence::new()
            .func_with("main", None, None)
            .body()
            .if_statement()
            .stack_block()
            .push_literal(5)
            .push_literal(2)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal(1)
            .push_literal(2)
            .push_literal(3)
            .push_literal(4)
            .end_stack_block(true)
            .push_stack_keyword("@pop_all", true)
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .return_statement()
            .end_body()
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_if_elif_else() {
        let tokens = tokenize_file_into_kinds("test/if_elif_else.hfs");
        let expected = TokenSequence::new()
            .func_with("fizz_buzz", Some(vec![Type::new_int(0)]), Some(vec![Type::new_string(0)]))
            .body()
            .if_statement()
            .stack_block()
            .push_literal(15)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(3)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("fizz")
            .end_stack_block(true)
            .end_body()
            .elif_statement()
            .stack_block()
            .push_literal(5)
            .push_operation(BuilderOperation::Modulo)
            .push_literal(0)
            .push_operation(BuilderOperation::Equals)
            .end_stack_block(false)
            .body()
            .stack_block()
            .push_literal("buzz")
            .end_stack_block(true)
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@pop", true)
            .stack_block()
            .push_literal("no fizzbuzz")
            .end_stack_block(true)
            .end_body()
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(530)
            .end_stack_block(false)
            .call_function("fizz_buzz", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_copy_and_move_func_calls() {
        let tokens = tokenize_file_into_kinds("test/copy_and_move_func_calls.hfs");
        let expected = TokenSequence::new()
            .func_with("max", Some(vec![Type::new_int(0), Type::new_int(0)]), Some(vec![Type::new_int(0)]))
            .body()
            .if_statement()
            .stack_block()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@dup", false)
            .push_stack_keyword("@rot", false)
            .push_operation(BuilderOperation::GreaterThan)
            .end_stack_block(false)
            .body()
            .return_statement()
            .end_body()
            .else_statement()
            .body()
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .end_body()
            .func_with("max3", Some(vec![Type::new_int(0), Type::new_int(0), Type::new_int(0)]), Some(vec![Type::new_int(0)]))
            .body()
            .push_stack_keyword("@rrot", true)
            .call_function("max", PassMode::Move)
            .call_function("max", PassMode::Copy)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", false)
            .push_stack_keyword("@swap", false)
            .push_stack_keyword("@pop", true)
            .end_body()
            .func_with("main", None, None)
            .body()
            .stack_block()
            .push_literal(50)
            .push_literal(10)
            .push_literal(30)
            .end_stack_block(false)
            .call_function("max3", PassMode::Move)
            .push_stack_keyword("@pop", true)
            .end_body()
            .build();
        assert_eq!(tokens, expected)
    }
}
