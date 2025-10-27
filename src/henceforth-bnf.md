<program> ::= <top_level_node>*

<top_level_node> ::= <var_decl>
                   | <function_decl>
                   | <statement>

<var_decl> ::= "let" <identifier> ":" <type> ";"

<function_decl> ::= "fn" <identifier> ":" <signature> <block_scope> 

<signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"

<type_list> ::= <type> ("," <type>)*

<type> ::= "i32" | "f32" | "bool" | "string"

<statement> ::= <if_stmt>
              | <while_stmt>
              | <stack_block>
              | <block_scope>
              | <return_stmt>
              | <break_stmt>
              | <continue_stmt>
              | <assignment_stmt>
              | ";"

<if_stmt> ::= "if" <stack_block> <block_scope> <else_stmt>?

<else_stmt> ::= "else" "if" <stack_block>  <block_scope> <else_stmt>?
              | "else" <block_scope> 

<while_stmt> ::= "while" <stack_block>  <block_scope> 

<stack_block> ::= "@" "(" <stack_expression>* ")"

<block_scope> ::= "{" <top_level_node>* "}"

<return_stmt> ::= "return" ";"

<break_stmt> ::= "break" ";"

<continue_stmt> ::= "continue" ";"

<assignment_stmt> ::= "&=" <identifier>
                    | ":=" <identifier>

<stack_expression> ::= <stack_operation>
                     | <identifier>
                     | <literal>
                     | <function_call_or_tuple_expr>

<function_call_or_tuple_expr> ::= <tuple_expression> <identifier>?

<tuple_expression> ::= "(" <stack_expression>* ")"

<tuple_type> ::= "(" <type_list> ")"

<stack_operation> ::= <binary_op>
                    | <unary_op>

<binary_op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"

<unary_op> ::= "!" | "~"

<literal> ::= <number> | <float> | <boolean> | <string>

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<number> ::= [0-9]+

<float> ::= [0-9]+ "." [0-9]+

<boolean> ::= "true" | "false"

<string> ::= '"' [^"]* '"'
