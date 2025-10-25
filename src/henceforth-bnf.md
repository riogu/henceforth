<program> ::= <top_level_node>*

<top_level_node> ::= <function_decl>

<function_decl> ::= "fn" <identifier> ":" <signature> "{" <scope_block> "}"

<signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"

<type_list> ::= <type> ("," <type>)*

<type> ::= "i32"

<scope_block> ::= (<var_decl> | <statement>)*

<var_decl> ::= "let" <identifier> ":" <type> ";"

<statement> ::= <if_stmt>
              | <stack_block>
              | <while_stmt>
              | <assignment>
              | <return_stmt>
              | <break_stmt>
              | <continue_stmt>
              | ";"

<if_stmt> ::= "if" <stack_block> "{" <scope_block> "}" <else_stmt>?

<else_stmt> ::= "else" "if" <stack_block> "{" <scope_block> "}" <else_stmt>?
              | "else" "{" <scope_block> "}"

<while_stmt> ::= "while" <stack_block> "{" <scope_block> "}"

<stack_block> ::= "@" "(" <expression_list> ")"

<expression_list> ::= <expression> (<expression>)*

<assignment> ::= "&=" <identifier> ";"
               | ":=" <identifier> ";"

<return_stmt> ::= "return" ";"

<break_stmt> ::= "break" ";"

<continue_stmt> ::= "continue" ";"

<expression> ::= <operation>
               | <identifier>
               | <literal>

<operation> ::= <binary_op>
              | <unary_op>
              | <stack_op>

<binary_op> ::= "+"
              | "-"
              | "*"
              | "/"
              | ">"
              | "<"
              | "=="
              | "&&"
              | "||"

<unary_op> ::= "!"

<stack_op> ::= "dup"
             | "pop"
             | "depth"

<literal> ::= <number>

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<number> ::= [0-9]+
