<program> ::= <function>*

<function> ::= "fn" <identifier> ":" <signature> "{" <statement>* "}"

<signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"

<type_list> ::= <type> ("," <type>)*

<type> ::= "i32"

<statement> ::= <let_decl>
              | <assignment>
              | <stack_block>
              | <while_loop>
              | <if_stmt>
              | <return_stmt>
              | <function_call>

<let_decl> ::= "let" <identifier> ":" <type> ";"

<assignment> ::= "&=" <identifier> ";"
               | <stack_expr> ":=" <identifier> ";"

<stack_block> ::= "@" "(" <stack_expr> ")" ";"?

<stack_expr> ::= <expr> (<expr>)*

<expr> ::= <number>
         | <identifier>
         | <binary_op>
         | <stack_op>

<binary_op> ::= "+"
              | "-"
              | "*"
              | "/"
              | ">"
              | "<"
              | "=="

<stack_op> ::= "dup"
             | "pop"
             | "depth"

<while_loop> ::= "while" <stack_block> "{" <statement>* "}"

<if_stmt> ::= "if" <stack_block> "{" <statement>* "}" <else_clause>?

<else_clause> ::= "else" "if" <stack_block> "{" <statement>* "}" <else_clause>?
                | "else" "{" <statement>* "}"

<return_stmt> ::= <stack_block> "return" ";"

<function_call> ::= <identifier> ";"

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<number> ::= [0-9]+
