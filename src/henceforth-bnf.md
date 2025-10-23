<program> ::= <top_level_node>*

<top_level_node> ::= <function_decl>

<function_decl> ::= "fn" <identifier> ":" <signature> "{" <block_scope> "}"

<signature> ::= "(" <type_list>? ")" "->" "(" <type_list>? ")"

<type_list> ::= <type> ("," <type>)*

<type> ::= "i32"

<block_scope> ::= <scope_item>*

<scope_item> ::= <var_decl>
               | <statement>

<var_decl> ::= "let" <identifier> ":" <type> ";"

<statement> ::= <if_stmt>
              | <stack_block>
              | <while_stmt>
              | <assignment>
              | <return_stmt>
              | <break_stmt>
              | <continue_stmt>
              | ";"

<if_stmt> ::= "if" <stack_block> "{" <block_scope> "}" <else_stmt>?

<else_stmt> ::= "else" "if" <stack_block> "{" <block_scope> "}" <else_stmt>?
              | "else" "{" <block_scope> "}"

<while_stmt> ::= "while" <stack_block> "{" <block_scope> "}"

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
