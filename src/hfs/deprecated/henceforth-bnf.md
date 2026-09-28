```bnf
<program> ::= <top-level-node>*

<top-level-node> ::= <var-decl>
                   | <function-decl>
                   | <statement>

<var-decl> ::= "let" <identifier> ":" <type> ";"

<function-decl> ::= "fn" <identifier> ":" <signature> <block-scope> 

<signature> ::= "(" <type-list>? ")" "->" "(" <type-list>? ")"

<type-list> ::= <type> ("," <type>)*

<type> ::= "i32" | "f32" | "bool" | "string"

<statement> ::= <if-stmt>
              | <while-stmt>
              | <stack-block>
              | <block-scope>
              | <return-stmt>
              | <break-stmt>
              | <continue-stmt>
              | <assignment-stmt>
              | <function-call>
              | <stack-keyword>
              | ";"

<if-stmt> ::= "if" <stack-block> <block-scope> <else-stmt>?

<else-stmt> ::= "else" "if" <stack-block>  <block-scope> <else-stmt>?
              | "else" <block-scope> 

<while-stmt> ::= "while" <stack-block>  <block-scope> 

<stack-block> ::= "@" "(" <stack-expression>* ")"

<block-scope> ::= "{" <top-level-node>* "}"

<return-stmt> ::= "return" ";"

<break-stmt> ::= "break" ";"

<continue-stmt> ::= "continue" ";"

<assignment-stmt> ::= "&=" <identifier>
                    | ":=" <identifier>
<function-call> ::= "&>" <identifier>
                  | ":>" <identifier>

<stack-expression> ::= <stack-operation>
                     | <identifier>
                     | <literal>
                     | <tuple-expression>
                     | <stack-keyword>


<tuple-expression> ::= "(" <stack-expression>* ")"

<tuple-type> ::= "(" <type-list> ")"

<stack-operation> ::= <binary-op>
                    | <unary-op>

<binary-op> ::= "+" | "-" | "*" | "/" | ">" | "<" | "==" | "&&" | "||"

<unary-op> ::= "!" | "~"

<literal> ::= <number> | <float> | <boolean> | <string>

<identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<number> ::= [0-9]+

<float> ::= [0-9]+ "." [0-9]+

<boolean> ::= "true" | "false"

<string> ::= '"' [^"]* '"'

<stack-keyword> ::= @pop | @pop_all | @dup | @swap | @over | @rot | @rrot | @nip | @tuck
```
