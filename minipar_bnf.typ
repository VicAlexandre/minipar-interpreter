#show raw: set text(font: "Fira Code", size: 7.2pt)

= Gramática do Minipar 2024.2 - Referência
#v(1cm)

```haskell
<program>         ::= <stmts>

<stmts>           ::= <stmt> <stmts>
                    | ε

<stmt>            ::= <compound_stmt>
                    | <simple_stmt>

<simple_stmt>     ::= <declaration>
                    | <assignment>
                    | <return_stmt>
                    | "break"
                    | "continue"

<compound_stmt>   ::= <function_stmt>
                    | <if_stmt>
                    | <for_stmt>
                    | <while_stmt>
                    | <seq_stmt>
                    | <par_stmt>
                    | <channel_stmt>

<assignment>      ::= ID "=" <expression>

<declaration>     ::= ID ":" type "=" <expression>

<return_stmt>     ::= "return" <expression>

<expression>      ::= <disjunction>

<block>           ::= "{" <stmts> "}"

<function_stmt>   ::= "func" ID "(" <parameters> ")" "->" type <block>

<parameters>      ::= <params>
                    | ε

<params>          ::= <param> "," <params>
                    | <param>

<param>           ::= ID ":" type <dflt>

<dflt>            ::= "=" <disjunction>
                    | ε

<if_stmt>         ::= "if" "(" <expression> ")" <block> <else_block>

<else_block>      ::= "else" <block>
                    | ε

<for_stmt> ::=    "for" "(" <assignment> ";" <expression> ";" <assignment> ")" <block>

<while_stmt>     ::= "while" "(" <expression> ")" <block>

<seq_stmt>       ::= "seq" <block>

<par_stmt>       ::= "par" <block>

<channel_stmt>   ::= <s_channel_stmt>
                  | <c_channel_stmt>

<c_channel_stmt> ::= "c_channel" ID "{" STRING "," NUMBER "}"

<disjunction>    ::= <disjunction> "||" <conjunction>
                  | <conjunction>

<conjunction>    ::= <conjunction> "&&" <equality>
                  | <equality>

<equality>       ::= <equality> ("==" | "!=") <comparison>
                  | <comparison>

<comparison>     ::= <comparison> (">" | "<" | ">=" | "<=") <sum>
                  | <sum>

<sum>            ::= <sum> ("+" | "-") <term>
                  | <term>

<term>           ::= <term> ("*" | "/" | "%") <unary>
                  | <unary>

<unary>          ::= ("!" | "-") <unary>
                  | <primary>

<local>          ::= <local> "." ID
                  | <local> <index>
                  | <local> "(" <arguments> ")"
                  | ID

<index>          ::= "[" NUMBER "]"

<arguments>      ::= <args>
                  | ε

<args>           ::= <args> "," <expression>
                  | <expression>

<primary>        ::= "(" <disjunction> ")"
                  | <local>
                  | STRING
                  | NUMBER
                  | "true"
                  | "false"

<type>          ::= "string"
                  | "number"
                  | "bool"
```
