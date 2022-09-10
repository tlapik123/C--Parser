# A simple parser for even simpler version of C

## Syntax
```hs
data Statement
    = Variable Name Type -- Name = Value;
    | Assgn Name Name Op Name -- Name = Name Op Name;
    | IfElse Comparison [Statement] [Statement] -- if (Comp) {Statements} else {Statements}
    | While Comparison [Statement] -- while (Comp) {Statements}
    | For Statement Comparison Statement [Statement] -- For (Variable; Comp; Assg;) {Statements}
    deriving (Eq, Show)
```
* accepted syntax follows ^ `Statement` data
  * declaring a variable
    * `Name = Value;`
    * value will be automatically given a type
  * Assignment
    * `Name = Name Op Name`
  * IfElse
    * `if (Comparison) {Statements} else {Statements}`
  * While
    * `while (Comparison) {Statements}`
  * For
    * `for (Variable; Comparison; Assignment;) {Statements}`
* Operand
  * `+`, `-`, `*`, `/`
* Comparison
  * `!=`, `==`

## Usage
* use `cabal repl` to open an interactive session for the project
* use `run parseStatement "text"` to parse given text as a statement
  * the parsed text needs to be without newlines
