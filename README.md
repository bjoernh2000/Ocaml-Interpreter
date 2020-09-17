Ocaml-Interpreter
===================
## What is it
This interpreter interprets low level commands, such as Add and Subtract. 
Furthermore, it allows the user to push and pop values from a stack, and 
also bind values to variables. You can create functions, which have scopes
that have variables that last until the end of the scope. 

Methods:

PushS  - String

PushN  - Name/Variable

PushB  - Boolean

PushU  - Unit

Add | Sub | Mul | Div | Rem | Neg  - Arithmetic operations of top elements on the stack

Pop  - Pop off Stack

Swap  - Swap top 2 elements on stack

Concat  - concat 2 strings on stack

And | Or | Not  - Boolean operations

Equal | LessThan  - Equate Numbers to Booleans

Bind  - Bind value to a name/variable

If  - Conditionals
 
Block  - A block that contains a list of commands

DefFun - Define function

Call  - Call function, and return to this point

Return  - Inside function, return the top elem on stack

DefInOut  - Function that also returns top element without return statement
