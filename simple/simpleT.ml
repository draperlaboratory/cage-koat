type combine =
  Stmts
| Ctrls
| IfsLoops
| Loops

type bexpr =
  True
| False
| BRandom
| Atom of Pc.atom
| Not of bexpr
| Or of bexpr * bexpr
| And of bexpr * bexpr

type nexpr = Poly.poly

type statement =
  Skip
| Halt
| Assume of bexpr
| Random of string
| Assign of string * nexpr
| ITE of bexpr * (statement list) * (statement list)
| While of bexpr * (statement list)
| Call of string option * string * string list
| Dummy1 of bexpr
| Dummy2 of string list
| Dummy3 of string

type fun_decl = string * string list * string option * string list * statement list

type program = fun_decl list * string list * statement list
