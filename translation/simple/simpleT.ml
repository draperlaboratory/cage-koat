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

let getindent indent = String.make indent ' '

(* Return the variables of a bexpr *)
let rec getVars c =
  match c with
    | True | False | BRandom -> []
    | Atom cc -> Pc.getVarsAtom cc
    | Not cc -> getVars cc
    | Or (c1, c2) | And (c1, c2) -> Utils.remdup ((getVars c1) @ (getVars c2))

(* Checks whether a bexpr is linear *)
let rec isLinear c =
  match c with
    | True | False | BRandom -> true
    | Atom cc -> Pc.isLinearAtom cc
    | Not cc -> isLinear cc
    | Or (c1, c2) | And (c1, c2) -> (isLinear c1) && (isLinear c2)
