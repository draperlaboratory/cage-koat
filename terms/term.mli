type funSym = string

type pos = int

type term = { fn : funSym; args : Poly.poly list }

val create :
  funSym ->
  (Big_int.big_int * (String.t * int) list) list list ->
  term
val create' : funSym * Poly.poly list -> term
val compare : term -> term -> int
val toString : term -> string
val getArgs : term -> Poly.poly list
val getFun : term -> funSym
val getArity : term -> int
val getVars : term -> Poly.var list
val instantiate : term -> (Poly.var * Poly.poly) list -> term
val renameVars : Poly.mapping list -> term -> term
val isLinear : term -> bool
val equal : term -> term -> bool
val makeFreshVar : unit -> Poly.var
val makeFreshVarMap : Poly.var list -> (Poly.var * Poly.poly) list
