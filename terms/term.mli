type funSym = string
type term = funSym * Poly.poly list
val create :
  string ->
  (Big_int.big_int * (String.t * int) list) list list ->
  term
val compare : term -> term -> int
val toString : term -> string
val getArgs : term -> Poly.poly list
val getFun : term -> funSym
val getArity : 'a * 'b list -> int
val getVars : term -> Poly.var list
val instantiate : term -> (String.t * Poly.poly) list -> term
val renameVars :
  (Poly.var * Poly.var) list ->
  term -> term
val isLinear : term -> bool
val equal : term -> term -> bool
