type atom =
    Equ of Poly.poly * Poly.poly
  | Neq of Poly.poly * Poly.poly
  | Geq of Poly.poly * Poly.poly
  | Gtr of Poly.poly * Poly.poly
  | Leq of Poly.poly * Poly.poly
  | Lss of Poly.poly * Poly.poly
type cond = atom list
val create :
  (string * (Big_int.big_int * (String.t * int) list) list *
   (Big_int.big_int * (String.t * int) list) list)
  list -> atom list
val construct_atom : string -> Poly.poly -> Poly.poly -> atom
val isLinear : atom list -> bool
val isLinearAtom : atom -> bool
val dropNonLinearAtoms : atom list -> atom list
val toString : atom list -> string
val toStringAtom : atom -> string
val compareAtom : atom -> atom -> int
val compare : atom list -> atom list -> int
val toDotString : atom list -> string
val toSMT : atom list -> string
val atomSMT : atom -> string
val renameVars : Poly.mapping list -> atom list -> atom list
val renameAtom : Poly.mapping list -> atom -> atom
val getVars : atom list -> Poly.var list
val getVarsAtom : atom -> Poly.var list
val isTrue : atom list -> Big_int.big_int Poly.VarMap.t -> bool
val isTrueAtom : Big_int.big_int Poly.VarMap.t -> atom -> bool
val instantiate : atom list -> (Poly.var * Poly.poly) list -> atom list
val instantiateOne : atom -> (Poly.var * Poly.poly) list -> atom
val equal : atom list -> atom list -> bool
val equalAtom : atom -> atom -> bool
val equalInternal : atom -> atom -> bool
val getLeqZeroTerms : atom -> Poly.poly list
val negateAtom : atom -> atom
(* cond list represents the disjuncts. *)
val negateCond : cond -> cond list
val andOverOr : cond list list -> cond list
val shareVars : Poly.poly -> cond -> bool
