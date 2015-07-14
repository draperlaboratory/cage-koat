type rule = {
  lhs : Term.term;
  rhss : Term.term list;
  cond : Pc.cond;
}
val createRule : Term.term -> Term.term list -> Pc.cond -> rule

val toString : rule -> string
val toDotString : rule -> string
val listToStringPrefix : string -> rule list -> string

val compare : rule -> rule -> int
val equal : rule -> rule -> bool

val getLeft : rule -> Term.term
val getRights : rule -> Term.term list
val getCond : rule -> Pc.cond
val getFuns : rule -> Term.funSym list
val getLeftFun : rule -> Term.funSym
val getRightFuns : rule -> Term.funSym list
val getRightVars : rule -> Poly.var list
val getVars : rule -> Poly.var list
val renameVars : Poly.var list -> rule -> rule
val createVarMapping : Poly.var list -> Poly.var list ->
  (Poly.var * Poly.var) list
val getNewVarName : Poly.var list -> Poly.var -> Poly.var
val isLinear : rule -> bool
val isRightLinear : rule -> bool
val isConstraintLinear : rule -> bool
val satisfiesVarCond : rule -> bool
val internalize : rule -> rule
val getSubstitution : Poly.var list -> Pc.cond -> Poly.var list ->
  (string * Poly.poly) list * Pc.cond
val findDefinition :
  Poly.var -> Poly.var list -> Pc.cond -> Pc.atom option -> Pc.atom option
val isEqu : Pc.atom -> bool
val hasUnitCoeff : Pc.atom -> Poly.var -> bool
val extract : Poly.var -> Pc.atom -> Poly.poly
val remove : Pc.cond -> Pc.atom -> Pc.cond
val isUnary : rule -> bool
val instantiate : rule -> (Poly.var * Poly.poly) list -> rule
val chainTwoRules : rule -> rule -> rule
val removeNeq : rule -> rule list
val restrictArguments : int list -> rule -> rule
