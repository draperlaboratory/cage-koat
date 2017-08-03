type expexp =
    Pol of Poly.poly
  | Sum of expexp * expexp
  | Mul of expexp * expexp
  | Exp of expexp * expexp
val eq_big_int : Big_int.big_int -> Big_int.big_int -> bool
val zero : expexp
val one : expexp
val equal : expexp -> expexp -> bool
val equalInternal : expexp -> expexp -> bool
val fromConstant : Big_int.big_int -> expexp
val fromVar : Poly.var -> expexp
val fromPoly : Poly.poly -> expexp
val isConst : expexp -> bool
val getConstant : expexp -> Big_int.big_int
val getSummands : expexp -> expexp list
val getFactors : expexp -> expexp list
val toString : expexp -> string
val toPoly : expexp -> Poly.poly
val normalize : expexp -> expexp
val evalConsts : expexp -> expexp
val distribute : expexp -> expexp
val normalizeSummands : expexp list -> Poly.poly -> expexp list
val sumup : expexp list -> expexp
val sumupAux : expexp list -> expexp
val simplifyMul : expexp -> expexp -> expexp
val normalizeMultiplicants : expexp list -> Poly.poly -> expexp list
val getMultiplicants : expexp -> expexp list
val mulup : expexp list -> expexp
val mulupAux : expexp list -> expexp
val simplifyExp : expexp -> expexp -> expexp
val exp : expexp -> expexp -> expexp
val add : expexp -> expexp -> expexp
val constmult : expexp -> Big_int.big_int -> expexp
val minus : expexp -> expexp -> expexp
val negate : expexp -> expexp
val mult : expexp -> expexp -> expexp
val getVars : expexp -> Poly.var list
val isSumOfVarsPlusConstant : expexp -> bool
val isScaledSumOfVarsPlusConstant : expexp -> bool
val getScaleFactor : expexp -> Big_int.big_int
val abs : expexp -> expexp
val instantiate : expexp -> (Poly.var * expexp) list -> expexp
val getPolMap :
  (Poly.var * expexp) list -> (Poly.var * Poly.poly) list option
val instantiateAuxVarPower :
  (Poly.var * expexp) list -> Poly.var * int -> expexp
val max : expexp -> expexp -> expexp
val getMax : expexp -> expexp -> expexp
val remdup : expexp list -> expexp list
val getPoly : expexp -> Poly.poly option

type expDegree =
| Polynomial of int
| Exponential of int

val getDegree : expexp -> expDegree
val compare : expexp -> expexp -> int
