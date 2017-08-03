type var = string

val mkVar : string -> var
val stringOfVar : var -> string

type mapping = var * var

module VarMap : Map.S with type key = var

(* x ^ int *)
type monomial = (var * int) list

(* m * [x^n + x^n-1 .... + x] + c *)
type poly = (Big_int.big_int * monomial) list * Big_int.big_int

val eq_big_int : Big_int.big_int -> Big_int.big_int -> bool
val equal :  poly -> poly -> bool
val equalMono : Big_int.big_int * monomial -> Big_int.big_int * monomial -> bool
val equalMonoList : monomial -> monomial -> bool

val compareMonomial : monomial -> monomial -> int
val compare : poly -> poly -> int
val construct_poly : (Big_int.big_int * (String.t * int) list) list -> poly
val getNewMonomial : (var * int) list -> (var * int) list -> (var * int) list
val fromVarPower : var -> int -> poly
val fromVar : var -> poly
val zero : poly
val one : poly

val getCoeff : poly -> monomial -> Big_int.big_int
val getConstant : poly -> Big_int.big_int
val fromConstant : Big_int.big_int -> poly
val isVar : poly -> bool
val isUnivariateLinearMonomial : monomial -> bool
val isVarPlusConstant : poly -> bool
val isSumOfVarsPlusConstant : poly -> bool
val isScaledSumOfVarsPlusConstant : poly -> bool
val getScaleFactor : poly -> Big_int.big_int
val isUnivariateLinear : poly -> bool
val isConst : poly -> bool
val isLinear : poly -> bool
val toString : poly -> string
val toStringMonomial : monomial -> string
val toStringSimple : poly -> string
val abs : poly -> poly

val toSMT : poly -> string
val toSMTone : Big_int.big_int * (var * int) list -> string
val toSMTprods : string list -> string
val expand : string * int -> string list
val const_to_string : Big_int.big_int -> string
val renameVars : (var * var) list -> poly -> poly
val renameMonomial : (var * var) list -> monomial -> monomial
val getVars : poly -> var list
val hasVars : poly -> bool
val shareVars : poly -> poly -> bool
val getVarsMonomial : monomial -> var list
val constmult : poly -> Big_int.big_int -> poly

val add : poly -> poly -> poly
val minus : poly -> poly -> poly
val negate : poly -> poly
val mult : poly -> poly -> poly
val pow : poly -> Big_int.big_int -> poly

val instantiate : poly -> (var * poly) list -> poly
val evaluate : poly -> Big_int.big_int VarMap.t -> Big_int.big_int

val power : Big_int.big_int -> int -> Big_int.big_int
val replaceVarPower : poly -> var * int -> poly -> poly
val getDegree : poly -> int
val getDegreeMono : monomial -> int

val max : poly -> poly -> poly
val toVar : poly -> var
