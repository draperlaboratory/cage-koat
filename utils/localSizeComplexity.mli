type localcomplexity =
  Max of Big_int.big_int
| MaxPlusConstant of Big_int.big_int
| SumPlusConstant of Big_int.big_int
| ScaledSumPlusConstant of Big_int.big_int * Big_int.big_int
| P of Expexp.expexp
| Unknown

type size_data = { bound : localcomplexity; active_vars : Term.pos list }

val create_size_data : localcomplexity * Term.pos list -> size_data
val unknown_size_data : size_data

type index = { rhsIdx : int; varIdx : Term.pos }

type local_size_data = index * size_data

module type S =
    sig

      module RuleT : AbstractRule.AbstractRule

      type trans_data = RuleT.rule * local_size_data
      type tds = trans_data list
        
      val getE : size_data -> Big_int.big_int
      val getS : size_data -> Big_int.big_int
      val getConstant : size_data -> Big_int.big_int
      val equal : size_data -> size_data -> bool
      val isConstant : size_data -> bool
      val complexity2localcomplexity :
        Complexity.t -> Poly.var list -> size_data
      val getVarNumList : Poly.var list -> Poly.var list -> int -> int list
      val toSmallestComplexity :
        size_data -> Poly.var list -> Complexity.t
      val getSum : int list -> Poly.var list -> Expexp.expexp
      val add : size_data -> size_data ->  Poly.var list -> size_data
      val addMax : Big_int.big_int -> int list -> size_data -> Poly.var list -> size_data
      val addMaxPlusConstant : Big_int.big_int -> int list -> size_data -> Poly.var list -> size_data
      val addSumPlusConstant : Big_int.big_int -> int list -> size_data -> Poly.var list -> size_data
      val addScaledSumPlusConstant : Big_int.big_int -> Big_int.big_int -> int list -> size_data ->
        Poly.var list -> size_data
      val addP : Expexp.expexp -> int list -> size_data -> Poly.var list -> size_data
      val disjoint : int list -> int list -> bool
      val unite : int list -> int list -> int list
      val addAsSmallestComplexities : size_data -> size_data -> Poly.var list -> size_data
      val addList : size_data list -> Poly.var list -> size_data
      val getMax : size_data -> size_data -> Poly.var list -> size_data
      val getExpexp : Complexity.t -> Expexp.expexp
      val listMax : size_data list -> Poly.var list -> size_data
      val toStringLocalComplexity : size_data -> string
      val varString : int list -> string
      val maxBound : Poly.var ref
      val maxPlusConstantBound : Poly.var ref
      val maxC : Big_int.big_int
      val maxS : Big_int.big_int
      val eConstant : Big_int.big_int ref
      val sConstant : Big_int.big_int ref
      (* This produces one 
         (rule, ((rhsIdx, argumentIdx), (local size bound, active variable idxs)))
         tuple per RV *)
      val computeLocalSizeComplexities : RuleT.rule list -> tds

      val computeLocalSizeComplexitiesForRule : RuleT.rule -> tds

      val computeLSCForTerm :
        Poly.var list ->
        (Poly.var * int) list ->
        Pc.atom list -> Pc.cond -> Poly.poly -> size_data
      val isMaxBound : Pc.atom list -> Poly.poly -> Big_int.big_int -> Poly.var list -> bool
      val isMaxPlusConstantBound : Pc.atom list -> Poly.poly -> Big_int.big_int -> Poly.var list
        -> bool
      val minimize :
        (Poly.var list -> bool) ->
        Poly.var list -> Poly.var list -> Poly.var list
      val minimizeC :
        (Big_int.big_int -> bool) ->
        Big_int.big_int -> Big_int.big_int -> Big_int.big_int
      val getDeps : Poly.var list -> Pc.atom list -> Poly.var list
      val getDepsFix :
        Poly.var list -> Poly.var list -> Pc.atom list -> Poly.var list
      val addOneDepLevel :
        Poly.var list -> Pc.atom list -> Poly.var list -> Poly.var list
      val getNew : Poly.var list -> Poly.var list -> Poly.var list
      val getVarNums : (Poly.var * int) list -> Poly.var list -> int list
      val equalLSC : local_size_data -> local_size_data -> bool
      val dumpLSCs : tds -> string
      val dumpOneLSC : trans_data -> string
      val dumpLSC : local_size_data -> string
      val dumpLSCDot : trans_data -> string
    end

module Make(RuleT : AbstractRule.AbstractRule) : S with module RuleT = RuleT
