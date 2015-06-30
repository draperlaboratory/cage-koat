type localcomplexity =
  Max of Big_int.big_int
| MaxPlusConstant of Big_int.big_int
| SumPlusConstant of Big_int.big_int
| ScaledSumPlusConstant of Big_int.big_int * Big_int.big_int
| P of Expexp.expexp
| Unknown

module type S =
    sig

      type r

      type size_data = localcomplexity * int list

      type local_size_data = r * ((int * int) * size_data)
      type lsds = local_size_data list
        
      val getE : localcomplexity * 'a -> Big_int.big_int
      val getS : localcomplexity * 'a -> Big_int.big_int
      val getConstant : localcomplexity * 'a -> Big_int.big_int
      val equal :
        localcomplexity * 'a list -> localcomplexity * 'a list -> bool
      val equalInternal :
        localcomplexity * 'a list -> localcomplexity * 'a list -> bool
      val equalVar : 'a list -> 'a list -> bool
      val isConstant : localcomplexity * 'a list -> bool
      val complexity2localcomplexity :
        Complexity.complexity -> Poly.var list -> size_data
      val getVarNumList : Poly.var list -> Poly.var list -> int -> int list
      val toSmallestComplexity :
        size_data -> Poly.var list -> Complexity.complexity
      val getSum : int list -> Poly.var list -> Expexp.expexp
      val add :
        size_data ->
        size_data ->
        Poly.var list -> size_data
      val addMax :
        Big_int.big_int ->
        int list ->
        size_data ->
        Poly.var list -> size_data
      val addMaxPlusConstant :
        Big_int.big_int ->
        int list ->
        size_data ->
        Poly.var list -> size_data
      val addSumPlusConstant :
        Big_int.big_int ->
        int list ->
        size_data ->
        Poly.var list -> size_data
      val addScaledSumPlusConstant :
        Big_int.big_int ->
        Big_int.big_int ->
        int list ->
        size_data ->
        Poly.var list -> size_data
      val addP :
        Expexp.expexp ->
        int list ->
        size_data ->
        Poly.var list -> size_data
      val disjoint : int list -> int list -> bool
      val unite : int list -> int list -> int list
      val addAsSmallestComplexities :
        size_data ->
        size_data ->
        Poly.var list -> size_data
      val addList :
        (size_data) list ->
        Poly.var list -> size_data
      val getMax :
        size_data ->
        size_data ->
        Poly.var list -> size_data
      val getExpexp : Complexity.complexity -> Expexp.expexp
      val listMax :
        (size_data) list ->
        Poly.var list -> size_data
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
      val computeLocalSizeComplexities : r list -> lsds

      val computeLocalSizeComplexitiesForRule : r -> lsds

      val computeLSCForTerm :
        Poly.var list ->
        (Poly.var * int) list ->
        Pc.atom list -> Pc.cond -> Poly.poly -> size_data
      val isMaxBound :
        Pc.atom list -> Poly.poly -> Big_int.big_int -> Poly.var list -> bool
      val isMaxPlusConstantBound :
        Pc.atom list -> Poly.poly -> Big_int.big_int -> Poly.var list -> bool
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
      val equalLSC :
        'a * (localcomplexity * 'b list) ->
        'a * (localcomplexity * 'b list) -> bool
      val dumpLSCs : lsds -> string
      val dumpOneLSC :
        local_size_data -> string
      val dumpLSC : (int * int) * (size_data) -> string
      val dumpLSCDot :
        local_size_data -> string
    end

module Make(RuleT : AbstractRule.AbstractRule) : S with type r = RuleT.rule
