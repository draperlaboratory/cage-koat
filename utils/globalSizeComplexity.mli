
module type S =
    sig
      module CTRSObl : Ctrsobl.S
      
      module LSC : LocalSizeComplexity.S with module RuleT = CTRSObl.CTRS.RuleT

      module RVMap : Map.S with type key = (CTRSObl.CTRS.RuleT.rule * (int * int))

      type size_data = Rvgraph.lc * int list

      type rule = CTRSObl.CTRS.RuleT.rule
      type global_trans_data = rule * ((int * int) * size_data)
        

      val c2lsc : Complexity.complexity -> Poly.var list -> LSC.size_data
      val getPol : Complexity.complexity -> Expexp.expexp
      val gscForNonTrivialScc :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         global_trans_data)
        array ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (global_trans_data list * 'c))
        array ->
        global_trans_data list ->
        (global_trans_data list *
         LSC.size_data)
        list -> LSC.size_data
      val getVarsizeProduct :
        CTRSObl.t ->
        global_trans_data list ->
        int list list -> Complexity.complexity
      val getVarsizeFactor :
        CTRSObl.t ->
        global_trans_data ->
        int list -> Complexity.complexity
      val getScaleProduct :
        CTRSObl.t ->
        global_trans_data list ->
        int list list -> Complexity.complexity
      val getScaleFactor :
        CTRSObl.t ->
        global_trans_data ->
        int list -> Complexity.complexity
      val isTooBig : global_trans_data -> bool
      val isPossiblyScaledSumPlusConstant :
        global_trans_data -> bool
      val isMaxPlusConstant :
        global_trans_data -> bool
      val isMax : global_trans_data -> bool
      val get_v_beta :
        Tgraph.G.t *
        (Tgraph.G.vertex *
         global_trans_data)
        array ->
        global_trans_data list ->
        global_trans_data -> int list
      val get_v_beta_nums :
        global_trans_data list -> int list
      val inScc :
        global_trans_data list ->
        global_trans_data -> bool
      val isNone : 'a option -> bool
      val getAsPol :
        global_trans_data -> LSC.size_data
      val getGSC :
        (global_trans_data list *
         LSC.size_data)
        list ->
        global_trans_data list ->
        LSC.size_data
      val getGSCForOne :
        (global_trans_data list *
         LSC.size_data)
        list ->
        global_trans_data -> LSC.size_data
      val getMaxPlusConstantTerm :
        CTRSObl.t ->
        Poly.var list ->
        global_trans_data ->
        Complexity.complexity
      val getPossiblyScaledSumPlusConstantTerm :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         global_trans_data)
        array ->
        global_trans_data list ->
        Poly.var list ->
        (global_trans_data list *
         LSC.size_data)
        list ->
        global_trans_data ->
        int list -> Complexity.complexity
      val getTermForSum :
        global_trans_data list ->
        Poly.var list ->
        (global_trans_data list *
         LSC.size_data)
        list -> int -> Complexity.complexity
      val takeout :
        global_trans_data list ->
        global_trans_data list ->
        global_trans_data list
      val getCondensedPreds :
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (global_trans_data list * 'c))
        array ->
        global_trans_data list list ->
        global_trans_data list list
      val getCondensedNums :
        (Tgraph.G.vertex *
         (global_trans_data list * 'c))
        array ->
        global_trans_data list list ->
        int list
      val computeCondensedPreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         (global_trans_data list * 'c))
        array -> int -> int list ref -> int list ref -> unit
      val hasCondensedEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         (global_trans_data list * 'c))
        array -> int -> int -> bool
      val getSccsNums :
        (Tgraph.G.vertex *
         (global_trans_data list * 'c))
        array ->
        int list ->
        global_trans_data list list
      val collectPreds :
        ('a * (('b * 'c) * 'd)) list ->
        'c list -> ('a * (('b * 'c) * 'd)) list list
      val getPredVarBounds :
        global_trans_data list list ->
        Poly.var list ->
        (global_trans_data list *
         LSC.size_data)
        list -> Complexity.complexity list list
      val computeGlobalSizeBound :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         global_trans_data)
        array ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (global_trans_data list * int))
        array ->
        (rule * ((int * int) * LSC.size_data)) list ->
        bool ->
        ((rule * ((int * int) * LSC.size_data)) list * LSC.size_data)
        list -> LSC.size_data
      val compute :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         global_trans_data)
        array -> LSC.size_data RVMap.t
      val empty : 'a RVMap.t
      val findEntry :
        LSC.size_data RVMap.t ->
        rule -> int -> int -> Poly.var list -> Complexity.complexity
      val extractSizeMapForRule :
        LSC.size_data RVMap.t ->
        rule ->
        int -> Poly.var list -> (string * Complexity.complexity) list
      val extractSizeMapForRuleForVars :
        LSC.size_data RVMap.t ->
        rule ->
        int -> Poly.var list -> (Poly.var * Complexity.complexity) list
      val dumpGSCs :
        (rule * ((int * int) * LSC.size_data)) list -> string
      val getSizeComplexitiesForRule :
        rule ->
        LSC.size_data RVMap.t ->
        (Poly.var list -> Complexity.complexity) list
      val printSizeComplexities :
        CTRSObl.t -> LSC.size_data RVMap.t -> string
    end

module Make(CTRSObl : Ctrsobl.S) : S with module CTRSObl = CTRSObl
