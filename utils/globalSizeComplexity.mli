

module Make :
  functor (CTRSObl : Ctrsobl.S) ->
    sig
      module LSC : LocalSizeComplexity.S with module RuleT = CTRSObl.CTRS.RuleT

      module RVMap : Map.S with type key = (CTRSObl.CTRS.RuleT.rule * (int * int))

      val c2lsc : Complexity.complexity -> Poly.var list -> LSC.size_data
      val getPol : Complexity.complexity -> Expexp.expexp
      val gscForNonTrivialScc :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))))
        array ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list * 'c))
        array ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list *
         LSC.size_data)
        list -> LSC.size_data
      val getVarsizeProduct :
        CTRSObl.t ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        'b list list -> Complexity.complexity
      val getVarsizeFactor :
        CTRSObl.t ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) ->
        'b list -> Complexity.complexity
      val getScaleProduct :
        CTRSObl.t ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        'b list list -> Complexity.complexity
      val getScaleFactor :
        CTRSObl.t ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) ->
        'b list -> Complexity.complexity
      val isTooBig : CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> bool
      val isPossiblyScaledSumPlusConstant :
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> bool
      val isMaxPlusConstant :
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> bool
      val isMax : CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> bool
      val get_v_beta :
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))))
        array ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> 'b list
      val get_v_beta_nums :
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list -> 'b list
      val inScc :
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> bool
      val isNone : 'a option -> bool
      val getAsPol :
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> LSC.size_data
      val getGSC :
        ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list *
         LSC.size_data)
        list ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        LSC.size_data
      val getGSCForOne :
        ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list *
         LSC.size_data)
        list ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) -> LSC.size_data
      val getMaxPlusConstantTerm :
        CTRSObl.t ->
        Poly.var list ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) ->
        Complexity.complexity
      val getPossiblyScaledSumPlusConstantTerm :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))))
        array ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        Poly.var list ->
        ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list *
         LSC.size_data)
        list ->
        CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list)) ->
        'b list -> Complexity.complexity
      val getTermForSum :
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        Poly.var list ->
        ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list *
         LSC.size_data)
        list -> 'b -> Complexity.complexity
      val takeout :
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list
      val getCondensedPreds :
        Tgraph.G.t *
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list * 'c))
        array ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list list ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list list
      val getCondensedNums :
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list * 'c))
        array ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list list ->
        int list
      val computeCondensedPreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list * 'c))
        array -> int -> int list ref -> int list ref -> unit
      val hasCondensedEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list * 'c))
        array -> int -> int -> bool
      val getSccsNums :
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list * 'c))
        array ->
        int list ->
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list list
      val collectPreds :
        ('a * (('b * 'c) * 'd)) list ->
        'c list -> ('a * (('b * 'c) * 'd)) list list
      val getPredVarBounds :
        (CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list list ->
        Poly.var list ->
        ((CTRSObl.CTRS.RuleT.rule * (('a * 'b) * (Rvgraph.lc * 'b list))) list *
         LSC.size_data)
        list -> Complexity.complexity list list
      val computeGlobalSizeBound :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (CTRSObl.CTRS.RuleT.rule * (('a * int) * (Rvgraph.lc * int list))))
        array ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         ((CTRSObl.CTRS.RuleT.rule * (('a * int) * (Rvgraph.lc * int list))) list * 'b))
        array ->
        (CTRSObl.CTRS.RuleT.rule * (('a * int) * LSC.size_data)) list ->
        bool ->
        ((CTRSObl.CTRS.RuleT.rule * (('a * int) * LSC.size_data)) list * LSC.size_data)
        list -> LSC.size_data
      val compute :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (CTRSObl.CTRS.RuleT.rule * ((int * int) * (Rvgraph.lc * int list))))
        array -> LSC.size_data RVMap.t
      val empty : 'a RVMap.t
      val findEntry :
        LSC.size_data RVMap.t ->
        CTRSObl.CTRS.RuleT.rule -> int -> int -> Poly.var list -> Complexity.complexity
      val extractSizeMapForRule :
        LSC.size_data RVMap.t ->
        CTRSObl.CTRS.RuleT.rule ->
        int -> Poly.var list -> (string * Complexity.complexity) list
      val extractSizeMapForRuleForVars :
        LSC.size_data RVMap.t ->
        CTRSObl.CTRS.RuleT.rule ->
        int -> Poly.var list -> (Poly.var * Complexity.complexity) list
      val dumpGSCs :
        (CTRSObl.CTRS.RuleT.rule * ((int * int) * LSC.size_data)) list -> string
      val getSizeComplexitiesForRule :
        CTRSObl.CTRS.RuleT.rule ->
        LSC.size_data RVMap.t ->
        (Poly.var list -> Complexity.complexity) list
      val printSizeComplexities :
        CTRSObl.t -> LSC.size_data RVMap.t -> string
    end
