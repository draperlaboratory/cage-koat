
module LC = LocalSizeComplexity

module type S =
    sig
      module CTRSObl : Ctrsobl.S
      
      module LSC : LC.S with module RuleT = CTRSObl.CTRS.RuleT

      module RVMap : Map.S with type key = (CTRSObl.CTRS.RuleT.rule * LC.index)

      type size_data = LC.size_data
      type rule = CTRSObl.CTRS.RuleT.rule
      type trans_data = LSC.trans_data
      type tds = LSC.tds

      val c2lsc : Complexity.t -> Poly.var list -> LC.size_data
      val getPol : Complexity.t -> Expexp.expexp
      val gscForNonTrivialScc : CTRSObl.t ->
        Tgraph.G.t * (Tgraph.G.vertex * trans_data) array ->
        Tgraph.G.t * (Tgraph.G.vertex * (tds * 'c)) array ->
        tds ->
        (tds * LC.size_data) list -> LC.size_data
      val getVarsizeProduct : CTRSObl.t -> tds -> int list list -> Complexity.t
      val getVarsizeFactor : CTRSObl.t -> trans_data -> int list -> Complexity.t
      val getScaleProduct : CTRSObl.t -> tds -> int list list -> Complexity.t
      val getScaleFactor : CTRSObl.t -> trans_data -> int list -> Complexity.t
      val isTooBig : trans_data -> bool
      val isPossiblyScaledSumPlusConstant : trans_data -> bool
      val isMaxPlusConstant : trans_data -> bool
      val isMax : trans_data -> bool
      val get_v_beta : Tgraph.G.t * (Tgraph.G.vertex * trans_data) array -> tds -> trans_data -> int list
      val get_v_beta_nums : tds -> int list
      val inScc : tds -> trans_data -> bool
      val getAsPol : trans_data -> LC.size_data
      val getGSC : (tds * LC.size_data) list -> tds -> LC.size_data
      val getGSCForOne : (tds * LC.size_data) list -> trans_data -> LC.size_data
      val getMaxPlusConstantTerm : CTRSObl.t -> Poly.var list -> trans_data -> Complexity.t
      val getPossiblyScaledSumPlusConstantTerm :
        CTRSObl.t ->
        Tgraph.G.t * (Tgraph.G.vertex * trans_data) array ->
        tds ->
        Poly.var list ->
        (tds * LC.size_data)
        list ->
        trans_data ->
        int list -> Complexity.t
      val getTermForSum :
        tds ->
        Poly.var list ->
        (tds * LC.size_data)
        list -> int -> Complexity.t
      val takeout : tds -> tds -> tds
      val getCondensedPreds :
        Tgraph.G.t * (Tgraph.G.vertex * (tds * 'c))
        array -> tds list -> tds list
      val getCondensedNums :
        (Tgraph.G.vertex * (tds * 'c))
        array ->
        tds list ->
        int list
      val computeCondensedPreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         (tds * 'c))
        array -> int -> int list ref -> int list ref -> unit
      val hasCondensedEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex * (tds * 'c))
        array -> int -> int -> bool
      val getSccsNums :
        (Tgraph.G.vertex * (tds * 'c))
        array -> int list -> tds list
      val getPredVarBounds :
        tds list ->
        Poly.var list ->
        (tds * LC.size_data) list -> Complexity.t list list
      
      (** computes global size bound for one SCC in the RVG *)
      val computeGlobalSizeBound :
        CTRSObl.t ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         trans_data)
        array ->
        Tgraph.G.t *
        (Tgraph.G.vertex *
         (tds * int))
        array ->
        tds ->
        bool ->
        (tds * LC.size_data)
        list -> LC.size_data
      val compute : CTRSObl.t -> Tgraph.G.t * (Tgraph.G.vertex * trans_data) array -> LC.size_data RVMap.t
      val empty : 'a RVMap.t
      val findEntry :
        LC.size_data RVMap.t ->
        rule -> int -> int -> Poly.var list -> Complexity.t
      val extractSizeMapForRule :
        LC.size_data RVMap.t ->
        rule ->
        int -> Poly.var list -> (string * Complexity.t) list
      val extractSizeMapForRuleForVars :
        LC.size_data RVMap.t ->
        rule ->
        int -> Poly.var list -> (Poly.var * Complexity.t) list
      val dumpGSCs :
        tds -> string
      val getSizeComplexitiesForRule :
        rule ->
        LC.size_data RVMap.t ->
        (Poly.var list -> Complexity.t) list
      val printSizeComplexities :
        CTRSObl.t -> LC.size_data RVMap.t -> string
    end

module Make(CTRSObl : Ctrsobl.S) : S with module CTRSObl = CTRSObl
