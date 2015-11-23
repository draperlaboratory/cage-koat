module type S =
    sig
      module RVG : Rvgraph.S
      module LSC : LocalSizeComplexity.S with module RuleT = RVG.TGraph.CTRSObl.CTRS.RuleT

      type rule = RVG.TGraph.CTRSObl.CTRS.RuleT.rule
      module RVMap : Map.S with type key = (rule * LocalSizeComplexity.index)

      type size_data = LocalSizeComplexity.size_data
      type trans_data = LSC.trans_data
      type gsc = size_data RVMap.t

      (** computes global size bound for one SCC in the RVG *)
      val compute : RVG.TGraph.CTRSObl.t -> RVG.rvg -> LocalSizeComplexity.size_data RVMap.t
      val empty : gsc
      val findEntry : gsc -> rule -> int -> int -> Poly.var list -> Complexity.t
      val extractSizeMapForRule : gsc -> rule -> int -> Poly.var list -> (Poly.var * Complexity.t) list
      val extractSizeMapForRuleForVars : gsc -> rule -> int -> Poly.var list -> (Poly.var * Complexity.t) list
      val dumpGSCs : LSC.tds -> string
      val printSizeComplexities : RVG.TGraph.CTRSObl.t -> gsc -> string
    end

module Make(RVG : Rvgraph.S) : S with module RVG = RVG
