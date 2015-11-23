type lc = LocalSizeComplexity.localcomplexity

type size_data = LocalSizeComplexity.size_data

module type S =
    sig
      module TGraph : Tgraph.S
      module LSC : LocalSizeComplexity.S with module RuleT = TGraph.CTRSObl.CTRS.RuleT
      module CTRS : Ctrs.S
      type rule = TGraph.CTRSObl.CTRS.RuleT.rule
      type rvg = Tgraph.G.t * (Tgraph.G.vertex * (rule * LocalSizeComplexity.local_size_data)) array

      val toDot : Tgraph.G.t * (Tgraph.G.vertex * LSC.trans_data) array -> string
      val getNodes : (Tgraph.G.vertex * LSC.trans_data) array -> string
      val getEdges : Tgraph.G.t -> (Tgraph.G.vertex * LSC.trans_data) array -> string
      val compute :
        (rule * LocalSizeComplexity.local_size_data) list -> TGraph.tgraph -> rvg
      exception Found of int
      val hasEdge :
        rvg -> rule * LocalSizeComplexity.local_size_data -> rule * LocalSizeComplexity.local_size_data -> bool
      val getPreds :
        rvg -> (rule * LocalSizeComplexity.local_size_data) list -> (rule * LocalSizeComplexity.local_size_data) list
      val removeNodes : rvg -> rule list -> rvg
      val addNodes : rvg -> LSC.tds -> TGraph.tgraph -> rvg
      val updateOptionRVGraph :
	rvg option -> rule list -> rule list -> TGraph.tgraph -> rvg option
      val keepNodes : rvg -> rule list -> rvg
      val condense :
	rvg -> Tgraph.G.t * (Tgraph.G.vertex * ((rule * LocalSizeComplexity.local_size_data) list * bool)) array
      val equalSccs :
        (rule * LocalSizeComplexity.local_size_data) list -> (rule * LocalSizeComplexity.local_size_data) list -> bool
      val getNodesInTopologicalOrder : Tgraph.G.t * (Tgraph.G.V.t * 'a) array -> 'a list
    end

module Make(TGraph : Tgraph.S) : S with module TGraph = TGraph
