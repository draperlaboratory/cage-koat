type lc = LocalSizeComplexity.localcomplexity

type size_data = LocalSizeComplexity.size_data

module Make(RuleT : AbstractRule.AbstractRule) :
    sig

      module LSC : LocalSizeComplexity.S with module RuleT = RuleT
      module CTRS : Ctrs.S
      module TGraph : Tgraph.S

      val toDot : Tgraph.G.t * (Tgraph.G.vertex * LSC.trans_data) array -> string
      val getNodes : (Tgraph.G.vertex * LSC.trans_data) array -> string
      val getEdges : Tgraph.G.t -> (Tgraph.G.vertex * LSC.trans_data) array -> string
      val compute :
        (RuleT.rule * LocalSizeComplexity.local_size_data) list ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        Tgraph.G.t * (int * (RuleT.rule * LocalSizeComplexity.local_size_data)) array
      val compute_edges :
        bool array array ->
        (int * (RuleT.rule * LocalSizeComplexity.local_size_data)) array ->
        int -> Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array -> unit
      val create_graph : int -> bool array array -> Tgraph.G.t
      exception Found of int
      val hasEdge :
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array ->
        RuleT.rule * LocalSizeComplexity.local_size_data ->
        RuleT.rule * LocalSizeComplexity.local_size_data -> bool
      val getNum :
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array -> RuleT.rule * LocalSizeComplexity.local_size_data -> int
      val getPreds :
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list
      val computePreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array -> int -> int list ref -> int list ref -> unit
      val getNums :
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list ->
        int list
      val isIn : int list ref -> int -> bool
      val hasEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array -> int -> int -> bool
      val getRvs :
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array ->
        int list ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list
      val removeNodes :
        Tgraph.G.t * (Tgraph.G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> Tgraph.G.t * (Tgraph.G.vertex * (RuleT.rule * 'a)) array
      val removeFromGraph : Tgraph.G.t -> Tgraph.G.vertex list -> Tgraph.G.t
      val removeFromArray :
        (Tgraph.G.vertex * (RuleT.rule * 'a)) array ->
        int list -> (Tgraph.G.vertex * (RuleT.rule * 'a)) array
      val getPairs :
        (Tgraph.G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> (int * Tgraph.G.vertex) list
      val addNodes :
        Tgraph.G.t * (Tgraph.G.vertex * LSC.trans_data) array ->
        LSC.tds ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        Tgraph.G.t * (Tgraph.G.vertex * LSC.trans_data) array
      val getMaxNum : (Tgraph.G.vertex * LSC.trans_data) array -> Tgraph.G.vertex
      val getNewPairs :
        LSC.tds -> Tgraph.G.vertex -> (Tgraph.G.vertex * LSC.trans_data) list
      val addToGraph : Tgraph.G.t -> Tgraph.G.vertex list -> Tgraph.G.t
      val addNeededEdges :
        Tgraph.G.t ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        (Tgraph.G.vertex * LSC.trans_data) array ->
        (Tgraph.G.vertex * LSC.trans_data) list -> Tgraph.G.t
      val addNeededEdgesOne :
        Tgraph.G.t ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        (Tgraph.G.vertex * LSC.trans_data) array ->
        Tgraph.G.vertex * LSC.trans_data -> Tgraph.G.t
      val addToArray :
        (Tgraph.G.vertex * LSC.trans_data) array ->
        (Tgraph.G.vertex * LSC.trans_data) list ->
        (Tgraph.G.vertex * LSC.trans_data) array
      val updateOptionRVGraph :
        (Tgraph.G.t * (Tgraph.G.vertex * LSC.trans_data) array) option ->
        RuleT.rule list ->
        RuleT.rule list ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        (Tgraph.G.t * (Tgraph.G.vertex * LSC.trans_data) array) option
      val keepNodes :
        Tgraph.G.t * (Tgraph.G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> Tgraph.G.t * (Tgraph.G.vertex * (RuleT.rule * 'a)) array
      val getComplementPairs :
        (Tgraph.G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> (int * Tgraph.G.vertex) list
      val getSccs : Tgraph.G.t * (Tgraph.G.V.t * 'a) array -> 'a list list
      val getRvsScc : (Tgraph.G.V.t * 'a) array -> Tgraph.G.V.t list -> 'a list
      val condense :
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array ->
        Tgraph.G.t *
        (int *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * bool))
        array
      val compute_condensed_edges :
        bool array array ->
        (int *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * bool))
        array ->
        int ->
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array -> unit
      val hasSccEdge :
        int *
        ((RuleT.rule * LocalSizeComplexity.local_size_data) list * bool) ->
        int *
        ((RuleT.rule * LocalSizeComplexity.local_size_data) list * bool) ->
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array -> bool
      val isTrivial :
        (RuleT.rule * LocalSizeComplexity.local_size_data) list ->
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * LocalSizeComplexity.local_size_data))
        array -> bool
      val equalSccs :
        (RuleT.rule * LocalSizeComplexity.local_size_data) list ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list -> bool
      val getNodesInTopologicalOrder : Tgraph.G.t * (Tgraph.G.V.t * 'a) array -> 'a list
      val getNodesTopo : (Tgraph.G.V.t * 'a) array -> Tgraph.G.V.t -> 'a
      val condensedToDot :
        Tgraph.G.t * (Tgraph.G.vertex * (LSC.trans_data list * bool)) array -> string
      val dumpScc : LSC.trans_data list * bool -> string
      val getCondensedNodes :
        (Tgraph.G.vertex * (LSC.trans_data list * bool)) array -> string
      val getCondensedEdges :
        Tgraph.G.t -> (Tgraph.G.vertex * (LSC.trans_data list * bool)) array -> string
      val getCondensedPreds :
        Tgraph.G.t *
        (Tgraph.G.vertex *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * 'c))
        array ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list list ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list list
      val getCondensedNums :
        (Tgraph.G.vertex *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * 'c))
        array ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list list ->
        int list
      val computeCondensedPreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * 'c))
        array -> int -> int list ref -> int list ref -> unit
      val hasCondensedEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * 'c))
        array -> int -> int -> bool
      val getSccsNums :
        (Tgraph.G.vertex *
         ((RuleT.rule * LocalSizeComplexity.local_size_data) list * 'c))
        array ->
        int list ->
        (RuleT.rule * LocalSizeComplexity.local_size_data) list list
    end
