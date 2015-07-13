type lc = LocalSizeComplexity.localcomplexity

module Make(RuleT : AbstractRule.AbstractRule) :
    sig

      module LSC : LocalSizeComplexity.S with module RuleT = RuleT
      module CTRS : Ctrs.S
      module TGraph : Tgraph.S

      val toDot : Tgraph.G.t * (Tgraph.G.vertex * LSC.local_trans_data) array -> string
      val getNodes : (Tgraph.G.vertex * LSC.local_trans_data) array -> string
      val getEdges : Tgraph.G.t -> (Tgraph.G.vertex * LSC.local_trans_data) array -> string
      val compute :
        (RuleT.rule * (('a * 'b) * ('c * 'b list))) list ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        Tgraph.G.t * (int * (RuleT.rule * (('a * 'b) * ('c * 'b list)))) array
      val compute_edges :
        bool array array ->
        (int * (RuleT.rule * (('a * 'b) * ('c * 'b list)))) array ->
        int -> Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array -> unit
      val first : 'a * ('b * 'c) -> 'a
      val second : 'a * ('b * 'c) -> 'b
      val third : 'a * ('b * 'c) -> 'c
      val create_graph : int -> bool array array -> Tgraph.G.t
      exception Found of int
      val hasEdge :
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        RuleT.rule * ('a * (lc * 'b list)) ->
        RuleT.rule * ('a * (lc * 'b list)) -> bool
      val getNum :
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> RuleT.rule * ('a * (lc * 'b list)) -> int
      val getPreds :
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        (RuleT.rule * ('a * (lc * 'b list))) list
      val computePreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> int -> int list ref -> int list ref -> unit
      val getNums :
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        int list
      val isIn : int list ref -> int -> bool
      val hasEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> int -> int -> bool
      val getRvs :
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        int list ->
        (RuleT.rule * ('a * (lc * 'b list))) list
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
        Tgraph.G.t * (Tgraph.G.vertex * LSC.local_trans_data) array ->
        LSC.ltds ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        Tgraph.G.t * (Tgraph.G.vertex * LSC.local_trans_data) array
      val getMaxNum : (Tgraph.G.vertex * LSC.local_trans_data) array -> Tgraph.G.vertex
      val getNewPairs :
        LSC.ltds -> Tgraph.G.vertex -> (Tgraph.G.vertex * LSC.local_trans_data) list
      val addToGraph : Tgraph.G.t -> Tgraph.G.vertex list -> Tgraph.G.t
      val addNeededEdges :
        Tgraph.G.t ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        (Tgraph.G.vertex * LSC.local_trans_data) array ->
        (Tgraph.G.vertex * LSC.local_trans_data) list -> Tgraph.G.t
      val addNeededEdgesOne :
        Tgraph.G.t ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        (Tgraph.G.vertex * LSC.local_trans_data) array ->
        Tgraph.G.vertex * LSC.local_trans_data -> Tgraph.G.t
      val addToArray :
        (Tgraph.G.vertex * LSC.local_trans_data) array ->
        (Tgraph.G.vertex * LSC.local_trans_data) list ->
        (Tgraph.G.vertex * LSC.local_trans_data) array
      val updateOptionRVGraph :
        (Tgraph.G.t * (Tgraph.G.vertex * LSC.local_trans_data) array) option ->
        RuleT.rule list ->
        RuleT.rule list ->
        Tgraph.G.t * (Tgraph.G.vertex * RuleT.rule) array ->
        (Tgraph.G.t * (Tgraph.G.vertex * LSC.local_trans_data) array) option
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
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        Tgraph.G.t *
        (int *
         ((RuleT.rule * ('a * (lc * 'b list))) list * bool))
        array
      val compute_condensed_edges :
        bool array array ->
        (int *
         ((RuleT.rule * ('a * (lc * 'b list))) list * bool))
        array ->
        int ->
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> unit
      val hasSccEdge :
        int *
        ((RuleT.rule * ('a * (lc * 'b list))) list * bool) ->
        int *
        ((RuleT.rule * ('a * (lc * 'b list))) list * bool) ->
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> bool
      val isTrivial :
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        Tgraph.G.t *
        (Tgraph.G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> bool
      val equalSccs :
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        (RuleT.rule * ('a * (lc * 'b list))) list -> bool
      val getNodesInTopologicalOrder : Tgraph.G.t * (Tgraph.G.V.t * 'a) array -> 'a list
      val getNodesTopo : (Tgraph.G.V.t * 'a) array -> Tgraph.G.V.t -> 'a
      val condensedToDot :
        Tgraph.G.t * (Tgraph.G.vertex * (LSC.local_trans_data list * bool)) array -> string
      val dumpScc : LSC.local_trans_data list * bool -> string
      val getCondensedNodes :
        (Tgraph.G.vertex * (LSC.local_trans_data list * bool)) array -> string
      val getCondensedEdges :
        Tgraph.G.t -> (Tgraph.G.vertex * (LSC.local_trans_data list * bool)) array -> string
      val getCondensedPreds :
        Tgraph.G.t *
        (Tgraph.G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list list ->
        (RuleT.rule * ('a * (lc * 'b list))) list list
      val getCondensedNums :
        (Tgraph.G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list list ->
        int list
      val computeCondensedPreds :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array -> int -> int list ref -> int list ref -> unit
      val hasCondensedEdgeNums :
        Tgraph.G.t ->
        (Tgraph.G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array -> int -> int -> bool
      val getSccsNums :
        (Tgraph.G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array ->
        int list ->
        (RuleT.rule * ('a * (lc * 'b list))) list list
    end
