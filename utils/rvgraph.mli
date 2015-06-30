type lc = LocalSizeComplexity.localcomplexity

module G : Graph.Sig.P with type t = Tgraph.G.t and type V.t = int

module Make(RuleT : AbstractRule.AbstractRule) :
    sig

      module LSC : LocalSizeComplexity.S with type r = RuleT.rule
      module CTRS : Ctrs.S
      module TGraph : Tgraph.S

      val toDot : G.t * (G.vertex * LSC.local_size_data) array -> string
      val getNodes : (G.vertex * LSC.local_size_data) array -> string
      val getEdges : G.t -> (G.vertex * LSC.local_size_data) array -> string
      val compute :
        (RuleT.rule * (('a * 'b) * ('c * 'b list))) list ->
        G.t * (G.vertex * RuleT.rule) array ->
        G.t * (int * (RuleT.rule * (('a * 'b) * ('c * 'b list)))) array
      val compute_edges :
        bool array array ->
        (int * (RuleT.rule * (('a * 'b) * ('c * 'b list)))) array ->
        int -> G.t * (G.vertex * RuleT.rule) array -> unit
      val first : 'a * ('b * 'c) -> 'a
      val second : 'a * ('b * 'c) -> 'b
      val third : 'a * ('b * 'c) -> 'c
      val create_graph : int -> bool array array -> G.t
      exception Found of int
      val hasEdge :
        G.t *
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        RuleT.rule * ('a * (lc * 'b list)) ->
        RuleT.rule * ('a * (lc * 'b list)) -> bool
      val getNum :
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> RuleT.rule * ('a * (lc * 'b list)) -> int
      val getPreds :
        G.t *
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        (RuleT.rule * ('a * (lc * 'b list))) list
      val computePreds :
        G.t ->
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> int -> int list ref -> int list ref -> unit
      val getNums :
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        int list
      val isIn : int list ref -> int -> bool
      val hasEdgeNums :
        G.t ->
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> int -> int -> bool
      val getRvs :
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        int list ->
        (RuleT.rule * ('a * (lc * 'b list))) list
      val removeNodes :
        G.t * (G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> G.t * (G.vertex * (RuleT.rule * 'a)) array
      val removeFromGraph : G.t -> G.vertex list -> G.t
      val removeFromArray :
        (G.vertex * (RuleT.rule * 'a)) array ->
        int list -> (G.vertex * (RuleT.rule * 'a)) array
      val getPairs :
        (G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> (int * G.vertex) list
      val addNodes :
        G.t * (G.vertex * LSC.local_size_data) array ->
        LSC.lsds ->
        G.t * (G.vertex * RuleT.rule) array ->
        G.t * (G.vertex * LSC.local_size_data) array
      val getMaxNum : (G.vertex * LSC.local_size_data) array -> G.vertex
      val getNewPairs :
        LSC.lsds -> G.vertex -> (G.vertex * LSC.local_size_data) list
      val addToGraph : G.t -> G.vertex list -> G.t
      val addNeededEdges :
        G.t ->
        G.t * (G.vertex * RuleT.rule) array ->
        (G.vertex * LSC.local_size_data) array ->
        (G.vertex * LSC.local_size_data) list -> G.t
      val addNeededEdgesOne :
        G.t ->
        G.t * (G.vertex * RuleT.rule) array ->
        (G.vertex * LSC.local_size_data) array ->
        G.vertex * LSC.local_size_data -> G.t
      val addToArray :
        (G.vertex * LSC.local_size_data) array ->
        (G.vertex * LSC.local_size_data) list ->
        (G.vertex * LSC.local_size_data) array
      val updateOptionRVGraph :
        (G.t * (G.vertex * LSC.local_size_data) array) option ->
        RuleT.rule list ->
        LSC.r list ->
        G.t * (G.vertex * RuleT.rule) array ->
        (G.t * (G.vertex * LSC.local_size_data) array) option
      val keepNodes :
        G.t * (G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> G.t * (G.vertex * (RuleT.rule * 'a)) array
      val getComplementPairs :
        (G.vertex * (RuleT.rule * 'a)) array ->
        RuleT.rule list -> (int * G.vertex) list
      val getSccs : G.t * (G.V.t * 'a) array -> 'a list list
      val getRvsScc : (G.V.t * 'a) array -> G.V.t list -> 'a list
      val condense :
        G.t *
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array ->
        G.t *
        (int *
         ((RuleT.rule * ('a * (lc * 'b list))) list * bool))
        array
      val compute_condensed_edges :
        bool array array ->
        (int *
         ((RuleT.rule * ('a * (lc * 'b list))) list * bool))
        array ->
        int ->
        G.t *
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> unit
      val hasSccEdge :
        int *
        ((RuleT.rule * ('a * (lc * 'b list))) list * bool) ->
        int *
        ((RuleT.rule * ('a * (lc * 'b list))) list * bool) ->
        G.t *
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> bool
      val isTrivial :
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        G.t *
        (G.vertex * (RuleT.rule * ('a * (lc * 'b list))))
        array -> bool
      val equalSccs :
        (RuleT.rule * ('a * (lc * 'b list))) list ->
        (RuleT.rule * ('a * (lc * 'b list))) list -> bool
      val getNodesInTopologicalOrder : G.t * (G.V.t * 'a) array -> 'a list
      val getNodesTopo : (G.V.t * 'a) array -> G.V.t -> 'a
      val condensedToDot :
        G.t * (G.vertex * (LSC.local_size_data list * bool)) array -> string
      val dumpScc : LSC.local_size_data list * bool -> string
      val getCondensedNodes :
        (G.vertex * (LSC.local_size_data list * bool)) array -> string
      val getCondensedEdges :
        G.t -> (G.vertex * (LSC.local_size_data list * bool)) array -> string
      val getCondensedPreds :
        G.t *
        (G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list list ->
        (RuleT.rule * ('a * (lc * 'b list))) list list
      val getCondensedNums :
        (G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array ->
        (RuleT.rule * ('a * (lc * 'b list))) list list ->
        int list
      val computeCondensedPreds :
        G.t ->
        (G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array -> int -> int list ref -> int list ref -> unit
      val hasCondensedEdgeNums :
        G.t ->
        (G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array -> int -> int -> bool
      val getSccsNums :
        (G.vertex *
         ((RuleT.rule * ('a * (lc * 'b list))) list * 'c))
        array ->
        int list ->
        (RuleT.rule * ('a * (lc * 'b list))) list list
    end
