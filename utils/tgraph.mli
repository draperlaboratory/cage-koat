module Int :
 sig
   type t = int
   val compare : t -> t -> int
   val hash : t -> int
   val equal : t -> t -> bool
 end

module G : Graph.Sig.P with type V.t = int

module type S =
    sig
      type r
      val toDot : G.t * (G.vertex * r) array -> string
      val getNodes : (G.vertex * r) array -> string
      val getEdges : G.t -> (G.vertex * r) array -> string
      val compute : r list -> G.t * (int * r) array
      val compute_edges :
        bool array array -> (int * r) array -> int -> unit
      val create_graph : int -> bool array array -> G.t
      val connectable : r -> r -> bool
      val connectableOne :
        Term.term -> Pc.cond -> Pc.cond -> Term.term -> bool
      val getSubstitution :
        Poly.poly list -> Poly.poly list -> (Poly.var * Poly.poly) list
      val getSubstitutionAux :
        Poly.poly list ->
        Poly.poly list ->
        (Poly.var * Poly.poly) list -> (Poly.var * Poly.poly) list
      val getNontrivialSccs : G.t * (G.V.t * 'a) array -> 'a list list
      val nontrivial : G.t -> G.V.t list -> bool
      val getTrsScc : (G.V.t * 'a) array -> G.V.t list -> 'a list
      val hasEdgeNums : G.t -> (G.vertex * 'a) array -> int -> int -> bool
      val computeReachable :
        G.t * (G.vertex * r) array ->
        r list -> r list
      val computeReachableAux :
        G.t ->
        (G.vertex * r) array ->
        int -> int list ref -> int list ref -> unit
      val computeReachableAuxStep :
        G.t ->
        (G.vertex * r) array ->
        int -> int list ref -> int list ref -> bool
      val isIn : int list ref -> int -> bool
      val getNums :
        (G.vertex * r) array -> r list -> int list
      val getRules :
        (G.vertex * r) array -> int list -> r list
      val computeSubsumed :
        G.t * (G.vertex * r) array ->
        r list -> r list -> r list
      val computeSubsumedAux :
        int list ->
        int list ->
        G.t -> (G.vertex * r) array -> int -> int list ref -> unit
      val computeSubsumedAuxStep :
        int list ->
        int list ->
        G.t -> (G.vertex * r) array -> int -> int list ref -> bool
      val isK : int list -> int list ref -> int -> bool
      val removeNodes :
        G.t * (G.vertex * r) array ->
        r list -> G.t * (G.vertex * r) array
      val removeFromGraph : G.t -> G.vertex list -> G.t
      val removeFromArray :
        (G.vertex * r) array ->
        int list -> (G.vertex * r) array
      val getPairs :
        (G.vertex * r) array ->
        r list -> (int * G.vertex) list
      val addNodes :
        G.t * (G.vertex * r) array ->
        r list -> G.t * (G.vertex * r) array
      val getMaxNum : (G.vertex * r) array -> G.vertex
      val getNewPairs :
        r list -> G.vertex -> (G.vertex * r) list
      val addToGraph : G.t -> G.vertex list -> G.t
      val addNeededEdges :
        G.t ->
        (G.vertex * r) array -> (G.vertex * r) list -> G.t
      val addNeededEdgesOne :
        G.t -> (G.vertex * r) array -> G.vertex * r -> G.t
      val addToArray :
        (G.vertex * r) array ->
        (G.vertex * r) list -> (G.vertex * r) array
      val keepNodes :
        G.t * (G.vertex * r) array ->
        r list -> G.t * (G.vertex * r) array
      val getComplementPairs :
        (G.vertex * r) array ->
        r list -> (int * G.vertex) list
      val getPreds :
        G.t * (G.vertex * r) array ->
        r list -> r list
      val computePreds :
        G.t ->
        (G.vertex * r) array ->
        int -> int list ref -> int list ref -> unit
      val getSuccs :
        G.t * (G.vertex * r) array ->
        r list -> r list
      val computeSuccs :
        G.t ->
        (G.vertex * r) array ->
        int -> int list ref -> int list ref -> unit
      exception Found of int
      val hasEdge :
        G.t * (G.vertex * r) array ->
        r -> r -> bool
      val getNum : (G.vertex * r) array -> r -> int
      val computeRulesInTwigs :
        G.t * (G.vertex * r) array -> r list
      val computeLeavesAux :
        G.t -> (G.vertex * r) array -> int -> int list ref -> unit
      val computeLeavesAuxStep :
        G.t -> (G.vertex * r) array -> int -> int list ref -> bool
    end


module Make(RuleT : AbstractRule.AbstractRule) : S with type r = RuleT.rule
