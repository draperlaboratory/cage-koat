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
      module CTRSObl : Ctrsobl.S
      type r = CTRSObl.CTRS.RuleT.rule
      type tgraph
      val toDot : tgraph -> string
      val compute : r list -> tgraph
      val connectable : r -> r -> bool
      val connectableOne : Term.term -> Pc.cond -> Pc.cond -> Term.term -> bool
      val getSubstitution : Poly.poly list -> Poly.poly list -> (Poly.var * Poly.poly) list
      val getNontrivialSccs : tgraph -> r list list
      val computeReachable : tgraph -> r list -> r list
      val computeSubsumed : tgraph -> r list -> r list -> r list
      val removeNodes : tgraph -> r list -> tgraph
      val addNodes : tgraph -> r list -> tgraph
      val keepNodes : tgraph -> r list -> tgraph
      val getPreds : tgraph -> r list -> r list
      val getSuccs : tgraph -> r list -> r list
      exception Found of int
      val hasEdge : tgraph -> r -> r -> bool
      val computeRulesInTwigs : tgraph -> (r -> bool) -> r list
      val empty : unit -> tgraph
    end


module Make(CTRSObl : Ctrsobl.S) : S with module CTRSObl = CTRSObl
