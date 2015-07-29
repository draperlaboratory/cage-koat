type ruleReachability =
| Cyclic        (* Reaches itself by some path *)
| ReachesCycle  (* hangs above a cycle *)
| Acyclic       (* part of a linear / branching path with no cycles *)

module Connectivity: sig
  type key = String.t
  type +'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

module Reaches : sig
  type elt = String.t
  type t
end

val infoMap : Reaches.t Connectivity.t -> ruleReachability Connectivity.t
val processRules : Comrule.rule list -> Reaches.t Connectivity.t

