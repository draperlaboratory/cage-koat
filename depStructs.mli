type argPos = {
  fName : string;
  pos : int;
}

type qual =
| Equal
| Delta
| Unknown

type ruleTrans = {
  lPos : argPos;
  rPos : argPos;
  qual : qual;
}

type fNameHash = {
  maxFName : int;
  mapping : (string,int) Hashtbl.t;
}

module QualEdge : sig
  type t = qual
  val compare : t -> t -> int
  val default : t
end

module Fnh : sig
  val getHash : unit -> fNameHash
  val setHash : fNameHash -> unit
end

module type Hashed = sig
  val getHash : unit -> fNameHash
 end

module ArgPosNodes :
  functor (F : Hashed) ->
    sig
      type t = argPos
      val compare : t -> t -> int
      val hash : t -> int
      val equal : t -> t -> bool
    end

module ArgPosGraph : sig
  type t =
    Graph.Imperative.Digraph.ConcreteLabeled(ArgPosNodes(Fnh))(QualEdge).t
end


(** For debug printing, etc. **)
val qualToString : qual -> string
val argPosToString : argPos -> string
val ruleTransToString : ruleTrans -> string


(** Produces a dot file representing information flow described by a ruletrans
    list *)
val doVis : ruleTrans list -> string -> unit

val buildFlowGraph : ruleTrans list -> ArgPosGraph.t
val draw : ArgPosGraph.t -> string -> unit
val reachable : argPos list -> ArgPosGraph.t -> (argPos -> bool)
