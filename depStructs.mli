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

module ArgPosGraph :
  sig
    type t =
        Graph.Imperative.Digraph.ConcreteLabeled(ArgPosNodes(Fnh))(QualEdge).t
    module V :
      sig
        type t = ArgPosNodes(Fnh).t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = ArgPosNodes(Fnh).t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = ArgPosNodes(Fnh).t * QualEdge.t * ArgPosNodes(Fnh).t
        val compare : t -> t -> int
        type vertex = ArgPosNodes(Fnh).t
        val src : t -> vertex
        val dst : t -> vertex
        type label = QualEdge.t
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> edge list
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val create : ?size:int -> unit -> t
    val clear : t -> unit
    val copy : t -> t
    val add_vertex : t -> vertex -> unit
    val remove_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> unit
    val add_edge_e : t -> edge -> unit
    val remove_edge : t -> vertex -> vertex -> unit
    val remove_edge_e : t -> edge -> unit
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
