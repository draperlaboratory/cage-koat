type reachablePosition = {
  argPos : DepStructs.argPos;
  qual : DepStructs.qual;
}

module ArgPosHash : sig
  type t = DepStructs.argPos
end

module ArgPosTable : sig
  type key = ArgPosHash.t
  type 'a t
  val find : 'a t -> key -> 'a
  val mem : 'a t -> key -> bool
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

type reachablePositions = reachablePosition ArgPosTable.t
type reachableGraph = reachablePositions ArgPosTable.t

(** Given a list of relationships mined from the rules of the transition system,
    produce a graph, where the nodes are functions * argument positions, and the
    edges represent influence between these argument positions for all rules. In
    particular, we care about whether or not there is a difference between the
    arguments.
*)
val computeGraph : DepStructs.ruleTrans list -> reachableGraph


(** Is a given argument position critical to its function in a reachableGraph

    That is, after the graph has saturated, is there a delta path between this
    argument and itself.  **)
val criticalArgument : reachableGraph -> DepStructs.argPos -> bool

(** do any of the arguments in the argPos list flow to a critical argument position? *)
val flowsToCritical : ?graph:reachableGraph option -> DepStructs.ruleTrans list -> DepStructs.argPos list -> bool

(** convert a set of relationships into a .dot file so that we can look at them. *)
val visualizeInformationFlow : DepStructs.ruleTrans list -> string -> unit
