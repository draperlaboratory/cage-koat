type reachableGraph

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

(** convert a set of relationships into a .dot file so that we can look at them. *)
val visualizeInformationFlow : DepStructs.ruleTrans list -> string -> unit
