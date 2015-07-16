(** Disjoint set forests **)

type 'a node = { mutable parent : 'a node; contents : 'a; }

(** construct a new set for the element a *)
val make_set : 'a -> 'a node

(** unions the two sets *)
val merge : 'a node -> 'a node -> unit

(** are two representative elements in the same set? *)
val same_set : 'a node -> 'a node -> bool
