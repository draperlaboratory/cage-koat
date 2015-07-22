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

module QualEdge : sig
  type t = qual
  val compare : t -> t -> int
  val default : t
end

(** For debug printing, etc. **)
val qualToString : qual -> string
val argPosToString : argPos -> string
val ruleTransToString : ruleTrans -> string

(** Produces a dot file representing information flow described by a ruletrans
    list *)
val doVis : ruleTrans list -> string -> unit


