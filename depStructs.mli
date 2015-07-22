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

val qualToString : qual -> string
val argPosToString : argPos -> string
val ruleTransToString : ruleTrans -> string
val doVis : ruleTrans list -> string -> unit

module QualEdge : sig
  type t = qual
  val compare : t -> t -> int
  val default : t
end
