val heuristicValue : int ref

module Make(RVG : Rvgraph.S) :
sig
(*  val getProof : 'a -> CTRSObl.t -> string list -> int -> int -> string *)
  val getProof : RVG.TGraph.CTRSObl.t -> RVG.TGraph.CTRSObl.t -> string list -> int -> int -> string
  val process : RVG.TGraph.CTRSObl.t -> (RVG.TGraph.CTRSObl.t * (int -> int -> string)) option
end
