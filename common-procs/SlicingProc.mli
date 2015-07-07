module Make(CTRSObl : Ctrsobl.S) :
sig
  val getProof : 'a -> CTRSObl.t -> string list -> int -> int -> string
  val process : CTRSObl.t -> (CTRSObl.t * (int -> int -> string)) option
end
