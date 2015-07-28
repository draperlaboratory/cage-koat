type t = P of Expexp.expexp | Unknown

val toStringCompetitionStyle : t -> string

val toString : t -> string

val equal : t -> t -> bool

val add : t -> t -> t

val listAdd : t list -> t

val mult : t -> t -> t

val apply : Expexp.expexp -> (Poly.var * t) list -> t

val getExpexp : t -> Expexp.expexp

val getPoly : t -> Poly.poly option

val dependsOnUnknown :
  Expexp.expexp -> (Poly.var * Expexp.expexp) list -> bool

val mappedToUnknown : (Poly.var * Expexp.expexp) list -> Poly.var -> bool

val sup : t list -> t

val strip : t -> Expexp.expexp
