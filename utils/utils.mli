val remdup : 'a list -> 'a list
val remove : 'a list -> 'a -> 'a list
val removeAll : 'a list -> 'a list -> 'a list
val remdupC : ('a -> 'a -> bool) -> 'a list -> 'a list
val removeC : ('a -> 'b -> bool) -> 'a list -> 'b -> 'a list
val removeAllC : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
val getList : int -> int -> int list
val getCopies : 'a -> int -> 'a list
val contains : 'a list -> 'a -> bool
val containsAll : 'a list -> 'a list -> bool
val containsOne : 'a list -> 'a list -> bool
val containsC : ('a -> 'b -> bool) -> 'a list -> 'b -> bool
val last : 'a list -> 'a
val dropLast : 'a list -> 'a list
val take : int -> 'a list -> 'a list
val split : ('a -> bool) -> 'a list -> 'a list * 'a list
val intersect : 'a list -> 'a list -> 'a list
val notIn : 'a list -> 'a list -> 'a list
val containsP : ('a -> 'b -> bool) -> 'b list -> 'a -> bool
val notInP : ('a -> 'b -> bool) -> 'b list -> 'a list -> 'a list
val iter3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> unit
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val combine4 : 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list
val unboxOption : 'a option -> 'a
val iteri : (int -> 'a -> 'b) -> 'a list -> unit
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val getIdx : 'a list -> 'a -> int
val concatMapStable : ('a -> 'b list) -> 'a list -> 'b list
val concatMap : ('a -> 'b list) -> 'a list -> 'b list
val concatMapi : (int -> 'a -> 'b list) -> 'a list -> 'b list
val tryFind : ('a -> bool) -> 'a list -> 'a option
val powSet : 'a list -> 'a list list
val getIndexedSubset : int list -> 'a list -> 'a list
val indexOf : ?cmp:('a -> 'a -> bool) -> 'a -> 'a list -> int
val indexesOf : ?cmp:('a -> 'a -> bool) -> 'a -> 'a list -> int list
val inexdesOfList : ?cmp:('a -> 'a -> bool) -> 'a list -> 'a list -> int list list
