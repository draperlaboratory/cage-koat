(** Disjoint set forests **)

type 'a node = {
  mutable parent : 'a node;
  contents : 'a;
}


let make_set a =
  (** construct a new set for the element a *)
  let rec node = {
    parent = node;
    contents = a;
  } in
  node


let rec find_root n =
  (** find the root member of a set *)
  if n.parent == n
  then n
  else find_root n.parent


let canonincal_member n =
  (** returns the representative element of a set *)
  (find_root n).contents


let merge n1 n2 =
  (** unions the two sets s.t. all elements have the same canonical member.  Merge is probably a bad word.  We don't actually
      bother to remove repeated elements or anything. *)
  let p1 = find_root n1
  and p2 = find_root n2 in
  p2.parent <- p1


let same_set n1 n2 =
  (** are two representative elements in the same set *)
  find_root n1 == find_root n2
