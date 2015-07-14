(*
  Various utilities

  @author Stephan Falke

  Copyright 2010-2014 Stephan Falke

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

(* Removed duplicates from a list *)
let rec remdup l =
  match l with
    | [] -> []
    | x::xs -> x::(remdup (List.filter (fun y -> x <> y) xs))

(* Removes the first occurence of an element from a list *)
let rec remove l e =
  match l with
    | [] -> []
    | x::xs -> if x = e then
                 xs
               else
                 x::(remove xs e)

(* Removes the first occurence of each element from a list from another list *)
let rec removeAll l l' =
  match l' with
    | [] -> l
    | e::rest -> removeAll (remove l e) rest


(* Removed duplicates from a list, using comparison comp *)
let rec remdupC comp l =
  match l with
    | [] -> []
    | x::xs -> x::(remdupC comp (List.filter (fun y -> not (comp x y)) xs))

(* Removes all occurences of an element from a list, using comparison comp *)
let rec removeC comp l e =
  match l with
    | [] -> []
    | x::xs -> if comp x e then
                 (removeC comp xs e)
               else
                 x::(removeC comp xs e)

(* Removes all occurences of each element from a list from another list, using comparison comp *)
let rec removeAllC comp l l' =
  match l' with
    | [] -> l
    | e::rest -> removeAllC comp (removeC comp l e) rest

(* Get a list containing the integers from i through n *)
let rec getList i n =
  if i > n then
    []
  else
    i::(getList (i + 1) n)

(* Get a list with n copies of e *)
let rec getCopies e n =
  if n <= 0 then
    []
  else
    e::(getCopies e (n - 1))

(* Checks whether e is in l *)
let rec contains l e =
  match l with
    | [] -> false
    | x::xs -> x = e || contains xs e

(* Checks whether l contains all elements of l' *)
let containsAll l l' =
  List.for_all (contains l) l'

(* Checks whether l contains one element of l' *)
let containsOne l l' =
  List.exists (contains l) l'

let rec containsC c l e =
  List.exists (fun t -> c t e) l

(* Get last element of l *)
let rec last l =
  match l with
    | [] -> failwith "Trying to get last element of an empty list"
    | [e] -> e
    | _::ll -> last ll

(* Drop last element of l *)
let rec dropLast l =
  match l with
    | [] -> failwith "Trying to drop last element of an empty list"
    | [e] -> []
    | e::ll -> e::(dropLast ll)

(* take n initial elements from l *)
let rec take n l =
  if n = 0 then
    []
  else
    (List.hd l)::(take (n - 1) (List.tl l))

(* split l according to c *)
let rec split c l =
  splitAux c l [] []
and splitAux c l good bad =
  match l with
    | [] -> (List.rev good, List.rev bad)
    | e::rest -> if c e then
                   splitAux c rest (e::good) bad
                 else
                   splitAux c rest good (e::bad)

(* returns elements that are in both lists *)
let intersect l l' =
  List.filter (fun e -> contains l e) l'

(* return elements from l' that are not in l *)
let notIn l l' =
  List.filter (fun e -> not (contains l e)) l'

(** Parametric contains: returns true if l contains an element e' with (c e e') *)
let containsP c l e =
  List.exists (fun e' -> c e e') l

(** Parametric version of notIn: return elements from l' that are not in l, where c is used for comparison (cf containsG) *)
let notInP c l l' =
  List.filter (fun e -> not (containsP c l e)) l'

let rec iter3 f xs ys zs =
  match xs with
    | [] -> ()
    | x::xrest -> f x (List.hd ys) (List.hd zs); iter3 f xrest (List.tl ys) (List.tl zs)

let rec map3 f xs ys zs =
  match xs with
    | [] -> []
    | x::xrest -> (f x (List.hd ys) (List.hd zs))::(map3 f xrest (List.tl ys) (List.tl zs))

let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
    ([], [], []) -> []
  | (a1::l1, a2::l2, a3::l3) -> (a1, a2, a3) :: combine3 l1 l2 l3
  | (_, _, _) -> invalid_arg "Utils.combine3"

let rec combine4 l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
    ([], [], [], []) -> []
  | (a1::l1, a2::l2, a3::l3, a4::l4) -> (a1, a2, a3, a4) :: combine4 l1 l2 l3 l4
  | (_, _, _, _) -> invalid_arg "Utils.combine4"

let unboxOption oOpt =
  match oOpt with
    | Some o -> o
    | _ -> failwith "trying to access Some value where None is"

let iteri f l =
  let rec iteri' i f l =
    match l with
      | []    -> ()
      | x::xs -> f i x ; (iteri' (i+1) f xs)
  in
  iteri' 0 f l

let mapi f l =
  let rec mapi' i f l =
    match l with
      | []    -> []
      | x::xs -> (f i x) :: (mapi' (i+1) f xs)
  in
  mapi' 0 f l

let rec getIdx l e =
  getIdxAux l e 0
and getIdxAux l e i =
  match l with
    | [] -> raise Not_found
    | e'::rest -> if e = e' then
                    i
                  else
                    getIdxAux rest e (i + 1)

(* Version of concatMap that for [v1; v2; ...; vn] produces (f v1) @ (f v2) @ ... @ (f vn). Not tail recursive. *)
let concatMapStable f l =
  List.fold_right (fun v acc -> (f v) @ acc) l []

let concatMap f l =
  List.fold_left (fun acc v -> (f v) @ acc) [] l

let concatMapi f l =
  let rec concatMapi' i f l =
    match l with
      | []    -> []
      | x::xs -> (f i x) @ (concatMapi' (i+1) f xs)
  in
  concatMapi' 0 f l

let rec tryFind f l =
  match l with
  | e::rest -> if f e then Some e else tryFind f rest
  | []      -> None

let powSet s =
  List.fold_left (fun state ele -> concatMap (fun s -> [s ; ele::s]) state) [[]] s

let getIndexedSubset idxsToKeep list =
  let rec getIndexedSubset' idxsToKeep idx list =
    match list with
    | [] -> []
    | (x::xs) ->
      if contains idxsToKeep idx then
        x::(getIndexedSubset' idxsToKeep (idx+1)) xs
      else
        (getIndexedSubset' idxsToKeep (idx+1)) xs
  in
  getIndexedSubset' idxsToKeep 0 list


let rec indexOfAux cmp index el list =
  match list with
    [] -> raise Not_found
  | hd::tl ->
    if cmp el hd
    then index
    else indexOfAux cmp (index + 1) el tl

let indexOf ?(cmp = (=)) el list = indexOfAux cmp 0 el list


let rec indexesOfAux cmp index el list =
  match list with
    [] -> []
  | hd::tl ->
    if cmp el hd
    then index :: indexesOfAux cmp (index + 1) el tl
    else indexesOfAux cmp (index + 1) el tl

let indexesOf ?(cmp = (=)) el list = indexesOfAux cmp 0 el list

(* come back and make this more efficient! [JTT 14-07-15] *)
let inexdesOfList ?(cmp = (=)) elements list =
  List.map (fun el -> indexesOfAux cmp 0 el list) elements
