type fname = string

module FMap = Map.Make(String)

type pos = int

type jtype =
| Int
| Object

type argument = {
  name : Poly.var;
  jtype : jtype;
}

type argList = argument list

type complexitySpec = {
  size : Complexity.t;
  upperTime : Complexity.t;
  lowerTime : Complexity.t;
  upperSpace : Complexity.t;
  lowerSpace : Complexity.t;
}

type functionSpec = {
  fname : fname;
  args : argList;
  secretArgs : pos list; (* argument positions *)
  complexity : complexitySpec;
  pure : bool;
}

type specMap = functionSpec FMap.t


type package = {
  pname : string;
  created : string;
  functions : specMap;
}

let emptyPackage = { pname = ""; created = ""; functions = FMap.empty }


let has_space specs f =
  let open Complexity in
  if FMap.mem f specs then
    let c = FMap.find f specs in
    match c.complexity.upperSpace with
    | P _     -> true
    | Unknown -> false
  else
    false

let get_space specs f =
  let open Complexity in
  let c = FMap.find f specs in
  match c.complexity.upperSpace with
  | P p     -> p
  | Unknown -> assert false
