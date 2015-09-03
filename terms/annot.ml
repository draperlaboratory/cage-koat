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
  fname : Term.funSym;
  args : argList;
  secretArgs : pos list; (* argument positions *)
  complexity : complexitySpec;
  pure : bool;
}

type specMap = functionSpec FMap.t


type t = {
  pname : string;
  created : string;
  functions : specMap;
}

let empty = { pname = ""; created = ""; functions = FMap.empty }
