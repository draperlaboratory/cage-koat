module FMap : Map.S with type key = Term.funSym

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
  secretArgs : Term.pos list; (* argument positions *)
  complexity : complexitySpec;
  pure : bool;
}

type specMap = functionSpec FMap.t

type package = {
  pname : string;
  created : string;
  functions : specMap;
}

val emptyPackage : package
