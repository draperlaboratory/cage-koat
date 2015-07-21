type argPos = {
  fName : string;
  pos : int;
}

type qual =
| Equal
| Delta
| Unkown

type ruleTrans = {
  lPos : argPos;
  rPos : argPos;
  qual : qual;
}


let qualToString = function
  | Equal -> "Equal"
  | Delta -> "Delta"
  | Unkown -> "?"

let argPosToString ap =
  Printf.sprintf "%s %i" ap.fName ap.pos

let ruleTransToString rt =
  Printf.sprintf "%s %s %s"
    (argPosToString rt.lPos)
    (qualToString rt.qual)
    (argPosToString rt.rPos)


type fNameHash = {
  maxFName : int;
  mapping : (string,int) Hashtbl.t;
}

(** For visualization, use with ocamlGraph **)
let makeFNameHash (rts : ruleTrans list) =
  let mapping = Hashtbl.create 10
  and v = ref 0 in
  let add s =
    if not (Hashtbl.mem mapping s) then
      begin
        Hashtbl.replace mapping s !v;
        v := !v + 1
      end in
  let addRT (rt : ruleTrans) =
    add rt.lPos.fName;
    add rt.rPos.fName in
  List.iter addRT rts;
  {maxFName = !v; mapping = mapping;}

let argPosHash (fnh : fNameHash) (ap : argPos) =
  ap.pos * fnh.maxFName + (Hashtbl.find fnh.mapping ap.fName)

let argPosEq (a : argPos) (b : argPos) =
  a.pos = b.pos && a.fName = b.fName

let argPosCompare (fnh : fNameHash) a b =
  let hash = argPosHash fnh in
  compare (hash a) (hash b)

module Fnh : sig
  val getHash : unit -> fNameHash
  val setHash : fNameHash -> unit
end = struct
  let storedHash = ref None
  let getHash () =
    match !storedHash with
    | None -> raise Not_found
    | Some h -> h
  let setHash fnh =
    storedHash := Some fnh
end

module type Hashed = sig
  val getHash : unit -> fNameHash
end

module ArgPosNodes (F : Hashed) : sig
  type t = argPos
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end =
struct
  type t = argPos
  let equal = argPosEq
  let hash = argPosHash (F.getHash ())
  let compare = argPosCompare (F.getHash ())
end

module ArgPosGraph = Graph.Imperative.Graph.Concrete(ArgPosNodes(Fnh))

module Vis = Graph.Graphviz.Dot(struct
  include ArgPosGraph (* use the graph module from above *)
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [`Shape `Box]
  let vertex_name v = argPosToString v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let doVis (rts : ruleTrans list) fname =
  let g =  ArgPosGraph.create () in
  let addNodes rt =
    let lVert = ArgPosGraph.V.create rt.lPos
    and rVert = ArgPosGraph.V.create rt.rPos in
    ArgPosGraph.add_vertex g lVert;
    ArgPosGraph.add_vertex g rVert;
    ArgPosGraph.add_edge g lVert rVert in
  List.iter addNodes rts;
  Vis.output_graph fname g;
  g