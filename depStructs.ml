type argPos = {
  fName : string;
  pos : int;
}

type qual =
| Equal
| Delta
| Unknown

type ruleTrans = {
  lPos : argPos;
  rPos : argPos;
  qual : qual;
}

type fNameHash = {
  maxFName : int;
  mapping : (string,int) Hashtbl.t;
}

let qualToString = function
  | Equal -> "Equal"
  | Delta -> "Delta"
  | Unknown -> "?"

let argPosToString ap =
  Printf.sprintf "%s_%i" ap.fName ap.pos

let ruleTransToString rt =
  Printf.sprintf "%s %s %s"
    (argPosToString rt.lPos)
    (qualToString rt.qual)
    (argPosToString rt.rPos)

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
    | None -> (Printf.eprintf "Couldn't find Function Sym Hash!\n"; raise Not_found)
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
  let equal a b = argPosEq a b
  let hash el = argPosHash (F.getHash ()) el
  let compare a b = argPosCompare (F.getHash ()) a b
end

module QualEdge : sig
  type t = qual
  val compare : t -> t -> int
  val default : t
end = struct
  type t = qual
  let compare q1 q2 = -1
  let default = Unknown
end

module ArgPosGraph = Graph.Imperative.Digraph.ConcreteLabeled(ArgPosNodes(Fnh))(QualEdge)
module Reachability = Graph.Fixpoint.Make (ArgPosGraph)(struct
    type vertex = ArgPosGraph.V.t
    type edge = ArgPosGraph.E.t
    type g = ArgPosGraph.t
    type data = bool
    let direction = Graph.Fixpoint.Forward
    let equal = (=)
    let join = (||)
    let analyze _ = (fun x -> x)
end)

let convertFname (path : string) =
  let dname = Filename.dirname path
  and fname = Filename.basename path in
  assert (Filename.check_suffix fname ".koat");
  let base = Filename.chop_suffix fname ".koat" in
  dname ^ "/" ^ base ^ ".dot"

let buildFlowGraph (rts : ruleTrans list) =
  let fnh = makeFNameHash rts in
  Fnh.setHash fnh;
  let g =  ArgPosGraph.create () in
  let addNodes rt =
    Printf.eprintf "Adding %s\n" (ruleTransToString rt);
    let lVert = ArgPosGraph.V.create rt.lPos
    and rVert = ArgPosGraph.V.create rt.rPos in
    let edge = ArgPosGraph.E.create lVert rt.qual rVert in
    ArgPosGraph.add_vertex g lVert;
    ArgPosGraph.add_vertex g rVert;
    ArgPosGraph.add_edge_e g edge in
  List.iter addNodes rts;
  g

let reachable (starts : argPos list)  (graph : ArgPosGraph.t) =
  let reachable = Reachability.analyze (fun a -> List.mem a starts) graph in
  let canReach (ap : argPos) =
    try
      ArgPosGraph.V.create ap |> reachable
    with Not_found -> false in
  canReach

let draw ?(augmentVertex = (fun v -> []) ) g fname =
  let module Vis = Graph.Graphviz.Dot(struct
  include ArgPosGraph (* use the graph module from above *)
  let red = `Color 0xFF0000
  and green = `Color 0x00FF00
  and blue = `Color 0x0000FF
  let edge_attributes e =
    match ArgPosGraph.E.label e with
    | Unknown -> [`Label " Unknown "; `Style `Dotted ; red]
    | Equal -> [`Label " = "; `Style `Solid; blue ]
    | Delta -> [`Label " d "; `Style `Bold; green ]
  let default_edge_attributes _ = []
  let get_subgraph vertex =
    let sgName = vertex.fName in
    Some { Graph.Graphviz.DotAttributes.sg_name = sgName;
           Graph.Graphviz.DotAttributes.sg_attributes = [`Label sgName]; }
  let vertex_attributes v =
    let base = [`Shape `Box; `Label (Printf.sprintf "%i" v.pos)]
    and addition = augmentVertex v in
    base @ addition
  let vertex_name v = argPosToString v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
  end) in
  let fname = convertFname fname in
  let fdesc = Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY;] 0o640 in
  let fStream = Unix.out_channel_of_descr fdesc in
  Vis.output_graph fStream g;
  Unix.close fdesc

let doVis (rts : ruleTrans list) fname =
  let g = buildFlowGraph rts in
  draw g fname
