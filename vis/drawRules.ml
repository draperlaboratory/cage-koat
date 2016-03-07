
let filename = ref ""


let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"


let rec speclist =
  [

    ("-help", Arg.Unit (fun () -> print_usage (); exit 1),
     "            - Display this list of options");
    ("--help", Arg.Unit (fun () -> print_usage (); exit 1), "");
    ("-log", Arg.Int (fun i -> Log.logging_level := i),
     Printf.sprintf "             - Print live log (level 1) or debug (level 5) output during proof [default %i]" !Log.logging_level);
    ("--log", Arg.Int (fun i -> Log.logging_level := i), "");
    ("-version", Arg.Unit (fun () -> Printf.printf "KoAT\nCopyright 2010-2014 Stephan Falke\nVersion %s\n" Git_sha1.git_sha1; exit 1),
     "         - Display the version of this program");
    ("--version", Arg.Unit (fun () -> Printf.printf "KoAT\nCopyright 2010-2014 Stephan Falke\nVersion %s\n" Git_sha1.git_sha1; exit 1), "");
  ]
and print_usage () =
  Arg.usage speclist usage


module FSymNode : sig
  type t = String.t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end =
struct
  type t = String.t
  let compare a b = String.compare a b
  let hash el = Hashtbl.hash el
  let equal = (=)
end

module RuleGraph = Graph.Imperative.Digraph.Concrete(FSymNode)

let draw fname g =
  let module Vis = Graph.Graphviz.Dot(struct
    include RuleGraph
  let edge_attributes _ = [`Style `Solid]
  let default_edge_attributes _ = []
  let get_subgraph vertex = None
  let vertex_attributes v = [`Shape `Box; `Label v ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
  end ) in
  let fdesc = Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY;] 0o640 in
  let fStream = Unix.out_channel_of_descr fdesc in
  Vis.output_graph fStream g;
  Unix.close fdesc

let buildGraph assoc =
  let g = RuleGraph.create () in
  let addNode (l,r) =
    let lVert = RuleGraph.V.create l
    and rVert = RuleGraph.V.create r in
    let edge = RuleGraph.E.create lVert () rVert in
    RuleGraph.add_vertex g lVert;
    RuleGraph.add_vertex g rVert;
    RuleGraph.add_edge_e g edge in
  List.iter addNode assoc;
  g

let convertFname (path : string) =
  let dname = Filename.dirname path
  and fname = Filename.basename path in
  assert (Filename.check_suffix fname ".koat");
  let base = Filename.chop_suffix fname ".koat" in
  dname ^ "/" ^ base ^ ".dot"

let main () =
  Arg.parse speclist (fun f -> filename := f) usage;
  if !filename = "" then
  (
    Printf.printf "%s\n" (Sys.argv.(0) ^ ": need a filename.");
    print_usage ();
    exit 1
  )
  else
    Log.init_timer ();
    let (_, cint) = Parser.parseCint !filename SimpleT.Ctrls in
    let edges = Comrule.getEdges cint in
    let graph = buildGraph edges in
    let fname = convertFname !filename in
    draw fname graph

let _ = main ()
