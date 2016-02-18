
let combine = ref SimpleT.Ctrls
let filename = ref ""
let ifacename = ref ""
let timeout = ref 0.0
let maxchaining = ref 15
let is_space = ref false

IFDEF HAVE_Z3 THEN
let supportedSmtSolvers = ["yices";"yices2";"z3";"cvc4";"mathsat5";"z3-internal"]
ELSE
let supportedSmtSolvers = ["yices";"yices2";"z3";"cvc4";"mathsat5";"z3-internal"]
  END

let stringToCombine s printer =
  if s = "statements" then
    SimpleT.Stmts
  else if s = "controlpoints" then
    SimpleT.Ctrls
  else if s = "ifs-loops" then
    SimpleT.IfsLoops
  else if s = "loops" then
    SimpleT.Loops
  else
  (
    Printf.printf "%s\n" (Sys.argv.(0) ^ ": unknown combination method `" ^ s ^ "'.");
    printer ();
    exit 1
  )

let checkSmtSolver s printer =
  if Utils.contains supportedSmtSolvers s then
    s
  else
    (
      Printf.printf "%s\n" (Sys.argv.(0) ^ ": unknown SMT solver `" ^ s ^ "'.");
      printer ();
      exit 1
    )

let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"


let rec speclist =
  [
    ("-combine", Arg.String (fun s -> combine := stringToCombine s print_usage),
     "         - Set the combination method for Simple programs (statements/controlpoints/ifs-loops/loops)");
    ("--combine", Arg.String (fun s -> combine := stringToCombine s print_usage), "");
    ("-timeout", Arg.Set_float timeout,
     Printf.sprintf "         - Set the timeout (seconds, 0 = no timeout) [default %.2f]" !timeout);
    ("--timeout", Arg.Set_float timeout, "");
    ("-smt-solver", Arg.String (fun s -> Smt.setSolver (checkSmtSolver
s print_usage)),
     "      - Set the SMT solver (" ^ (String.concat "/" supportedSmtSolvers) ^ ")");
    ("--smt-solver", Arg.String (fun s -> Smt.setSolver (checkSmtSolver s print_usage)), "");
    ("-max-chaining", Arg.Set_int maxchaining,
     Printf.sprintf "    - Set the maximum number of chaining processor applications [default %i]" !maxchaining);
    ("--max-chaining", Arg.Set_int maxchaining, "");
    ("-help", Arg.Unit (fun () -> print_usage (); exit 1),
     "            - Display this list of options");
    ("--help", Arg.Unit (fun () -> print_usage (); exit 1), "");
    ("-log", Arg.Int (fun i -> Log.logging_level := i),
     Printf.sprintf "             - Print live log (level 1) or debug (level 5) output during proof [default %i]" !Log.logging_level);
    ("--log", Arg.Int (fun i -> Log.logging_level := i), "");
    ("-version", Arg.Unit (fun () -> Printf.printf "KoAT\nCopyright 2010-2014 Stephan Falke\nVersion %s\n" Git_sha1.git_sha1; exit 1),
     "         - Display the version of this program");
    ("--version", Arg.Unit (fun () -> Printf.printf "KoAT\nCopyright 2010-2014 Stephan Falke\nVersion %s\n" Git_sha1.git_sha1; exit 1), "");
    ("-iface-file", Arg.Set_string ifacename,
     "      - Set the .json interface file for specifying complexity");
    ("-space-complexity", Arg.Set is_space,
     "- Compute the space complexity instead of time")
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

let draw g =
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
  let fname = "foo.dot" in
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
    let (startFun, cint) = Parser.parseCint !filename !combine in
    let ctype = if !is_space then Complexity.Space else Complexity.Time in
    let edges = Comrule.getEdges cint in
    let graph = buildGraph edges in
    draw graph

let _ = main ()
