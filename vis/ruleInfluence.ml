open DepStructs

type argument = {
  var : Poly.poly;
  pos : int;
  qual : qual;
}

type rulePos =
| LHS of argument
| RHS of argument
| Cond of argument

let rulePosToString = function
  | LHS a -> Printf.sprintf "LHS %i %s %s" a.pos (Poly.toString a.var) (qualToString a.qual)
  | RHS a -> Printf.sprintf "RHS %i %s %s" a.pos  (Poly.toString a.var) (qualToString a.qual)
  | Cond a -> Printf.sprintf "Cond %i %s %s" a.pos  (Poly.toString a.var) (qualToString a.qual)

let getPoly = function
  | LHS a
  | RHS a
  | Cond a -> a.var

let getQual = function
  | LHS a
  | RHS a
  | Cond a -> a.qual

let withQual qual = function
  | LHS a -> LHS { a with qual = qual }
  | RHS a -> RHS { a with qual = qual }
  | Cond a -> Cond { a with qual = qual }

let shareVars rp1 rp2 =
  Poly.shareVars (getPoly rp1) (getPoly rp2)

module type ArgOffset = sig
  val lhsArgs : int
  val condArgs : int
end

module VarNodes (M : ArgOffset) = struct
  type t = rulePos
  let qualOffset a = if a.qual = Equal then 0 else 1
  let myIndex a = 2 * a.pos + (qualOffset a)
  let lhsOffset = 0
  let condOffset = 2 * M.lhsArgs
  let rhsOffset = condOffset + 2 * M.condArgs
  let hash = function
  (* lhs 0 Equal -> 0, lhs 0 Delta -> 1 ... *)
    | LHS a ->  lhsOffset  + (myIndex a)
    | Cond a -> condOffset + (myIndex a)
    | RHS a ->  rhsOffset  + (myIndex a)
  let compare a b = compare (hash a) (hash b)
  let equal a b = match (a,b) with
    | LHS a1, LHS a2
    | RHS a1, RHS a2
    | Cond a1, Cond a2 -> a1.pos = a2.pos
    | _,_ -> false
  let (=) = equal
  let (>) a b = (hash a) > (hash b)
  let (>=) a b = (hash a) >= (hash b)
end


(* This looks horrifying, but soldier on.  To get a perfect hash function on the
   nodes in the graph, we need to make the modules locally, since we don't know
   how large the rules are going to be until the function gets called, so it
   all ends up bundled in here. *)

let processRule (rule : Comrule.rule) =
  let lfName = rule.Comrule.lhs.Term.fn
  and lhsArgs = rule.Comrule.lhs.Term.args
  and cond = rule.Comrule.cond in
  let module AO = struct
    let lhsArgs = List.length lhsArgs
    let condArgs = 2 * (List.length cond) end in
  let module Node = VarNodes(AO) in
  let module G =
        Graph.Imperative.Digraph.ConcreteLabeled(Node)(QualEdge) in
  let module Reachability = Graph.Fixpoint.Make(G) (struct
    type vertex = G.E.vertex
    type edge = G.E.t
    type g = G.t
    type data = bool
    let direction = Graph.Fixpoint.Forward
    let equal = (=)
    let join = (||)
    let analyze _ = (fun x -> x) end)  in
  let graph = G.create () in
  let lhsAdd i poly =
    let toAdd = LHS { var = poly; pos = i; qual = Equal } in
    G.add_vertex graph toAdd;
    toAdd in
  let condAdd left atom =
    let right = left + 1
    and (qual, p1, p2) = match atom with
      | Pc.Equ (p1, p2) -> (Equal, p1, p2)
      | Pc.Neq (p1, p2)
      | Pc.Geq (p1, p2)
      | Pc.Gtr (p1, p2)
      | Pc.Leq (p1, p2)
      | Pc.Lss (p1, p2) -> (Delta, p1, p2) in
    let a1 = Cond { var = p1; pos = left; qual = Equal; }
    and a2 = Cond { var = p2; pos = right; qual = Equal; } in
    (** Information really flows both ways across the inequality *)
    G.E.create a1 qual (withQual qual a2) |> G.add_edge_e graph;
    G.E.create a2 qual (withQual qual a1) |> G.add_edge_e graph;
    (a1,a2) in
  let addEdge ?(graph = graph) rp1 rp2 =
    if shareVars rp1 rp2 then
      let qual = if Poly.compare (getPoly rp1) (getPoly rp2) = 0 then
          Equal else
          Delta in
        G.E.create rp1 qual (withQual qual rp2) |> G.add_edge_e graph;
        G.E.create (withQual qual rp2) qual rp1 |> G.add_edge_e graph;
        G.E.create (withQual qual rp1) qual rp2 |> G.add_edge_e graph;
        G.E.create rp2 qual (withQual qual rp1) |> G.add_edge_e graph in
  let joinToCond ?(graph = graph) (lhsNode : rulePos) ((c1 : rulePos), (c2 : rulePos)) =
    addEdge lhsNode c1;
    addEdge lhsNode c2  in
  let (lhsNodes : rulePos list) = List.mapi lhsAdd lhsArgs
  and (condNodes : (rulePos * rulePos) list) = List.mapi condAdd cond in
  let dealWithRHS r =
    let rFName = r.Term.fn
    and rArgs = r.Term.args in
    let graph' = G.copy(graph) in
    let rhsNodes = List.mapi (fun i poly ->
      let toAdd = RHS { var = poly; pos = i; qual = Equal } in
      G.add_vertex graph' toAdd;
      toAdd) rArgs in
    (* Stitch the right hand side nodes together *)
    List.iter (fun rn ->
      List.iter (addEdge ~graph:graph' rn) lhsNodes;
      List.iter (joinToCond ~graph:graph' rn) condNodes) rhsNodes;
    (* graph is ready for reachability analysis.  Now look at each argument position *)
    Utils.concatMapi (fun li lhsN ->
      let reachable = Reachability.analyze (Node.equal lhsN) graph' in
      let rtest n = try reachable n with Not_found -> false in
      let lpos = { fName = lfName; pos = li; p = getPoly lhsN } in
      List.fold_left (fun (accum, ri) rn ->
        let ri' = ri + 1 in
        let delta = withQual Delta rn in
        let rpos = { fName = rFName; pos = ri; p = getPoly rn } in
        if rtest delta
        then ({lPos = lpos; rPos = rpos; qual = Delta;} :: accum, ri')
        else if rtest rn
        then ({lPos = lpos; rPos = rpos; qual = Equal;} :: accum, ri')
        else (accum, ri')) ([], 0) rhsNodes |> fst) lhsNodes in
  List.iter (fun lhsN -> List.iter (joinToCond lhsN) condNodes) lhsNodes;
  Utils.concatMap dealWithRHS rule.Comrule.rhss


let main () =
  let usage = ""
  and filename = ref "" in
  Arg.parse [] (fun f -> filename := f) usage;
  if !filename = "" then
    begin
      Printf.eprintf "Expected filename as only argument.\n";
      exit 1
    end
  else
    begin
      Printf.printf "Processing %s\n\n" !filename;
      let entrFun, system = Parser.parseCint !filename SimpleT.Stmts in
      Utils.concatMap processRule system
    end
