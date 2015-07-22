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
end


let processRule (rule : Comrule.rule) =
  let (lfName, lhsArgs) = rule.Comrule.lhs
  and cond = rule.Comrule.cond in
  let module AO = struct
    let lhsArgs = List.length lhsArgs
    let condArgs = 2 * (List.length cond)
  end in
  let module Node = VarNodes(AO) in
  let module RuleGraph =
        Graph.Imperative.Digraph.ConcreteLabeled(Node)(QualEdge) in
  let module Reachability = Graph.Fixpoint.Make(RuleGraph)
        (struct
          type vertex = RuleGraph.E.vertex
          type edge = RuleGraph.E.t
          type g = RuleGraph.t
          type data = bool
          let direction = Graph.Fixpoint.Forward
          let equal = (=)
          let join = (||)
          let analyze _ = (fun x -> x)
         end)  in
  let graph = RuleGraph.create () in
  let lhsAdd i poly =
    let toAdd = LHS { var = poly;
                      pos = i;
                      qual = Equal } in
    RuleGraph.add_vertex graph toAdd;
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
    (** We need both the identity nodes for each element of the inequality.
        Then, we need the nodes that capture the nature of the delta.
     **)
    let a1 = Cond { var = p1; pos = left; qual = Equal; }
    and a2 = Cond { var = p2; pos = right; qual = Equal; }
    and a1' = Cond { var = p1; pos = left; qual = qual; }
    and a2' = Cond { var = p2; pos = right; qual = qual; } in
    (** Information really flows both ways across the inequality *)
    let edge1 = RuleGraph.E.create a1 qual a2'
    and edge2 = RuleGraph.E.create a2 qual a1' in
    RuleGraph.add_edge_e graph edge1;
    RuleGraph.add_edge_e graph edge2;
    (a1,a2) in
  let addEdge ?(graph = graph) rp1 rp2 =
    if shareVars rp1 rp2 then
      begin
        let qual = if Poly.compare (getPoly rp1) (getPoly rp2) = 0 then
            Equal else
            Delta in
        let rp1' = if qual = (getQual rp1) then rp1
          else withQual qual rp1 in
        let rp2' =
          if qual = (getQual rp2) then rp2
          else withQual qual rp2 in
        let edge = RuleGraph.E.create rp1 qual rp2'
        and edge2 = RuleGraph.E.create rp2' qual rp1
        and edge3 = RuleGraph.E.create rp1' qual rp2
        and edge4 = RuleGraph.E.create rp2 qual rp1' in
        RuleGraph.add_edge_e graph edge;
        RuleGraph.add_edge_e graph edge2;
        RuleGraph.add_edge_e graph edge3;
        RuleGraph.add_edge_e graph edge4
      end in
  let joinToCond ?(graph = graph) (lhsNode : rulePos) ((c1 : rulePos), (c2 : rulePos)) =
    addEdge lhsNode c1;
    addEdge lhsNode c2  in
  let (lhsNodes : rulePos list) = List.mapi lhsAdd lhsArgs
  and (condNodes : (rulePos * rulePos) list) = List.mapi condAdd cond in
  List.iter (fun lhsN -> List.iter (joinToCond lhsN) condNodes) lhsNodes;
  let dealWithRHS (rFName, rArgs) =
    let graph' = RuleGraph.copy(graph) in
    let rhsNodes = List.mapi (fun i poly -> 
      let toAdd = RHS { var = poly; pos = i; qual = Equal } in
      RuleGraph.add_vertex graph' toAdd;
      toAdd) rArgs in
    (* Stitch the right hand side nodes together *)
    List.iter (fun rn ->
      List.iter (addEdge ~graph:graph' rn) lhsNodes;
      List.iter (joinToCond ~graph:graph' rn) condNodes) rhsNodes;
    (* graph is ready for reachability analysis.  Now look at each argument position *)
    Utils.concatMapi (fun li lhsN ->
      let reachable = Reachability.analyze (Node.equal lhsN) graph' in
      let rtest n = try reachable n with Not_found -> false in
      let lpos = { fName = lfName; pos = li; } in
      List.fold_left (fun (accum, ri) rn ->
        let ri' = ri + 1 in
        let delta = withQual Delta rn in
        let rpos = { fName = rFName; pos = ri; } in
        if rtest delta
        then ({lPos = lpos; rPos = rpos; qual = Delta;} :: accum, ri')
        else if rtest rn
        then ({lPos = lpos; rPos = rpos; qual = Equal;} :: accum, ri')
        else (accum, ri')) ([], 0) rhsNodes |> fst) lhsNodes in
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
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      Utils.concatMap processRule system
    end
