(* Entry point for information flow module *)
open DepStructs

let usage = ""

type argument = {
  position : int;
  vars : string list;
  fName : string;
}

type cond = {
  position : int;
  vars : string list;
}

type var =
| Argument of argument
| Cond of cond

type edge = {
  source : var;
  sink : var;
  qual : qual;
}

type node = {
  arrivedBy : edge option;
  parent : node option;
  tip : var;
  encounteredDelta : bool;
}

(** Debug prints **)

let varsToString vList =
  List.fold_left (fun accum v ->
    if accum = ""
    then v
    else Printf.sprintf "%s, %s" accum v) "" vList

let argumentToString (arg : argument) =
  Printf.sprintf "%s %i %s" arg.fName arg.position (varsToString arg.vars)

let condToString (cond : cond) =
  Printf.sprintf "cond %i %s" cond.position (varsToString cond.vars)

let varToString = function
  | Argument a -> argumentToString a
  | Cond c -> condToString c

let edgeToString e =
  Printf.sprintf "%s -> %s : %s"
    (varToString e.source)
    (varToString e.sink)
    (qualToString e.qual)

let rec pathToString (n : node) =
    match n.arrivedBy with
  | None -> varToString n.tip
  | Some e -> Printf.sprintf "%s\t%s"
    (match n.parent with
    | None -> varToString n.tip
    | Some p -> pathToString p) (edgeToString e)

(** Search Code **)

let rec getRoot (n : node) =
  match n.parent with
  | None -> n
  | Some n' -> getRoot n'

(* this is wrong, it has to encompass the root! *)
let key (n : node) =
  let root = getRoot n in
  (root.tip, n.tip)

let better (a : node) (b : node) =
  a.encounteredDelta ||
    a.encounteredDelta = b.encounteredDelta

let varsOf = function
  | Argument a -> a.vars
  | Cond c -> c.vars

let varsByArg (pList : Poly.poly list) =
  List.map Poly.getVars pList

let varsByAtom = function
  | Pc.Equ (p1, p2)
  | Pc.Neq (p1, p2)
  | Pc.Geq (p1, p2)
  | Pc.Gtr (p1, p2)
  | Pc.Leq (p1, p2)
  | Pc.Lss (p1, p2) -> (Poly.getVars p1) @ (Poly.getVars p2) |> Utils.remdup


let argFilter fName = function
  | Cond _ -> true
  | Argument b -> b.fName != fName

let condFilter c c' =
  c != c'

let makeFilter = function
  | Argument a -> argFilter a.fName
  | Cond _ as cnd -> condFilter cnd

let expand (varTable : (string, var list) Hashtbl.t) var =
  let touches = varsOf var
  and filter = makeFilter var in
  let filterConcat accum var =
    List.filter filter (Hashtbl.find varTable var) @ accum in
  let adjacent = List.fold_left filterConcat [] touches |> Utils.remdup in
  List.map (fun dest -> {source = var; sink = dest; qual = Unkown;}) adjacent

let processClosed (closed : ((var * var), node) Hashtbl.t) =
  let lookup = Hashtbl.create 100 in
  Hashtbl.iter (fun (rootVars, tipVars) node ->
(*    Printf.eprintf "Processing var: %s\n%s\n\n" (varToString tipVars) (pathToString node);*)
    let vars = varsOf tipVars in
    List.iter (fun v ->
      try
        let prev = Hashtbl.find lookup v in
        node :: prev |> Hashtbl.replace lookup v
      with Not_found ->
        Hashtbl.replace lookup v [node]) vars) closed;
  lookup

let breadthFirst (openList : node list) (expand : node -> edge list) =
  let closed = Hashtbl.create 100 in
  let handleNode n =
    try
      let kv = key n in
      let prev = Hashtbl.find closed kv in
      if not (better prev n) (* prev isn't better than n, re-open *)
      then
        begin
          Hashtbl.replace closed kv n;
          expand n
        end
      else []
    with Not_found ->
      Hashtbl.replace closed (key n) n;
      expand n in
  let rec search = function
    | [] -> processClosed closed
    | hd::tl ->
      let edges = handleNode hd in
      let children = List.map
        (fun e -> { arrivedBy = Some e;
                    parent = Some hd;
                    tip = e.sink;
                    encounteredDelta = hd.encounteredDelta || e.qual = Delta;})
        edges in
      tl @ children |> search in
  search openList

(** Find the connectivity in a rule **)

let preProcess (lhs : Term.term) (cond : Pc.cond) =
  let varTable = Hashtbl.create 100 in (* varName -> vars *)
  let addArg a =
    varsOf a |>
        List.iter
      (fun v ->
        try
          let prev = Hashtbl.find varTable v in
          a::prev |> Hashtbl.replace varTable v
        with Not_found -> Hashtbl.replace varTable v [a]) in
  let (fname, args) = lhs in
  let varsByArgs = varsByArg args
  and makeArg i e =
    let arg = Argument {position = i; vars = e; fName = fname} in
    addArg arg;
    arg in
  let lhsVars =  List.mapi makeArg varsByArgs in
  let varsByCond = List.map varsByAtom cond in
  let makeCond i e =
    let cond = Cond {position = i; vars = e;} in
    addArg cond in
  List.iteri makeCond varsByCond;
  let startingNodes =
    List.map (fun v -> { arrivedBy = None; parent = None; tip = v; encounteredDelta = false;})
      lhsVars in
  let lookup = breadthFirst startingNodes (fun v -> (expand varTable v.tip)) in
  let generateMapping rhs =
    Printf.eprintf "RHS: %s\n" (Term.toString rhs);
    let (rname, args) = rhs in
    let varsByArgs = varsByArg args in
    let relations =
      Utils.concatMapi (fun argPos vList  ->
        let uniqueHits =
        List.fold_left (fun accum e ->
          try
            let hits = Hashtbl.find lookup e in
            (Utils.remdup hits) @ accum
          with Not_found ->
            accum)
          [] vList in
        List.map (fun node ->
          let root = getRoot node in
          match root.tip with
          | Cond c -> failwith "Conds can't be roots!"
          | Argument a ->
            {lPos = {fName = a.fName;
                     pos = a.position;};
             rPos = {fName = rname;
                     pos = argPos;};
             qual = if not node.encounteredDelta then Equal else Delta;}) uniqueHits)
        varsByArgs in
    let relations' = Utils.remdup relations in
    List.iter (fun r -> Printf.eprintf "%s\n" (ruleTransToString r)) relations';
    relations'
  in generateMapping

let processRule (rule : Comrule.rule) =
  Printf.eprintf "Rule: %s\n" (Comrule.toString rule);
  Utils.concatMap (preProcess rule.Comrule.lhs rule.Comrule.cond) rule.Comrule.rhss

let main () =
  let filename = ref "" in
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
