(* Entry point for information flow module *)

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

type qual =
| Equal
| Delta
| Unkown

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

let qualToString = function
  | Equal -> "Equal"
  | Delta -> "Delta"
  | Unkown -> "?"

let edgeToString e =
  Printf.sprintf "%s -> %s : %s"
    (varToString e.source)
    (varToString e.sink)
    (qualToString e.qual)

let pathToString (n : node) =
  match n.arrivedBy with
  | None -> ""
  | Some e -> Printf.sprintf "%s" (edgeToString e)

let key (n : node) = n.tip
let better (a : node) (b : node) =
  a.encounteredDelta ||
    a.encounteredDelta = b.encounteredDelta

type path = edge list

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

let processClosed (closed : (var, node) Hashtbl.t) =
  let lookup = Hashtbl.create 100 in
  Hashtbl.iter (fun var node ->
    let vars = varsOf var in
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
    let (_, args) = rhs in
    let varsByArgs = varsByArg args in
    let nodesByArgs =
      List.map (fun vList ->
        List.fold_left (fun accum e ->
          try
            (Utils.remdup (Hashtbl.find lookup e)) @ accum
          with Not_found -> accum)
          [] vList)
        varsByArgs in
    nodesByArgs
  in generateMapping

let processRule (rule : Comrule.rule) =
  List.map (preProcess rule.Comrule.lhs rule.Comrule.cond) rule.Comrule.rhss

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
      Printf.printf "%s\n" (Cint.toString system);
      Printf.printf "Starting at %s\n" entrFun;
      List.map processRule system
    end

let _ = main()
