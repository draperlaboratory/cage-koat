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
    addArg cond;
    cond in
  let condVars = List.mapi makeCond varsByCond in
  (fun rhs -> ())

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
      Printf.printf "Starting at %s\n" entrFun
    end

let _ = main()
