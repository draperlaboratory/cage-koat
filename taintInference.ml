(** Compute taint analysis going forward from a set of argument positions *)

open DepStructs
module CR = Comrule

type rewrite = {
  rhs : Term.term;
  cond : Pc.cond;
}

type branchInfo = {
  fName : string;
  cond : Pc.cond;
  id : int;
}

type influence = int

type fSym = string
type apKey = (fSym * int)

type argMap = (fSym, Poly.var list) Hashtbl.t (* what variables are defined by which functions *)
type ruleMap = (fSym, rewrite list) Hashtbl.t (* lhs fSym -> rhs * cond mapping *)
type argInfluenceMap = (apKey , influence list) Hashtbl.t
type branchInfluenceMap = (fSym, branchInfo list) Hashtbl.t

let debugPrintBIM (bim : branchInfluenceMap) =
  Hashtbl.iter (fun key influencers ->
    Printf.eprintf "%s is influenced by branches [" key;
    List.iter (fun bi -> Printf.eprintf "%s_%i " bi.fName bi.id) influencers;
    Printf.eprintf "]\n") bim

let equal (a : branchInfo) (b : branchInfo) =
  a.id = b.id && a.fName = b.fName

let key (ap : argPos) = ap.fName, ap.pos


let rec makeRuleMapHelp accum = function
  | [] -> accum
  | rule::tl ->
    let lhsSym = rule.CR.lhs.Term.fn in
    let cond = rule.CR.cond in
    let prev = try Hashtbl.find accum lhsSym with Not_found -> [] in
    let toAdd =
      List.fold_left
        (fun accum el -> { rhs = el; cond = cond;} :: accum) prev rule.CR.rhss
    in
    Hashtbl.replace accum lhsSym toAdd;
    makeRuleMapHelp accum tl

let makeRuleMap (rules : CR.rule list) =
  (** produce a mapping of funSym -> rhs * conds, so I can quickly see what a
      given rule rewrites to.**)
  makeRuleMapHelp (Hashtbl.create (List.length rules)) rules


let makeVarMap rules =
  let tbl = Hashtbl.create (List.length rules) in
  let rec makeVarMapHelp = function
    | [] -> tbl
    | rule::tl ->
      Hashtbl.replace tbl rule.CR.lhs.Term.fn (Term.getVars rule.CR.lhs);
      makeVarMapHelp tl in
  makeVarMapHelp rules


let getAnnotationSecrets fname rules =
  let argTable = Hashtbl.create (List.length rules) in
  List.iter
    (fun rule ->
      Hashtbl.replace argTable rule.CR.lhs.Term.fn rule.CR.lhs.Term.args;
      List.iter (fun rhs ->
        if not (Hashtbl.mem argTable rhs.Term.fn)
        then Hashtbl.replace argTable rhs.Term.fn rhs.Term.args) rule.CR.rhss)
    rules;
  let annotation = Annot_parser.parsePackageFile fname in
  let functions = annotation.Annot.functions in
  let makeSecretArgPos fname accum secrets =
    let args = Hashtbl.find argTable fname in
    List.fold_left (fun a i ->
      { fName = fname; pos = i; p = List.nth args i} :: a) accum secrets in
  let posWithDups = Annot.FMap.fold
    (fun _ ele accum -> makeSecretArgPos ele.Annot.fname accum ele.Annot.secretArgs)
    functions [] in
  Utils.remdup posWithDups


let rec branchInfluenceSearch (bim : branchInfluenceMap) (rm : ruleMap)
    (seen : fSym list) (influences : branchInfo list) (current : fSym) =
  let rec checkCycle = function
    | [] -> false
    | hd::tl -> hd = current || checkCycle tl in
  let prev = try Hashtbl.find bim current with Not_found -> [] in
  let influences' = Utils.remdupC equal (prev @ influences) in
  let recurHelp (children : rewrite list)=
    let seen' = current :: seen
    and i = ref 0 in
    let recurBase = branchInfluenceSearch bim rm seen' in
    List.fold_left (fun accum (child : rewrite) ->
      let branchInfo = { fName = current; cond = child.cond; id = !i } in
      let influences'' = branchInfo :: influences' in
      i := !i + 1;
      accum @ (recurBase influences'' child.rhs.Term.fn)) [] children in
  (* update the influences *)
  Hashtbl.replace bim current influences';
  (* maybe recur *)
  if checkCycle seen then
    (* current is the begining of a loop, collect it so we know what to propogate from later. *)
    [current] else
    begin
      (* not a cycle, keep pushing info to the kids *)
      let children = try Hashtbl.find rm current with Not_found -> [] (* for terminals *) in
      recurHelp children
    end

let condenseSymbol (rm : ruleMap) (fs : fSym) (branchInfos : branchInfo list) =
  (* TODO -- We should be asking the SMT solver this question. *)
  let rewrites = Hashtbl.find rm fs in
  if (List.length rewrites) == (List.length branchInfos) then
    [] else
    branchInfos

let condenseInfluenceList (rm : ruleMap) (bil: branchInfo list) =
  let bySymbol = Hashtbl.create (List.length bil) in
  List.iter (fun (el : branchInfo) ->
    let prev = try Hashtbl.find bySymbol el.fName with Not_found -> [] in
    Hashtbl.replace bySymbol el.fName (el::prev)) bil;
  Hashtbl.fold (fun key branchInfos accum ->
    condenseSymbol rm key branchInfos @ accum) bySymbol []

let condenseBranchInfluences (rm : ruleMap) (bim : branchInfluenceMap) =
  Hashtbl.iter (fun k e -> Hashtbl.replace bim k (condenseInfluenceList rm e)) bim;
  bim


let findBranchInfluence (ruleMap : ruleMap) (startingPositions : argPos list) =
  let branchInfluences = Hashtbl.create (Hashtbl.length ruleMap) in
  let cycleStarts =
    Utils.remdup(
      List.fold_left (fun accum (el : argPos) ->
        accum @ (branchInfluenceSearch branchInfluences ruleMap [] [] el.fName))
        [] startingPositions)
  in
  debugPrintBIM branchInfluences;
  (* Initializing pass is done, now go again and propogate all information from
     cycles through *)
  List.iter (fun fSym ->
    Printf.eprintf "Updating from top of cycle %s\n" fSym;
    ignore(branchInfluenceSearch branchInfluences ruleMap [] [] fSym))
    cycleStarts;
  debugPrintBIM branchInfluences;
  (* now we need to condense the influencers s.t. we notice branches behind the
     post-dominator and remove their influence. *)
  condenseBranchInfluences ruleMap branchInfluences

let main () =
  let usage = "" in
  let filename = ref "" in
  let annotation = ref "" in
  Arg.parse
    [
      "--its", Arg.Set_string filename, "Sets the path to the integer transition system";
      "--annot", Arg.Set_string annotation, "Sets the path to the annotation file";
    ] (fun _ -> ()) usage;
  if !filename = "" then
    begin
      Printf.eprintf "Must supply an its filename using --its.\n";
      exit 1
    end
  else if !annotation = "" then
    begin
      Printf.eprintf "Must supply an annotation filename using --annot.\n";
      exit 1
    end
  else
    begin
      Printf.printf "taintInference %s %s\n\n" !filename !annotation;
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      let pos = getAnnotationSecrets !annotation system in
      let ruleMap = makeRuleMap system in
      let argMap = makeVarMap system in
      let branchInfluences = findBranchInfluence ruleMap pos in
      debugPrintBIM branchInfluences;
      branchInfluences, argMap
    end


let _ = main ()
