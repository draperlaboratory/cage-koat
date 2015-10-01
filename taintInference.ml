(** Compute taint analysis going forward from a set of argument positions *)

open DepStructs
module CR = Comrule

type rewrite = {
  lhs : Term.term;
  rhs : Term.term;
  cond : Pc.cond;
}

type branchInfo = {
  fName : string;
  cond : Pc.cond;
  id : int;
}

type influence =
| PassThrough of argPos
| Equal of argPos
| Delta of argPos
| Fresh of argPos
| Branch of argPos

type fSym = string
type apKey = (fSym * int)

type argMap = (fSym, Poly.var list) Hashtbl.t (* what variables are defined by which functions *)
type ruleMap = (fSym, rewrite list) Hashtbl.t (* lhs fSym -> rhs * cond mapping *)
type argInfluenceMap = (apKey , influence list) Hashtbl.t
type branchInfluenceMap = (fSym, branchInfo list) Hashtbl.t

let debugPrintTrans t =
  Printf.eprintf "%s -> %s :|: %s\n"
    (Term.toString t.lhs)
    (Term.toString t.rhs)
    (Pc.toString t.cond)

let debugPrintBIM (bim : branchInfluenceMap) =
  Hashtbl.iter (fun key influencers ->
    Printf.eprintf "%s is influenced by branches [ " key;
    List.iter (fun bi -> Printf.eprintf "%s_%i " bi.fName bi.id) influencers;
    Printf.eprintf "]\n") bim

let debugPrintInf = function
    | PassThrough ap -> Printf.eprintf " PT:%s_%i" ap.fName ap.pos
    | Equal ap -> Printf.eprintf " Eq:%s_%i" ap.fName ap.pos
    | Delta ap -> Printf.eprintf " Dl:%s_%i" ap.fName ap.pos
    | Fresh ap -> Printf.eprintf " Fr:%s_%i" ap.fName ap.pos
    | Branch ap -> Printf.eprintf " Br:%s_%i" ap.fName ap.pos

let debugPrintAIM (aim : argInfluenceMap) =
  Hashtbl.iter (fun (fSym, int) influencers ->
    Printf.eprintf "%s_%i influences [" fSym int;
    List.iter debugPrintInf influencers;
    Printf.eprintf " ]\n") aim

let equal (a : branchInfo) (b : branchInfo) =
  a.id = b.id && a.fName = b.fName

let key (ap : argPos) = ap.fName, ap.pos

let equalAP a b = a.pos = b.pos && a.fName = b.fName

let equalInf a = function
  | PassThrough b ->
    begin
      match a with
      | PassThrough a' -> equalAP a' b
      | _ -> false
    end
  | Equal b ->
    begin
      match a with
      | Equal a' -> equalAP a' b
      | _ -> false
    end
  | Delta b ->
    begin
      match a with
      | Delta a' -> equalAP a' b
      | _ -> false
    end
  | Fresh b ->
    begin
      match a with
      | Fresh a' -> equalAP a' b
      | _ -> false
    end
  | Branch b ->
    begin
      match a with
      | Branch a' -> equalAP a' b
      | _ -> false
    end


let rec makeRuleMapHelp accum = function
  | [] -> accum
  | rule::tl ->
    let lhsSym = rule.CR.lhs.Term.fn in
    let cond = rule.CR.cond in
    let prev = try Hashtbl.find accum lhsSym with Not_found -> [] in
    let toAdd =
      List.fold_left
        (fun accum el -> { lhs = rule.CR.lhs;
                           rhs = el;
                           cond = cond;} :: accum) prev rule.CR.rhss
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
  (* Initializing pass is done, now go again and propogate all information from
     cycles through *)
  List.iter (fun fSym ->
    ignore(branchInfluenceSearch branchInfluences ruleMap [] [] fSym))
    cycleStarts;
  (* now we need to condense the influencers s.t. we notice branches behind the
     post-dominator and remove their influence. *)
  condenseBranchInfluences ruleMap branchInfluences


let computeBaseArgumentInfluence (rm : ruleMap) (bim : branchInfluenceMap)
    (startingPositions :argPos list) =
  (* Start by seeding the table with the branch influences. *)
  let aim = Hashtbl.create 1000 in
  let updateAim (key : apKey) toAdd =
    let prev = try Hashtbl.find aim key with Not_found -> [] in
    if not (List.exists (equalInf toAdd) prev) then
      Hashtbl.replace aim key (toAdd::prev)
  in
  let rec findInCond cond fsym ind (accum : apKey list) = function
    | [] -> accum
    | hd::tl ->
      let accum' =
        if not (Pc.shareVars hd cond) then accum
        else (fsym, ind)::accum in
      findInCond cond fsym (ind + 1) accum' tl in
  let updateLeft influenced branches =
    let toAdd = Branch influenced in
    List.iter (fun bi ->
      let trans = List.nth (Hashtbl.find rm bi.fName) bi.id in
      debugPrintTrans trans;
      (* find all argument positions on the left influencing the condition *)
      let (keys : apKey list) = findInCond trans.cond trans.lhs.Term.fn 0 [] trans.lhs.Term.args in
(*      (* find all argument positions on the right influencing the condition *)
      let (keys' : apKey list) = findInCond trans.cond trans.rhs.Term.fn 0 keys trans.rhs.Term.args in *)
      List.iter
        (fun ((fSym,ind) as k : apKey) ->
          Printf.eprintf "%s_%i influencing" fSym ind;
          debugPrintInf toAdd;
          Printf.eprintf "\n";
          updateAim k toAdd)
        keys)
      branches in
  let incorporateRules branches transitions =
    List.iter
      (fun trans ->
        let lfsym = trans.lhs.Term.fn
        and rfsym = trans.rhs.Term.fn
        and isFresh rhArg =
          not (List.exists (Poly.shareVars rhArg) trans.lhs.Term.args) in
        Printf.eprintf "%s -> %s\n" lfsym rfsym;
        List.iteri (fun rhIndex rhArg ->
          let rhAP = { fName = rfsym; pos = rhIndex; p = rhArg;} in
          if not (Poly.isConst rhArg) then
            (* the right hand argument is fresh *)
            if isFresh rhArg then
                List.iteri (fun lhIndex lhArg ->
                  let (key : string * int) = lfsym, lhIndex in
                  let toAdd = Fresh rhAP in
                  let _ = { fName = lfsym; pos = lhIndex; p = lhArg } in
                  (* if there's a branch in play,
                     it's influencing a fresh variable. *)
                  updateLeft rhAP branches;
                (* then, every lhs variable influences this
                   rhs position *)
                  updateAim key toAdd) trans.lhs.Term.args
            else
              List.iteri (fun lhIndex lhArg ->
                let key = lfsym, lhIndex in
                let _ = { fName = lfsym; pos = lhIndex; p = lhArg } in
                if lhIndex = rhIndex && Poly.equal lhArg rhArg
                  (* arguments and indexes are equal. straight up passthrough *)
                then
                    updateAim key (PassThrough rhAP)
                  (* argument changes position. Equal, but order changes *)
                else if Poly.equal lhArg rhArg then begin
                  updateAim key (Equal rhAP);
                  updateLeft rhAP branches
                end
                  (* some variables are shared -- it's a delta on the previous argument. *)
                else if Poly.shareVars lhArg rhArg then begin
                  updateAim key (Delta rhAP);
                  updateLeft rhAP branches
                end
                  (* no relationship whatsoever. *)
                else ()
              ) trans.lhs.Term.args
        ) trans.rhs.Term.args
      ) transitions
  in
  Hashtbl.iter (fun fSym ruleList ->
    let branches =
      try
        Hashtbl.find bim fSym
      with Not_found -> [] in
    incorporateRules branches ruleList;
  ) rm;
  aim


let rec computeInfluences (aim : argInfluenceMap) =
  let update = ref false in
  let stripAP = function
    | PassThrough ap
    | Equal ap
    | Delta ap
    | Fresh ap
    | Branch ap -> ap in
  let updateInfluences apKey influences =
    (* merege the list of influencers for all of the children of this guy into
       this guy. *)
    let replacement =
      List.fold_left (fun accum inf ->
        let ap = stripAP inf in
        let toAdd = try Hashtbl.find aim (key ap) with Not_found -> [] in
        let accum' = Utils.remdupC equalInf (toAdd @ accum) in
      (* if the update adds anything, make a note of it *)
        if List.length accum <> List.length accum' then
          update := true;
        accum') influences influences in
    Hashtbl.replace aim apKey replacement
  in
  (* run an iteration of the updating *)
  Hashtbl.iter updateInfluences aim;
  if !update then
    (* repeat until you hit a fixed state. *)
    computeInfluences aim


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
      let argInfluence = computeBaseArgumentInfluence ruleMap branchInfluences pos in
      debugPrintBIM branchInfluences;
      Printf.eprintf "\n";
      debugPrintAIM argInfluence;
      Printf.eprintf "\n";
      computeInfluences argInfluence;
      Printf.eprintf "After computing TC:\n";
      debugPrintAIM argInfluence;
      branchInfluences, argMap
    end


let _ = main ()
