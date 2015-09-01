open DepStructs
module DB = DetectBranch
module CR = Comrule
module RL = RuleLoop

let rec safe = function
  | [] -> true
  | hd::tl ->
    begin
      match hd with
      | RL.Cyclic
      | RL.ReachesCycle -> false
      | RL.Acyclic -> safe tl
    end

let marryBranchInfo imap bmapEl =
  let getSym e = e.DB.right.Term.fn in
  let getCon e =
    let sym = getSym e in
    try let res = RL.Connectivity.find sym imap in
        res
    with _ ->
      (* If this isn't Bottom, we've got an issue. *)
      failwith (Printf.sprintf "Couldn't find %s in imap.\n" sym) in
  let rec reduce seen = function
    | [] -> seen
    | hd::tl ->
      if List.mem hd seen
      then reduce seen tl
      else reduce (hd::seen) tl in
  let reachableInfos = List.map getCon bmapEl |> reduce [] in
  safe reachableInfos


let secretBranches (system : CR.rule list) (secrets : argPos list) =
  (* argument position flow graph *)
  let flowGraph =  Utils.concatMap RuleInfluence.processRule system |> Dep.computeGraph
  (* map function symbol to branches *)
  and branchMap = DB.processRelationships system
  (* map from function symbol to connectivity of that symbol *)
  and infoMap = RL.processRules system |> RL.infoMap in
  Printf.eprintf "Defining safe branches...\n";
  let safeBranches = DB.Branches.map (marryBranchInfo infoMap) branchMap in
  Printf.eprintf "Mapping over secrets...\n";
  List.map (fun secret ->
    let reachablefrom =
      try
        let rp =
          try
            Dep.ArgPosTable.find flowGraph secret
          with _ -> failwith (Printf.sprintf "Can't find %s %i in flowGraph"
          secret.fName secret.pos)
        in
        Dep.ArgPosTable.fold (fun key el accum -> el.Dep.argPos::accum) rp secrets
      with Not_found -> [] in
    let dangerousBranches =
    List.fold_left (fun accum apos ->
      try
        let fName = apos.fName in
        let branch =
          try DB.Branches.find fName branchMap
          with _ -> failwith (Printf.sprintf "Can't find %s in branchmap" fName)
        and safe =
          try DB.Branches.find fName safeBranches
          with _ -> failwith (Printf.sprintf "Can't find %s in safeBranches" fName)
        and influence = DB.argPosInfluencesBranch apos in
        if (not safe) &&
          List.fold_left (fun accum b -> accum || influence b) false branch
        then Utils.remdup (fName :: accum)
        else accum
      with Not_found -> accum) [] reachablefrom in
    dangerousBranches
  ) secrets

let getAnnotationSecrets fname =
  let annotation = Annot_parser.parsePackageFile fname in
  let functions = annotation.Annot.functions in
  let makeSecretArgPos fname accum secrets =
    List.fold_left (fun a i ->
      { fName = fname; pos = i; p = [], Big_int.zero_big_int;} :: a) accum secrets in
  let posWithDups = Annot.FMap.fold
    (fun _ ele accum -> makeSecretArgPos ele.Annot.fname accum ele.Annot.secretArgs)
    functions [] in
  Utils.remdup posWithDups

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
      Printf.printf "DetectLeak %s\n\n" !filename;
      let pos = getAnnotationSecrets !annotation in
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      let unsafe = secretBranches system in
      List.iter
        (fun l -> List.iter (Printf.eprintf "%s ") l; Printf.eprintf "\n")
        (unsafe pos)
    end


let _ = main ()
