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
  let getSym e = fst e.DB.right in
  let getCon e =
    let sym = getSym e in
    try RL.Connectivity.find sym imap
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

let main () =
  let usage = "" in
  let filename = ref "" in
  Arg.parse [] (fun f -> filename := f) usage;
  if !filename = "" then
    begin
      Printf.eprintf "Expected filename as only argument.\n";
      exit 1
    end
  else
    begin
      Printf.printf "DetectLeak %s\n\n" !filename;
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      let unsafe = secretBranches system in
      let pos = { fName = "f"; pos = 0; p = [], Big_int.zero_big_int} in
      List.iter (fun l -> List.iter (Printf.eprintf "%s ") l; Printf.eprintf "\n") (unsafe [pos])
    end


let _ = main ()
