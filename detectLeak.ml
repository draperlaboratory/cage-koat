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
  let getCon e = RL.Connectivity.find (getSym e) imap in
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
  let safeBranches = DB.Branches.map (marryBranchInfo infoMap) branchMap in
  List.map (fun secret ->
    let reachablefrom =
      try
        let rp = Hashtbl.find flowGraph secret in
        Hashtbl.fold (fun key el accum -> key::accum) rp []
      with Not_found -> [] in
    let dangerousBranches =
    List.fold_left (fun accum rp ->
      try
        (* if the symbol isn't part of a branch, it's a safe branch. *)
        if try DB.Branches.find rp.fName safeBranches with Not_found -> true
        then accum
        else rp.fName::accum
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
      Printf.printf "aoenuthoaensthu %s\n\n" !filename;
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      secretBranches system
    end


let _ = main ()
