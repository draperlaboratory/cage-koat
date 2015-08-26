open DepStructs

type reachablePosition = {
  argPos : argPos;
  qual : qual;
}

module ArgPosHash = struct
  type t = argPos
  let equal a b = a.pos = b.pos && a.fName = b.fName
  let hash i = Hashtbl.hash (i.pos, i.fName)
end

module ArgPosTable = Hashtbl.Make(ArgPosHash)

type reachablePositions = reachablePosition ArgPosTable.t
type reachableGraph = reachablePositions ArgPosTable.t

(** Is a given argument position critical to its function in a reachableGraph

    That is, after the graph has saturated, is there a delta path between this
    argument and itself.  **)
let criticalArgument graph (pos : argPos) =
  let reachableFrom = ArgPosTable.find graph pos in
  try
    let rp = ArgPosTable.find reachableFrom pos in
    match rp.qual with
    | Delta -> true
    | _ -> false
  with Not_found -> false


(**
   Updates a reachablePositions with a new reachablePosition.
   Returns whether or not the reachablePositions was updated.

   A reachablePositions is updated whenever either:
     The passed position has not been seen before
     The passed position is Equal and the previous was Unknown
     The passed position is Delta, and the previous was not Delta
**)
let updateReachable rpt (pos : reachablePosition) =
  try
    let prev = ArgPosTable.find rpt pos.argPos in
    match (prev.qual, pos.qual) with
    | (Equal, Delta)
    | (Unknown, Delta)
    | (Unknown, Equal) ->
      begin
        ArgPosTable.replace rpt pos.argPos pos;
        true
      end
    | _ -> false
  with Not_found ->
    ArgPosTable.replace rpt pos.argPos pos;
    true

(**
   Wrapper aronud update reachable so it operates on reachableGraphs and not
   just reachablePositions
**)

let addEdge graph (src : argPos) (dest : reachablePosition) =
  try
    let reachable = ArgPosTable.find graph src in
    updateReachable reachable dest
  with Not_found ->
    let toAdd = ArgPosTable.create 10 in
    ArgPosTable.replace graph src toAdd;
    updateReachable toAdd dest

(**
   Given a reachableGraph, a source argPos, and a destination of reachablePosition,
   we merge the reachablePositions of the destination into those of the source,
   paying special attention to the quality of the transition.

   Returns true if something was updated in the merge
**)
let merge graph (src : argPos) (dest : reachablePosition) =
  let srcReach = ArgPosTable.find graph src in
  (* if the dest has no reachable positions (it bottoms), then we won't update *)
  (ArgPosTable.mem graph dest.argPos) &&
  (* otherwise, dest touches things... *)
    begin
      let destReach = ArgPosTable.find graph dest.argPos in
      let toFold =
        match dest.qual with
        | Delta ->
          (fun protoPos protoEdge accum ->
            let addedEdge = { argPos = protoEdge.argPos;
                              qual = Delta;} in
            updateReachable srcReach addedEdge || accum)
        | _ ->
          (fun destPos destEdge accum ->
            updateReachable srcReach destEdge || accum) in
      ArgPosTable.fold toFold destReach false
    end

let doMerges graph (src : argPos) =
  let srcReach = ArgPosTable.find graph src in
  ArgPosTable.fold (fun _ rp accum -> merge graph src rp || accum) srcReach false

(** Given the list of transition relationships found by analyzing
    the rules one by one, builds an initial representation of the rule graph,
    appropriate for determining information flow. *)
let processRelationships relationships =
  let graph = ArgPosTable.create 100 in
  let starts =
    List.map
      (fun rt ->
        let pos = rt.lPos
        and node = {argPos = rt.rPos; qual = rt.qual} in
        ignore (addEdge graph pos node);
        pos)
      relationships in
  starts, graph

(** compose relationships between rules until we hit a fixpoint *)
let rec saturate graph startingPoints =
  let recurP =
    List.fold_left
      (fun accum updatePos ->
        doMerges graph updatePos || accum) false startingPoints in
  if recurP then
    saturate graph startingPoints

(** Given a list of relationships mined from the rules of the transition system,
    produce a graph, where the nodes are functions * argument positions, and the
    edges represent influence between these argument positions for all rules. In
    particular, we care about whether or not there is a difference between the
    arguments.
*)
let computeGraph relationships =
  let starts, graph = processRelationships relationships in
  saturate graph starts;
  graph

let flowsToCritical ?(graph = None) relationships starts =
  let graph = match graph with None -> computeGraph relationships | Some g -> g in
  let isCritical = criticalArgument graph in
  (* this is inefficient.  We sholud strive to construct the set of critical
     args [JTT 27-07-15] *)
  let criticalArguments =
    List.fold_left (fun accum el ->
      let accum' = if isCritical el.lPos then el.lPos :: accum else accum in
      let accum'' = if isCritical el.rPos then el.rPos :: accum' else accum' in
      accum'') [] relationships in
  let canReach = reachable starts (buildFlowGraph relationships) in
  let rec ans = function
    | [] -> false
    | hd::tl -> canReach hd || ans tl in
  ans criticalArguments

(** convert a set of relationships into a .dot file so that we can look at them. *)
let visualizeInformationFlow relationships inputFname =
  let graph = computeGraph relationships in
  let isCritical = criticalArgument graph in
  let visGraph = buildFlowGraph relationships in
  let highlight v =
    if isCritical v
    then [`Color 0x33CC33; `Style `Bold]
    else if flowsToCritical ~graph:(Some graph) relationships [v]
    then [`Color 0x336600; `Style `Bold]
    else [] in
  draw ~augmentVertex:highlight visGraph inputFname

(*
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
      let relationships = Utils.concatMap RuleInfluence.processRule system in
      let graph = computeGraph relationships in
      let _ (*isCritical*) = criticalArgument graph in
      visualizeInformationFlow relationships !filename
    end


let _ = main ()
*)
