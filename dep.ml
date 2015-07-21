open DepStructs

let usage = ""

type reachablePosition = {
  argPos : argPos;
  qual : qual;
}

type reachablePositions = (argPos, reachablePosition) Hashtbl.t
type reachableGraph = (argPos, reachablePositions) Hashtbl.t


(** Is a given argument position critical to its function in a reachableGraph

    That is, after the graph has saturated, is there a delta path between this
    argument and itself.  **)
let criticalArgument (graph : reachableGraph) (pos : argPos) =
  let reachableFrom = Hashtbl.find graph pos in
  try
    let rp = Hashtbl.find reachableFrom pos in
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
let updateReachable (rpt : reachablePositions) (pos : reachablePosition) =
  try
    let prev = Hashtbl.find rpt pos.argPos in
    match (prev.qual, pos.qual) with
    | (Equal, Delta)
    | (Unkown, Delta)
    | (Unkown, Equal) ->
      begin
        Hashtbl.replace rpt pos.argPos pos;
        true
      end
    | _ -> false
  with Not_found ->
    Hashtbl.replace rpt pos.argPos pos;
    true

(**
   Wrapper aronud update reachable so it operates on reachableGraphs and not
   just reachablePositions
**)

let addEdge (graph : reachableGraph) (src : argPos) (dest : reachablePosition) =
  try
    let reachable = Hashtbl.find graph src in
    updateReachable reachable dest
  with Not_found ->
    let toAdd = Hashtbl.create 10 in
    Hashtbl.replace graph src toAdd;
    updateReachable toAdd dest

(**
   Given a reachableGraph, a source argPos, and a destination of reachablePosition,
   we merge the reachablePositions of the destination into those of the source,
   paying special attention to the quality of the transition.

   Returns true if something was updated in the merge
**)
let merge (graph : reachableGraph) (src : argPos) (dest : reachablePosition) =
  let srcReach = Hashtbl.find graph src in
  let destReach = Hashtbl.find graph dest.argPos in
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
  Hashtbl.fold toFold destReach false

let doMerges (graph : reachableGraph) (src : argPos) =
  let srcReach = Hashtbl.find graph src in
  Hashtbl.fold (fun _ rp accum -> merge graph src rp || accum) srcReach false


let processRelationships relationships =
  ignore(doVis relationships "foo.dot");
  let graph = Hashtbl.create 100 in
  let starts =
    List.map
      (fun rt ->
        let pos = rt.lPos
        and node = {argPos = rt.rPos; qual = rt.qual} in
        ignore (addEdge graph pos node);
        pos)
      relationships in
  starts, graph

let rec saturate graph startingPoints =
  let recurP =
    List.fold_left
      (fun accum updatePos ->
        doMerges graph updatePos || accum) false startingPoints in
  if recurP then
    saturate graph startingPoints

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
      Printf.printf "aoenuthoaensthu %s\n\n" !filename;
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      let relationships = Utils.concatMap RuleInfluence.processRule system in
      processRelationships relationships
    end

let _ = main ()
