module CR = Comrule

module Connectivity = Map.Make(String)
module Reaches = Set.Make(String)

type ruleReachability =
| Cyclic        (* Reaches itself by some path *)
| ReachesCycle  (* hangs above a cycle *)
| Acyclic       (* part of a linear / branching path with no cycles *)

let rec ofList ?(s = Reaches.empty) = function
  | [] -> s
  | hd::tl -> ofList ~s:(Reaches.add hd s) tl

type head = Term.funSym
type tail = Reaches.t (*ignore conditionals for now *)

let merge (s1 : Reaches.t) (s2 : Reaches.t) =
  let unioned = Reaches.union s1 s2 in
  let subsets = Reaches.subset s1 s2 || Reaches.subset s2 s1 in
  let changed = not subsets && not (Reaches.equal s1 s2) in
  unioned, changed

let stepMap map =
  let delta = ref false
  and internalMerge el (delta, set) =
    let ac, d = merge (Connectivity.find el map) set in
    (d || delta, ac) in
  let rec updateConEl r =
    let (updated, r') = Reaches.fold internalMerge r (false, r) in
    if not updated
    then r'
    else
      begin
        delta := true;
        updateConEl r'
      end in
  let ret = Connectivity.map updateConEl map in
  !delta, ret

let rec saturateConnectivity map =
  let changed, map' = stepMap map in
  if not changed || Connectivity.equal (=) map map'
  then map'
  else saturateConnectivity map'

let inLoop key reachable = Reaches.mem key reachable
let loopMap map = Connectivity.mapi inLoop map
let infoMap map =
  let loops = loopMap map in
  let childAcyclic r =
    let childCycles = try Connectivity.find r loops with Not_found -> false in
    not childCycles in
  Connectivity.mapi (fun key reaches ->
    if Connectivity.find key loops
    then Cyclic
    else if Reaches.for_all childAcyclic reaches
    then Acyclic
    else ReachesCycle) map

let stubRule map symbol =
  if Connectivity.mem symbol map
  then map
  else Connectivity.add symbol Reaches.empty map

let rec processRulesInt map = function
  | [] -> map
  | hd::tl ->
    let hFun = hd.CR.lhs.Term.fn
    and rhsSyms = List.map (fun n -> n.Term.fn) hd.CR.rhss in
    let tail = ofList rhsSyms in
    let prev = try Connectivity.find hFun map with Not_found -> Reaches.empty in
    let toAdd = Reaches.union prev tail in
    let map' = List.fold_left stubRule map rhsSyms in
    processRulesInt (Connectivity.add hFun toAdd map') tl

let processRules lst =
  let base = Connectivity.add DetectBranch.bottomKey Reaches.empty Connectivity.empty in
  saturateConnectivity (processRulesInt base lst)

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
      Printf.eprintf "Rule Connectivity %s\n\n" !filename;
      flush stderr;
      let entrFun, system = Parser.parseCint !filename Simple.Stmts in
      let rules = processRules system in
      let conMap = saturateConnectivity rules in
      conMap
    end


let _ = main ()

*)
