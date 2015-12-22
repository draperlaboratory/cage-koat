(*
  Termination graph

  @author Stephan Falke

  Copyright 2010-2014 Stephan Falke

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

open AbstractRule

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Persistent.Digraph.Concrete(Int)
exception Found of int

module Make(CTRSObl : Ctrsobl.S) = struct
  module CTRSObl = CTRSObl
  module RuleT = CTRSObl.CTRS.RuleT
  module SCC = Graph.Components.Make(G)

  type r = RuleT.rule
  type tgraph = G.t * (G.vertex * r) array

  let rec toDot tgraph =
    "digraph kittel {\n" ^
    (getNodes tgraph) ^
    "\n" ^
    (getEdges tgraph) ^
    "\n\n}\n"
  and getNodes (_, trsa) =
    let res = ref "" in
      for i = 0 to (Array.length trsa - 1) do
        res := !res ^ "\n  \"" ^ (RuleT.toDotString (snd trsa.(i))) ^ "\""
      done;
      !res
  and getEdges (g, trsa) =
    let len = Array.length trsa
    and accu = ref [] in
    for i = 0 to (len - 1) do
      for j = 0 to (len - 1) do
        if G.mem_edge g (fst trsa.(i)) (fst trsa.(j)) then
          accu := !accu @ ["  \"" ^ (RuleT.toDotString (snd trsa.(i))) ^ "\" -> \"" ^ (RuleT.toDotString (snd trsa.(j))) ^ "\""]
      done
    done;
    String.concat "\n" !accu

  (* Compute termination graph of trs *)
  let rec compute trs =
    let len = List.length trs in
      let trsa = Array.init len (fun i -> (i, List.nth trs i))
      and edges = Array.make_matrix len len false in
        compute_edges edges trsa len;
        (create_graph len edges, trsa)
  and compute_edges edges trsa len =
    for i = 0 to (len - 1) do
      let r1 = (snd trsa.(i)) in
        for j = 0 to (len - 1) do
          if connectable r1 (snd trsa.(j)) then
            edges.(i).(j) <- true
        done
    done
  and create_graph len edges =
    let res = ref G.empty in
      for i = 0 to (len - 1) do
        res := G.add_vertex !res i
      done;
      for i = 0 to (len - 1) do
        for j = 0 to (len - 1) do
          if edges.(i).(j) then
            res := G.add_edge !res i j
        done
      done;
      !res
  and connectable r1 r2' =
    let r2 = RuleT.renameVars (RuleT.getVars r1) r2' in
      let lt = RuleT.getLeft r2 in
        List.exists (connectableOne lt (RuleT.getCond r1) (RuleT.getCond r2)) (RuleT.getRights r1)
  and connectableOne lt c1 c2 rt =
    if (Term.getFun rt) <> (Term.getFun lt) then
      false
    else
      let rargs = Term.getArgs rt
      and largs = Term.getArgs lt in
        let sigma = getSubstitution largs rargs in
          let c2sigma = Pc.instantiate c2 sigma in
            Smt.isSatisfiable (Pc.dropNonLinearAtoms (c1 @ c2sigma)) <> Ynm.No
  and getSubstitution largs rargs =
    getSubstitutionAux largs rargs []
  and getSubstitutionAux largs rargs accu =
    match largs with
      | [] -> accu
      | x::l -> getSubstitutionAux l (List.tl rargs) ((List.hd (Poly.getVars x), List.hd rargs)::accu)

  (* Return nontrivial SCCs *)
  let rec getNontrivialSccs (g, trsa) =
    let sccs = SCC.scc_list g in
      let nontrivial = List.filter (nontrivial g) sccs in
        List.map (fun scc -> getTrsScc trsa scc) nontrivial
  and nontrivial g scc =
    match scc with
      | [] -> false
      | [x] -> G.mem_edge g x x
      | _ -> true
  and getTrsScc trsa nums =
    let res = ref [] in
      for j = 0 to (Array.length trsa - 1) do
        let elem = trsa.(j) in
          if Utils.contains nums (fst elem) then
            res := (snd elem)::!res
      done;
      List.rev !res

  let hasEdgeNums g trsa i j =
    G.mem_edge g (fst trsa.(i)) (fst trsa.(j))

  (* Compute reachable nodes *)
  let rec computeReachable (g, trsa) startNodes =
    let frontier = ref (getNums trsa startNodes)
    and reachable = ref (getNums trsa startNodes) in
      computeReachableAux g trsa (Array.length trsa) frontier reachable;
      getRules trsa !reachable
  and computeReachableAux g trsa len frontier reachable =
    if computeReachableAuxStep g trsa len frontier reachable then
      computeReachableAux g trsa len frontier reachable
  and computeReachableAuxStep g trsa len frontier reachable =
    let new_frontier = ref [] in
      for i = 0 to (len - 1) do
        if (Utils.contains !frontier i) then
          for j = 0 to (len - 1) do
            if hasEdgeNums g trsa i j && not (Utils.contains !reachable j) then
            (
              reachable := j::!reachable;
              new_frontier := j::!new_frontier
            )
          done;
      done;
      if !new_frontier = [] then
        false
      else
      (
        frontier := !new_frontier;
        true
      )
  and getNums trsa rules =
    let res = ref [] in
      for i = 0 to (Array.length trsa) - 1 do
        let rule = snd trsa.(i) in
        if (List.exists (fun rule' -> RuleT.equal rule rule') rules) then
          res := i::!res
      done;
      !res
  and getRules trsa nums =
    List.map (fun i -> (snd trsa.(i))) nums

  (* Compute rules in s that are subsumed by rules in k *)
  let rec computeSubsumed (g, trsa) s k =
    let subsumed = ref []
    and sl = getNums trsa s
    and kl = getNums trsa k in
      computeSubsumedAux sl kl g trsa (Array.length trsa) subsumed;
      getRules trsa !subsumed
  and computeSubsumedAux sl kl g trsa len subsumed =
    if computeSubsumedAuxStep sl kl g trsa len subsumed then
      computeSubsumedAux sl kl g trsa len subsumed
  and computeSubsumedAuxStep sl kl g trsa len subsumed =
    let res = ref false in
      for i = 0 to (len - 1) do
        if (Utils.contains sl i) && (not (isK kl subsumed i)) then
          let goodrow = ref true in
            for j = 0 to (len - 1) do
              if hasEdgeNums g trsa j i && not (isK kl subsumed j) then
                goodrow := false
            done;
            if !goodrow then
            (
              subsumed := i::!subsumed;
              res := true
            )
      done;
      !res
  and isK kl subsumed j =
    (Utils.contains !subsumed j) || (Utils.contains kl j)

  (* remove nodes *)
  let rec removeNodes (g, trsa) rules =
    let bad = getPairs trsa rules in
      (removeFromGraph g (List.map snd bad), removeFromArray trsa (List.map fst bad))
  and removeFromGraph g toRemove =
    match toRemove with
      | [] -> g
      | i::more -> removeFromGraph (G.remove_vertex g i) more
  and removeFromArray trsa badnums =
    let res = ref [] in
      for i = 0 to (Array.length trsa) - 1 do
        if not (Utils.contains badnums i) then
          res := (trsa.(i))::!res
      done;
      Array.of_list (List.rev !res)
  and getPairs trsa rules =
    let res = ref [] in
      for i = 0 to (Array.length trsa) - 1 do
        let entry = trsa.(i) in
        let rule = snd entry in
          if (List.exists (fun rule' -> RuleT.equal rule rule') rules) then
            res := (i, fst entry)::!res
      done;
      !res

  (* add nodes *)
  let rec addNodes (g, trsa) rules =
    let rec getNewPairs rules j =
      match rules with
      | [] -> []
      | rule::more -> (j, rule)::(getNewPairs more (j + 1)) in
    let rec addToGraph g nums =
      match nums with
      | [] -> g
      | i::more -> addToGraph (G.add_vertex g i) more in
    let addNeededEdgesOne g trsa (i, rule) =
      let res = ref g in
      if connectable rule rule then
        res := G.add_edge !res i i;
      for j = 0 to (Array.length trsa - 1) do
        let entry = trsa.(j) in
        if i <> (fst entry) then
          (
            if connectable rule (snd entry) then
              res := G.add_edge !res i (fst entry);
            if connectable (snd entry) rule then
              res := G.add_edge !res (fst entry) i
          )
      done;
      !res in
    let rec addNeededEdges g trsa news =
      match news with
      | [] -> g
      | irule::more -> addNeededEdges (addNeededEdgesOne g trsa irule) trsa more in
    let addToArray trsa news =
      let trsa' = Array.init (List.length news) (fun i -> List.nth news i) in
      Array.append trsa trsa' in
    
    let maxUsedRuleIdx = Array.fold_left (fun m value -> max m (fst value)) 0 trsa in
    let news = getNewPairs rules (maxUsedRuleIdx + 1) in
    let (g', trsa') = (addToGraph g (List.map fst news), addToArray trsa news) in
    let res = (addNeededEdges g' trsa' news, trsa') in
    res

  (* only keep certain nodes *)
  let rec keepNodes (g, trsa) rules =
    let bad = getComplementPairs trsa rules in
      (removeFromGraph g (List.map snd bad), removeFromArray trsa (List.map fst bad))
  and getComplementPairs trsa rules =
    let res = ref [] in
      for i = 0 to (Array.length trsa) - 1 do
        let entry = trsa.(i) in
        let rule = snd entry in
          if not (List.exists (fun rule' -> RuleT.equal rule rule') rules) then
            res := (i, fst entry)::!res
      done;
      !res

  (* compute predecessors of rules *)
  let rec getPreds (g, trsa) rules =
    let preds = ref []
    and rulesnums = ref (getNums trsa rules) in
      computePreds g trsa (Array.length trsa) rulesnums preds;
      getRules trsa (Utils.remdup !preds)
  and computePreds g trsa len rulesnums preds =
    for i = 0 to (len - 1) do
      if (Utils.contains !rulesnums i) then
        for j = 0 to (len - 1) do
          if (hasEdgeNums g trsa j i) then
            preds := j::!preds
        done;
    done

  (* compute sucessors of rules *)
  let rec getSuccs (g, trsa) rules =
    let succs = ref []
    and rulesnums = ref (getNums trsa rules) in
      computeSuccs g trsa (Array.length trsa) rulesnums succs;
      getRules trsa (Utils.remdup !succs)
  and computeSuccs g trsa len rulesnums succs =
    for i = 0 to (len - 1) do
      if (Utils.contains !rulesnums i) then
        for j = 0 to (len - 1) do
          if (hasEdgeNums g trsa i j) then
            succs := j::!succs
        done;
    done

  exception Found of int

  (* determine whether there is an edge *)
  let rec hasEdge (g, trsa) rule1 rule2 =
    let rule1num = getNum trsa rule1
    and rule2num = getNum trsa rule2 in
      hasEdgeNums g trsa rule1num rule2num
  and getNum trsa rule =
    try
      for i = 0 to (Array.length trsa) - 1 do
        if (RuleT.equal rule (snd trsa.(i))) then
        (
          raise (Found i)
        )
      done;
      failwith "Did not find rule!"
    with
      | Found i -> i

  (* Compute rules in twigs *)
  let rec computeRulesInTwigs (g, trsa) filter =
    let leaves = ref [] in
      computeLeavesAux g trsa (Array.length trsa) filter leaves;
      getRules trsa !leaves
  and computeLeavesAux g trsa len filter leaves =
    if computeLeavesAuxStep g trsa len filter leaves then
      computeLeavesAux g trsa len filter leaves
  and computeLeavesAuxStep g trsa len filter leaves =
    let res = ref false in
      for i = 0 to (len - 1) do
        if not (Utils.contains !leaves i) then
          if (filter (snd trsa.(i))) then
            begin
              let goodrow = ref true in
              for j = 0 to (len - 1) do
                if hasEdgeNums g trsa i j && not (Utils.contains !leaves j) then
                  goodrow := false
              done;
              if !goodrow then
                (
                  leaves := i::!leaves;
                  res := true
                )
            end
      done;
      !res

  let empty () = (G.empty, Array.of_list [])
end

module type S =
    sig
      module CTRSObl : Ctrsobl.S 
      type r = CTRSObl.CTRS.RuleT.rule
      type tgraph
      val toDot : tgraph -> string
      val compute : r list -> tgraph
      val connectable : r -> r -> bool
      val connectableOne : Term.term -> Pc.cond -> Pc.cond -> Term.term -> bool
      val getSubstitution : Poly.poly list -> Poly.poly list -> (Poly.var * Poly.poly) list
      val getNontrivialSccs : tgraph -> r list list
      val computeReachable : tgraph -> r list -> r list
      val computeSubsumed : tgraph -> r list -> r list -> r list
      val removeNodes : tgraph -> r list -> tgraph
      val addNodes : tgraph -> r list -> tgraph
      val keepNodes : tgraph -> r list -> tgraph
      val getPreds : tgraph -> r list -> r list
      val getSuccs : tgraph -> r list -> r list
      exception Found of int
      val hasEdge : tgraph -> r -> r -> bool
      val computeRulesInTwigs : tgraph -> (r -> bool) -> r list
      val empty : unit -> tgraph
    end
