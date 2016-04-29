(*
  Combined multivariate complexity function processor

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

module RVG     = Cintfarkaspolo.RVG
module CTRS    = Cintfarkaspolo.CTRS
module CTRSObl = Cintfarkaspolo.CTRSObl
module GSC     = Cintfarkaspolo.GSC
module LSC     = Cintfarkaspolo.LSC
module TGraph  = Cintfarkaspolo.TGraph

open CTRSObl
open CTRS

module KnowledgeProc = KnowledgePropagationProc.Make(RVG)
module UnreachableProc = DeleteUnreachableProc.Make(RVG)
module UnsatProc = DeleteUnsatProc.Make(RVG)
module ChainProc = ComplexityChainProc.Make(RVG)
module SlicingProc = SlicingProc.Make(RVG)

IFDEF HAVE_APRON THEN
module ApronInvariantsProc = ApronInvariantsProcessor.MakeKoatProc(RVG)
END

type state = {
  obl : CTRSObl.t;
  tgraph : TGraph.tgraph;
  rvgraph : RVG.rvg option;
  outi : int;
}

type proof = int -> int -> String.t

type processor = CTRSObl.t -> TGraph.tgraph -> RVG.rvg option ->
  ((CTRSObl.t * TGraph.tgraph * RVG.rvg option) * proof) option

let i = ref 1
let (proofs : proof list ref) = ref []
let (output_nums : int list ref) = ref []
let (input_nums : int list ref) = ref []
let did_ai = ref false

let printState prefix state =
  Printf.eprintf "\n%s Obligations: %s\n" prefix (CTRSObl.toString state.obl)

(** Pre run validation **)
let checkComrules arity lvars trs =
  let rec internal = function
  | [] -> ()
  | rule::rest ->
    let lhs = Comrule.getLeft rule in
    if ((Term.getArity lhs) <> arity) || (Term.getVars lhs <> lvars) then
      raise (Cint_aux.ParseException (0, 0, "Error: Not all rules have the same variables!"))
    else if List.exists (fun r -> (Term.getArity r <> arity)) (Comrule.getRights rule) then
      raise (Cint_aux.ParseException (0, 0, "Error: Not all function symbols have the same arity!"))
    else
      internal rest in
  internal trs

let check = function
  | [] -> raise (Cint_aux.ParseException
                   (0, 0, "Error: Cannot handle empty CINT!"))
  | trs ->
    let first = List.hd trs in
    let arity = Term.getArity (Comrule.getLeft first)
    and lvars = Term.getVars (Comrule.getLeft first) in
    checkComrules arity lvars trs

let checkStartCondition tgraph trs startfun =
  let startComrules = List.filter (fun rule -> (Term.getFun (Comrule.getLeft rule)) = startfun) trs in
  match TGraph.getPreds tgraph startComrules with
  | [] -> ()
  | _ -> raise (Cint_aux.ParseException (0, 0, "Error: Start nodes have incoming edges!"))

(** Output **)


let getOverallCost tgraph globalSizeComplexities state =
  let ctrsobl = state.obl in
  let vars = CTRS.getVars ctrsobl.ctrs in
  let getCostForRule tgraph globalSizeComplexities vars rule =
    let apply c m = match c with
      | Complexity.P e     -> Complexity.apply e m
      | Complexity.Unknown -> Complexity.Unknown
    in
    let preRules = TGraph.getPreds tgraph [rule] in
    let getCostPerPreRule ruleCost globalSizeComplexities vars preRule =
      let csmap = GSC.extractSizeMapForRule globalSizeComplexities preRule 0 vars in
      (* List.iter (fun (p,c) -> Printf.eprintf "%s -> %s\n" p (Complexity.toString c)) csmap;*)
      let res = apply ruleCost csmap in
      (* List.iter (fun v -> Printf.eprintf " %s" v) vars;
         Printf.eprintf "\npreRuleComp: %s\n" (Complexity.toString res); *)
      res
    in
    let ruleComplexity = CTRSObl.getComplexity ctrsobl rule in
    (**
       JTT - 12/22/15

       Rule cost still contains variables from the TRS as it was given in the
       input file.  However, getCostForRule expects all variable names to be of
       the form X_i, where i is the argument position of that particular
       variable.  The mapping should have happened early, but we compute and
       apply it here.  Otherwise, we would end up with unknown complexity where
       we can actually compute the proper value.

        MS - 02/10/16
        ruleCost can have variables not occuring on the lhs; we eliminate
        free variables by expressing local size bounds as an expression in X_i
        or Unknown
    *)
    let sigma          = List.mapi (fun i var -> Poly.toVar var, Complexity.P (Expexp.fromVar (Printf.sprintf "X_%i" (i + 1)))) rule.Comrule.lhs.Term.args in
    let ruleCost       = CTRSObl.getCost ctrsobl rule in
    let fvars          = List.filter (fun v -> not (List.mem v vars)) (Expexp.getVars ruleCost) in
    (* List.iter (fun v -> Printf.eprintf " %s" v) fvars; *)
    let localSize v    = LSC.computeLocalSizeComplexityForTerm rule (Poly.fromVar v) in
    let toComplexity b = LSC.localcomplexity2complexity b vars in
    let normalise c    = apply c sigma in
    let fvmap          = List.map (fun fv -> (fv, normalise (toComplexity (localSize fv)))) fvars in
    (* List.iter (fun (p,c) -> Printf.eprintf "%s -> %s\n" p (Complexity.toString c)) (sigma @ fvmap); *)
    let ruleCost       = Complexity.apply ruleCost (sigma @ fvmap) in

    (* Printf.eprintf "RuleData: %s Comp: %s Cost: %s\n"
      (Comrule.toString rule)
      (Complexity.toString ruleComplexity)
      (Expexp.toString ruleCost); *)
    let result =
      match preRules with
      | [] -> Complexity.mult ruleComplexity ruleCost
      | _ -> Complexity.mult ruleComplexity
              (Complexity.sup
                 (List.map
                    (getCostPerPreRule ruleCost globalSizeComplexities vars)
                    preRules)) in
    (* Printf.eprintf "Result: %s\n" (Complexity.toString result); *)
    result in
  Complexity.add
    (Complexity.listAdd (List.map (getCostForRule tgraph globalSizeComplexities vars) ctrsobl.ctrs.rules))
    (Complexity.P ctrsobl.leafCost)

let rec attachProofs onums tproofs = function
  | [] -> ""
  | i::is -> ((List.hd tproofs) i (List.hd onums)) ^ "\n\n" ^ (attachProofs (List.tl onums) (List.tl tproofs) is)

let getProof (ctrsobl, _, _, _) inums onums theproofs () =
  "Initial complexity problem:\n1:" ^
    (CTRSObl.toString ctrsobl) ^
    "\n\n" ^
    (attachProofs onums theproofs inums)

(** Solver loop / control flow **)
let insertRVGraphIfNeeded state =
  match state.rvgraph with
  | Some _ -> state
  | None ->
      let lscs = LSC.computeLocalSizeComplexities state.obl.ctrs.rules in
      let state' = {state with rvgraph = Some (RVG.compute lscs state.tgraph); } in
      state'


let update (newctrsobl, newTGraph, newRVGraph) proof ini =
  let outi = !i + 1 in
  let state' = { obl = newctrsobl;
                 tgraph = newTGraph;
                 rvgraph = newRVGraph;
                 outi = outi; } in
  i := outi;
  proofs := proof::!proofs;
  input_nums := ini::!input_nums;
  output_nums := outi::!output_nums;
  state'

let run (proc : processor) state =
  if CTRSObl.isSolved state.obl then state
  else
    match (proc state.obl state.tgraph state.rvgraph) with
    | None -> state
    | Some (newData, p) -> update newData p state.outi

let run_ite (proc1 : processor) proc2 proc3 state =
  (* if proc1 succeeds, run proc2, else run proc3 *)
  if not (CTRSObl.isSolved state.obl) then
    match (proc1 state.obl state.tgraph state.rvgraph) with
    | None -> proc3 state
    | Some (newData, p) -> update newData p state.outi |> proc2
  else
    state


let doNothing state = state

let doUnreachableRemoval state =
  run UnreachableProc.process state

let doKnowledgePropagation state =
  run KnowledgeProc.process state

let sliceState state =
  let obl = state.obl in
  match SlicingProc.process obl with
  | None -> state
  | Some (sliced, _) ->
    { state with
      obl = sliced;
      tgraph = TGraph.compute sliced.ctrs.rules;
      rvgraph = None; }

let (|>) v f = f v

let rec doLoop state =
  state |> doUnreachableRemoval |> doKnowledgePropagation |> doFarkasConstant

and doFarkasConstant state =
  run_ite (Cintfarkaspolo.process false 0) doLoop doFarkasConstantSizeBound state

and doFarkasConstantSizeBound state =
  state |> insertRVGraphIfNeeded |> run_ite (Cintfarkaspolo.process true 0) doLoop doFarkas

and doFarkas state =
  run_ite (Cintfarkaspolo.process false 1) doLoop doFarkasSizeBound state

and doFarkasSizeBound state =
  run_ite (Cintfarkaspolo.process true 1) doLoop doDesperateMeasures state

and doDesperateMeasures state =
IFDEF HAVE_APRON THEN
  if not(!did_ai) then
    state |> doApronInvariants |> run UnsatProc.process |> doLoop (* New invariants may show transitions to be unusable *)
  else
    doChain1 state
ELSE
  doChain1 state
END

and doApronInvariants state =
  did_ai := true;
IFDEF HAVE_APRON THEN
  run ApronInvariantsProc.process state
ELSE
  state
END

and doChain1 state =
  run_ite (ChainProc.process 1) doLoop doChain2 state

and doChain2 state =
  run_ite (ChainProc.process 2) doLoop doExpFarkas state

and doExpFarkas state =
  run_ite (Cintexpfarkaspolo.process false) doLoop doExpFarkasSizeBound state

and doExpFarkasSizeBound state =
  run_ite (Cintexpfarkaspolo.process true) doLoop doNothing state


let process cint maxchaining startfun ctype =
  check cint;
  i := 1;
  proofs := [];
  input_nums := [];
  output_nums := [];
  ChainProc.max_chaining := maxchaining;
  ChainProc.done_chaining := 0;
  let initObl = CTRSObl.getInitialObl cint startfun ctype in
  let maybeSlicedObl =
    match (SlicingProc.process initObl) with
    | None -> initObl
    | Some (newctrsobl, proof) ->
      i := 2;
      proofs := proof::!proofs;
      input_nums := 1::!input_nums;
      output_nums := 2::!output_nums;
      newctrsobl in
  let tgraph = TGraph.compute maybeSlicedObl.ctrs.rules in
  checkStartCondition tgraph maybeSlicedObl.ctrs.rules startfun;
  let initial = { obl = maybeSlicedObl; tgraph = tgraph; rvgraph = None; outi = !i; } in
  let todo = initial |> doUnreachableRemoval |> sliceState |> doLoop in
  proofs := List.rev !proofs;
  input_nums := List.rev !input_nums;
  output_nums := List.rev !output_nums;
  let todo = insertRVGraphIfNeeded todo in
  let rvgraph = Utils.unboxOption todo.rvgraph in
  let globalSizeComplexities = GSC.compute todo.obl rvgraph in
  Some (getOverallCost todo.tgraph globalSizeComplexities todo,
        (* Why is initObl passed to getProof and not ctrsobl? *)
        getProof (initObl, todo.tgraph, rvgraph, 1) !input_nums !output_nums !proofs)
