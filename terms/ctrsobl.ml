(*
  Complexity TRSs obligations

  @author Stephan Falke, Marc Brockschmidt

  Copyright 2010-2014 Stephan Falke
  Copyright 2014 Microsoft Research

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

module Make(CTRS : Ctrs.S) = struct
  module CTRS = CTRS
  module RuleT = CTRS.RuleT
  module RuleMap = CTRS.RuleMap
  module FMap = Annot.FMap
  open Ctrs


  type t =
    { ctrs : CTRS.t ;
      cost : Expexp.expexp RuleMap.t ;
      complexity : Complexity.t RuleMap.t ;
      leafCost : Expexp.expexp ;
    }

  let getComplexity obl rule =
    RuleMap.find rule obl.complexity
  let getCost obl rule =
    RuleMap.find rule obl.cost

  let toStringPrefix prefix obl =
    let open Printf in
    let open CTRS in
    let rulesString =
      if obl.ctrs.rules = [] then
        prefix ^ "\t(none)"
      else
        let complCostRuleStrings =
          List.map (fun r -> (sprintf "(%s, %s)" (Complexity.toString (getComplexity obl r)) (Expexp.toString (getCost obl r)), RuleT.toString r)) obl.ctrs.rules in
        let maxLen = List.fold_left (fun m (s, _) -> max m (String.length s)) 0 complCostRuleStrings in
        let toStringPrefixOne prefix maxLen (ccS, rS) =
          prefix ^ ccS ^ (String.make (maxLen + 4 - (String.length ccS)) ' ') ^ rS in
        String.concat "\n" (List.map (toStringPrefixOne (prefix ^ "\t") maxLen) complCostRuleStrings) in
    sprintf "%sT:\n%s\n%sstart location:\t%s\n%sleaf cost:\t%s" prefix rulesString prefix obl.ctrs.startFun prefix (Expexp.toString obl.leafCost)
  let toString obl =
    toStringPrefix "\t" obl
  let toStringNumber obl i =
    (string_of_int i) ^ ":" ^ (toString obl)

  let isSolved obl =
    RuleMap.for_all (fun _ c -> c <> Complexity.Unknown) obl.complexity

  let hasUnknownComplexity obl rule =
    getComplexity obl rule = Complexity.Unknown

  let getUnknownComplexityRules obl =
    RuleMap.fold (fun r c res -> if c = Complexity.Unknown then r::res else res) obl.complexity []

  let getKnownComplexityRules obl =
    RuleMap.fold (fun r c res -> if c <> Complexity.Unknown then r::res else res) obl.complexity []

  (* Takes a function symbol f, and creates a dummy rule with head f. *)
  let makeDummyRules fs rule =
    (* is this correct? *)
    let vs = RuleT.getVars rule in
    let args = List.map Poly.fromVar vs in
    let make_dummy f =
      let lhs = (f, args) in
      let rhs = ("__dummyRhs", args) in
      RuleT.createRule lhs [rhs] []
    in
    List.map make_dummy fs
    
  (* Takes a rule and, if the head symbol is "f", returns
     a cubic complexity, otherwise returns Unknown *)
  let initComp specs rule start =
    let f = RuleT.getLeftFun rule in
    if FMap.mem f specs then
      let cs = FMap.find f specs in
      cs.Annot.complexity.Annot.upperTime
    else if f = start then
      Complexity.P Expexp.one
    else
      Complexity.Unknown

  let has_space specs f =
    let open Annot in
    let open Complexity in
    if FMap.mem f specs then
      let c = FMap.find f specs in
      match c.complexity.upperSpace with
      | P _     -> true
      | Unknown -> false
    else
      false

  let get_space specs f =
    let open Annot in
    let open Complexity in
    let c = FMap.find f specs in
    match c.complexity.upperSpace with
    | P p     -> p
    | Unknown -> assert false


  let spaceOrTimeWeight specs rule is_space =
    let alloc_fun = "new" in
    if is_space then begin
      let f = RuleT.getLeftFun rule in
      if f = alloc_fun then
        Expexp.one
      else if has_space specs f then
        get_space specs f
      else
        Expexp.zero
    end
    else
      Expexp.one


  let getInitialObl specs rules start =
    let open Expexp in
    let open CTRS in
    let rules = match rules with
      | [] -> []
      | r :: _ -> 
        let keys = List.map fst (FMap.bindings specs) in
        makeDummyRules keys r @ rules
    in
    let (rules, initCost, initCompl) =
      List.fold_left
        (fun (rules, cost, compl) rule ->
          (rule::rules, RuleMap.add rule (spaceOrTimeWeight specs rule false) cost,
                        RuleMap.add rule (initComp specs rule start) compl))
        ([], RuleMap.empty, RuleMap.empty) rules in
    {
      ctrs = { rules = rules ;
               startFun = start ;
             } ;
      cost = initCost ;
      complexity = initCompl ;
      leafCost = zero ;
    }

  let haveSameComplexities obl1 obl2 =
    RuleMap.cardinal obl1.complexity = RuleMap.cardinal obl2.complexity &&
    RuleMap.for_all (fun rule compl1 -> RuleMap.mem rule obl2.complexity && Complexity.equal compl1 (RuleMap.find rule obl2.complexity)) obl1.complexity
end

module type S =
    sig
      module CTRS : Ctrs.S
      type t = {
        ctrs : CTRS.t;
        cost : Expexp.expexp CTRS.RuleMap.t;
        complexity : Complexity.t CTRS.RuleMap.t;
        leafCost : Expexp.expexp;
      }
      val getComplexity : t -> CTRS.RuleT.rule -> Complexity.t
      val getCost : t -> CTRS.RuleT.rule -> Expexp.expexp
      val toStringPrefix : string -> t -> string
      val toString : t -> string
      val toStringNumber : t -> int -> string
      val isSolved : t -> bool
      val hasUnknownComplexity : t -> CTRS.RuleT.rule -> bool
      val getUnknownComplexityRules : t -> CTRS.RuleT.rule list
      val getKnownComplexityRules : t -> CTRS.RuleT.rule list
      val getInitialObl : Annot.specMap -> CTRS.RuleT.rule list -> Term.funSym -> t
      val haveSameComplexities : t -> t -> bool
    end

