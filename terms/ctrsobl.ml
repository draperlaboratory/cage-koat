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

  let complCostRuleStrings obl =
    let maxLen = ref 0 in
    let asString r =
      let (s, _) as tup = Printf.sprintf "(Comp: %s, Cost: %s)"
      (Complexity.toString (getComplexity obl r))
      (Expexp.toString (getCost obl r)), RuleT.toString r in
      maxLen := max !maxLen (String.length s);
      tup in
    List.map asString obl.ctrs.CTRS.rules, !maxLen

  let toStringPrefix prefix obl =
    let open CTRS in
    let rulesString = match obl.ctrs.rules with
      | [] -> prefix ^ "\t(none)"
      | _ ->
        let complCostRuleStrings, maxLen = complCostRuleStrings obl in
        let toStringPrefixOne prefix maxLen (ccS, rS) =
          Printf.sprintf "%s%s%s%s" prefix ccS (String.make (maxLen + 4 - (String.length ccS)) ' ') rS in
        String.concat "\n" (List.map (toStringPrefixOne (prefix ^ "\t") maxLen) complCostRuleStrings) in
    Printf.sprintf "%sT:\n%s\n%sstart location:\t%s\n%sleaf cost:\t%s"
      prefix
      rulesString
      prefix
      obl.ctrs.startFun
      prefix
      (Expexp.toString obl.leafCost)

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

  let dummyTerm args = Term.create' ("__dummyRhs", args)

  (* Takes a rule and, if the head symbol is in specs, returns
     the assigned complexity, otherwise returns Unknown *)
  let initComp start rule =
    let f = RuleT.getLeftFun rule in
    if f = start then
      Complexity.P Expexp.one
    else
      Complexity.Unknown

  let spaceOrTimeWeight rule =
    let p = RuleT.getUpperBound rule in
    (*Printf.eprintf "ctrsobl %s : %s \n" (RuleT.toString rule) (Poly.toString p);*)
    Expexp.Pol p


  let getInitialObl rules start ctype =
    let open Expexp in
    let open CTRS in
    let initComp = initComp start in
    let folder (rules, costs, compl) rule =
      let rules' = rule::rules
      and costs' = RuleMap.add rule (spaceOrTimeWeight rule) costs
      and compl' = RuleMap.add rule (initComp rule) compl in
      (rules', costs', compl') in
    let (rules, initCost, initCompl) =
      List.fold_left folder ([], RuleMap.empty, RuleMap.empty) rules in
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
    RuleMap.for_all (fun rule compl1 -> RuleMap.mem rule obl2.complexity &&
      Complexity.equal compl1 (RuleMap.find rule obl2.complexity)) obl1.complexity
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
      val getInitialObl : CTRS.RuleT.rule list -> Term.funSym -> Complexity.ctype -> t
      val haveSameComplexities : t -> t -> bool
    end

