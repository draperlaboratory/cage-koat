(*
  Knowledge propagation

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

module Make (RVG : Rvgraph.S) = struct
  module TGraph = RVG.TGraph
  module CTRSObl = TGraph.CTRSObl
  module CTRS = CTRSObl.CTRS
  module RuleT = CTRS.RuleT

  open CTRSObl
  open CTRS

  let propagateComplexities ctrsobl subsumed tgraph =
    let updateOneSubsumedRule tgraph complexities rule =
      let pre = TGraph.getPreds tgraph [rule] in
      Log.debug (Printf.sprintf "Rule '%s' has predecessors\n  %s"
                   (RuleT.toString rule)
                   (String.concat "\n  " (List.map (fun r -> (Complexity.toString (CTRSObl.getComplexity ctrsobl r))
                     ^ "  == " ^ (RuleT.toString r)) pre)));
      let preComplexitiesSum = Complexity.listAdd (List.map (fun r -> CTRS.RuleMap.find r complexities) pre) in
      CTRS.RuleMap.add rule preComplexitiesSum complexities in
    { ctrsobl with complexity = List.fold_left (updateOneSubsumedRule tgraph) ctrsobl.complexity subsumed }

  let getProof nctrsobl ini outi =
    "Repeatedly propagating knowledge in problem " ^
    (string_of_int ini) ^
    " produces the following problem:\n" ^
    (CTRSObl.toStringNumber nctrsobl outi)

  (* Remove subsumed rules *)
  let process ctrsobl tgraph rvgraph =
    if CTRSObl.isSolved ctrsobl then
      None
    else begin
      Log.log "Trying Knowledge Propagation processor...";
      let s = CTRSObl.getUnknownComplexityRules ctrsobl in
      let k = CTRSObl.getKnownComplexityRules ctrsobl in
      let subsumed = List.rev (TGraph.computeSubsumed tgraph s k) in
      if subsumed = [] then
        None
      else
        let nctrsobl = propagateComplexities ctrsobl subsumed tgraph in
        Some ((nctrsobl, tgraph, rvgraph), getProof nctrsobl)
    end
end
