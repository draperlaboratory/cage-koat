(*
  Complexity TRSs

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

module type S =
    sig
      type r
      module RuleMap : Map.S with type key = r
      val removeRulesFromMap :
        'a RuleMap.t -> RuleMap.key list -> 'a RuleMap.t
      type t = { rules : r list; startFun : Term.funSym; }
      val contains : t -> r -> bool
      val getVars : t -> Poly.var list
    end


module Make(RuleT : AbstractRule) = struct

  type r = RuleT.rule

  module RuleMap =
    Map.Make(struct
      type t = RuleT.rule
      let compare = RuleT.compare
    end)

  let removeRulesFromMap ruleMap rules =
    List.fold_left (fun newMap rule -> RuleMap.remove rule newMap) ruleMap rules

  type t = { rules : RuleT.rule list ;
             startFun : Term.funSym ;
           }

  let contains ctrs rule =
    Utils.containsC (fun r e -> RuleT.equal r e) ctrs.rules rule

  let getVars ctrs =
    if ctrs.rules = [] then
      []
    else
      Term.getVars (RuleT.getLeft (List.hd ctrs.rules))
end
