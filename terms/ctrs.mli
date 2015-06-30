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

module Make (RuleT : AbstractRule.AbstractRule) : S with type r = RuleT.rule

