module type S =
    sig
      module RuleT : AbstractRule.AbstractRule
      module RuleMap : Map.S with type key = RuleT.rule
      val removeRulesFromMap :
        'a RuleMap.t -> RuleMap.key list -> 'a RuleMap.t
      type t = { rules : RuleT.rule list; startFun : Term.funSym; }
      val contains : t -> RuleT.rule -> bool
      val getVars : t -> Poly.var list
    end

module Make (RuleT : AbstractRule.AbstractRule) : S with module RuleT = RuleT
