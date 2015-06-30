module Make :
  functor (RuleT : AbstractRule.AbstractRule) ->
    sig
      module CTRS : Ctrs.S with type r = RuleT.rule
      (* module RuleMap : Map.S with type key = RuleT.rule *)
      type t = {
        ctrs : CTRS.t;
        cost : Expexp.expexp CTRS.RuleMap.t;
        complexity : Complexity.complexity CTRS.RuleMap.t;
        leafCost : Expexp.expexp;
      }
      val getComplexity : t -> RuleT.rule -> Complexity.complexity
      val getCost : t -> RuleT.rule -> Expexp.expexp
      val toStringPrefix : string -> t -> string
      val toString : t -> string
      val toStringNumber : t -> int -> string
      val isSolved : t -> bool
      val hasUnknownComplexity : t -> RuleT.rule -> bool
      val getUnknownComplexityRules : t -> RuleT.rule list
      val getKnownComplexityRules : t -> RuleT.rule list
      val getInitialObl : RuleT.rule list -> Term.funSym -> t
      val haveSameComplexities : t -> t -> bool
    end
