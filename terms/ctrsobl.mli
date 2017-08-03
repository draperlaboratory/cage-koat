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
      val getInitialObl : CTRS.RuleT.rule list ->
        Term.funSym -> t
      val haveSameComplexities : t -> t -> bool
    end



module Make(CTRS : Ctrs.S) : S with module CTRS = CTRS

