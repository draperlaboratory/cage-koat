type localcomplexity =
  Max of Big_int.big_int
| MaxPlusConstant of Big_int.big_int
| SumPlusConstant of Big_int.big_int
| ScaledSumPlusConstant of Big_int.big_int * Big_int.big_int
| P of Expexp.expexp
| Unknown

type size_data = { bound : localcomplexity; active_vars : Term.pos list }

val create_size_data : localcomplexity * Term.pos list -> size_data
val unknown_size_data : size_data

type index = { rhsIdx : Term.pos
	     ; varIdx : Term.pos }

type local_size_data = index * size_data

module type S =
    sig
      module RuleT : AbstractRule.AbstractRule

      type trans_data = RuleT.rule * local_size_data
      type tds = trans_data list

      (* This produces one 
         (rule, ((rhsIdx, argumentIdx), (local size bound, active variable idxs)))
         tuple per RV *)
      val computeLocalSizeComplexities : RuleT.rule list -> tds

      val getConstantSummand : size_data -> Big_int.big_int
      val getScalarFactor : size_data -> Big_int.big_int
      val toSmallestComplexity : size_data -> Poly.var list -> Complexity.t
      val isConstant : size_data -> bool

      val equalLSC : local_size_data -> local_size_data -> bool
      val complexity2localcomplexity : Complexity.t -> Poly.var list -> size_data

      val listSum : size_data list -> Poly.var list -> size_data
      val listMax : size_data list -> Poly.var list -> size_data

      val toStringLocalComplexity : size_data -> string
      val dumpOneLSC : trans_data -> string
      val dumpLSCDot : trans_data -> string

      val computeLocalSizeComplexityForTerm : RuleT.rule -> Poly.poly -> size_data
      val localcomplexity2complexity : size_data -> Poly.var list -> Complexity.t
    end

module Make(RuleT : AbstractRule.AbstractRule) : S with module RuleT = RuleT
