
module RuleT : AbstractRule.AbstractRule with type rule = Rule.rule

module CTRSObl : Ctrsobl.S with module CTRS.RuleT = RuleT

module TGraph : Tgraph.S

module GSC : GlobalSizeComplexity.S

module SlicingProc :
  sig
    val getProof : CTRSObl.t -> CTRSObl.t -> string list -> int -> int -> string
    val process : CTRSObl.t -> (CTRSObl.t * (int -> int -> string)) option
  end

type tgraph = Tgraph.G.t * (Tgraph.G.vertex * Cseparate.TGraph.r) array
type rvgraph = (Tgraph.G.t * (Tgraph.G.vertex * GSC.trans_data) array) option

val check : Rule.rule list -> unit
val checkRules : int -> Poly.var list -> Rule.rule list -> unit
val checkStartCondition : Tgraph.G.t * (Tgraph.G.vertex * TGraph.r) array ->
  TGraph.r list -> Term.funSym -> unit
val process : RuleT.rule list -> int -> Term.funSym -> Complexity.ctype ->
  (Complexity.t * (unit -> string)) option
val processInner : Cseparate.CTRSObl.t -> tgraph -> rvgraph ->
  (Complexity.t * LocalSizeComplexity.size_data Cseparate.GSC.RVMap.t * (unit -> string))
    option
val getOverallCost : tgraph -> LocalSizeComplexity.size_data GSC.RVMap.t ->
  CTRSObl.t * tgraph * rvgraph * int -> Complexity.t
val getProof : CTRSObl.t * tgraph * rvgraph *  int ->
  int list -> int list -> (int -> int -> string) list -> unit -> string
val attachProofs : int list -> int list -> (int -> int -> string) list -> string
val update : CTRSObl.t * tgraph * rvgraph ->
  (int -> int -> string) -> int -> unit
val run : (CTRSObl.t -> tgraph -> rvgraph ->
   ((CTRSObl.t * tgraph * rvgraph) * (int -> int -> string)) option) ->
  unit
val run_ite : (CTRSObl.t -> tgraph -> rvgraph ->
               ((CTRSObl.t * tgraph * rvgraph) * (int -> int -> string)) option) ->
  (unit -> unit) -> (unit -> unit) -> unit
val insertRVGraphIfNeeded : unit -> unit
val doInitial : unit -> unit
val doInitialCleaning : unit -> unit
val doMaybeSeparateLoop : unit -> unit
val doLoop : unit -> unit
val doApronInvariants : unit -> unit
val doUnreachableRemoval : unit -> unit
val doKnowledgePropagation : unit -> unit
val doSeparate : unit -> unit
val doSeparationCleanup : unit -> unit
val doFarkasConstant : unit -> unit
val doFarkasConstantSizeBound : unit -> unit
val doFarkas : unit -> unit
val doFarkasSizeBound : unit -> unit
val doFarkasMinimal : unit -> unit
val doDesperateMeasures : unit -> unit
val doChain1 : unit -> unit
val doChain2 : unit -> unit
val doNothing : unit -> unit
