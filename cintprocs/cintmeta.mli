module CTRSObl = Cintfarkaspolo.CTRSObl
val process :
  CTRSObl.CTRS.RuleT.rule list ->
  int ->
  Term.funSym ->
  Annot.t ->
  Complexity.ctype ->
  (Complexity.t * (unit -> string)) option
