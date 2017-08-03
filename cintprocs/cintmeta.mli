val process :
  Cintfarkaspolo.CTRSObl.CTRS.RuleT.rule list ->
  int ->
  bool ->
  Term.funSym ->
  (Complexity.t * (unit -> string)) option
