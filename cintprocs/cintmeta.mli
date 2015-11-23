val process :
  Cintfarkaspolo.CTRSObl.CTRS.RuleT.rule list ->
  int ->
  Term.funSym ->
  Complexity.ctype ->
  (Complexity.t * (unit -> string)) option
