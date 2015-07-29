(** Given a list of rules describing the rewrite system, and a list of argument
    positions deemed to be sensivitve, return information about which functions
    lead to branches which could expose information about the secrets.

    The return is grouped according to secret, such that each list of strings in the list of lists of strings
    is associated with the same element position in the argument position list.
*)
val secretBranches : Comrule.rule list -> DepStructs.argPos list -> Term.funSym list list
