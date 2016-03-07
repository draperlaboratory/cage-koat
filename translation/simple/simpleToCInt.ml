(***************************)
(* Convert program to Cint *)
(***************************)
open SimpleT

let allvars = ref []
let addedfuns = ref []
let start_vars = ref []

let rename_rule r =
  let toks = Str.split (Str.regexp "_") (Term.getFun (Comrule.getLeft r)) in
    if List.length toks = 3 then
      let f = List.nth toks 1
      and comp = List.nth toks 2
      and vars = Comrule.getVars r in
      let newvars = List.map (fun x -> x ^ "_" ^ f) vars in
      let varmapping = List.map2 (fun x x' -> (x, Poly.fromVar x')) vars newvars in
      begin
        if not (Utils.contains !addedfuns f) then
          begin
            allvars := !allvars @ newvars;
            addedfuns := f::!addedfuns;
          end;
        let resR = Comrule.instantiate r varmapping in
        if comp = "start" then
          start_vars := (f, Term.getVars (Comrule.getLeft resR))::!start_vars;
        resR
      end
    else
      r

let rename tuprules (_, vars, _) =
  allvars := !allvars @ vars;
  List.map rename_rule tuprules

let rec getIdx e i = function
  | [] -> -1
  | e'::rest ->
    if (e == e') || (e = e') then
      i
    else
      getIdx e (i + 1) rest

let get_fun t =
  let toks = Str.split (Str.regexp "_") (Term.getFun t) in
    if List.length toks = 3 then
      List.nth toks 1
    else
      "<main>"

let rec getNewArgsAux oldargs vars allvars accu =
  match allvars with
    | [] -> accu
    | v::vs -> let idx = getIdx v 0 vars in
                 if idx = -1 then
                   getNewArgsAux oldargs vars vs (accu @ [Poly.fromVar v])
                 else
                   getNewArgsAux oldargs vars vs (accu @ [List.nth oldargs idx])

let getNewArgs oldargs vars =
  getNewArgsAux oldargs vars !allvars []

let get_call_vars f = List.assoc f !start_vars

let blast_term_left vars t =
  Term.create' (Term.getFun t, getNewArgs (Term.getArgs t) vars)

let blast_term_right vars left_fun t =
  let right_fun = get_fun t in
    if left_fun <> right_fun then
      Term.create' (Term.getFun t, getNewArgs (Term.getArgs t)
        (get_call_vars right_fun))
    else
      Term.create' (Term.getFun t, getNewArgs (Term.getArgs t) vars)

let blast_rule r =
  let vars = Term.getVars (Comrule.getLeft r)
  and left_fun = get_fun (Comrule.getLeft r) in
    let blast_l = blast_term_left vars (Comrule.getLeft r)
    and blast_rs = List.map (blast_term_right vars left_fun) (Comrule.getRights r) in
      (blast_l, blast_rs, (Comrule.getCond r))

let blast tuprules =
  List.map blast_rule tuprules

let createCint combine prog =
  SimpleToTRS.control_points := [];
  SimpleToTRS.if_loop_points := [];
  SimpleToTRS.loop_points := [];
  SimpleToTRS.lhsterms := [];
  SimpleToTRS.random_count := 0;
  allvars := [];
  addedfuns := [];
  start_vars := [];
  let tuprules = SimpleToTRS.createrules combine prog in
  let renamed_tuprules = rename tuprules prog in
  let blasted_tuprules = blast renamed_tuprules in
  ("eval_start", SimpleToTRS.toComrules blasted_tuprules)


