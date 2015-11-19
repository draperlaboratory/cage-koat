(***************************)
(* Convert program to Cint *)
(***************************)
open SimpleT

let allvars = ref []
let addedfuns = ref []
let start_vars = ref []

let rec createCint combine prog =
  Simple.control_points := [];
  Simple.if_loop_points := [];
  Simple.loop_points := [];
  Simple.lhsterms := [];
  Simple.random_count := 0;
  allvars := [];
  addedfuns := [];
  start_vars := [];
  let tuprules = Simple.createrules combine prog in
    let renamed_tuprules = rename tuprules prog in
      let blasted_tuprules = blast renamed_tuprules in
        ("eval_start", Simple.toComrules blasted_tuprules)

and rename tuprules (_, vars, _) =
  let res = List.map rename_rule tuprules in
    allvars := !allvars @ vars;
    res

and rename_rule r =
  let toks = Str.split (Str.regexp "_") (Term.getFun (Comrule.getLeft r)) in
    if List.length toks = 3 then
      let f = List.nth toks 1
      and comp = List.nth toks 2 in
        let vars = Comrule.getVars r in
          let newvars = List.map (fun x -> x ^ "_" ^ f) vars in
            let varmapping = List.map2 (fun x x' -> (x, Poly.fromVar x')) vars newvars in
            (
              if not (Utils.contains !addedfuns f) then
              (
                allvars := !allvars @ newvars;
                addedfuns := f::!addedfuns;
              );
              let resR = Comrule.instantiate r varmapping in
                if comp = "start" then
                (
                  start_vars := (f, Term.getVars (Comrule.getLeft resR))::!start_vars
                );
                resR
            )
    else
      r

and blast tuprules =
  List.map blast_rule tuprules

and blast_rule r =
  let vars = Term.getVars (Comrule.getLeft r)
  and left_fun = get_fun (Comrule.getLeft r) in
    let blast_l = blast_term_left vars (Comrule.getLeft r)
    and blast_rs = List.map (blast_term_right vars left_fun) (Comrule.getRights r) in
      (blast_l, blast_rs, (Comrule.getCond r))

and get_fun t =
  let toks = Str.split (Str.regexp "_") (Term.getFun t) in
    if List.length toks = 3 then
      List.nth toks 1
    else
      "<main>"
and blast_term_left vars t =
  Term.create' (Term.getFun t, getNewArgs (Term.getArgs t) vars)

and blast_term_right vars left_fun t =
  let right_fun = get_fun t in
    if left_fun <> right_fun then
      Term.create' (Term.getFun t, getNewArgs (Term.getArgs t)
        (get_call_vars right_fun))
    else
      Term.create' (Term.getFun t, getNewArgs (Term.getArgs t) vars)

and get_call_vars f =
  List.assoc f !start_vars

and getNewArgs oldargs vars =
  getNewArgsAux oldargs vars !allvars []

and getNewArgsAux oldargs vars allvars accu =
  match allvars with
    | [] -> accu
    | v::vs -> let idx = getIdx v vars 0 in
                 if idx = -1 then
                   getNewArgsAux oldargs vars vs (accu @ [Poly.fromVar v])
                 else
                   getNewArgsAux oldargs vars vs (accu @ [List.nth oldargs idx])

and getIdx e l i =
  match l with
    | [] -> -1
    | e'::rest -> if (e == e') || (e = e') then
                    i
                  else
                    getIdx e rest (i + 1)
