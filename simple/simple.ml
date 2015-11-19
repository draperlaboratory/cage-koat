(*
  Programs written in a fragment of Simple

  @author Stephan Falke

  Copyright 2010-2014 Stephan Falke

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

open SimpleT

(**********************)
(* Formula conversion *)
(**********************)

(* Convert to NNF *)
let rec toNNF = function
  | Or (x, y) -> Or (toNNF x, toNNF y)
  | And (x, y) -> And (toNNF x, toNNF y)
  | Not (Or (x, y)) -> And (toNNF (Not x), toNNF (Not y))
  | Not (And (x, y)) -> Or (toNNF (Not x), toNNF (Not y))
  | Not (Not x) -> toNNF x
  | Not (True) -> False
  | Not (False) -> True
  | Not (BRandom) -> BRandom
  | c -> c

(* convert to DNF *)
let rec toDNF c =
  toDNFAux (toNNF c)
and toDNFAux = function
  | Or (x, y) -> Or (toDNFAux x, toDNFAux y)
  | And (x, y) -> toDNFAux2 y x (* note the reversal of order *)
  | c -> c
and toDNFAux2 y = function
    | Or (z1, z2) -> Or (toDNFAux (And (z1, y)), toDNFAux (And (z2, y)))
    | x -> toDNFAux3 x y
and toDNFAux3 x = function
  | Or (z1, z2) -> Or (toDNFAux (And (x, z1)), toDNFAux (And (x, z2)))
  | y -> toDNFAux4 x y
and toDNFAux4 x y =
  And (toDNFAux x, toDNFAux y)


(********************************)
(* Convert a program into a trs *)
(********************************)

let control_points = ref []
let if_loop_points = ref []
let loop_points = ref []
let lhsterms = ref []
let random_count = ref 0

let prepend list refList =
  refList := list @ !refList

let rec createTrs combine prog =
  control_points := [];
  if_loop_points := [];
  lhsterms := [];
  random_count := 0;
  let rules = createrules combine prog in
    flatten_rules rules

and flatten_rules rules =
  List.flatten (List.map flatten_rule rules)

and flatten_rule r =
  List.map (fun rhs -> Rule.create (Comrule.getLeft r) rhs (Comrule.getCond r) ) (Comrule.getRights r)

and createrules combine (funs, vars, stmts) : Comrule.rule list =
  let funsrules = List.flatten (List.map getRulesForFunDecl funs) in
    let mainrules = getRules stmts "" vars vars in
      let rules : (Term.term * Term.term list * bexpr) list = funsrules @ mainrules in
        let rules_dnf : (Term.term * Term.term list * Pc.cond) list =
          List.flatten (List.map dnfify rules) in
          match combine with
            | Stmts -> toComrules (normalizeConds rules_dnf)
            | Ctrls -> toComrules (normalizeConds (combineRules !control_points rules_dnf))
            | IfsLoops -> toComrules (normalizeConds (combineRules !if_loop_points rules_dnf))
            | Loops -> toComrules (normalizeConds (combineRules !loop_points rules_dnf))

and toComrules =
  (* anything we had about weights for rules comes here to die *)
  List.map (fun (l, rs, c) -> Comrule.createRule l rs c)

and getRulesForFunDecl (f, in_vars, out_var, local_vars, stmts) =
  let vars = in_vars @ (getVar out_var) @ local_vars in
    getRules stmts f vars in_vars

and getVar out_var =
  match out_var with
    | None -> []
    | Some v -> [v]

and getRules stmts f vars in_vars =
  control_points := (if f = "" then ["eval_start"; "eval_stop"] else ["eval_" ^ f ^ "_start"; "eval_" ^ f ^ "_stop"]) @ !control_points;
  if_loop_points := (if f = "" then ["eval_start"; "eval_stop"] else ["eval_" ^ f ^ "_start"; "eval_" ^ f ^ "_stop"]) @ !if_loop_points;
  loop_points := (if f = "" then ["eval_start"; "eval_stop"] else ["eval_" ^ f ^ "_start"; "eval_" ^ f ^ "_stop"]) @ !loop_points;
  lhsterms := createLhsTerms vars;
  let t1 = Term.create' ((if f = "" then "eval_start" else ("eval_" ^ f ^ "_start")),
    createLhsTerms in_vars) in
  let t2 = Term.create' (eval 1 f, !lhsterms) in
  let tail = (List.flatten (fst (createRulesForStmts stmts 1 true f))) in
  (t1, [t2], True)::tail

and normalizeConds rules =
  List.map (fun (l, r, c) -> (l, r, Utils.remdupC Pc.equalAtom c)) rules

and dnfify (lhs, rhs, c) =
  let c' = toDNF (removeNegation (toNNF c)) in
    let dcs = List.filter (fun c -> not (isFalse c)) (getDualClauses c') in
      List.map (fun dc -> (lhs, rhs, toPc dc)) dcs

and removeNegation c =
  match c with
    | Or (c1, c2) -> Or (removeNegation c1, removeNegation c2)
    | And (c1, c2) -> And (removeNegation c1, removeNegation c2)
    | Not (Atom c) -> negateAtom c
    | _ -> c

and negateAtom c =
  match c with
    | Pc.Equ (l, r) -> Or (Atom (Pc.Gtr (l, r)), Atom (Pc.Gtr (r, l)))
    | Pc.Neq (l, r) -> Atom (Pc.Equ (l, r))
    | Pc.Geq (l, r) -> Atom (Pc.Lss (l, r))
    | Pc.Gtr (l, r) -> Atom (Pc.Leq (l, r))
    | Pc.Leq (l, r) -> Atom (Pc.Gtr (l, r))
    | Pc.Lss (l, r) -> Atom (Pc.Geq (l, r))

and getDualClauses c =
  match c with
    | Or (c1, c2) -> (getDualClauses c1) @ (getDualClauses c2)
    | _ -> [c]

and isFalse dc =
  match dc with
    | And (c1, c2) -> (isFalse c1) || (isFalse c2)
    | Atom _ -> false
    | True | BRandom -> false
    | False -> true
    | _ -> failwith "Internal error in Simple.isFalse"

and toPc dc =
  match dc with
    | And (c1, c2) -> (toPc c1) @ (toPc c2)
    | Atom a -> [a]
    | True -> []
    | BRandom -> []
    | _ -> failwith "Internal error in Simple.toPc"

and createLhsTerms vars =
  List.map Poly.fromVar vars

and createRulesForStmts stmts i halt_for_last f =
  match stmts with
    | [] -> if halt_for_last then
              let lhs = Term.create' (eval i f, !lhsterms) in
                let rhs = Term.create' ((if f = "" then "eval_stop" else ("eval_" ^ f ^ "_stop")),
                  !lhsterms) in
                  let res = [(lhs, [rhs], True)] in
                    ([res], i + 1)
            else
              ([], i)
    | s::ss -> let (rules, newi) = createRulesForStmt s i f in
                 let (rr, newesti) = createRulesForStmts ss newi halt_for_last f in
                   (rules::rr, newesti)

and createRulesForStmt stmt i f =
  match stmt with
    | Skip -> let lhs = Term.create' (eval i f, !lhsterms)
              and rhs = Term.create' (eval (i + 1) f, !lhsterms) in
                ([(lhs, [rhs], True)], i + 1)
    | Halt -> let lhs = Term.create' (eval i f, !lhsterms)
              and rhs = Term.create' ("eval_stop", !lhsterms) in
                ([(lhs, [rhs], True)], i + 1)
    | Assume c -> control_points := (eval i f)::!control_points;
                  let lhs = Term.create' (eval i f, !lhsterms)
                  and rhs1 = Term.create' (eval (i + 1) f, !lhsterms)
                  and rhs2 = Term.create' ("eval_stop", !lhsterms) in
                    ([(lhs, [rhs1], c); (lhs, [rhs2], Not c)], i + 1)
    | Random x -> let lhs = Term.create' (eval i f, !lhsterms)
                  and rhs = Term.create' (eval (i + 1) f,
                                          getAssignment !lhsterms x (getNextRandom ())) in
                    ([(lhs, [rhs], True)], i + 1)
    | Assign (x, p) -> let lhs = Term.create' (eval i f, !lhsterms)
                       and rhs = Term.create' (eval (i + 1) f, getAssignment !lhsterms x p) in
                         ([(lhs, [rhs], True)], i + 1)
    | ITE (c, t, e) -> control_points := (eval i f)::!control_points;
                       if_loop_points := (eval i f)::!if_loop_points;
                       let lhs = Term.create' (eval i f, !lhsterms)
                       and rhs1 = Term.create' (eval (i + 1) f, !lhsterms)
                       and (rules1, newi1) = createRulesForStmts t (i + 1) false f in
                         let rhs2 = Term.create' (eval (newi1 + 1) f, !lhsterms)
                         and (rules2, newi2) = createRulesForStmts e (newi1 + 1) false f in
                           let merge1lhs = Term.create' (eval newi1 f, !lhsterms)
                           and merge2lhs = Term.create' (eval newi2 f, !lhsterms)
                           and mergerhs = Term.create' (eval (newi2 + 1) f, !lhsterms) in
                             let split1 = (lhs, [rhs1], c)
                             and split2 = (lhs, [rhs2], Not c)
                             and merge1 = (merge1lhs, [mergerhs], True)
                             and merge2 = (merge2lhs, [mergerhs], True) in
                               ([split1] @ (List.flatten rules1) @ [merge1] @ [split2] @ (List.flatten rules2) @ [merge2], newi2 + 1)
    | While (c, b) -> control_points := (eval i f)::!control_points;
                      if_loop_points := (eval i f)::!if_loop_points;
                      loop_points := (eval i f)::!loop_points;
                      let lhs = Term.create' (eval i f, !lhsterms)
                      and rhs1 = Term.create' (eval (i + 1) f, !lhsterms)
                      and (rules, newi) = createRulesForStmts b (i + 1) false f in
                        let backlhs = Term.create' (eval newi f, !lhsterms)
                        and rhs2 = Term.create' (eval (newi + 1) f, !lhsterms) in
                          let split1 = (lhs, [rhs1], c)
                          and back = (backlhs, [lhs], True)
                          and split2 = (lhs, [rhs2], Not c) in
                            ([split1] @ (List.flatten rules) @ [back] @ [split2], newi + 1)
    | Call (x, g, ys) -> control_points := (eval i f)::(eval (i + 1) f)::!control_points;
                         if_loop_points := (eval i f)::(eval (i + 1) f)::!if_loop_points;
                         loop_points := (eval i f)::(eval (i + 1) f)::!loop_points;
                         let lhs = Term.create' (eval i f, !lhsterms)
                         and callargs = List.map Poly.fromVar ys
                         and zapped = getZapped !lhsterms x in
                           let rhs1 = Term.create' ("eval_" ^ g ^ "_start", callargs)
                           and rhs2 = Term.create' (eval (i + 1) f, zapped) in
                             ([(lhs, [rhs1; rhs2], True)], i + 1)
    | Dummy1 _ | Dummy2 _ | Dummy3 _ -> failwith "Internal error in Simple.createRulesForStmt"

and eval i f =
  if f = "" then
    "eval_" ^ (string_of_int i)
  else
    "eval_" ^ f ^ "_" ^ (string_of_int i)

and getZapped vars zappedVar =
  match zappedVar with
    | None -> vars
    | Some v -> getZappedAux vars v

and getZappedAux vars v =
  match vars with
    | [] -> []
    | x::xs -> if (List.nth (Poly.getVars x) 0) = v then
                 (getNextRandom ())::xs
               else
                 x::(getZappedAux xs v)

and getNextRandom () =
  incr random_count;
  Poly.fromVar ("random_" ^ (string_of_int !random_count))

and getAssignment lhsterms x newx =
  match lhsterms with
    | [] -> failwith "Internal error in Simple.getAssignment"
    | p::pp -> let y = List.hd (Poly.getVars p) in
                 if x = y then
                   newx::pp
                 else
                   p::(getAssignment pp x newx)

and combineRules keepfuns trs =
  let (good, junk) = split_trs keepfuns trs in
    combine keepfuns good junk

and combine keepfuns good junk =
  match good with
    | [] -> []
    | r::rr -> (combine_all keepfuns r junk) @ (combine keepfuns rr junk)

and combine_all keepfuns r junk =
  let todo = [r]
  and accu = [] in
    combine_all_loop keepfuns todo accu junk

and combine_all_loop keepfuns todo accu junk =
  match todo with
    | [] -> accu
    | r::rr -> if (hasMoreThanOne r) || (Utils.contains keepfuns (Term.getFun (getRhs r))) then
                 combine_all_loop keepfuns rr (accu @ [r]) junk
               else
                 let newtodos = combine_one r junk in
                   combine_all_loop keepfuns (newtodos @ rr) accu junk

and combine_one r junk =
  let rules = getDefiningRules junk (Term.getFun (getRhs r)) in
    List.map (combine_rule r) rules

and combine_rule (l1, r1s, c1) (l2, r2s, c2) =
  let r1 = getRhs (l1, r1s, c1) in
    let subby = getSubstitution (Term.getArgs r1) (Term.getArgs l2) in
      (l1, List.map (fun r2 -> Term.instantiate r2 subby) r2s, Utils.remdupC Pc.equalAtom (c1 @ (Pc.instantiate c2 subby)))

and getDefiningRules trs f =
  List.filter (fun (l, _, _) -> Term.getFun l = f) trs

and hasMoreThanOne (_, rs, _) =
  match rs with
    | [r] -> false
    | _ -> true

and getRhs (_, rhss, _) =
  match rhss with
    | [rhs] -> rhs
    | _ -> failwith "Internal error in Simple.getRhs"

and getSubstitution args args' =
  match args' with
    | [] -> []
    | x::xx -> (getName x, List.hd args)::(getSubstitution (List.tl args) xx)

and getName poly =
  List.hd (Poly.getVars poly)

and split_trs keepfuns trs =
  match trs with
    | [] -> ([], [])
    | (l,rs,c)::rr -> let (good, junk) = split_trs keepfuns rr in
                 if (Utils.contains keepfuns (Term.getFun l)) then
                   ((l,rs,c)::good, junk)
                 else
                   (good, (l,rs,c)::junk)

(***************************)
(* Convert program to Cint *)
(***************************)
let allvars = ref []
let addedfuns = ref []
let start_vars = ref []

let rec createCint combine prog =
  control_points := [];
  if_loop_points := [];
  lhsterms := [];
  random_count := 0;
  allvars := [];
  addedfuns := [];
  start_vars := [];
  let tuprules = createrules combine prog in
    let renamed_tuprules = rename tuprules prog in
      let blasted_tuprules = blast renamed_tuprules in
        ("eval_start", toComrules blasted_tuprules)

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
