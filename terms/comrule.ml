(*
  Rules

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

type rule = {
  lhs : Term.term;
  rhss : Term.term list;
  cond : Pc.cond;
  lowerBound : Poly.poly;
  upperBound : Poly.poly;
}

type cint = rule list

type system = {
  startSym : Term.term;
  cint : cint;
}


(* Create a string for a rule *)
let rec toString r =
  (Term.toString r.lhs) ^ " -> " ^ (toStringRhss r.rhss) ^
  (if r.cond = [] then "" else " [ " ^ (Pc.toString r.cond) ^ " ]")
and toStringRhss rs =
  "Com_" ^ (string_of_int (List.length rs)) ^ "(" ^
  String.concat ", " (List.map Term.toString rs) ^ ")"
and listToStringPrefix prefix rules =
  String.concat "\n" (List.map (fun r -> prefix ^ toString r) rules)
    
(* Create a comrule. *)
let createRule l rs c =
  { lhs = l;
    rhss = rs;
    cond = Utils.remdupC Pc.equalAtom c;
    lowerBound = Poly.one;
    upperBound = Poly.one;
  }

(* Create a comrule with bounds. *)
let createWeightedRule l rs c lb ub =
  let r =
    { lhs = l;
      rhss = rs;
      cond = c;
      lowerBound = lb;
      upperBound = ub; }
  and varsInLB = Poly.hasVars lb
  and varsInUB = Poly.hasVars ub
  and ineq = Pc.Geq (ub, lb) in
  (* JTT - We would want to ensure lb <= ub using PC where possible.
     Unfortunately, we can't always know -- especially where conditionals force
     values onto variables. *)
  if (not varsInLB) && (not varsInUB) then
    begin
      if Pc.isTrueAtom Poly.VarMap.empty ineq then
        r else
        failwith (Printf.sprintf
                    "Weighted rule has lower bound larger than upper bound: %s"
                    (toString r))
    end
  else if Poly.equal lb ub then
    r
  else
    r

(* Create a rule which creates a rule of the form f(x1,...,xn) -> g(x1,...,xn) *)
let createSimpleRule f g vars =
  let poly_vars = List.map Poly.fromVar vars in
  let fx = Term.create' (f, poly_vars) in
  let gx = Term.create' (g, poly_vars) in
  createWeightedRule fx [gx] [Pc.trivial_atom] Poly.zero Poly.zero

      
let compare r1 r2 =
  let lComp = Term.compare r1.lhs r2.lhs in
  if lComp <> 0 then
    lComp
  else
    let rhsNum1 = List.length r1.rhss in
    let rhsNum2 = List.length r2.rhss in
    if rhsNum1 < rhsNum2 then
      -1
    else if rhsNum1 > rhsNum2 then
      1
    else
      let rhsComp =
        List.fold_left2
          (fun acc t1 t2 -> if acc <> 0 then acc else Term.compare t1 t2) 0 r1.rhss r2.rhss in
      if rhsComp <> 0 then
        rhsComp
      else
        let cComp = Pc.compare r1.cond r2.cond in
        if cComp <> 0 then
          cComp
        else
          let uComp = Poly.compare r1.upperBound r2.upperBound in
          if uComp <> 0 then
            uComp
          else
            Poly.compare r1.lowerBound r2.lowerBound

(* Create a string for a rule *)
let toDotString r =
  (Term.toString r.lhs) ^ " -> " ^ (toStringRhss r.rhss) ^
  (if r.cond = [] then "" else " [ " ^ (Pc.toDotString r.cond) ^ " ]")

(* Get lhs of a rule *)
let getLeft r =
  r.lhs

(* Get rhs of a rule *)
let getRights r =
  r.rhss

(* Get cond of a rule *)
let getCond r =
  r.cond

(* Get function symbols from both sides *)
let getFuns r =
  Utils.remdup ((Term.getFun r.lhs)::(List.map Term.getFun r.rhss))

(* Get function symbols from a list of rules *)
let getFunsList rs =
  Utils.remdup (List.concat (List.map getFuns rs))
    
let getLowerBound r =
  r.lowerBound

let getUpperBound r =
  r.upperBound

(* Get function symbol from left side *)
let getLeftFun r =
  Term.getFun r.lhs

(* Get function symbols from right side *)
let getRightFuns r =
  Utils.remdup (List.map Term.getFun r.rhss)

(* Return the variables in the right-hand sides *)
let getRightVars r =
  Utils.remdup (Utils.concatMap Term.getVars r.rhss)

(* Return the variables of a rule *)
let getVars r =
  Utils.remdup ((Term.getVars r.lhs)
                @ (getRightVars r)
                @ (Pc.getVars r.cond)
                @ (Poly.getVars r.lowerBound)
                @ (Poly.getVars r.upperBound))

let getSlicingVars r =
  Utils.remdup ((Pc.getVars r.cond)
                @ (Poly.getVars r.lowerBound)
                @ (Poly.getVars r.upperBound))

(* Renames the variables in a rule *)
let rec renameVars badvars r =
  let vars = getVars r in
    let varmapping = createVarMapping badvars vars in
      { lhs = Term.renameVars varmapping r.lhs;
        rhss = List.map (Term.renameVars varmapping) r.rhss;
        cond = Pc.renameVars varmapping r.cond;
        lowerBound = Poly.renameVars varmapping r.lowerBound;
        upperBound = Poly.renameVars varmapping r.upperBound;
      }
and createVarMapping badvars vars =
  match vars with
    | [] -> []
    | v::vv -> let vnew = getNewVarName badvars v in
                 (v, vnew)::(createVarMapping (vnew::badvars) vv)
and getNewVarName badvars v =
  if (Utils.contains badvars v) then
    getNewVarName badvars (v ^ "'")
  else
    v

(* Determines whether a rule is linear *)
let isLinear r =
  (Term.isLinear r.lhs) && (List.for_all Term.isLinear r.rhss) && (Pc.isLinear r.cond)

(* Determines whether right-hand sides of a rule are linear *)
let isRightLinear r =
  List.for_all Term.isLinear r.rhss

(* Determines whether the constraint of a rule is linear *)
let isConstraintLinear r =
  Pc.isLinear r.cond

let rec equal rule1 rule2 =
  rule1 == rule2 || equalInternal rule1 rule2
and equalInternal rule1 rule2 =
  (List.length rule1.rhss) = (List.length rule2.rhss)
    && (Term.equal rule1.lhs rule2.lhs)
    && (List.for_all2 Term.equal rule1.rhss rule2.rhss)
    && (Pc.equal rule1.cond rule2.cond)

(* Determines whether V(rs) is contained V(l) *)
let satisfiesVarCond r =
  Utils.containsAll (Term.getVars r.lhs) (Utils.remdup (List.flatten (List.map Term.getVars r.rhss)))


let hasDefinition x = function
  | Pc.Equ (s, t) ->
    let sub = Poly.minus s t in
    let coeff = Poly.getCoeff sub [(x, 1)] in
    if Poly.eq_big_int (Big_int.abs_big_int coeff) Big_int.unit_big_int then
      let sub' = Poly.minus sub (Poly.constmult (Poly.fromVar x) coeff) in
      not (Utils.contains (Poly.getVars sub') x)
    else
      false
  | _ -> false

let rec findDefinition x lvars c cand =
  match c with
    | [] -> cand
    | d::rest -> let dvars = Pc.getVarsAtom d in
                   if Utils.contains dvars x then
                     if (hasDefinition x d) && (Utils.containsAll (x::lvars) dvars) then
                       if cand = None then
                         findDefinition x lvars rest (Some d)
                       else
                         None
                     else
                       None
                   else
                     findDefinition x lvars rest cand

let extract x = function
  | Pc.Equ (s, t) -> let sub = Poly.minus s t in
                     let coeff = Poly.getCoeff sub [(x, 1)] in
                     if Poly.eq_big_int coeff Big_int.unit_big_int then
                       Poly.constmult (Poly.minus sub (Poly.fromVar x)) (Big_int.minus_big_int Big_int.unit_big_int)
                     else
                       Poly.minus sub (Poly.fromVar x)
  | _ -> failwith "Internal error in Rule.extract"


(* filters (first) equivalent atoms from the list of conditions *)
let rec remove d = function
  | [] -> []
  | d'::rest -> if Pc.equalAtom d d' then
      rest
    else
      d'::(remove d rest)

let rec getSubstitution newVars c lvars =
  match newVars with
    | [] -> ([], c)
    | x::rest -> match findDefinition x lvars c None with
                   | None -> getSubstitution rest c lvars
                   | Some d -> let (sigma, newc) = getSubstitution rest (remove d c) lvars in
                                 ((x, extract x d)::sigma, newc)

let rec internalize r =
  let lvars = Term.getVars r.lhs
  and rvars = getRightVars r in
    let newVars = Utils.notIn lvars rvars in
      let (sigma, newC) = getSubstitution newVars r.cond lvars in
        if sigma = [] then
          r
        else
          { lhs = r.lhs;
            rhss = List.map (fun r -> Term.instantiate r sigma) r.rhss;
            cond = newC;
            (* JTT 11-12-15 -- It isn't clear that I shouldn't instantiate these too. *)
            lowerBound = Poly.instantiate r.lowerBound sigma;
            upperBound = Poly.instantiate r.upperBound sigma;
          }


(* only one rhs? *)
let isUnary r =
  List.length r.rhss = 1

(* Instantiate a rule *)
let instantiate r varmap =
  { lhs = Term.instantiate r.lhs varmap;
    rhss = List.map (fun r -> Term.instantiate r varmap) r.rhss;
    cond = Pc.instantiate r.cond varmap;
    lowerBound = Poly.instantiate r.lowerBound varmap;
    upperBound = Poly.instantiate r.upperBound varmap;
  }

let chainTwoRules rule1 rule2 =
  match rule1.rhss with
  | [_] ->
    let renamedRule2 = renameVars (getVars rule1) rule2 in
    let subby =
      List.combine
        (List.map (fun a -> List.hd (Poly.getVars a))
           (Term.getArgs renamedRule2.lhs))
        (Term.getArgs (List.hd rule1.rhss)) in (* rule1.rhss must be of form [x] *)
    { lhs = rule1.lhs ;
      rhss = List.map (fun r -> Term.instantiate r subby) renamedRule2.rhss;
      cond = Utils.remdupC Pc.equalAtom (rule1.cond @ (Pc.instantiate renamedRule2.cond subby));
      lowerBound = Poly.add rule1.lowerBound (Poly.instantiate renamedRule2.lowerBound subby);
      upperBound = Poly.add rule1.upperBound (Poly.instantiate renamedRule2.upperBound subby);
    }
  | _ -> failwith "Trying to chain rule1 and rule2 where rule1 is non-unary"


let removeNeq r =
  let rec removeNeqConstraint c =
    let addIn atom cs = List.map (fun c -> atom::c) cs in
    match c with
    | [] -> [[]]
    | a::rest -> let c's = removeNeqConstraint rest in
      match a with
      | Pc.Neq (l, r) -> (addIn (Pc.Gtr (l, r)) c's) @ (addIn (Pc.Lss (l, r)) c's)
      | _ -> addIn a c's
  in
  let c's = removeNeqConstraint r.cond in
    List.map (fun c' -> {lhs = r.lhs ; rhss = r.rhss ; cond = c'; lowerBound = r.lowerBound; upperBound = r.upperBound }) c's

let restrictArguments indexSet rule =
  { lhs = Term.create' (Term.getFun rule.lhs,
                        Utils.getIndexedSubset indexSet (Term.getArgs rule.lhs)) ;
    rhss = List.map (fun rhs -> Term.create' (Term.getFun rhs,
                                              Utils.getIndexedSubset indexSet (Term.getArgs rhs)))
      rule.rhss ;
    cond = rule.cond;
    lowerBound = rule.lowerBound;
    upperBound = rule.upperBound;
  }

let rec buildNewArgs = function
  | x when x < 0 -> []
  | i -> (Printf.sprintf "Ar_%i" i):: (buildNewArgs (i - 1))

let rec firstN lst i =
  if i == 0 then
    []
  else
    match lst with
    | [] -> failwith "Splitting list beyond its end!"
    | hd::tl -> hd::(firstN tl (i - 1))

let pad maxArity term =
  let pel = Poly.fromVar "ArityPad" in
  let rec makePad = function
    | 0 -> []
    | x -> pel::(makePad (x - 1)) in
  let toAdd = maxArity - (Term.getArity term) in
  let padding = makePad toAdd in
  { Term.fn = term.Term.fn;
    Term.args = term.Term.args @ padding; }

let rec sigmaToString = function
  | [] -> Printf.sprintf "\n"
  | (s,p)::tl ->
    Printf.sprintf "%s -> %s\n" s (Poly.toString p) ^ (sigmaToString tl)

let buildMapping (newArgs : Poly.var list) (cr : rule) =
  let maxArity = List.length newArgs in
  let lhs = cr.lhs in
  let toMap = firstN newArgs (Term.getArity lhs) in
  let sigma = List.map2 (fun var poly -> Poly.toVar poly, Poly.fromVar var)
    toMap lhs.Term.args in
  let cond = Pc.instantiate cr.cond sigma in
  let lhs = { lhs with Term.args = List.map Poly.fromVar newArgs } in
  let rhss = List.map
    (fun rh ->
      let rh' = Term.instantiate rh sigma in
      let rh'' = pad maxArity rh' in
      rh'')
    cr.rhss in
  let ret =
  {lhs; rhss; cond; (* punning *)
   lowerBound = Poly.instantiate cr.lowerBound sigma;
   upperBound = Poly.instantiate cr.upperBound sigma;} in
 (* Printf.eprintf "%s\n\nbecomes\n\n%s\n\n" (toString cr) (toString ret); *)
  ret

let crArity cr =
  let lhsAr = Term.getArity cr.lhs in
  List.fold_left (fun (maxArity, fixedArity) rhs ->
    let thisArity = Term.getArity rhs in
    if thisArity > maxArity
    then thisArity, false
    else if thisArity = maxArity
    then maxArity, fixedArity
    else maxArity, false) (lhsAr, true) cr.rhss

let maximumArity cint =
  let init,rst = match cint with
      [] -> (0,true), []
    | hd::tl -> crArity hd, tl in
  let folder (maxArity, fixedArity) cr =
    let crArity, selfFixed = crArity cr in
    if crArity > maxArity
    then crArity, false
    else if crArity = maxArity
    then maxArity, (fixedArity && selfFixed)
    else maxArity, false in
  List.fold_left folder init rst

let getArgs = function
  | [] -> []
  | hd::_ -> List.map Poly.toVar hd.lhs.Term.args

let fixArity cint =
  let maxArity,_ = maximumArity cint in
  (* we don't have to fix the arity, just skip it. *)
(*  if fixedArity then cint
  else
*)
    (* not everything has the same arity, do a transform *)
    let newArgs = List.rev (buildNewArgs (maxArity - 1)) in
    let transform = buildMapping newArgs in
    List.map transform cint

let getEdges cint =
  List.flatten
    (List.map (fun r ->
      List.map (fun rhs -> r.lhs.Term.fn, rhs.Term.fn) r.rhss)
       cint)
