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

(* Create a comrule. *)
let createRule l rs c =
  { lhs = l;
    rhss = rs;
    cond = c;
    lowerBound = Poly.one;
    upperBound = Poly.one;
  }

(* Create a comrule with bounds. *)
let createWeightedRule l rs c lb ub =
  { lhs = l;
    rhss = rs;
    cond = c;
    lowerBound = lb;
    upperBound = ub;
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
        Pc.compare r1.cond r2.cond

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
  Utils.remdup ((Term.getVars r.lhs) @ (getRightVars r) @ (Pc.getVars r.cond))

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
            lowerBound = r.lowerBound;
            upperBound = r.upperBound;
          }
and getSubstitution newVars c lvars =
  match newVars with
    | [] -> ([], c)
    | x::rest -> match findDefinition x lvars c None with
                   | None -> getSubstitution rest c lvars
                   | Some d -> let (sigma, newc) = getSubstitution rest (remove c d) lvars in
                                 ((x, extract x d)::sigma, newc)
and findDefinition x lvars c cand =
  match c with
    | [] -> cand
    | d::rest -> let dvars = Pc.getVarsAtom d in
                   if Utils.contains dvars x then
                     if (isEqu d) && (hasUnitCoeff d x) && (Utils.containsAll (x::lvars) dvars) then
                       if cand = None then
                         findDefinition x lvars rest (Some d)
                       else
                         None
                     else
                       None
                   else
                     findDefinition x lvars rest cand
and isEqu c =
  match c with
    | Pc.Equ _ -> true
    | _ -> false
and hasUnitCoeff c x =
  match c with
    | Pc.Equ (s, t) -> let sub = Poly.minus s t in
                         let coeff = Poly.getCoeff sub [(x, 1)] in
                           if Poly.eq_big_int (Big_int.abs_big_int coeff) Big_int.unit_big_int then
                             let sub' = Poly.minus sub (Poly.constmult (Poly.fromVar x) coeff) in
                               not (Utils.contains (Poly.getVars sub') x)
                           else
                             false
    | _ -> failwith "Internal error in Rule.hasUnitCoeff"
and extract x d =
  match d with
    | Pc.Equ (s, t) -> let sub = Poly.minus s t in
                         let coeff = Poly.getCoeff sub [(x, 1)] in
                           if Poly.eq_big_int coeff Big_int.unit_big_int then
                             Poly.constmult (Poly.minus sub (Poly.fromVar x)) (Big_int.minus_big_int Big_int.unit_big_int)
                           else
                             Poly.minus sub (Poly.fromVar x)

    | _ -> failwith "Internal error in Rule.extract"
and remove c d =
  match c with
    | [] -> []
    | d'::rest -> if Pc.equalAtom d d' then
                    rest
                  else
                    d'::(remove rest d)

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
