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
  let condString = if r.cond = [] then "" else Printf.sprintf " [ %s ]" (Pc.toString r.cond) in
  Printf.sprintf "%s -> %s%s" (Term.toString r.lhs) (toStringRhss r.rhss) condString
and toStringRhss rs =
  Printf.sprintf "Com_%i(%s)" (List.length rs) (String.concat ", " (List.map Term.toString rs))
and listToStringPrefix prefix rules =
  String.concat "\n" (List.map (fun r -> prefix ^ toString r) rules)
    
let toDotString = toString

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
let getLeftVars r =
  Utils.remdup (Term.getVars r.lhs)
  
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
    | v::vv -> let vnew = Poly.getFreshVarFrom badvars v in
                 (v, vnew)::(createVarMapping (vnew::badvars) vv)

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


let getFreeVars r =
  let lvars = getLeftVars r in
  let rvars = getRightVars r in
  Utils.notIn lvars rvars

(* TODO: move this function to preprocess.ml *)
let rec internalize r =
  let lvars = getLeftVars r in
  let rvars = getRightVars r in
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

let rec sigmaToString = function
  | [] -> Printf.sprintf "\n"
  | (s,p)::tl ->
    Printf.sprintf "%s -> %s\n%s" s (Poly.toString p) (sigmaToString tl)

let getArgs = function
  | [] -> []
  | hd::_ -> List.map Poly.toVar hd.lhs.Term.args

let getEdges cint =
  List.flatten
    (List.map (fun r ->
      List.map (fun rhs -> r.lhs.Term.fn, rhs.Term.fn) r.rhss)
       cint)
