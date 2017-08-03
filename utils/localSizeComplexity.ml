(*
  Local size complexity stuff

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

open AbstractRule

type localcomplexity =
    Max of Big_int.big_int
  | MaxPlusConstant of Big_int.big_int
  | SumPlusConstant of Big_int.big_int
  | ScaledSumPlusConstant of Big_int.big_int * Big_int.big_int
  | P of Expexp.expexp
  | Unknown

type size_data = { bound : localcomplexity; active_vars : Term.pos list }

let create_size_data (b, avs) = { bound = b; active_vars = avs }
let unknown_size_data = { bound = Unknown; active_vars = [] }

type index = { rhsIdx : int; varIdx : int }
type local_size_data = index * size_data

module Make (RuleT : AbstractRule) = struct
  module RuleT = RuleT

  type trans_data = RuleT.rule * local_size_data
  type tds = trans_data list

  let getConstantSummand c =
    match c.bound with
    | (Max e) -> e
    | (MaxPlusConstant e) -> e
    | (SumPlusConstant e) -> e
    | (ScaledSumPlusConstant (e, _)) -> e
    | _ -> failwith "Internal error in LocalSizeComplexity.getConstantSummand"

  let getScalarFactor c =
    match c.bound with
    | (SumPlusConstant _) -> Big_int.unit_big_int
    | (ScaledSumPlusConstant (_, s)) -> s
    | _ -> failwith "Internal error in LocalSizeComplexity.getConstantScalarFactor"

  let getConstant c =
    match c.bound with
    | P p -> Expexp.getConstant p
    | _ -> failwith "Internal error in LocalSizeComplexity.getConstant"

  let rec equal c d =
    c == d || equalInternal c d
  and equalInternal c d =
    let v = c.active_vars in
    let v' = d.active_vars in
    match (c.bound, d.bound) with
    | (Max e, Max e') -> Poly.eq_big_int e e' && equalVar v v'
    | (MaxPlusConstant e, MaxPlusConstant e') -> Poly.eq_big_int e e' && equalVar v v'
    | (SumPlusConstant e, SumPlusConstant e') -> Poly.eq_big_int e e' && equalVar v v'
    | (P p, P q) -> (Expexp.equal p q) && equalVar v v'
    | (Unknown, Unknown) -> equalVar v v'
    | _ -> false
  and equalVar v v' =
    (v == v') || ((List.length v = List.length v') && (List.for_all2 (fun a b -> (a == b) || (a = b)) v v'))

  let isConstant c =
    match c.bound with
    | (Max _) -> false
    | (MaxPlusConstant _) -> false
    | (SumPlusConstant _) -> false
    | (ScaledSumPlusConstant _) -> false
    | (P p) -> (c.active_vars = []) && (Expexp.isConst p)
    | (Unknown) -> false

  let rec complexity2localcomplexity c vars =
    match c with
    | Complexity.P p ->
       if (not (Expexp.isConst p)) && (Expexp.isSumOfVarsPlusConstant p) then
	 create_size_data (SumPlusConstant (Expexp.getConstant p), getVarNums p vars)
       else if (not (Expexp.isConst p)) && (Expexp.isScaledSumOfVarsPlusConstant p) then
	 create_size_data (ScaledSumPlusConstant (Expexp.getConstant p, Expexp.getScaleFactor p), getVarNums p vars)
       else
	 create_size_data (P p, getVarNums p vars)
    | Complexity.Unknown -> unknown_size_data
  and getVarNums p vars =
    let pv = Expexp.getVars p in
    getVarNumList pv vars 0
  and getVarNumList pv vars i =
    match vars with
    | [] -> []
    | v::rest ->
       if Utils.contains pv v then
         i::(getVarNumList pv rest (i + 1))
       else
         getVarNumList pv rest (i + 1)

  let rec toSmallestComplexity c vars =
    let v = c.active_vars in
    match c.bound with
    | (Max e) -> Complexity.P (Expexp.add (getSum v vars) (Expexp.fromConstant e))
    | (MaxPlusConstant e) -> Complexity.P (Expexp.add (getSum v vars) (Expexp.fromConstant e))
    | (SumPlusConstant e) -> Complexity.P (Expexp.add (getSum v vars) (Expexp.fromConstant e))
    | (ScaledSumPlusConstant (e, s)) -> Complexity.P (Expexp.constmult (Expexp.add (getSum v vars) (Expexp.fromConstant e)) s)
    | (P p) -> Complexity.P p
    | (Unknown) -> Complexity.Unknown
  and getSum v vars =
    match v with
    | [] -> Expexp.zero
    | x::rest -> Expexp.add (Expexp.fromVar (List.nth vars x)) (getSum rest vars)

  let rec add c d vars =
    let v = c.active_vars in
    match c.bound with
    | (Max e) -> addMax e v d vars
    | (MaxPlusConstant e) -> addMaxPlusConstant e v d vars
    | (SumPlusConstant e) -> addSumPlusConstant e v d vars
    | (ScaledSumPlusConstant (e, s)) -> addScaledSumPlusConstant e s v d vars
    | (P p) -> addP p v d vars
    | (Unknown) -> unknown_size_data
  and addMax e v d vars : size_data =
    let v' = d.active_vars in
    match d.bound with
    | Max e'
    | MaxPlusConstant e'
    | SumPlusConstant e' ->
       if disjoint v v' then
	 create_size_data (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
       else
	 addAsSmallestComplexities (create_size_data (Max e, v)) d vars
    | ScaledSumPlusConstant (e', s) ->
       create_size_data (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
    | P p' ->
       if isConstant d then
	 create_size_data (Max (Big_int.add_big_int e (getConstant d)), v)
       else
	 addAsSmallestComplexities (create_size_data (Max e, v)) d vars
    | Unknown -> unknown_size_data
  and addMaxPlusConstant e v d vars =
    let v' = d.active_vars in
    match d.bound with
    | Max e'
    | MaxPlusConstant e'
    | SumPlusConstant e' ->
       if disjoint v v' then
	 create_size_data (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
       else
	 addAsSmallestComplexities (create_size_data (MaxPlusConstant e, v)) d vars
    | ScaledSumPlusConstant (e', s) ->
       create_size_data (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
    | P p' ->
       if isConstant d then
	 create_size_data (MaxPlusConstant (Big_int.add_big_int e (getConstant d)), v)
       else
	 addAsSmallestComplexities (create_size_data (MaxPlusConstant e, v)) d vars
    | Unknown -> unknown_size_data
  and addSumPlusConstant e v d vars =
    let v' = d.active_vars in
    match d.bound with
    | Max e'
    | MaxPlusConstant e'
    | SumPlusConstant e' ->
       if disjoint v v' then
	 create_size_data (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
       else
	 addAsSmallestComplexities (create_size_data (SumPlusConstant e, v)) d vars
    | ScaledSumPlusConstant (e', s) ->
       create_size_data (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
    | P p' ->
       if isConstant d then
	 create_size_data
	   (SumPlusConstant (Big_int.add_big_int e (getConstant d)), v)
       else
	 addAsSmallestComplexities (create_size_data (SumPlusConstant e, v)) d vars
    | Unknown -> unknown_size_data
  and addScaledSumPlusConstant e s v d vars =
    let v' = d.active_vars in
    match d.bound with
    | Max e'
    | MaxPlusConstant e'
    | SumPlusConstant e' ->
       create_size_data (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
    | ScaledSumPlusConstant (e', s') ->
       create_size_data (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s s'), unite v v')
    | P p' ->
       if isConstant d then
	 create_size_data
	   (ScaledSumPlusConstant (Big_int.add_big_int e (getConstant d), s), v)
       else
         addAsSmallestComplexities (create_size_data (SumPlusConstant e, v)) d vars
    | Unknown -> unknown_size_data
  and addP p v d vars =
    let v' = d.active_vars in
    match d.bound with
    | P p' -> create_size_data (P (Expexp.add p p'), unite v v')
    | _ -> add d (create_size_data (P p, v)) vars
  and disjoint v v' =
    (Utils.intersect v v') = []
  and unite v v' =
    List.sort compare (Utils.remdup (v @ v'))
  and addAsSmallestComplexities c d vars : size_data =
    let cc = (Complexity.add (toSmallestComplexity c vars) (toSmallestComplexity d vars)) in
    complexity2localcomplexity cc vars

  let rec listSum l vars =
    match l with
    | [] -> create_size_data (P Expexp.zero, [])
    | [x] -> x
    | x::y::rest -> listSum ((add x y vars)::rest) vars

  let rec getMax c d vars =
    let v = c.active_vars in
    let v' = d.active_vars in
    match c.bound with
    | Max e -> (
      match d.bound with
      | Max e' ->
	 create_size_data (Max (Big_int.max_big_int e e'), unite v v')
      | MaxPlusConstant e' ->
	 create_size_data (MaxPlusConstant (Big_int.max_big_int e e'), unite v v')
      | SumPlusConstant e' ->
	 create_size_data (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
      | ScaledSumPlusConstant (e', s) ->
	 create_size_data (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
      | P p' ->
	 if isConstant d then
	   create_size_data (MaxPlusConstant (Big_int.add_big_int e (getConstant d)), v)
	 else
	   let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
	   create_size_data (P m, getVarNums m vars)
      | Unknown -> unknown_size_data)
    | MaxPlusConstant e -> (
      match d.bound with
      | Max e'
      | MaxPlusConstant e' ->
	 create_size_data (MaxPlusConstant (Big_int.max_big_int e e'), unite v v')
      | SumPlusConstant e' ->
	 create_size_data (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
      | ScaledSumPlusConstant (e', s) ->
	 create_size_data (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
      | P p' ->
	 if isConstant d then
	   create_size_data (MaxPlusConstant (Big_int.add_big_int e (getConstant d)), v)
	 else
	   let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
	   create_size_data (P m, getVarNums m vars)
      | Unknown -> unknown_size_data)
    | SumPlusConstant e -> (
      match d.bound with
      | Max e'
      | MaxPlusConstant e'
      | SumPlusConstant e' ->
	 create_size_data (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
      | ScaledSumPlusConstant (e', s) ->
	 create_size_data (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
      | P p' ->
	 if isConstant d then
	   create_size_data (SumPlusConstant (Big_int.add_big_int e (getConstant d)), v)
	 else
	   let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
	   create_size_data (P m, getVarNums m vars)
      | Unknown -> unknown_size_data)
    | ScaledSumPlusConstant (e, s) -> (
      match d.bound with
      | Max e'
      | MaxPlusConstant e'
      | SumPlusConstant e' ->
	 create_size_data (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
      | ScaledSumPlusConstant (e', s') ->
	 create_size_data (ScaledSumPlusConstant (Big_int.max_big_int e e', Big_int.max_big_int s s'), unite v v')
      | P p' ->
	 if isConstant d then
	   create_size_data (ScaledSumPlusConstant (Big_int.add_big_int e (getConstant d), s), v)
	 else
	   let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
	   create_size_data (P m, getVarNums m vars)
      | Unknown -> unknown_size_data)
    | P p -> (
      match d.bound with
      | P p' ->
	 let m = Expexp.max p p' in
         create_size_data (P m, getVarNums m vars)
      | _ -> getMax d c vars)
    | Unknown -> unknown_size_data
  and getExpexp c =
    match c with
    | Complexity.P p -> p
    | _ -> failwith "Internal error in LocalSizeComplexity.getExpexp"
  and listMax list vars =
    match list with
    | [] -> create_size_data (P Expexp.zero, [])
    | [x] -> x
    | x::y::rest -> listMax ((getMax x y vars)::rest) vars

  let rec toStringLocalComplexity c =
    let v = c.active_vars in
    match c.bound with
    | Max e -> "=(" ^ (Big_int.string_of_big_int e) ^ ") " ^ (varString v)
    | MaxPlusConstant e -> "+.(" ^ (Big_int.string_of_big_int e) ^ ") " ^ (varString v)
    | SumPlusConstant e -> "+_(" ^ (Big_int.string_of_big_int e) ^ ") " ^ (varString v)
    | ScaledSumPlusConstant (e, s) -> "*.(" ^ (Big_int.string_of_big_int e) ^ ", " ^ (Big_int.string_of_big_int s) ^ ") " ^ (varString v)
    | P p -> Expexp.toString p
    | Unknown -> "?"
  and varString v =
    "[" ^ (String.concat ";" (List.map string_of_int v)) ^ "]"

  let getDependedVariables tvars cond =
    let getNew x newvars = List.filter (fun v -> not (Utils.contains x v)) newvars in
    let rec addOneDepLevel x cond news =
      match cond with
      | [] -> x @ (Utils.remdup news)
      | c::rest ->
	 let condVars = Pc.getVars cond in
	 if Utils.containsOne x condVars then
           addOneDepLevel x rest (news @ (getNew x condVars))
	 else
           addOneDepLevel x rest news in
    let rec getDepsFixpoint x y cond =
      if x = y then
	x
      else
	getDepsFixpoint (addOneDepLevel x cond []) x cond in
    getDepsFixpoint tvars [] cond

  (* compute local size complexities *)
  let maxBound = ref (Poly.mkVar "")
  let maxPlusConstantBound = ref (Poly.mkVar "")
  let maxC = Big_int.big_int_of_int 1024
  let maxS = Big_int.big_int_of_int 1024
  let eConstant = ref Big_int.zero_big_int
  let sConstant = ref Big_int.zero_big_int

  let rec computeLocalSizeComplexities trs = Utils.concatMap computeLocalSizeComplexitiesForRule trs
  and computeLocalSizeComplexitiesForRule rule : tds =
    let fullCond = RuleT.getCond rule in
    let linCond = Pc.dropNonLinearAtoms fullCond in
    let lvars = Term.getVars (RuleT.getLeft rule) in
    let lvarswithnums = Utils.mapi (fun i v -> (v, i)) lvars in
    (* This produces one (rule, ((rhsIdx, argumentIdx), (local size bound, active variable idxs))) tuple per RV *)
    Utils.concatMap
      (fun (rhs, rhsIdx) ->
       Utils.mapi
         (fun varIdx argument -> (rule, ({ rhsIdx = rhsIdx; varIdx = varIdx }, computeLSCForTerm lvars lvarswithnums linCond fullCond argument)))
         (Term.getArgs rhs))
      (Utils.mapi (fun idx rhs -> (rhs, idx)) (RuleT.getRights rule))
  and computeLSCForTerm lvars lvarswithnums linCond fullCond t =
    let isLinear = Poly.isLinear t in
    if isLinear && (Smt.isConstantBound linCond t maxC) then
      if Poly.isConst t then
        create_size_data (P (Expexp.fromConstant (Big_int.abs_big_int (Poly.getConstant t))), [])
      else
        let e = minimizeC (Smt.isConstantBound linCond t) Big_int.zero_big_int maxC in
        create_size_data (P (Expexp.fromConstant e), [])
    else
      let alltvars = Poly.getVars t in
      let tvars = Utils.intersect lvars alltvars
      and isRegular = Utils.containsAll lvars alltvars in
      if isLinear && isRegular && (List.length tvars = 1) && (Poly.isSumOfVarsPlusConstant t) && (Poly.eq_big_int Big_int.zero_big_int (Poly.getConstant t)) then
        create_size_data (Max Big_int.zero_big_int, getVarNums lvarswithnums tvars)
      else
        (
          maxBound := Poly.mkVar "";
          maxPlusConstantBound := Poly.mkVar "";
          let deps = Utils.intersect lvars (getDependedVariables alltvars linCond) in
          if isLinear && isMaxBound linCond t maxC deps then
            let minimal = [!maxBound] in
            let e = minimizeC (fun c -> Smt.isMaxBound linCond t c minimal) Big_int.zero_big_int maxC in
            create_size_data (Max e, getVarNums lvarswithnums minimal)
          else if isLinear && isRegular && (List.length tvars = 1) && (Poly.isSumOfVarsPlusConstant t) && isMaxPlusConstantBound linCond t (Big_int.abs_big_int (Poly.getConstant t)) deps then
            (* t = v_1 + ... + v_l + k, for v a lhs variable and k some constant. Start optimizing constant from k, not from maxC.
                     This converges far faster for cases like "x + 1" or "x - 1"... *)
            let minimal = [!maxPlusConstantBound] in
            let e = minimizeC (fun c -> Smt.isMaxPlusConstantBound linCond t c minimal) Big_int.zero_big_int (Big_int.abs_big_int (Poly.getConstant t)) in
            create_size_data (MaxPlusConstant e, getVarNums lvarswithnums minimal)
          else if isLinear && isMaxPlusConstantBound linCond t maxC deps then
            let minimal = [!maxPlusConstantBound] in
            let e = minimizeC (fun c -> Smt.isMaxPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
            create_size_data (MaxPlusConstant e, getVarNums lvarswithnums minimal)
          else if isLinear && Smt.isMaxBound linCond t maxC deps then
            let minimal = minimize (Smt.isMaxBound linCond t maxC) deps [] in
            let e = minimizeC (fun c -> Smt.isMaxBound linCond t c minimal) Big_int.zero_big_int maxC in
            create_size_data (Max e, getVarNums lvarswithnums minimal)
          else if isLinear && Smt.isMaxPlusConstantBound linCond t maxC deps then
            let minimal = minimize (Smt.isMaxPlusConstantBound linCond t maxC) deps [] in
            let e = minimizeC (fun c -> Smt.isMaxPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
            create_size_data (MaxPlusConstant e, getVarNums lvarswithnums minimal)
          else if isLinear && (Smt.isSumPlusConstantBound linCond t maxC tvars) then
            let minimal = minimize (Smt.isSumPlusConstantBound linCond t maxC) tvars [] in
            let e = minimizeC (fun c -> Smt.isSumPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
            create_size_data (SumPlusConstant e, getVarNums lvarswithnums minimal)
          else if isLinear && (Smt.isSumPlusConstantBound linCond t maxC deps) then
            let minimal = minimize (Smt.isSumPlusConstantBound linCond t maxC) deps [] in
            let e = minimizeC (fun c -> Smt.isSumPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
            create_size_data (SumPlusConstant e, getVarNums lvarswithnums minimal)
          else if isLinear && (Smt.isScaledSumPlusConstantBound linCond t maxC maxS deps) then
            let minimal = minimize (Smt.isScaledSumPlusConstantBound linCond t maxC maxS) deps [] in
            let e = minimizeC (fun c -> Smt.isScaledSumPlusConstantBound linCond t c maxS minimal) Big_int.zero_big_int maxC in
            let s = minimizeC (fun s -> Smt.isScaledSumPlusConstantBound linCond t e s minimal) Big_int.zero_big_int maxS in
            create_size_data (ScaledSumPlusConstant (e, s), getVarNums lvarswithnums minimal)
          else if isRegular then
            (* use syntactic criteria *)
            if (Poly.isSumOfVarsPlusConstant t) then
              create_size_data (SumPlusConstant (Poly.getConstant t), getVarNums lvarswithnums tvars)
            else
              create_size_data (P (Expexp.fromPoly (Poly.abs t)), getVarNums lvarswithnums tvars)
          else if Poly.isVar t then
            (
              let (varPoly, negVarPoly) = (t, Poly.negate t) in
              let extractUpperBoundFor t acc p =
                match p with
                | Pc.Leq (a, b) when Poly.equal a t && Utils.containsAll (Poly.getVars b) lvars -> Some b
                | _ -> acc
              in

              (* abs(poly) if v >= 0 && v <= poly *)
              let checkLowerZeroArbitraryUpper =
                let isZeroLowerBound p =
                  match p with
                  | Pc.Geq (a, b) -> Poly.equal a varPoly && Poly.equal b Poly.zero
                  | _ -> false
                in
                if List.exists isZeroLowerBound linCond then
                  List.fold_left (extractUpperBoundFor varPoly) None fullCond
                else
                  None
              in

              (* abs(poly) if v < 0 && -v <= poly *)
              let checkUpperZeroArbitraryLower =
                let isZeroUpperBound p =
                  match p with
                  | Pc.Lss (a, b) -> Poly.equal a varPoly && Poly.equal b Poly.zero
                  | _ -> false
                in
                if List.exists isZeroUpperBound linCond then
                  List.fold_left (extractUpperBoundFor negVarPoly) None fullCond
                else
                  None
              in

              match (checkLowerZeroArbitraryUpper, checkUpperZeroArbitraryLower) with
              | (Some b, _)
              | (_, Some b) ->
                 create_size_data (P (Expexp.fromPoly (Poly.abs b)), getVarNums lvarswithnums (Poly.getVars b))
              | _ ->
                 unknown_size_data
            )
          else
            unknown_size_data
        )
  and isMaxBound cond t c a =
    match a with
    | [] -> false
    | v::rest ->
       if Smt.isMaxBound cond t c [v] then
         (
           maxBound := v;
           true
         )
       else
         isMaxBound cond t c rest
  and isMaxPlusConstantBound cond t c a =
    match a with
    | [] -> false
    | v::rest ->
       if Smt.isMaxPlusConstantBound cond t c [v] then
         (
           maxPlusConstantBound := v;
           true
         )
       else
         isMaxPlusConstantBound cond t c rest
  and minimize check a accu =
    match a with
    | [] -> accu
    | i::rest ->
       if (check (accu@rest)) then
         minimize check rest accu
       else
         minimize check rest (accu @ [i])
  and minimizeC check lower upper =
    if Big_int.ge_big_int lower upper then
      upper
    else
      let middle = Big_int.shift_right_big_int (Big_int.add_big_int lower upper) 1 in
      if (check middle) then
        minimizeC check lower middle
      else
        minimizeC check (Big_int.add_big_int middle Big_int.unit_big_int) upper

  and getVarNums lvars tvars =
    match tvars with
    | [] -> []
    | v::rest -> (
      let tmp = getVarNums lvars rest in
      try
        let x = List.assoc v lvars in
        x::tmp
      with
      | Not_found -> tmp
    )

  let equalLSC (i, c) (i', c') =
    (i = i') && (equal c c')

  let rec dumpLSCs ruleWithLSCs =
    String.concat "\n" (List.map dumpOneLSC ruleWithLSCs)
  and dumpOneLSC (rule, lsb) =
    (RuleT.toString rule) ^ " :: " ^ (dumpLSC lsb)
  and dumpLSC ({ rhsIdx = i; varIdx = j }, c_vars) =
    Printf.sprintf "%i-%i: %s" i j (toStringLocalComplexity c_vars)
  and dumpLSCDot (rule, lsb) =
    (RuleT.toDotString rule) ^ ":: " ^ (dumpLSC lsb)

  let computeLocalSizeComplexityForTerm rule v =
    let lvars = Term.getVars (RuleT.getLeft rule) in
    let lvarswithnums = Utils.mapi (fun i v -> (v, i)) lvars in
    let fullCond = RuleT.getCond rule in
    let linCond = Pc.dropNonLinearAtoms fullCond in
    computeLSCForTerm lvars lvarswithnums linCond fullCond v
  let localcomplexity2complexity b vars = toSmallestComplexity b vars

end

module type S =
  sig
    module RuleT : AbstractRule.AbstractRule

    type trans_data = RuleT.rule * local_size_data
    type tds = trans_data list

    (* This produces one
         (rule, ((rhsIdx, argumentIdx), (local size bound, active variable idxs)))
         tuple per RV *)
    val computeLocalSizeComplexities : RuleT.rule list -> tds

    val getConstantSummand : size_data -> Big_int.big_int
    val getScalarFactor : size_data -> Big_int.big_int
    val toSmallestComplexity : size_data -> Poly.var list -> Complexity.t
    val isConstant : size_data -> bool

    val equalLSC : local_size_data -> local_size_data -> bool
    val complexity2localcomplexity : Complexity.t -> Poly.var list -> size_data

    val listSum : size_data list -> Poly.var list -> size_data
    val listMax : size_data list -> Poly.var list -> size_data

    val toStringLocalComplexity : size_data -> string
    val dumpOneLSC : trans_data -> string
    val dumpLSCDot : trans_data -> string

    val computeLocalSizeComplexityForTerm : RuleT.rule -> Poly.poly -> size_data
    val localcomplexity2complexity : size_data -> Poly.var list -> Complexity.t
  end
