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

type localcomplexity = Max of Big_int.big_int
                       | MaxPlusConstant of Big_int.big_int
                       | SumPlusConstant of Big_int.big_int
                       | ScaledSumPlusConstant of Big_int.big_int * Big_int.big_int
                       | P of Expexp.expexp
                       | Unknown

module Make (RuleT : AbstractRule) = struct

  module RuleT = RuleT


  type size_data = localcomplexity * int list

  type local_trans_data = RuleT.rule * ((int * int) * size_data)
  type ltds = local_trans_data list

  let getE c =
    match c with
      | (Max e, _) -> e
      | (MaxPlusConstant e, _) -> e
      | (SumPlusConstant e, _) -> e
      | (ScaledSumPlusConstant (e, _), _) -> e
      | _ -> failwith "Internal error in LocalSizeComplexity.getE"

  let getS c =
    match c with
      | (SumPlusConstant _ , _) -> Big_int.unit_big_int
      | (ScaledSumPlusConstant (_, s), _) -> s
      | _ -> failwith "Internal error in LocalSizeComplexity.getS"

  let getConstant c =
    match c with
      | (P p, v) -> Expexp.getConstant p
      | _ -> failwith "Internal error in LocalSizeComplexity.getConstant"

  let rec equal c d =
    c == d || equalInternal c d
  and equalInternal c d =
    match (c, d) with
      | ((Max e, v), (Max e', v')) -> Poly.eq_big_int e e' && equalVar v v'
      | ((MaxPlusConstant e, v), (MaxPlusConstant e', v')) -> Poly.eq_big_int e e' && equalVar v v'
      | ((SumPlusConstant e, v), (SumPlusConstant e', v')) -> Poly.eq_big_int e e' && equalVar v v'
      | ((P p, v), (P q, v')) -> (Expexp.equal p q) && equalVar v v'
      | ((Unknown, v), (Unknown, v')) -> equalVar v v'
      | _ -> false
  and equalVar v v' =
    (v == v') || ((List.length v = List.length v') && (List.for_all2 (fun a b -> (a == b) || (a = b)) v v'))

  let isConstant c =
    match c with
      | (Max _, _) -> false
      | (MaxPlusConstant _, _) -> false
      | (SumPlusConstant _, _) -> false
      | (ScaledSumPlusConstant _, _) -> false
      | (P p, v) -> (v = []) && (Expexp.isConst p)
      | (Unknown, _) -> false

  let rec complexity2localcomplexity c vars =
    match c with
      | Complexity.P p -> if (not (Expexp.isConst p)) && (Expexp.isSumOfVarsPlusConstant p) then
                       (SumPlusConstant (Expexp.getConstant p), getVarNums p vars)
                     else if (not (Expexp.isConst p)) && (Expexp.isScaledSumOfVarsPlusConstant p) then
                       (ScaledSumPlusConstant (Expexp.getConstant p, Expexp.getScaleFactor p), getVarNums p vars)
                     else
                       (P p, getVarNums p vars)
      | Complexity.Unknown -> (Unknown, [])
  and getVarNums p vars =
    let pv = Expexp.getVars p in
      getVarNumList pv vars 0
  and getVarNumList pv vars i =
    match vars with
      | [] -> []
      | v::rest -> if Utils.contains pv v then
                     i::(getVarNumList pv rest (i + 1))
                   else
                     getVarNumList pv rest (i + 1)

  let rec toSmallestComplexity c vars =
    match c with
      | (Max e, v) -> Complexity.P (Expexp.add (getSum v vars) (Expexp.fromConstant e))
      | (MaxPlusConstant e, v) -> Complexity.P (Expexp.add (getSum v vars) (Expexp.fromConstant e))
      | (SumPlusConstant e, v) -> Complexity.P (Expexp.add (getSum v vars) (Expexp.fromConstant e))
      | (ScaledSumPlusConstant (e, s), v) -> Complexity.P (Expexp.constmult (Expexp.add (getSum v vars) (Expexp.fromConstant e)) s)
      | (P p, _) -> Complexity.P p
      | (Unknown, _) -> Complexity.Unknown
  and getSum v vars =
    match v with
      | [] -> Expexp.zero
      | x::rest -> Expexp.add (Expexp.fromVar (List.nth vars x)) (getSum rest vars)

  let rec add c d vars =
    match c with
      | (Max e, v) -> addMax e v d vars
      | (MaxPlusConstant e, v) -> addMaxPlusConstant e v d vars
      | (SumPlusConstant e, v) -> addSumPlusConstant e v d vars
      | (ScaledSumPlusConstant (e, s), v) -> addScaledSumPlusConstant e s v d vars
      | (P p, v) -> addP p v d vars
      | (Unknown, v) -> (Unknown, [])
  and addMax e v d vars =
    match d with
      | (Max e', v') -> if disjoint v v' then
                          (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                        else
                          addAsSmallestComplexities (Max e, v) d vars
      | (MaxPlusConstant e', v') ->  if disjoint v v' then
                                       (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                                     else
                                       addAsSmallestComplexities (Max e, v) d vars
      | (SumPlusConstant e', v') ->  if disjoint v v' then
                                       (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                                     else
                                       addAsSmallestComplexities (Max e, v) d vars
      | (ScaledSumPlusConstant (e', s), v') -> (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
      | (P p', v') -> if isConstant d then
                        (Max (Big_int.add_big_int e (getConstant d)), v)
                      else
                        addAsSmallestComplexities (Max e, v) d vars
      | (Unknown, v') -> (Unknown, [])
  and addMaxPlusConstant e v d vars =
    match d with
      | (Max e', v') -> if disjoint v v' then
                          (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                        else
                          addAsSmallestComplexities (MaxPlusConstant e, v) d vars
      | (MaxPlusConstant e', v') ->  if disjoint v v' then
                                       (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                                     else
                                       addAsSmallestComplexities (MaxPlusConstant e, v) d vars
      | (SumPlusConstant e', v') ->  if disjoint v v' then
                                       (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                                     else
                                       addAsSmallestComplexities (MaxPlusConstant e, v) d vars
      | (ScaledSumPlusConstant (e', s), v') -> (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
      | (P p', v') -> if isConstant d then
                        (MaxPlusConstant (Big_int.add_big_int e (getConstant d)), v)
                      else
                        addAsSmallestComplexities (MaxPlusConstant e, v) d vars
      | (Unknown, v') -> (Unknown, [])
  and addSumPlusConstant e v d vars =
    match d with
      | (Max e', v') -> if disjoint v v' then
                          (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                        else
                          addAsSmallestComplexities (SumPlusConstant e, v) d vars
      | (MaxPlusConstant e', v') -> if disjoint v v' then
                                      (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                                    else
                                      addAsSmallestComplexities (SumPlusConstant e, v) d vars
      | (SumPlusConstant e', v') -> if disjoint v v' then
                                      (SumPlusConstant (Big_int.add_big_int e e'), unite v v')
                                    else
                                      addAsSmallestComplexities (SumPlusConstant e, v) d vars
      | (ScaledSumPlusConstant (e', s), v') -> (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
      | (P p', v') -> if isConstant d then
                        (SumPlusConstant (Big_int.add_big_int e (getConstant d)), v)
                      else
                        addAsSmallestComplexities (SumPlusConstant e, v) d vars
      | (Unknown, v') -> (Unknown, [])
  and addScaledSumPlusConstant e s v d vars =
    match d with
      | (Max e', v') -> (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
      | (MaxPlusConstant e', v') -> (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
      | (SumPlusConstant e', v') ->  (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s Big_int.unit_big_int), unite v v')
      | (ScaledSumPlusConstant (e', s'), v') -> (ScaledSumPlusConstant (Big_int.add_big_int e e', Big_int.add_big_int s s'), unite v v')
      | (P p', v') -> if isConstant d then
                        (ScaledSumPlusConstant (Big_int.add_big_int e (getConstant d), s), v)
                      else
                        addAsSmallestComplexities (SumPlusConstant e, v) d vars
      | (Unknown, v') -> (Unknown, [])
  and addP p v d vars =
    match d with
      | (P p', v') -> (P (Expexp.add p p'), unite v v')
      | _ -> add d (P p, v) vars
  and disjoint v v' =
    (Utils.intersect v v') = []
  and unite v v' =
    List.sort compare (Utils.remdup (v @ v'))
  and addAsSmallestComplexities c d vars =
    let cc = (Complexity.add (toSmallestComplexity c vars) (toSmallestComplexity d vars)) in
      complexity2localcomplexity cc vars

  let rec addList l vars =
    match l with
      | [] -> (P Expexp.zero, [])
      | [x] -> x
      | x::y::rest -> addList ((add x y vars)::rest) vars

  let rec getMax c d vars =
    match c with
      | (Max e, v) -> (
                        match d with
                          | (Max e', v') -> (Max (Big_int.max_big_int e e'), unite v v')
                          | (MaxPlusConstant e', v') -> (MaxPlusConstant (Big_int.max_big_int e e'), unite v v')
                          | (SumPlusConstant e', v') -> (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
                          | (ScaledSumPlusConstant (e', s), v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
                          | (P p', v') -> if isConstant d then
                                            (MaxPlusConstant (Big_int.add_big_int e (getConstant d)), v)
                                          else
                                            let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
                                              (P m, getVarNums m vars)
                          | (Unknown, v') -> (Unknown, [])
                      )
      | (MaxPlusConstant e, v) -> (
                                    match d with
                                      | (Max e', v') -> (MaxPlusConstant (Big_int.max_big_int e e'), unite v v')
                                      | (MaxPlusConstant e', v') -> (MaxPlusConstant (Big_int.max_big_int e e'), unite v v')
                                      | (SumPlusConstant e', v') -> (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
                                      | (ScaledSumPlusConstant (e', s), v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
                                      | (P p', v') -> if isConstant d then
                                                        (MaxPlusConstant (Big_int.add_big_int e (getConstant d)), v)
                                                      else
                                                        let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
                                                          (P m, getVarNums m vars)
                                      | (Unknown, v') -> (Unknown, [])
                                  )
      | (SumPlusConstant e, v) -> (
                                    match d with
                                      | (Max e', v') -> (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
                                      | (MaxPlusConstant e', v') -> (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
                                      | (SumPlusConstant e', v') -> (SumPlusConstant (Big_int.max_big_int e e'), unite v v')
                                      | (ScaledSumPlusConstant (e', s), v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
                                      | (P p', v') -> if isConstant d then
                                                        (SumPlusConstant (Big_int.add_big_int e (getConstant d)), v)
                                                      else
                                                        let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
                                                          (P m, getVarNums m vars)
                                      | (Unknown, v') -> (Unknown, [])
                                  )
      | (ScaledSumPlusConstant (e, s), v) -> (
                                    match d with
                                      | (Max e', v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
                                      | (MaxPlusConstant e', v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
                                      | (SumPlusConstant e', v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', s), unite v v')
                                      | (ScaledSumPlusConstant (e', s'), v') -> (ScaledSumPlusConstant (Big_int.max_big_int e e', Big_int.max_big_int s s'), unite v v')
                                      | (P p', v') -> if isConstant d then
                                                        (ScaledSumPlusConstant (Big_int.add_big_int e (getConstant d), s), v)
                                                      else
                                                        let m = Expexp.max (getExpexp (toSmallestComplexity c vars)) p' in
                                                          (P m, getVarNums m vars)
                                      | (Unknown, v') -> (Unknown, [])
                                  )
      | (P p, v) -> (
                      match d with
                        | (P p', v') -> let m = Expexp.max p p' in
                                          (P m, getVarNums m vars)
                        | _ -> getMax d c vars
                    )
      | (Unknown, v) -> (Unknown, [])
  and getExpexp c =
    match c with
      | Complexity.P p -> p
      | _ -> failwith "Internal error in LocalSizeComplexity.getExpexp"
  and listMax list vars =
    match list with
      | [] -> (P Expexp.zero, [])
      | [x] -> x
      | x::y::rest -> listMax ((getMax x y vars)::rest) vars

  let rec toStringLocalComplexity c =
    match c with
      | (Max e, v) -> "=(" ^ (Big_int.string_of_big_int e) ^ ") " ^ (varString v)
      | (MaxPlusConstant e, v) -> "+.(" ^ (Big_int.string_of_big_int e) ^ ") " ^ (varString v)
      | (SumPlusConstant e, v) -> "+_(" ^ (Big_int.string_of_big_int e) ^ ") " ^ (varString v)
      | (ScaledSumPlusConstant (e, s), v) -> "*.(" ^ (Big_int.string_of_big_int e) ^ ", " ^ (Big_int.string_of_big_int s) ^ ") " ^ (varString v)
      | (P p, v) -> Expexp.toString p
      | (Unknown, v) -> "?"
  and varString v =
    "[" ^ (String.concat ";" (List.map string_of_int v)) ^ "]"

  (* compute local size complexities *)
  let maxBound = ref ""
  let maxPlusConstantBound = ref ""
  let maxC = Big_int.big_int_of_int 1024
  let maxS = Big_int.big_int_of_int 1024
  let eConstant = ref Big_int.zero_big_int
  let sConstant = ref Big_int.zero_big_int

  let rec computeLocalSizeComplexities trs =
    Utils.concatMap computeLocalSizeComplexitiesForRule trs
  and computeLocalSizeComplexitiesForRule rule =
    let fullCond = RuleT.getCond rule in
    let linCond = Pc.dropNonLinearAtoms fullCond in
    let lvars = Term.getVars (RuleT.getLeft rule) in
    let lvarswithnums = Utils.mapi (fun i v -> (v, i)) lvars in
    (* This produces one (rule, ((rhsIdx, argumentIdx), (local size bound, active variable idxs))) tuple per RV *)
    Utils.concatMap
      (fun (rhs, rhsIdx) ->
        Utils.mapi
          (fun argIdx argument -> (rule, ((rhsIdx, argIdx), computeLSCForTerm lvars lvarswithnums linCond fullCond argument)))
          (Term.getArgs rhs))
      (Utils.mapi (fun idx rhs -> (rhs, idx)) (RuleT.getRights rule))
  and computeLSCForTerm lvars lvarswithnums linCond fullCond t =
    let isLinear = Poly.isLinear t in
      if isLinear && (Smt.isConstantBound linCond t maxC) then
        (
          if Poly.isConst t then
            (P (Expexp.fromConstant (Big_int.abs_big_int (Poly.getConstant t))), [])
          else
            let e = minimizeC (Smt.isConstantBound linCond t) Big_int.zero_big_int maxC in
            (P (Expexp.fromConstant e), [])
        )
      else
        let alltvars = Poly.getVars t in
          let tvars = Utils.intersect lvars alltvars
          and isRegular = Utils.containsAll lvars alltvars in
            if isLinear && isRegular && (List.length tvars = 1) && (Poly.isSumOfVarsPlusConstant t) && (Poly.eq_big_int Big_int.zero_big_int (Poly.getConstant t)) then
              (Max Big_int.zero_big_int, getVarNums lvarswithnums tvars)
            else
            (
              maxBound := "";
              maxPlusConstantBound := "";
              let deps = Utils.intersect lvars (getDeps tvars linCond) in
                if isLinear && isMaxBound linCond t maxC deps then
                  let minimal = [!maxBound] in
                    let e = minimizeC (fun c -> Smt.isMaxBound linCond t c minimal) Big_int.zero_big_int maxC in
                      (Max e, getVarNums lvarswithnums minimal)
                else if isLinear && isRegular && (List.length tvars = 1) && (Poly.isSumOfVarsPlusConstant t) && isMaxPlusConstantBound linCond t (Big_int.abs_big_int (Poly.getConstant t)) deps then
                  (* t = v_1 + ... + v_l + k, for v a lhs variable and k some constant. Start optimizing constant from k, not from maxC.
                     This converges far faster for cases like "x + 1" or "x - 1"... *)
                  let minimal = [!maxPlusConstantBound] in
                    let e = minimizeC (fun c -> Smt.isMaxPlusConstantBound linCond t c minimal) Big_int.zero_big_int (Big_int.abs_big_int (Poly.getConstant t)) in
                      (MaxPlusConstant e, getVarNums lvarswithnums minimal)
                else if isLinear && isMaxPlusConstantBound linCond t maxC deps then
                  let minimal = [!maxPlusConstantBound] in
                    let e = minimizeC (fun c -> Smt.isMaxPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
                      (MaxPlusConstant e, getVarNums lvarswithnums minimal)
                else if isLinear && Smt.isMaxBound linCond t maxC deps then
                  let minimal = minimize (Smt.isMaxBound linCond t maxC) deps [] in
                    let e = minimizeC (fun c -> Smt.isMaxBound linCond t c minimal) Big_int.zero_big_int maxC in
                      (Max e, getVarNums lvarswithnums minimal)
                else if isLinear && Smt.isMaxPlusConstantBound linCond t maxC deps then
                  let minimal = minimize (Smt.isMaxPlusConstantBound linCond t maxC) deps [] in
                    let e = minimizeC (fun c -> Smt.isMaxPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
                      (MaxPlusConstant e, getVarNums lvarswithnums minimal)
                else if isLinear && (Smt.isSumPlusConstantBound linCond t maxC tvars) then
                  let minimal = minimize (Smt.isSumPlusConstantBound linCond t maxC) tvars [] in
                    let e = minimizeC (fun c -> Smt.isSumPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
                      (SumPlusConstant e, getVarNums lvarswithnums minimal)
                else if isLinear && (Smt.isSumPlusConstantBound linCond t maxC deps) then
                  let minimal = minimize (Smt.isSumPlusConstantBound linCond t maxC) deps [] in
                    let e = minimizeC (fun c -> Smt.isSumPlusConstantBound linCond t c minimal) Big_int.zero_big_int maxC in
                      (SumPlusConstant e, getVarNums lvarswithnums minimal)
                else if isLinear && (Smt.isScaledSumPlusConstantBound linCond t maxC maxS deps) then
                  let minimal = minimize (Smt.isScaledSumPlusConstantBound linCond t maxC maxS) deps [] in
                    let e = minimizeC (fun c -> Smt.isScaledSumPlusConstantBound linCond t c maxS minimal) Big_int.zero_big_int maxC in
                      let s = minimizeC (fun s -> Smt.isScaledSumPlusConstantBound linCond t e s minimal) Big_int.zero_big_int maxS in
                        (ScaledSumPlusConstant (e, s), getVarNums lvarswithnums minimal)
                else if isRegular then
                  (* use syntactic criteria *)
                  if (Poly.isSumOfVarsPlusConstant t) then
                    (SumPlusConstant (Poly.getConstant t), getVarNums lvarswithnums tvars)
                  else
                    (P (Expexp.fromPoly (Poly.abs t)), getVarNums lvarswithnums tvars)
                else if (Poly.isVar t) && (let (varN, suffL) = (Poly.toString t, String.length "_sep") in String.length varN > suffL && (String.sub varN ((String.length varN) - suffL) suffL) = "_sep") then
                  (* This is a variable on an edge obtained from separating out a part of the program.
                     We generate conditions of the form v_sep >= 0 && v_sep <= boundTerm (or v_sep < 0 && -v_sep <= boundTerm).
                     Try extracting those. *)
                  (
                    let (varPoly, negVarPoly) = (t, Poly.negate t) in
                    let extractUpperBoundFor t acc p =
                      match p with
                      | Pc.Leq (a, b) when Poly.equal a t -> Some b
                      | _ -> acc
                    in

                    (* Case v_sep >= 0 *)
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

                    (* Case v_sep < 0 *)
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
                      (P (Expexp.fromPoly (Poly.abs b)), getVarNums lvarswithnums (Poly.getVars b))
                    | _ ->
                      (Unknown, [])
                  )
                else
                  (Unknown, [])
            )
  and isMaxBound cond t c a =
    match a with
      | [] -> false
      | v::rest -> if Smt.isMaxBound cond t c [v] then
                   (
                     maxBound := v;
                     true
                   )
                   else
                     isMaxBound cond t c rest
  and isMaxPlusConstantBound cond t c a =
    match a with
      | [] -> false
      | v::rest -> if Smt.isMaxPlusConstantBound cond t c [v] then
                   (
                     maxPlusConstantBound := v;
                     true
                   )
                   else
                     isMaxPlusConstantBound cond t c rest
  and minimize check a accu =
    match a with
      | [] -> accu
      | i::rest -> if (check (accu@rest)) then
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
  and getDeps tvars cond =
    getDepsFix tvars [] cond
  and getDepsFix x y cond =
    if x = y then
      x
    else
      getDepsFix (addOneDepLevel x cond []) x cond
  and addOneDepLevel x cond news =
    match cond with
      | [] -> x @ (Utils.remdup news)
      | c::rest -> let condVars = Pc.getVars cond in
                     if Utils.containsOne x condVars then
                       addOneDepLevel x rest (news @ (getNew x condVars))
                     else
                       addOneDepLevel x rest news
  and getNew x newvars =
    List.filter (fun v -> not (Utils.contains x v)) newvars
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
  and dumpLSC ((i, j), c_vars) =
    Printf.sprintf "%i-%i: %s" i j (toStringLocalComplexity c_vars)
  and dumpLSCDot (rule, lsb) =
    (RuleT.toDotString rule) ^ ":: " ^ (dumpLSC lsb)
end

module type S =
    sig

      module RuleT : AbstractRule.AbstractRule

      type size_data = localcomplexity * int list

      type local_trans_data = RuleT.rule * ((int * int) * size_data)
      type ltds = local_trans_data list
        
      val getE : localcomplexity * 'a -> Big_int.big_int
      val getS : localcomplexity * 'a -> Big_int.big_int
      val getConstant : localcomplexity * 'a -> Big_int.big_int
      val equal :
        localcomplexity * 'a list -> localcomplexity * 'a list -> bool
      val equalInternal :
        localcomplexity * 'a list -> localcomplexity * 'a list -> bool
      val equalVar : 'a list -> 'a list -> bool
      val isConstant : localcomplexity * 'a list -> bool
      val complexity2localcomplexity :
        Complexity.t -> Poly.var list -> size_data
      val getVarNumList : Poly.var list -> Poly.var list -> int -> int list
      val toSmallestComplexity :
        size_data -> Poly.var list -> Complexity.t
      val getSum : int list -> Poly.var list -> Expexp.expexp
      val add : size_data -> size_data ->  Poly.var list -> size_data
      val addMax : Big_int.big_int -> int list -> size_data -> Poly.var list -> size_data
      val addMaxPlusConstant : Big_int.big_int -> int list -> size_data -> Poly.var list -> size_data
      val addSumPlusConstant : Big_int.big_int -> int list -> size_data -> Poly.var list -> size_data
      val addScaledSumPlusConstant : Big_int.big_int -> Big_int.big_int -> int list -> size_data ->
        Poly.var list -> size_data
      val addP : Expexp.expexp -> int list -> size_data -> Poly.var list -> size_data
      val disjoint : int list -> int list -> bool
      val unite : int list -> int list -> int list
      val addAsSmallestComplexities : size_data -> size_data -> Poly.var list -> size_data
      val addList : size_data list -> Poly.var list -> size_data
      val getMax : size_data -> size_data -> Poly.var list -> size_data
      val getExpexp : Complexity.t -> Expexp.expexp
      val listMax : size_data list -> Poly.var list -> size_data
      val toStringLocalComplexity : size_data -> string
      val varString : int list -> string
      val maxBound : Poly.var ref
      val maxPlusConstantBound : Poly.var ref
      val maxC : Big_int.big_int
      val maxS : Big_int.big_int
      val eConstant : Big_int.big_int ref
      val sConstant : Big_int.big_int ref
      (* This produces one 
         (rule, ((rhsIdx, argumentIdx), (local size bound, active variable idxs)))
         tuple per RV *)
      val computeLocalSizeComplexities : RuleT.rule list -> ltds

      val computeLocalSizeComplexitiesForRule : RuleT.rule -> ltds

      val computeLSCForTerm :
        Poly.var list ->
        (Poly.var * int) list ->
        Pc.atom list -> Pc.cond -> Poly.poly -> size_data
      val isMaxBound : Pc.atom list -> Poly.poly -> Big_int.big_int -> Poly.var list -> bool
      val isMaxPlusConstantBound : Pc.atom list -> Poly.poly -> Big_int.big_int -> Poly.var list
        -> bool
      val minimize :
        (Poly.var list -> bool) ->
        Poly.var list -> Poly.var list -> Poly.var list
      val minimizeC :
        (Big_int.big_int -> bool) ->
        Big_int.big_int -> Big_int.big_int -> Big_int.big_int
      val getDeps : Poly.var list -> Pc.atom list -> Poly.var list
      val getDepsFix :
        Poly.var list -> Poly.var list -> Pc.atom list -> Poly.var list
      val addOneDepLevel :
        Poly.var list -> Pc.atom list -> Poly.var list -> Poly.var list
      val getNew : Poly.var list -> Poly.var list -> Poly.var list
      val getVarNums : (Poly.var * int) list -> Poly.var list -> int list
      val equalLSC :
        'a * (localcomplexity * 'b list) ->
        'a * (localcomplexity * 'b list) -> bool
      val dumpLSCs : ltds -> string
      val dumpOneLSC : local_trans_data -> string
      val dumpLSC : (int * int) * (size_data) -> string
      val dumpLSCDot : local_trans_data -> string
    end
