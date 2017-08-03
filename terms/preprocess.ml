(*

KoAT

@author Cody Roux

*)

module CR = Comrule

(* General parse exception. *)
exception PreProcessException of string

(********* Helpers, filters, and such **********)

(* is the character disallowed *)
let isBad v =
  (v.[0] = '$') || (v.[0] = '_') || (v.[0] >= 'A' && v.[0] <= 'Z')


let rec check_distinct seen = function
  | [] -> true
  | a::l -> let varname = List.hd (Poly.getVars a) in
            if (Utils.contains seen varname) then
              false
            else
              check_distinct (varname::seen) l

(* takes every comrule with a <> constraint in the conditional, and produces
   a list of rules with disjoint constraints covering the original rule *)
let removeNeq startFun trs =
  (startFun, Utils.concatMap CR.removeNeq trs)

(* rewrites rhss so that they don't contain the same variables that exist on the lhs? *)
let internalize startFun cint =
  (startFun, List.map CR.internalize cint)

(* attaches primes to a variable name until it is unique *)
let rec attachPrimes cand used =
  if (Utils.contains used cand) then
    attachPrimes (cand ^ "'") used
  else
    cand

(* remove duplicate constraints from a rule*)
let remdupCR r =
  CR.createWeightedRule
    (CR.getLeft r)
    (CR.getRights r)
    (Utils.remdupC Pc.equalAtom (CR.getCond r))
    (CR.getLowerBound r)
    (CR.getUpperBound r)

let remdup cint =
  List.map remdupCR cint

(* remove bad characters from a variable *)
let getCand v =
  let rest = (String.sub v 1 ((String.length v) - 1)) in
    if v.[0] = '$' then
      "dollar" ^ rest
    else if v.[0] = '_' then
      "underscore" ^ rest
    else
      String.uncapitalize v

(* remove illegal chars and attach primes until v is unique in used *)
let getNewName v used =
  let cand = getCand v in
    attachPrimes cand used

(* rename functions so that their names obey some syntax rules *)
let rec createFunMapping used = function
  | [] -> []
  | f::ff -> if isBad f then
      let fnew = getNewName f used in
      (f, fnew)::(createFunMapping (fnew::used) ff)
    else
      (f, f)::(createFunMapping used ff)

(* apply a mappting to a term *)
let applyFunMappingTerm mapping { Term.fn = f; Term.args = args } =
  { Term.fn = List.assoc f mapping; Term.args = args }

(* apply a mapping to a comrule *)
let applyFunMapping mapping r =
  CR.createWeightedRule
    (applyFunMappingTerm mapping (CR.getLeft r))
    (List.map (applyFunMappingTerm mapping) (CR.getRights r))
    (CR.getCond r)
    (CR.getLowerBound r)
    (CR.getUpperBound r)


(* Replace the function names in the rules with sanitized names *)
let sanitizeFuns startFun cint =
  let funs = Cint.getFuns cint in
  let mapping = createFunMapping funs funs in
  let refreshed_trs = List.map (applyFunMapping mapping) cint in
  try
    let newStart = List.assoc startFun mapping in
    (refreshed_trs, newStart)
  with
  | Not_found -> failwith ("Start symbol '" ^ startFun ^ "' not found in a rule LHS!")


let rec createVarMapping vars used =
  match vars with
    | [] -> []
    | v::vv -> if isBad v then
                 let vnew = getNewName v used in
                   (v, vnew)::(createVarMapping vv (vnew::used))
               else
                 createVarMapping vv used

let sanitizeRule r =
  let vars = CR.getVars r in
    let varmapping = createVarMapping vars vars in
      CR.createWeightedRule
        (Term.renameVars varmapping (CR.getLeft r))
        (List.map (fun rhs -> Term.renameVars varmapping rhs) (CR.getRights r))
        (Pc.renameVars varmapping (CR.getCond r))
        (Poly.renameVars varmapping (CR.getLowerBound r))
        (Poly.renameVars varmapping (CR.getUpperBound r))


let sanitize startFun cint =
  let (newtrs, newstart) = sanitizeFuns startFun cint in
    (newstart, List.map sanitizeRule newtrs)

(**************** Verifying Correctness *******************)

let checkLhsRule r =
  let args = Term.getArgs (CR.getLeft r) in
  if (List.for_all Poly.isVar args) && (check_distinct [] args) then
    ()
  else
    raise (PreProcessException "Arguments on lhs need to be distinct variables!")

let rec checkLhs startFun cint =
  List.iter checkLhsRule cint;
  (startFun, cint)

(* returns the arity of a term so long as the function names agree *)
let getAritiesOne { Term.fn = f; Term.args = args } g =
  if f = g then
    [ List.length args ]
  else
    []

(* produces a list of all arities that the function symbol f appears with in a cint *)
let rec getArities f = function
  | [] -> []
  | r::rr ->
    (getAritiesOne (CR.getLeft r) f)
    @ (Utils.concatMap (fun r -> getAritiesOne r f) (CR.getRights r))
    @ (getArities f rr)

(* makes sure that every occurance of the function symbol f have the same arity *)
let checkArityFun cint f =
  let arities = Utils.remdup (getArities f cint) in
    if List.length arities > 1 then
      raise (PreProcessException ("All occurrences of function symbol " ^ f ^ " need to have the same arity!"))

(* makes sure that checkArityFun holds for every function in cint *)
let checkArity startFun cint =
  let funs = Utils.remdup (List.flatten (List.map (fun rule -> (CR.getFuns rule)) cint)) in
  List.iter (checkArityFun cint) funs;
  (startFun, cint)

(* raise a parse exception if the cint is empty *)
let checkEmpty startFun cint =
  match cint with
  | [] -> raise (PreProcessException "A CINT cannot be empty!")
  | _ -> (startFun, cint)

let removeDups startFun cint =
  (startFun, remdup cint)


let crArity cr =
  let lhsAr = Term.getArity (CR.getLeft cr) in
  List.fold_left (fun maxArity rhs ->
    let thisArity = Term.getArity rhs in
    if thisArity > maxArity
    then thisArity
    else if thisArity = maxArity
    then maxArity
    else maxArity) lhsAr (CR.getRights cr)

let maximumArity cint =
  let init,rst = match cint with
      [] -> 0, []
    | hd::tl -> crArity hd, tl in
  let folder maxArity cr =
    let crArity = crArity cr in
    if crArity > maxArity
    then crArity
    else if crArity = maxArity
    then maxArity
    else maxArity in
  List.fold_left folder init rst


let rec buildNewArgs = function
  | x when x < 0 -> []
  | i -> Poly.mkVar (Printf.sprintf "Ar_%i" i) :: (buildNewArgs (i - 1))

let pad maxArity term =
  let pel = Poly.fromVar "ArityPad" in
  let rec makePad = function
    | 0 -> []
    | x -> pel::(makePad (x - 1)) in
  let toAdd = maxArity - (Term.getArity term) in
  let padding = makePad toAdd in
  { Term.fn = term.Term.fn;
    Term.args = term.Term.args @ padding; }

let buildMapping (newArgs : Poly.var list) (cr : CR.rule) =
  let maxArity = List.length newArgs in
  let lhs = CR.getLeft cr in
  let toMap = Utils.take (Term.getArity lhs) newArgs in
  let sigma = List.map2 (fun var poly -> Poly.toVar poly, Poly.fromVar var)
    toMap lhs.Term.args in
  let cond = Pc.instantiate (CR.getCond cr) sigma in
  let lhs = { lhs with Term.args = List.map Poly.fromVar newArgs } in
  let rhss = List.map
    (fun rh ->
      let rh' = Term.instantiate rh sigma in
      let rh'' = pad maxArity rh' in
      rh'')
    (CR.getRights cr) in
  let lowerBound = Poly.instantiate (CR.getLowerBound cr) sigma in
  let upperBound = Poly.instantiate (CR.getUpperBound cr) sigma in
  CR.createWeightedRule lhs rhss cond lowerBound upperBound
     
(* Fix the arities of all symbols to a common constant *)
let fixArity startFun cint =
   let fixArityCint cint =
     let maxArity = maximumArity cint in
     let newArgs = List.rev (buildNewArgs (maxArity - 1)) in
     let transform = buildMapping newArgs in
     List.map transform cint
   in
  (startFun, fixArityCint cint)


let checkVarsAndArityCint arity lvars cint =
  let rec internal = function
  | [] -> cint
  | rule::rest ->
    let lhs = CR.getLeft rule in
    if ((Term.getArity lhs) <> arity) || (Term.getVars lhs <> lvars) then
      raise (PreProcessException "Error: Not all rules have the same variables!")
    else if List.exists (fun r -> (Term.getArity r <> arity)) (CR.getRights rule) then
      raise (PreProcessException "Error: Not all function symbols have the same arity!")
    else
      internal rest in
  internal cint
    
let mk_new_start startFun lvars fun_symbols =
  let start_symbol = getNewName "koat_start" fun_symbols in
  let start_rule = CR.createSimpleRule start_symbol startFun lvars in
  (start_symbol, start_rule)


let addNewStart startFun cint =
  let first = List.hd cint in
  let lvars = Term.getVars (CR.getLeft first) in
  let fun_symbols = CR.getFunsList cint in
  let (new_start, new_start_rule) = mk_new_start startFun lvars fun_symbols in
  (new_start, new_start_rule::cint)

let checkVarsAndArity startFun cint =
  let first = List.hd cint in
  let lvars = Term.getVars (CR.getLeft first) in
  let arity = Term.getArity (CR.getLeft first) in
  (startFun, checkVarsAndArityCint arity lvars cint)

let checkStartCondition startFun cint =
  let rhsFuns = Utils.concatMap CR.getRightFuns cint in
  if List.mem startFun rhsFuns then
    failwith
      (Printf.sprintf
         "internal error in Preprocess.checkStartCondition: start function %s appears in an RHS!"
         startFun)
  else (startFun, cint)


let rec freshenRHSVarsCint cint =
  match cint with
  | [] -> []
  | r::rs ->
     let fv = CR.getFreeVars r in
     let subst = Term.makeFreshVarMap fv in
     (CR.instantiate r subst) :: freshenRHSVarsCint rs

let freshenRHSVars startFun cint = (startFun, freshenRHSVarsCint cint)

    
let runPreProcessors pps = fun startFun cint ->
  List.fold_left (fun (s, cs) p -> p s cs) (startFun, cint) pps

(** Perform all transforms/checks on the input *)
let preprocess startFun cint =
  let pps = [
    checkEmpty;
    checkArity;
    checkLhs;
    removeDups;
    fixArity;
    sanitize;
    removeNeq;
    internalize;
    addNewStart;
    freshenRHSVars;
    checkVarsAndArity;
    checkStartCondition
  ] in
  runPreProcessors pps startFun cint

