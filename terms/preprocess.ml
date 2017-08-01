(*

KoAT

@author Cody Roux

*)

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
  (startFun, Utils.concatMap Comrule.removeNeq trs)

(* rewrites rhss so that they don't contain the same variables that exist on the lhs? *)
let internalize startFun cint =
  (startFun, List.map Comrule.internalize cint)

(* attaches primes to a variable name until it is unique *)
let rec attachPrimes cand used =
  if (Utils.contains used cand) then
    attachPrimes (cand ^ "'") used
  else
    cand

(* remove duplicate constraints from a rule*)
let remdupComrule r =
  Comrule.createWeightedRule
    (Comrule.getLeft r)
    (Comrule.getRights r)
    (Utils.remdupC Pc.equalAtom (Comrule.getCond r))
    (Comrule.getLowerBound r)
    (Comrule.getUpperBound r)

let remdup cint =
  List.map remdupComrule cint

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
  Comrule.createWeightedRule
    (applyFunMappingTerm mapping (Comrule.getLeft r))
    (List.map (applyFunMappingTerm mapping) (Comrule.getRights r))
    (Comrule.getCond r)
    (Comrule.getLowerBound r)
    (Comrule.getUpperBound r)


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
  let vars = Comrule.getVars r in
    let varmapping = createVarMapping vars vars in
      Comrule.createWeightedRule
        (Term.renameVars varmapping (Comrule.getLeft r))
        (List.map (fun rhs -> Term.renameVars varmapping rhs) (Comrule.getRights r))
        (Pc.renameVars varmapping (Comrule.getCond r))
        (Poly.renameVars varmapping (Comrule.getLowerBound r))
        (Poly.renameVars varmapping (Comrule.getUpperBound r))


let sanitize startFun cint =
  let (newtrs, newstart) = sanitizeFuns startFun cint in
    (newstart, List.map sanitizeRule newtrs)

(**************** Verifying Correctness *******************)

let checkLhsRule r =
  let args = Term.getArgs (Comrule.getLeft r) in
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
    (getAritiesOne (Comrule.getLeft r) f)
    @ (Utils.concatMap (fun r -> getAritiesOne r f) (Comrule.getRights r))
    @ (getArities f rr)

(* makes sure that every occurance of the function symbol f have the same arity *)
let checkArityFun cint f =
  let arities = Utils.remdup (getArities f cint) in
    if List.length arities > 1 then
      raise (PreProcessException ("All occurrences of function symbol " ^ f ^ " need to have the same arity!"))

(* makes sure that checkArityFun holds for every function in cint *)
let checkArity startFun cint =
  let funs = Utils.remdup (List.flatten (List.map (fun rule -> (Comrule.getFuns rule)) cint)) in
  List.iter (checkArityFun cint) funs;
  (startFun, cint)

(* raise a parse exception if the cint is empty *)
let checkEmpty startFun cint =
  match cint with
  | [] -> raise (PreProcessException "A CINT cannot be empty!")
  | _ -> (startFun, cint)

let removeDups startFun cint =
  (startFun, remdup cint)

let fixArity startFun cint =
  (startFun, Comrule.fixArity cint)

    
(** Pre run validation **)
let preprocessComrules arity lvars cint =
  let rec internal = function
  | [] -> cint
  | rule::rest ->
    let lhs = Comrule.getLeft rule in
    if ((Term.getArity lhs) <> arity) || (Term.getVars lhs <> lvars) then
      raise (PreProcessException "Error: Not all rules have the same variables!")
    else if List.exists (fun r -> (Term.getArity r <> arity)) (Comrule.getRights rule) then
      raise (PreProcessException "Error: Not all function symbols have the same arity!")
    else
      internal rest in
  internal cint
    
let mk_new_start startfun lvars fun_symbols =
  let start_symbol = getNewName "koat_start" fun_symbols in
  let start_rule = Comrule.createSimpleRule start_symbol startfun lvars in
  (start_symbol, start_rule)


let addNewStart startFun cint =
  let first = List.hd cint in
  let lvars = Term.getVars (Comrule.getLeft first) in
  let fun_symbols = Comrule.getFunsList cint in
  let (new_start, new_start_rule) = mk_new_start startFun lvars fun_symbols in
  (new_start, new_start_rule::cint)

let checkVarsAndArity startFun cint =
  let first = List.hd cint in
  let lvars = Term.getVars (Comrule.getLeft first) in
  let arity = Term.getArity (Comrule.getLeft first) in
  (startFun, preprocessComrules arity lvars cint)

let checkStartCondition startfun cint =
  let rhsFuns = Utils.concatMap Comrule.getRightFuns cint in
  if List.mem startfun rhsFuns then
    failwith ("internal error in Preprocess.checkStartCondition: start function " ^ startfun ^ " appears in an RHS!")
  else (startfun, cint)


let runPreProcessors pps = fun startfun cint ->
  List.fold_left (fun (s, cs) p -> p s cs) (startfun, cint) pps

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
    checkVarsAndArity;
    checkStartCondition
  ] in
  runPreProcessors pps startFun cint

