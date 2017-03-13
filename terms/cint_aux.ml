(*
  Convenience function of CINT parsing

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

(* General parse exception. *)
exception ParseException of int * int * string

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
(* takes every comrule with a <> constrain in the conditional, and produces
   a list of rules with disjoint constraints covering the original rule *)
let removeNeq trs =
  Utils.concatMap Comrule.removeNeq trs

(* rewrites rhss so that they don't contain the same variables that exist on the lhs? *)
let internalize cint =
  List.map Comrule.internalize cint

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

(* rename functions so that their arguments obey rules *)
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


let sanitizeFuns trs startFun =
  let funs = Cint.getFuns trs in
  let mapping = createFunMapping funs funs in
  let refreshed_trs = List.map (applyFunMapping mapping) trs in
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


let sanitize trs startFun =
  let (newtrs, newstart) = sanitizeFuns trs startFun in
    (newstart, List.map sanitizeRule newtrs)

(**************** Verifying Correctness *******************)

let check_lhs_rule r =
  let args = Term.getArgs (Comrule.getLeft r) in
    (List.for_all Poly.isVar args) && (check_distinct [] args)

let rec check_lhs = function
  | [] -> []
  | r::rr -> if check_lhs_rule r then
      r::(check_lhs rr)
    else
      raise (ParseException (0, 0, "Arguments on lhs need to be distinct variables!"))

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
let check_arity_fun cint f =
  let arities = Utils.remdup (getArities f cint) in
    if List.length arities > 1 then
      raise (ParseException (0, 0, "All occurrences of function symbol " ^ f ^ " need to have the same arity!"))

(* makes sure that check_arity_fun holds for every function in cint *)
let check_arity cint =
  let funs = Utils.remdup (List.flatten (List.map (fun rule -> (Comrule.getFuns rule)) cint)) in
  List.iter (check_arity_fun cint) funs

(* raise a parse exception if the cint is empty *)
let checkEmpty = function
  | [] -> raise (ParseException (0, 0, "A CINT cannot be empty!"))
  | _ -> ()


(* perform all important tests on the cint *)
let check cint =
  checkEmpty cint;
  check_arity cint;
  check_lhs cint

let readIn filename =
  let inchan = open_in filename in
  try
    Cint_lexer.pos := 1;
    Cint_lexer.line := 1;
    let lexbuf = Lexing.from_channel inchan in
    let res = Cint_parser.cint_eol Cint_lexer.token lexbuf in
    close_in inchan;
    res
  with
    | Parsing.Parse_error ->
        raise
          (
            ParseException
              (
                !Cint_lexer.line,
                !Cint_lexer.pos,
                Printf.sprintf "Error: Parse error in line %d at position %d." !Cint_lexer.line !Cint_lexer.pos
              )
          )
    | Cint_lexer.Unknown ->
        raise
          (
            ParseException
              (
                !Cint_lexer.line,
                !Cint_lexer.pos,
                Printf.sprintf "Error: Unknown token in line %d at position %d." !Cint_lexer.line !Cint_lexer.pos
              )
          )


let getCint fileName =
    let (startFun, tmp) = readIn fileName in
    (* remove duplicate rules *)
    let noDups = check (remdup tmp) in
    (* fix functions arity *)
    let fixedArity = Comrule.fixArity noDups in
    let (startFun', tmp2) = sanitize fixedArity startFun in
    (startFun', internalize (removeNeq tmp2))


(* Parses a cint from a filename *)
let parse filename =
  getCint filename

