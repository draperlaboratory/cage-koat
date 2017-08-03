(*
  Terms

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

type funSym = string

type pos = int

type term = { fn : funSym; args : Poly.poly list }

(* Create term from what the parser gives *)
let create f arglist =
  let args = List.map Poly.construct_poly arglist in
   { fn = f; args = args }

let create' (f, args) = { fn = f; args = args }

let compare { fn = f1; args = as1 } { fn = f2; args = as2 } =
  let fComp = String.compare f1 f2 in
  if fComp <> 0 then
    fComp
  else
    let aNum1 = List.length as1 in
    let aNum2 = List.length as2 in
    if aNum1 < aNum2 then
      -1
    else if aNum1 > aNum2 then
      1
    else
      List.fold_left2
        (fun acc a1 a2 -> if acc <> 0 then acc else Poly.compare a1 a2) 0 as1 as2

(* Create a string for a term *)
let toString { fn = f; args = args } =
  Printf.sprintf "%s(%s)" f (String.concat ", " (List.map Poly.toString args))

(* Get the argument integer terms *)
let getArgs t =
  t.args

(* Get the function symbol *)
let getFun t =
  t.fn

(* Returns the arity of a term *)
let getArity t =
  List.length t.args

(* Return the variables of a term *)
let getVars t =
  Utils.remdup (Utils.concatMapStable Poly.getVars t.args)

(* Instantiate a term *)
let instantiate { fn = f; args = args } bindings =
  create' (f, List.map (fun poly -> Poly.instantiate poly bindings) args)

(* Renames the variables in a term *)
let renameVars varmapping { fn = f; args = args } =
  { fn = f; args = List.map (Poly.renameVars varmapping) args }

(* Determines whether a term is linear *)
let isLinear t =
  List.for_all Poly.isLinear t.args

let rec equal t1 t2 =
  t1 == t2 || equalInternal t1 t2
and equalInternal { fn = f1; args = args1 } { fn = f2; args = args2 } =
  f1 = f2 && (List.for_all2 Poly.equal args1 args2)
  
let makeFreshVar =
  let tic = ref 0 in
  function () ->
    if !tic = Pervasives.max_int then
      failwith "makeFreshVar: max exceeded maximum possible value!"
    else
      let candidateVar = Poly.mkVar (Printf.sprintf "Fresh_%i" !tic) in
      tic := !tic + 1;
      candidateVar

let makeFreshVarMap vs =
  let freshVarList = List.map (fun _ -> Poly.fromVar (makeFreshVar ())) vs in
  List.combine vs freshVarList
