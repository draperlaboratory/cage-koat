(*
  Convenience function for parsing

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

(* Create a string for a program *)
let var_string out_var =
  match out_var with
    | None -> ""
    | Some v -> v ^ ": int"

let var_list_string vars =
  String.concat ", " (List.map (fun x -> x ^ ": int") vars)

let toStringVars = function
  | [] -> ""
  | vars_decl -> "var " ^ (var_list_string vars_decl) ^ ";\n"

let getLHSCall = function
  | None -> "()"
  | Some v -> v

let rec toStringBexpr = function
  | True -> "true"
  | False -> "false"
  | BRandom -> "brandom"
  | Atom cc -> Pc.toStringAtom cc
  | Not cc -> "not (" ^ (toStringBexpr cc) ^ ")"
  | Or (c1, c2) -> "(" ^ (toStringBexpr c1) ^ ") or (" ^ (toStringBexpr c2) ^ ")"
  | And (c1, c2) -> "(" ^ (toStringBexpr c1) ^ ") and (" ^ (toStringBexpr c2) ^ ")"

let rec toStringStmt indent = function
    | Skip -> (getindent indent) ^ "skip;"
    | Halt -> (getindent indent) ^ "halt;"
    | Assume c -> (getindent indent) ^ "assume (" ^ (toStringBexpr c) ^ ");"
    | Random x -> (getindent indent) ^ x ^ " = random;"
    | Assign (x, p) -> (getindent indent) ^ x ^ " = " ^ (Poly.toStringSimple p) ^ ";"
    | ITE (c, t, e) ->
      let istring = getindent indent
      and indent' = indent + 2 in
      istring ^ "if (" ^ (toStringBexpr c) ^ ") then\n" ^
        (toStringStmts indent' t) ^ "\n" ^ istring ^ "else\n" ^
        (toStringStmts indent' e) ^ "\n" ^ istring ^ "endif;"
    | While (c, b) -> (getindent indent) ^ "while (" ^ (toStringBexpr c) ^ ") do\n" ^
                      (toStringStmts (indent + 2) b) ^ "\n" ^ (getindent indent) ^ "done;"
    | Call (x, f, ys) -> (getindent indent) ^ (getLHSCall x) ^ " = " ^ f ^ "(" ^ (String.concat ", " ys) ^ ");"
    | Dummy1 _
    | Dummy2 _
    | Dummy3 _ -> failwith "Internal error in Simple_aux.toString"

and toStringStmts indent stmts =
  String.concat "\n" (List.map (toStringStmt indent) stmts)

let toStringDecl (f, in_vars, out_var, local_vars, stmts) =
  "proc " ^ f ^ "(" ^ (var_list_string in_vars) ^ ") returns (" ^
    (var_string out_var) ^ ")\n" ^ (toStringVars local_vars) ^
    "begin\n" ^ (toStringStmts 2 stmts) ^ "\nend\n\n"

let toStringDecls fun_decls =
  String.concat "\n" (List.map toStringDecl fun_decls)

(* General parse exception. *)
exception ParseException of int * int * string

(* Parses a program from an in_channel. *)
let rec getProgram chan =
  try
    Simple_lexer.pos := 1;
    Simple_lexer.line := 1;
    let lexbuf = Lexing.from_channel chan in
      check (Simple_parser.program Simple_lexer.token lexbuf)
  with
    | Parsing.Parse_error ->
        raise
          (
            ParseException
              (
                !Simple_lexer.line,
                !Simple_lexer.pos,
                Printf.sprintf "Error: Parse error in line %d at position %d." !Simple_lexer.line !Simple_lexer.pos
              )
          )
    | Simple_lexer.Unknown ->
        raise
          (
            ParseException
              (
                !Simple_lexer.line,
                !Simple_lexer.pos,
                Printf.sprintf "Error: Unknown token in line %d at position %d." !Simple_lexer.line !Simple_lexer.pos
              )
          )
and check (fun_decls, vars, stmts) =
  match check_fun_decls fun_decls with
    | Some errorstmt -> raise (ParseException (0, 0, getVarErrorString errorstmt))
    | None ->
      (
        match stmts with
          | [] -> raise (ParseException (0, 0, "A program cannot be empty!"))
          | _ ->
            (
               match check_vars vars stmts with
                 | None -> (fun_decls, vars, stmts)
                 | Some errorstmt -> raise (ParseException (0, 0, getVarErrorString errorstmt))
            )
      )
and check_fun_decls funds =
  match funds with
    | [] -> None
    | fund::more -> let tmp = check_fun_decl fund in
                      match tmp with
                        | None -> check_fun_decls more
                        | Some _ -> tmp
and check_fun_decl (f, in_vars, out_var_o, local_vars, stmts) =
  let allvars = (in_vars @ (getIt out_var_o) @ local_vars) in
    let tmp = check_multiple allvars in
      if tmp <> None then
        Some (Dummy3 f)
      else
        check_vars allvars stmts
and getVarErrorString errorstmt =
  match errorstmt with
    | Dummy1 c -> Printf.sprintf "Use of undeclared variable in '%s'!" (toStringBexpr c)
    | Dummy2 vars -> Printf.sprintf "Multiple variable declaration in '%s'!" ((String.concat ": int, " vars) ^ ": int")
    | Dummy3 f -> Printf.sprintf "Multiple variable declaration in function '%s'!" f
    | _ -> Printf.sprintf "Use of undeclared variable in '%s'!" (toStringStmt 0 errorstmt)
and check_vars vars stmts =
  let var_dec_check = check_multiple vars in
    if var_dec_check <> None then
      var_dec_check
    else
      match stmts with
        | [] -> None
        | s::stmts' -> let tmp = check_vars_one vars s in
                         match tmp with
                           | None -> check_vars vars stmts'
                           | Some _ -> tmp
and check_multiple vars =
  if (List.length (Utils.remdup vars)) <> (List.length vars) then
    Some (Dummy2 vars)
  else
    None
and check_vars_one vars stmt =
  match stmt with
    | Skip -> None
    | Halt -> None
    | Assume c -> if (Utils.containsAll vars (getVars c)) then None else Some (Dummy1 c)
    | Random x -> if (Utils.contains vars x) then None else Some stmt
    | Assign (x, p) -> if (Utils.containsAll vars (x::Poly.getVars p)) then None else Some stmt
    | ITE (c, t, e) -> if (Utils.containsAll vars (getVars c)) then
                                let tmp = check_vars vars t in
                                  match tmp with
                                    | None -> check_vars vars e
                                    | Some _ -> tmp
                              else
                                Some (Dummy1 c)
    | While (c, b) -> if (Utils.containsAll vars (getVars c)) then
                               check_vars vars b
                             else
                               Some (Dummy1 c)
    | Call (x, f, ys) -> if Utils.containsAll vars ((getIt x) @ ys) then
                                  None
                                else
                                  Some stmt
    | Dummy1 _
    | Dummy2 _
    | Dummy3 _ -> failwith "Internal error in Simple_aux.check_vars_one"
and getIt x =
  match x with
    | None -> []
    | Some v -> [v]
