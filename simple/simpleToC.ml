(* Convert program to C *)
open Simple

let needAssume = ref false
let needNondef = ref false
let needExit = ref false

let toStringAtomC = function
  | Pc.Equ (l, r) -> (Poly.toString l) ^ " == " ^ (Poly.toString r)
  | Pc.Neq (l, r) -> (Poly.toString l) ^ " != " ^ (Poly.toString r)
  | Pc.Geq (l, r) -> (Poly.toString l) ^ " >= " ^ (Poly.toString r)
  | Pc.Gtr (l, r) -> (Poly.toString l) ^ " > " ^ (Poly.toString r)
  | Pc.Leq (l, r) -> (Poly.toString l) ^ " <= " ^ (Poly.toString r)
  | Pc.Lss (l, r) -> (Poly.toString l) ^ " < " ^ (Poly.toString r)

let rec toCBexpr = function
  | True -> "1"
  | False -> "0"
  | BRandom -> needNondef := true; "nondef()"
  | Atom cc -> toStringAtomC cc
  | Not cc -> "!(" ^ (toCBexpr cc) ^ ")"
  | Or (c1, c2) -> "(" ^ (toCBexpr c1) ^ ") || (" ^ (toCBexpr c2) ^ ")"
  | And (c1, c2) -> "(" ^ (toCBexpr c1) ^ ") && (" ^ (toCBexpr c2) ^ ")"

let getCLHSCall = function
  | None -> ""
  | Some v -> v ^ " = "

let rec toCstmts stmts indent =
  String.concat "\n" (List.map (toCStmt indent) stmts)

and toCStmt indent = function
    | Skip -> (getindent indent) ^ ""
    | Halt -> needExit := true; (getindent indent) ^ "exit(0);"
    | Assume c -> needAssume := true; (getindent indent) ^ "assume (" ^ (toCBexpr c) ^ ");"
    | Random x -> needNondef := true; (getindent indent) ^ x ^ " = nondef();"
    | Assign (x, p) -> (getindent indent) ^ x ^ " = " ^ (Poly.toStringSimple p) ^ ";"
    | ITE (c, t, e) -> (getindent indent) ^ "if (" ^ (toCBexpr c) ^ ") {\n" ^
                       (toCstmts t (indent + 4)) ^ "\n" ^ (getindent indent) ^ "} else {\n" ^
                       (toCstmts e (indent + 4)) ^ "\n" ^ (getindent indent) ^ "}"
    | While (c, b) -> (getindent indent) ^ "while (" ^ (toCBexpr c) ^ ") {\n" ^
                      (toCstmts b (indent + 4)) ^ "\n" ^ (getindent indent) ^ "}"
    | Call (x, f, ys) -> (getindent indent) ^ (getCLHSCall x) ^ f ^ "(" ^ (String.concat ", " ys) ^ ");"
    | Dummy1 _
    | Dummy2 _
    | Dummy3 _ -> failwith "Internal error in Simple.toC"

let var_list_Cstring vars =
  String.concat ", int " vars

let toCvars vars =
  "int " ^ (var_list_Cstring vars)

let var_list_Cstring_args vars =
  if vars = [] then
    ""
  else
    toCvars vars

let addReturn = function
  | None -> ""
  | Some v -> "\n    return " ^ v ^ ";"

let out_var_string = function
  | None -> ""
  | Some v -> "    int " ^ v ^ ";\n"

let decl_to_Cstring (f, in_vars, out_var, local_vars, stmts) =
  (if out_var = None then "" else "int ") ^
  f ^
  "(" ^ (var_list_Cstring_args in_vars) ^ ")\n{\n" ^
  (out_var_string out_var) ^
  (if local_vars = [] then "" else ("    " ^ (toCvars local_vars) ^ ";\n")) ^
  (toCstmts stmts 4) ^
  (addReturn out_var) ^
  "\n}\n\n"

let decls_to_Cstring funs =
  String.concat "" (List.map decl_to_Cstring funs)

let toC (funs, vars, stmts) =
  needAssume := false;
  needNondef := false;
  needExit := false;
  let decls = decls_to_Cstring funs
  and body = "void test_fun(" ^ (toCvars vars) ^ ")\n{\n" ^ (toCstmts stmts 4) ^ "\n}" in
  (if !needExit then "#include <stdlib.h>\n\n" else "") ^
    (if !needAssume then "void assume(int);\n" else "") ^
    (if !needNondef then "int nondef(void);\n" else "") ^
    (if !needAssume || !needNondef then "\n" else "") ^
    decls ^ body
