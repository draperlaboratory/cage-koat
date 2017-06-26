
let readIn filename =
  let inchan = open_in filename in
  try
    Its_lexer.pos := 1;
    Its_lexer.line := 1;
    let lexbuf = Lexing.from_channel inchan in
    let res = Its_parser.cint_eol Its_lexer.token lexbuf in
    close_in inchan;
    res
  with
    | Parsing.Parse_error ->
        raise
          (
            Cint_aux.ParseException
              (
                !Its_lexer.line,
                !Its_lexer.pos,
                Printf.sprintf "Error: Parse error in line %d at position %d." !Its_lexer.line !Its_lexer.pos
              )
          )
    | Its_lexer.Unknown ->
        raise
          (
            Cint_aux.ParseException
              (
                !Its_lexer.line,
                !Its_lexer.pos,
                Printf.sprintf "Error: Unknown token in line %d at position %d." !Its_lexer.line !Its_lexer.pos
              )
          )


let getIts fileName =
    let (startFun, tmp) = readIn fileName in
    (* remove duplicate rules *)
    let noDups = Cint_aux.check (Cint_aux.remdup tmp) in
    (* fix functions arity *)
    let fixedArity = Comrule.fixArity noDups in
    let (startFun', tmp2) = Cint_aux.sanitize fixedArity startFun in
    (startFun', Cint_aux.internalize (Cint_aux.removeNeq tmp2))


(* Parses a cint from a filename *)
let parse filename =
  getIts filename

