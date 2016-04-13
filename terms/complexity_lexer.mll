(*
  Lexer for CINTs

  @author Jordan Thayer
*)

{
  open Complexity_parser
  exception Unknown

  let pos = ref 1
  let line = ref 1
}

rule token = parse
  | '#'[^'\n']*                      { EOL }
  | '/''/''/''*''*''*'[^'\n']*'*''*''*''/''/''/' { EOL }
  | [' ']                            { incr pos; token lexbuf }
  | ['\t']                           { pos := !pos + 8; token lexbuf }
  | ['\r' '\n']                      { pos := 1; incr line; EOL }
  | [',']                            { incr pos; COMMA }
  | 'p''o''w'                        { pos := !pos + 3; EXP }
  | ['a'-'z' 'A'-'Z' '_' '$']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'' '.']*
                                     { let s = Lexing.lexeme lexbuf in
                                         pos := !pos + (String.length s);
                                         IDENT s }
  | ['+' '-' ]                       { let s = Lexing.lexeme lexbuf in
                                         pos := !pos + (String.length s);
                                         INFIX s }
  | [ '*' ]                          { incr pos; TIMES }
  | [ '^' ]                          { incr pos; POWER }
  | '0'                              { incr pos; INT "0" }
  | ['1'-'9']['0'-'9']*              { let s = Lexing.lexeme lexbuf in
                                         pos := !pos + (String.length s);
                                         INT s }
  | '('                              { incr pos; OPENPAR }
  | ')'                              { incr pos; CLOSEPAR }
  | eof                              { EOF }
  | _                                { raise Unknown }
