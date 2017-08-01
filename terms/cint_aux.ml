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


(* Parses a cint from a filename *)
let parse filename = readIn filename


