/*
  Parser for Complexity Results

  @author Jordan Thayer
  @license This file is distributed under the MIT License (MIT)
           See LICENSE for details
*/

%token <string> IDENT INFIX VAR INT
%token COMMA OPENPAR CLOSEPAR EOL EOF TIMES POWER EXP

%left INFIX
%left TIMES
%left POWER
%nonassoc IDENT

%start poly
%type <Poly.poly> poly

%%
poly:
 | poly_help {$1, Big_int.zero_big_int }

poly_help:
| mult_monomial
    { [ $1 ] }
| mult_monomial INFIX poly_help
    { let push_sign sgn l =
        match l with
          | [] -> []
          | (c, s)::l' -> (Big_int.mult_big_int c sgn, s)::l'
      in
        $1 :: (push_sign (if $2 = "+" then Big_int.unit_big_int else (Big_int.minus_big_int Big_int.unit_big_int)) $3)
    }
| mult_monomial INFIX OPENPAR poly_help CLOSEPAR
    { let rec push_sign_all sgn l =
        match l with
          | [] -> []
          | (c, s)::l' -> (Big_int.mult_big_int c sgn, s)::(push_sign_all sgn l')
      in
        $1 :: (push_sign_all (if $2 = "+" then Big_int.unit_big_int else (Big_int.minus_big_int Big_int.unit_big_int)) $4)
    }
| mult_monomial INFIX OPENPAR poly_help CLOSEPAR INFIX poly_help
    { let rec push_sign sgn l =
        match l with
          | [] -> []
          | (c, s)::l' -> (Big_int.mult_big_int c sgn, s)::l'
      and push_sign_all sgn l =
        match l with
          | [] -> []
          | (c, s)::l' -> (Big_int.mult_big_int c sgn, s)::(push_sign sgn l')
      in
        [$1] @ (push_sign_all (if $2 = "+" then Big_int.unit_big_int else (Big_int.minus_big_int Big_int.unit_big_int)) $4) @ (push_sign (if $6 = "+" then Big_int.unit_big_int else (Big_int.minus_big_int Big_int.unit_big_int)) $7)
    }
;

mult_monomial:
| INT
    { (Big_int.big_int_of_string $1, [("$!@", 1)]) }
| INFIX INT
    { if $1 = "+" then
        (Big_int.big_int_of_string $2, [("$!@", 1)])
      else
        (Big_int.minus_big_int (Big_int.big_int_of_string $2), [("$!@", 1)]) }
| INFIX OPENPAR INT CLOSEPAR
    { if $1 = "+" then
        (Big_int.big_int_of_string $3, [("$!@", 1)])
      else
        (Big_int.minus_big_int (Big_int.big_int_of_string $3), [("$!@", 1)]) }
| monomial
    { (Big_int.unit_big_int, $1) }
| INFIX monomial
    { if $1 = "+" then
        (Big_int.unit_big_int, $2)
      else
        (Big_int.minus_big_int Big_int.unit_big_int, $2) }
| INT TIMES monomial
    { (Big_int.big_int_of_string $1, $3) }
| var_power TIMES INT
    { (Big_int.big_int_of_string $3, [$1]) }
| INFIX INT TIMES monomial
    { if $1 = "+" then
        (Big_int.big_int_of_string $2, $4)
      else
        (Big_int.minus_big_int (Big_int.big_int_of_string $2), $4) }
| INFIX OPENPAR INT CLOSEPAR TIMES monomial
    { if $1 = "+" then
        (Big_int.big_int_of_string $3, $6)
      else
        (Big_int.minus_big_int (Big_int.big_int_of_string $3), $6) }
;

monomial:
| var_power
    { [ $1 ] }
| var_power TIMES monomial
    { $1 :: $3 }
;

var_power:
| IDENT
    { ($1, 1) }
| IDENT POWER INT
    { ($1, int_of_string $3) }

;

varlist:
| IDENT
    { }
| IDENT varlist
    { }
;
