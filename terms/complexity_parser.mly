/*
  Parser for Complexity Results

  @author Jordan Thayer
  @license This file is distributed under the MIT License (MIT)
           See LICENSE for details
*/

%token <string> IDENT VAR INT
%token COMMA OPENPAR CLOSEPAR EOL EOF TIMES POWER EXP PLUS MINUS 

%left PLUS
%left MINUS
%left TIMES
%left POWER
%nonassoc IDENT

%start exp
%type <Expexp.expexp> exp

%%

exp:
 | EXP OPENPAR exp COMMA exp CLOSEPAR { Expexp.Exp ($3, $5) }
 | exp TIMES exp { Expexp.Mul ($1, $3) }
 | exp PLUS exp { Expexp.Sum ($1, $3) }
 | exp MINUS exp { Expexp.Sum ($1, $3) }
 | exp TIMES OPENPAR exp CLOSEPAR { Expexp.Mul ($1, $4) }
 | exp PLUS OPENPAR exp CLOSEPAR { Expexp.Sum ($1, $4) }
 | exp MINUS OPENPAR exp CLOSEPAR { Expexp.Sum ($1, $4) }
 | mult_monomial { Expexp.Pol ([$1], Big_int.zero_big_int) }
;

mult_monomial:
 | INT { Big_int.big_int_of_string $1, [("$!@", 1)] }
 | monomial { (Big_int.unit_big_int, $1) }
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
