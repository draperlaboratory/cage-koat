/*
  Parser for ITSs.
  Like 'cint_parser' but uses a simplified and generalised poly-expression parser.

  @license This file is distributed under the MIT License (MIT)
           See LICENSE for details
*/

%token <string> IDENT VAR INT
%token TO OPENWTO CLOSEWTO COMMA OPENPAR CLOSEPAR OPENSQ CLOSESQ CONSTRAINTSEP EOL EQU NEQ EOF GEQ GTR LEQ LSS CONJ
PLUS MINUS TIMES POWER GOAL COMPLEXITY STARTTERM FUNCTIONSYMBOLS VART RULES COM TRUE

%left PLUS MINUS
%left TIMES
%left POWER
%nonassoc IDENT
%nonassoc UMINUS

%start cint_eol poly
%type <string * Cint.cint> cint_eol
%type <Poly.poly> poly

%%

cint_eol :
| header OPENPAR RULES eols rules
    { ($1, $5) }
| eols header OPENPAR RULES eols rules
    { ($2, $6) }
;

header :
| OPENPAR GOAL COMPLEXITY CLOSEPAR eols OPENPAR STARTTERM OPENPAR FUNCTIONSYMBOLS IDENT CLOSEPAR CLOSEPAR eols OPENPAR VART varlist CLOSEPAR eols
    { $10 }
| OPENPAR GOAL COMPLEXITY CLOSEPAR eols OPENPAR STARTTERM OPENPAR FUNCTIONSYMBOLS IDENT CLOSEPAR CLOSEPAR eols OPENPAR VART CLOSEPAR eols
    { $10 }
;

rules : 
| CLOSEPAR EOF      { [] }
| CLOSEPAR eols EOF { [] }
| rule eols rules   { ($1::$3) }
| wrule eols rules  { ($1::$3) }
;

eols:
| EOL      { }
| EOL eols { }
;

rule :
| term TO COM OPENPAR term_list CLOSEPAR                          { Comrule.createRule $1 $5 (Pc.create' []) }
| term TO term                                                    { Comrule.createRule $1 [$3] (Pc.create' []) }
| term TO COM OPENPAR term_list CLOSEPAR CONSTRAINTSEP cond_list  { Comrule.createRule $1 $5 (Pc.create' $8) }
| term TO term CONSTRAINTSEP cond_list                            { Comrule.createRule $1 [$3] (Pc.create' $5) }
| term TO COM OPENPAR term_list CLOSEPAR OPENSQ cond_list CLOSESQ { Comrule.createRule $1 $5 (Pc.create' $8) }
| term TO term OPENSQ cond_list CLOSESQ                           { Comrule.createRule $1 [$3] (Pc.create' $5) }
;

wrule :
| term OPENWTO poly COMMA poly CLOSEWTO COM OPENPAR term_list CLOSEPAR                          { Comrule.createWeightedRule $1 $9 (Pc.create' []) ($3) ($5) }
| term OPENWTO poly COMMA poly CLOSEWTO term                                                    { Comrule.createWeightedRule $1 [$7] (Pc.create' []) ($3) ($5) }
| term OPENWTO poly COMMA poly CLOSEWTO COM OPENPAR term_list CLOSEPAR CONSTRAINTSEP cond_list  { Comrule.createWeightedRule $1 $9 (Pc.create' $12) ($3) ($5) }
| term OPENWTO poly COMMA poly CLOSEWTO term CONSTRAINTSEP cond_list                            { Comrule.createWeightedRule $1 [$7] (Pc.create' $9) ($3) ($5) }
| term OPENWTO poly COMMA poly CLOSEWTO COM OPENPAR term_list CLOSEPAR OPENSQ cond_list CLOSESQ { Comrule.createWeightedRule $1 $9 (Pc.create' $12) ($3) ($5) }
| term OPENWTO poly COMMA poly CLOSEWTO term OPENSQ cond_list CLOSESQ                           { Comrule.createWeightedRule $1 [$7] (Pc.create' $9) ($3) ($5) }
;

term :
| IDENT OPENPAR CLOSEPAR           { Term.create' ($1,[]) }
| IDENT OPENPAR poly_list CLOSEPAR { Term.create' ($1,$3) }
;

term_list :
| term                 { [ $1 ] }
| term COMMA term_list { $1 :: $3 }
;

poly_list :
| poly                 { [ $1 ] }
| poly COMMA poly_list { $1 :: $3 }
;

poly :
| OPENPAR poly CLOSEPAR   { $2 }
| poly PLUS poly          { Poly.add $1 $3 }
| poly MINUS poly         { Poly.minus $1 $3 }
| poly TIMES poly         { Poly.mult $1 $3 }
| MINUS poly %prec UMINUS { Poly.negate $2 }
| INT                     { Poly.fromConstant (Big_int.big_int_of_string $1) }
| IDENT POWER INT         { Poly.fromVarPower (Poly.mkVar $1) 2 }
| IDENT                   { Poly.fromVar (Poly.mkVar $1) }
;

cond_list:
| TRUE                { [] }
| cond                { [ $1 ] }
| cond CONJ cond_list { $1 :: $3 }
;

cond:
| poly EQU poly { ("Equ", $1, $3) }
| poly NEQ poly { ("Neq", $1, $3) }
| poly GEQ poly { ("Geq", $1, $3) }
| poly GTR poly { ("Gtr", $1, $3) }
| poly LEQ poly { ("Leq", $1, $3) }
| poly LSS poly { ("Lss", $1, $3) }
;

varlist:
| IDENT         { }
| IDENT varlist { }
;
