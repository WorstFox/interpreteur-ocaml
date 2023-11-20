%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)
open Affichage
open Eval
open Fonction
open FonctionRec

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token DIV
%token IF THEN ELSE TRUE FALSE
%token AND OR NOT
%token SUPEG SUP INFEG INF EG DIFF
%token PRINT
%token LET IN
%token <string> VAR
%token FUN FLECHE REC ANDREC
%token REF MAJ
%token PTVIRG EMPTY PTEXCLM UPTEXCLM
%token FOR TO DO DONE WHILE
%token BEGIN END
%token VIRGULE
%token LCROCH RCROCH CONS
%token MATCH WITH PIPE
%token FUNCTION
%token UNDER
%token EOF             /* retour à la ligne */

%nonassoc UNDER
%right LET IN FUN FLECHE FUNCTION REC ANDREC
%right FOR TO DO DONE WHILE MATCH WITH PIPE
%right PTVIRG
%nonassoc IF THEN ELSE

%left VIRGULE
%nonassoc EMPTY
%right MAJ
%left OR
%left AND
%right CONS
%nonassoc NOT

%nonassoc SUPEG SUP INFEG INF EG DIFF
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV /* associativité gauche: a*b*c, c'est (a*b)*c */

%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%nonassoc REF
%nonassoc PRINT
%right PTEXCLM 
%nonassoc LCROCH RCROCH
%nonassoc LPAREN RPAREN BEGIN END


		    
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
  | LET VAR variable_fun suitevar EG exprseq PTVIRG PTVIRG main { LetIn(Var $2, Val(VFun ($3, construitFun $4 $6, empty_env)), $9) } 
  | LET variable EG exprseq PTVIRG PTVIRG main                  { LetIn($2, $4, $7) } 
  | LET REC suitefunrec PTVIRG PTVIRG main                      { construitFunRec $3 $6 }
  | exprseq EOF                                                 { $1 }  /* on veut reconnaître une expression */
;
  exprseq:
  | expression %prec UNDER                                      { $1 }
  | exprseq PTVIRG exprseq                                      { LetIn(EmptyVar, $1, $3) }  
;
  variable_fun:
  | EMPTY                                                       { EmptyVar }
  | VAR                                                         { Var $1 }
  | LPAREN expression RPAREN                                    { $2 }
  | LPAREN RPAREN                                               { EmptyVar }
;
  variable:
  | EMPTY                                                       { EmptyVar }
  | VAR                                                         { Var $1 }
  | variable VIRGULE variable                                   { Couple($1, $3) }
  | variable CONS variable                                      { Cons($1, $3) }
  | LPAREN variable RPAREN                                      { $2 }
  | LPAREN RPAREN                                               { EmptyVar }
;
  sexpr:
  | LPAREN exprseq RPAREN                                       { $2 }
  | INT                                                         { Val (VInt $1 ) }
  | FALSE							{ Val (VBool false) }
  | TRUE							{ Val (VBool true) }
  | VAR                                                         { Var $1 }
  | LCROCH liste RCROCH                                         { $2 }
  | LCROCH RCROCH                                               { EmptyList }
  | LPAREN RPAREN                                               { Val(VUnit) }
  | PTEXCLM sexpr                                               { OpRef($2, Call) }
;
  applic:
  | applic sexpr                                                { Apply($1, $2) } /* soit on a d'autres arguments avant le dernier */
  | sexpr sexpr                                                 { Apply($1, $2) } /* soit on est à la fin */
;
  suitevar:
  |                                                             { [] }
  | variable_fun suitevar                                       { $1 :: $2 }
;
  suitevar_function:
  |                                                             { [] }
  | variable suitevar_function                                  { $1 :: $2 }
;
  suitefunrec:
  | VAR suitevar EG exprseq                                     { [(Var $1, construitFun $2 $4)] }
  | VAR suitevar EG exprseq ANDREC suitefunrec                  { (Var $1, construitFun $2 $4)::$6 }
;
  liste:
  | expression                                                  { Cons($1, EmptyList) }
  | expression PTVIRG liste                                     { Cons($1, $3) }
;

  matching:
  | expression FLECHE exprseq                                   { Case($1, $3, EndCase) }
  | expression FLECHE exprseq PIPE matching                     { Case($1, $3, $5) }
;

  expression:			    /* règles de grammaire pour les expressions */
  | INT                                                         { Val (VInt $1) }
  | BEGIN exprseq END                                           { $2 }
  | LPAREN exprseq RPAREN                                       { $2 } /* on récupère le deuxième élément */
  | expression PLUS expression                                  { OpArithm($1,$3, Add) }
  | expression MINUS expression                                 { OpArithm($1,$3, Minus) }
  | expression TIMES expression                                 { OpArithm($1,$3, Mul) }
  | MINUS expression %prec UMINUS                               { OpArithm(Val (VInt 0), $2, Minus) }
  | TRUE                                                        { Val (VBool true) }
  | FALSE                                                       { Val (VBool false) }
  | expression DIV expression                                   { OpArithm($1, $3, Div) }
  | expression SUPEG expression                                 { OpComp($1, $3, Supeg) }
  | expression SUP expression                                   { OpComp($1, $3, Sup) }
  | expression INFEG expression                                 { OpComp($1, $3, Infeg) }
  | expression INF expression                                   { OpComp($1, $3, Inf) }
  | expression EG expression                                    { OpComp($1, $3, Eg) }
  | expression DIFF expression                                  { OpComp($1, $3, Diff) }
  | IF expression THEN exprseq ELSE exprseq                     { Ite($2, $4, $6) }
  | expression OR expression                                    { OpBool($1, $3, Or) }
  | expression AND expression                                   { OpBool($1, $3, And) }
  | NOT expression                                              { Not($2) }
  | PRINT expression                                            { PrInt $2 }
  | VAR                                                         { Var $1 }
  | EMPTY                                                       { EmptyVar }
  | PTEXCLM sexpr                                               { OpRef($2, Call) }
  | applic                                                      { $1 }
  | FUN variable suitevar FLECHE exprseq                        { Val(VFun ($2, construitFun $3 $5, empty_env)) }
  | FUNCTION variable suitevar_function FLECHE exprseq          { Val(VFun ($2, construitFun $3 $5, empty_env)) }
  /* fun VAR -> suiteFUN avec suiteFUN de la forme : expression OU VAR' -> suiteFUN */
  | LET REC suitefunrec IN exprseq                              { construitFunRec $3 $5 }
  | LET VAR variable_fun suitevar EG exprseq IN exprseq         { LetIn(Var $2, Val(VFun ($3, construitFun $4 $6, empty_env)), $8) }
  /* comme précédemment, mais de avec le format f x y ... = ... in ... */
  | LET variable EG exprseq IN exprseq                          { LetIn($2, $4, $6) }
  | REF expression                                              { OpRef($2, Ref) }
  | expression MAJ expression                                   { Maj($1, $3) }
  | LPAREN RPAREN                                               { Val (VUnit) }
  | FOR variable EG exprseq TO exprseq DO exprseq DONE          { For($2, $4, $6, $8) }
  | WHILE expression DO exprseq DONE                            { While($2, $4) }
  | expression VIRGULE expression                               { Couple($1, $3) }
  | LCROCH liste RCROCH                                         { $2 }
  | LCROCH RCROCH                                               { EmptyList }
  | expression CONS expression                                  { Cons($1, $3) }
  | MATCH exprseq WITH PIPE matching                            { MatchWith($2, $5) }
;
