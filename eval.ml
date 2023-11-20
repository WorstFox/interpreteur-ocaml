
(* expressions et types *)
open Expr

(* exceptions *)
open Exceptions

(* fonctions auxiliaires *)
open Utiles

(* opérations et instructions associées *)
open OpArithm
open OpBool
open OpComp
open IfThenElse
open LetIn
open PrInt
open Fonction
open FonctionRec
open Reference
open Boucle
open Motif
open Liste
open MatchWith

(* environnement et mémoire *)
let memory =
  { t_max = 2000; t_act = 0; memo = Array.make 2000 VMemVide;}

(* s�mantique op�rationnelle � grands pas *)
let rec eval e envir =
  match e with
  (* Valeurs *)
  | Val v -> (
    match v with
      | VFun (x, exp, envirfun) -> if verifbiendef e envir memory then VFun (x,exp,envir@envirfun) else raise PasBienDef
      | VFunRec (f, exp, er) -> if verifbiendef e envir memory then VFunRec (f, Val (eval exp (er@envir)), er) else raise PasBienDef
      | _ -> v
    )
  | Var(s) ->
    val_of_var (Var s) envir
  | EmptyVar ->
    VUnit

  (* Opérations arithmétiques *)
  | OpArithm(e1, e2, constr) ->
    opArithm e1 e2 constr envir eval
  
  (* Opérations de comparaison *)
  | OpComp(e1, e2, constr) ->
    opComp e1 e2 constr envir eval
    
  (* Opérations booléennes *)
  | OpBool(e1, e2, Or) -> (* Or *)
    bool_or e1 e2 envir eval
  | OpBool(e1, e2, And) -> (* And *)
    bool_and e1 e2 envir eval
  | Not e1 ->
    bool_not e1 envir eval
    
  (* if _ then _ else _ *)
  | Ite(e1, e2, e3) ->
    if_then_else e1 e2 e3 envir eval
    
  (* Let _ in _ *)
  | LetIn(e1, e2, e3) ->
    letIn e1 e2 e3 envir eval
    
  (* Fonctions et applications *)
  | Apply(e1, e2) ->
    apply e1 e2 envir eval memory
    
  (* Références et opérations associées *)
  | OpRef(e1, constr) ->
    opRef e1 constr memory envir eval
  | Maj(e1, e2) -> (* rajouter exception si Var s n'est pas une adresse ? *)
    maj e1 e2 memory envir eval
    
  (* Boucles *)
  | For(e1, e2, e3, e4) -> (* e1 : variable; e2, e3 : int; e4 : unit *)
    boucle_for e1 e2 e3 e4 envir eval
  | While(e1, e2) -> (* e1 : bool; e2 : unit *)
    boucle_while e1 e2 envir eval

  | PrInt e -> 
    prInt e envir eval

  | Couple(e1, e2) ->
    couplage e1 e2 envir eval
  
  | EmptyList -> VEmptyList
  | Cons(h, q) ->
    list_cons h q envir eval
  
  | MatchWith(e, cases) -> (* match e with case c ... *)
    match_with e cases envir eval
  | EndCase | Case(_, _, _) -> raise NotExpected