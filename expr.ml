(* définition sous-types *)
type arithm =
  | Add
  | Minus
  | Mul
  | Div

type bool_ =
  | And
  | Or

type comp =
  | Supeg
  | Sup
  | Infeg
  | Inf
  | Eg
  | Diff

type ref =
  | Ref
  | Call

(* d�finition des diff�rents types : tout est � reprendre et �tendre *)
type expr =
  | Val of valeur
  | Var of string
  | EmptyVar

  | OpArithm of expr * expr * arithm
  | OpBool of expr * expr * bool_
  | Not of expr
  | OpComp of expr * expr * comp

  | Ite of expr * expr * expr (* if e1 then e2 else e3 *)

  | LetIn of expr * expr * expr (* let e1 = e2 in e3 *)

  | Apply of expr * expr (* e1 e2 [applique e1 à e2] *)

  | OpRef of expr * ref
  | Maj of expr * expr (* e1 := e2 *)

  | For of expr * expr * expr * expr (* for e1 = e2 to e3 do e4 done *)
  | While of expr * expr (* while e1 do e2 done *)

  | PrInt of expr

  | Couple of expr * expr

  | EmptyList
  | Cons of expr * expr
  
  | MatchWith of expr * expr
  | Case of expr * expr * expr
  | EndCase

and valeur =
  | VInt of int
  | VBool of bool
  | VFun of expr * expr * env
  | VFunRec of expr * expr * env (* Le premier est le nom de la fonction, le deuxieme la fonction elle meme (anonyme) et le troisieme l'environnement recursif*)
  | VAddress of int
  | VMemVide (* pour les cases mémoires vides *)
  | VUnit
  | Exc of int
  | VCouple of valeur * valeur
  | VEmptyList
  | VCons of valeur * valeur

and varval = 
  expr * valeur 

and env =
  varval list

let empty_env = []
type storage = 
  { t_max : int; mutable t_act : int; memo : valeur array;} (* taille maximale, taille actuelle, mémoire *)
