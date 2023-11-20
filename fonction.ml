open Utiles
open Exceptions
open Expr
open FonctionRec
open Affichage
open Motif

let rec construitFun l e = match l with
  | [] -> e
  | t::q -> Val (VFun(t, construitFun q e, empty_env ))

(*let affichenv lenv lexp = (aff_aux "Env" lenv (affiche_valvar affiche_expr affiche_val); print_string "ARAR"; affiche_expr lexp; print_newline ())*)

let rec ajoute_envir x =
  match x with
  | Var s -> [(Var s, VInt 0)]
  | Couple(u, v) -> (ajoute_envir u) @ (ajoute_envir v)
  | Cons(h, q) -> (ajoute_envir h) @ (ajoute_envir q)
  | _ -> []

let rec verifbiendef f envext memory =
  (*affichenv envext f;*)
  match f with
 | Val v -> (
    match v with
      | VFun (var, exp, envint) ->
         (verifbiendef exp ((ajoute_envir var)@(envint@envext)) memory)
      | VFunRec (var, exp, envrec) ->
         verifbiendef exp ((remplienvirrec envrec)@envext) memory
      | _ -> true 
  )
 | EmptyVar | EmptyList | EndCase -> true
 | OpRef(Var s, Call) ->
    verifbiendef (Var s) envext memory
 | OpArithm (x,y,_) -> (verifbiendef x envext memory) && (verifbiendef y envext memory) 
 | OpComp (x,y,_) -> (verifbiendef x envext memory) && (verifbiendef y envext memory)
 | OpBool (x,y,_) -> (verifbiendef x envext memory) && (verifbiendef y envext memory)
 | Not x -> verifbiendef x envext memory
 | Ite (a,b,c) -> (verifbiendef a envext memory) && (verifbiendef b envext memory) && (verifbiendef c envext memory)
 | LetIn (x,y,e) -> (verifbiendef y envext memory) && (verifbiendef e ((ajoute_envir x)@envext) memory)
 | Apply (f,g) -> (verifbiendef f envext memory) && (verifbiendef g envext memory)
 | OpRef (a, _) -> (verifbiendef a envext memory)
 | Maj (a,b) -> (verifbiendef a envext memory) && (verifbiendef b envext memory)
 | For (a,b,c,d) -> (verifbiendef a envext memory) && (verifbiendef b envext memory) 
  && (verifbiendef c envext memory) && (verifbiendef d envext memory)
 | While (c,b) -> (verifbiendef c envext memory) && (verifbiendef b envext memory) 
 | PrInt x -> verifbiendef x envext memory
 | Var x -> 
         if dedans (Var x) envext then true else raise (MissingVariable x)
 | Couple(x, y) -> (verifbiendef x envext memory) && (verifbiendef y envext memory)
 | Cons(h, q) -> (verifbiendef h envext memory) && (verifbiendef q envext memory)
 | MatchWith(e, c) -> (verifbiendef e envext memory) && (verifbiendef c envext memory)
 | Case(c, ec, sc) -> (verifbiendef ec ((ajoute_envir c)@envext) memory) && (verifbiendef sc envext memory)

let rec apply e1 e2 envext eval memory =
  let v2 = eval e2 envext in
  if verifbiendef e1 envext memory then (
  match e1 with
    | Val v -> 
      ( match v with
        | VFun (var, exp, envint) ->
          eval exp ((decouplage var v2)@(envint@envext)) (* L'environnement de la fonction a la priorité sur l'extérieur *) 
        | VFunRec (var, Val (VFun (var2, exp, envint)), envirrec) ->
          eval exp ((remplienvirrec envirrec)@(decouplage var2 v2)@envint@envext)
        | VAddress i ->
          apply (Val(memory.memo.(i))) e2 envext eval memory
        | _ -> raise NotFunction
      )

    | Var s -> apply (Val (val_of_var (Var s) envext)) (Val(v2)) envext eval memory

    | Apply (f1, f2)  -> apply (Val (apply f1 f2 envext eval memory)) (Val(v2)) envext eval memory
    | LetIn (x,y,e) -> apply e (Val(v2)) ((decouplage x (eval y envext))@envext) eval memory
    | OpRef(e, Call) -> apply (Val(val_of_var e envext)) (Val(v2)) envext eval memory
    | _ -> 
      let v1 = eval e1 envext in
      apply (Val(v1)) (Val(v2)) envext eval memory 
  )
  else raise NotFunction
