open Utiles
open Exceptions
open Expr

let couplage e1 e2 envir eval = (* transforme l'expression e1, e2 en valeur v1, v2 *)
  let v2 = eval e2 envir in
  let v1 = eval e1 envir in
  VCouple(v1, v2)

let rec decouplage var v = (* découple [ou pas si ya pas de couple] v en var, et renvoie le bout d'environnement associé *)
  match var, v with
  | Var s, v ->
    [(Var s, v)]
  | EmptyVar, _ ->
    []
  | Couple(var1, var2), VCouple(v1, v2) ->
    (decouplage var1 v1) @ (decouplage var2 v2)
  | Cons(h1, q1), VCons(h2, q2) ->
    (decouplage h1 h2) @ (decouplage q1 q2)
  | EmptyList, VEmptyList ->
    []
  | _, _ ->
    raise NotConsistant