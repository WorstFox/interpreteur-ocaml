open Utiles
open Exceptions
open Expr
open Motif

let letIn var1 e2 e3 env eval =
  let v2 = eval e2 env in
  let v3 = eval e3 ((decouplage var1 v2) @ env) in (* on ajoute les nouveaux couples à l'environnement *)
  v3
