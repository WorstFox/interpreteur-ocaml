open Expr

let rec compute_exc exclist poubelle envir eval continu = 
  match exclist with
  | [] -> continu poubelle
  | t::q -> 
    (
    match (eval t envir) with
    | Val (Exc i) -> Exc i
    | u -> compute_exc q (u::poubelle) envir eval continu
    )