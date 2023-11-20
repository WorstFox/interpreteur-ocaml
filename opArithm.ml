open Utiles
open Exceptions
open Expr

let add e1 e2 env eval =
  let v2 = eval e2 env in
  let v1 = eval e1 env in (* on récupère les valeurs à comparer *)
  (match v1, v2 with
  | VInt n1, VInt n2 -> VInt (n1 + n2)
  | _, _ -> raise NotInt
  )

let minnus e1 e2 env eval =
  let v2 = eval e2 env in
  let v1 = eval e1 env in (* on récupère les valeurs à comparer *)
  (match v1, v2 with
  | VInt n1, VInt n2 -> VInt (n1-n2) 
  | _, _ -> raise NotInt
  )

let mul e1 e2 env eval =
  let v2 = eval e2 env in
  let v1 = eval e1 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VInt (n1 * n2)
  | _, _ -> raise NotInt
  )

let div e1 e2 env eval =
  let v2 = eval e2 env in
  let v1 = eval e1 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VInt (n1 / n2)
  | _, _ -> raise NotInt
  )

let opArithm e1 e2 constr env eval =
  match constr with
  | Add ->
    add e1 e2 env eval
  | Minus ->
    minnus e1 e2 env eval
  | Mul ->
    mul e1 e2 env eval
  | Div ->
    div e1 e2 env eval
