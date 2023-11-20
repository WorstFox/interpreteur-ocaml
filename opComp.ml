open Utiles
open Exceptions
open Expr

let supeg e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VBool (n1 >= n2)
  | VBool n1, VBool n2 -> VBool (n1 >= n2)
  | _, _ -> raise NotConsistant
  )

let sup e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VBool (n1 > n2)
  | VBool n1, VBool n2 -> VBool (n1 > n2)
  | _, _ -> raise NotConsistant
  )

let infeg e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VBool (n1 <= n2)
  | VBool n1, VBool n2 -> VBool (n1 <= n2)
  | _, _ -> raise NotConsistant
  )

let inf e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VBool (n1 < n2)
  | VBool n1, VBool n2 -> VBool (n1 < n2)
  | _, _ -> raise NotConsistant
  )

let eg e1 e2 env eval =
  let rec eg_aux e1 e2 env eval =
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match v1, v2 with
    | VInt n1, VInt n2 -> n1 = n2
    | VBool n1, VBool n2 -> n1 = n2
    | VCouple(v11, v12), VCouple(v21, v22) ->
      (eg_aux (Val(v11)) (Val(v21)) env eval) && (eg_aux (Val(v12)) (Val(v22)) env eval)
    | VEmptyList, VEmptyList -> true
    | VEmptyList, _ -> false
    | _, VEmptyList -> false
    | VCons(v11, v12), VCons(v21, v22) ->
      (eg_aux (Val(v11)) (Val(v21)) env eval) && (eg_aux (Val(v12)) (Val(v22)) env eval)
    | _, _ -> raise NotConsistant
    )
  in
  VBool(eg_aux e1 e2 env eval)

let diff e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VInt n1, VInt n2 -> VBool (n1 != n2)
  | VBool n1, VBool n2 -> VBool (n1 != n2)
  | _, _ -> raise NotConsistant
  )

let opComp e1 e2 constr env eval =
  match constr with
  | Supeg ->
    supeg e1 e2 env eval
  | Sup ->
    sup e1 e2 env eval
  | Infeg ->
    infeg e1 e2 env eval
  | Inf ->
    inf e1 e2 env eval
  | Eg ->
    eg e1 e2 env eval
  | Diff ->
    diff e1 e2 env eval
