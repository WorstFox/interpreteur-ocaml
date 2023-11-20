open Utiles
open Exceptions
open Expr

let bool_or e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VBool n1, VBool n2 -> VBool (n1 || n2)
  | _, _ -> raise NotBool
  )

let bool_and e1 e2 env eval =
  let v1 = eval e1 env in
  let v2 = eval e2 env in
  (match v1, v2 with
  | VBool n1, VBool n2 -> VBool (n1 && n2)
  | _, _ -> raise NotBool
  )

let bool_not e1 env eval =
  let v1 = eval e1 env in
  (match v1 with
  | VBool n1 -> VBool(not n1)
  | _ -> raise NotBool
  )
