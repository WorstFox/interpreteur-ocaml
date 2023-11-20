open Utiles
open Exceptions
open Expr

let if_then_else e1 e2 e3 env eval =
    let v1 = eval e1 env in
    (match v1 with
    | VBool true -> eval e2 env
    | VBool false -> eval e3 env
    | _ -> raise NotBool
    )
