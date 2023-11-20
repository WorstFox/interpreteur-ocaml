open Exceptions
open Expr


let prInt e envir eval =
  match eval e envir with
    | VInt x -> print_int x; print_newline (); VInt x
    | _ -> raise NotInt