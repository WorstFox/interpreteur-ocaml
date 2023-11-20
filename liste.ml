open Exceptions
open Expr
open Utiles

let rec list_cons h q envir eval =
  match q with
  | EmptyList ->
    let vh = eval h envir in
    VCons(vh, VEmptyList)
  | Cons(h1, q1) ->
    let vq = list_cons h1 q1 envir eval in
    let vh = eval h envir in
    VCons(vh, vq)
  | _ ->
    raise NotList