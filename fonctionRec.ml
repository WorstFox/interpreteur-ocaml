open Exceptions
open Expr
open Utiles

let construitFunRec l0 e =
  let envirrec =
    List.map
    (fun (f,exp) -> (f,VFunRec(f,exp,empty_env)))
    l0
  in
  let rec aux l =
    match l with
    | [] -> raise NotExpected
    | (f,exp)::q -> LetIn(f, Val (VFunRec(f, exp, envirrec)), e)
  in aux l0

let remplienvirrec envirrec = 
  List.map
  (function 
      | Var f, VFunRec (var, exp, _) -> (Var f,VFunRec (var, exp, envirrec))
      | _, VFunRec _ -> raise NotVariable
      | Var f, _ -> raise NotRecursiveFunction 
      | _ -> raise NotVariable
  )
  envirrec

let applyRec var exp envint envirrec e2 envext eval =
  eval exp ((remplienvirrec envirrec)@[(var, eval e2 envext)]@envint@envext)

let affiche_envirrec l e affiche_expr =
  print_string "let rec ";
  aff_infixe
  " and "
  l
  (fun x -> match x with
    | a, VFunRec(x,eint,_) -> affiche_expr a; print_string " = "; affiche_expr eint ;
    | _, _ -> raise NotRecursiveFunction
  );
  print_string " in ";
  affiche_expr e;
