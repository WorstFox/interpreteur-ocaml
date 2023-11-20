open Utiles
open Exceptions
open Expr

let opRef e1 constr memory env eval =
  match constr with
  | Ref ->
    let v1 = eval e1 env in
    if memory.t_act = memory.t_max then (* on vérifie que notre mémoire n'est pas pleine *)
      raise StackOverflow
    else
      begin
        memory.memo.(memory.t_act) <- v1; (* on affecte une case mémoire à notre variable *)
        memory.t_act <- memory.t_act + 1;
        VAddress(memory.t_act - 1) (* on renvoie une valeur indice qui renvoie l'adresse de la case mémoire *)
      end
  | Call -> (* renvoie VUnit si la case n'est pas allouée *)
    let v1 = eval e1 env in
    (match v1 with
    | VAddress i -> (* on va chercher dans la mémoire *)
      memory.memo.(i)
    | _ -> print_int 0; raise NotReference
    )

let maj e1 e2 memory env eval =
  match e1 with
  | Var s ->
    let v1 = eval (Var s) env in
    let v2 = eval e2 env in
    (match v1 with
    | VAddress i -> 
      memory.memo.(i) <- v2; (* on modifie l'intérieur de la case *)
      VUnit
    | _ -> 
      raise NotReference
    )
  | _ -> raise NotVariable
