open Utiles
open Exceptions
open Expr

let boucle_while e1 e2 envir eval =
    let v1 = eval e1 envir in (* on récupère la valeur de la condition *)
    (match v1 with
    | VBool true -> (* si elle est vérifiée on effectue un tour de boucle *)
      let _ = eval e2 envir in
      eval (While(e1, e2)) envir
    | VBool false -> (* sinon on arrête *)
      VUnit
    | _ -> raise NotExpected
    )

let boucle_for e1 e2 e3 e4 envir eval = (* e1 : variable; e2, e3 : int; e4 : unit *)
  match e1 with
  | EmptyVar ->
    let v2 = eval e2 envir in
    let v3 = eval e3 envir in
    let _ = eval e4 envir in (* on effectue à vide e4 *)
    (match v2, v3 with (* on vérifie si on est arrivé au bout de la boucle, sinon on continue *)
    | VInt n2, VInt n3 when n2 = n3 ->
      VUnit
    | VInt n2, VInt n3 when n2 < n3 ->
      eval (For(EmptyVar, Val (VInt (n2 + 1)), Val (VInt n3), e4)) envir
    | VInt n2, VInt n3 when n2 > n3 ->
      eval (For(EmptyVar, Val (VInt (n2 - 1)), Val (VInt n3), e4)) envir
    | _, _ -> raise NotInt
    )
  | Var s ->
    let v2 = eval e2 envir in
    let v3 = eval e3 envir in
    (
      match v2, v3 with
        | VInt n2, VInt n3 ->
          let eval4 = LetIn(Var s, OpRef(Var s, Call), e4) in (* on sort i de la référence pour l'évaluation *)
          let instruction = LetIn(EmptyVar, eval4, Maj(Var s, OpArithm(OpRef(Var s, Call), Val (VInt 1), Add))) in (* on incrémente i *)
          let condition = OpComp(OpRef(Var s, Call), Val (VInt n3), Infeg) in (* on crée la condition de terminaison *)
          let boucle = While(condition, instruction) in (* on assemble notre boucle while qui imite la boucle for *)
          let initialisation = LetIn(Var s, OpRef(Val (VInt n2), Ref), boucle) in (* on assemble le tout et on initialise i *)
          eval initialisation envir
        | _, _ -> raise NotInt        
      
    )
  | _ -> raise NotVariable
