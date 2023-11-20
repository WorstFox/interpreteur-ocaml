open Expr
open Type
open Exceptions 
type subst = (expr * types) list
           
let compteur_type = ref 0
let new_type () = incr compteur_type; X(!compteur_type)

let rec all_assoc x t j = (* associe à chaque occurence de la variable (au sens large) x le type t *)
  match x with
  | Var(u) ->
    (match j with
    | (Var(v), t1) :: q when u = v -> (t, t1) :: (all_assoc (Var(u)) t q) (* on crée l'association des types *)
    | (LetIn(Var(v), _, _), _) :: _ when u = v -> [] (* si on rencontre un let in avec le même nom de variable, on s'arrête *)
    | _ :: q -> all_assoc (Var(u)) t q
    | [] -> []
    )
  | Couple(y, z) -> (* si on a un couple, on associe les variables dans le couple *)
    let t1 = new_type () in
    let t2 = new_type () in
    (t, Etoile(t1, t2)) :: (all_assoc y t1 j) @ (all_assoc z t2 j)
  | Cons(y, z) -> (* comme pour couple *)
    let t1 = new_type () in
    let t2 = new_type () in
    (t1, t2) :: (t, List(t1)) :: (all_assoc y t1 j) @ (all_assoc z t2 j)
  | _ -> [] (* si on a d'autres expressions, on ignore *)

let rec trouvepb e t expr_to_type = (* crée le problème d'unification et le numéro de type auquel chaque expression est associées *)
  match e with
  (* si on a des opérations arithmétiques, on veut des int *)
  | Val(VInt(_)) ->
    [(t, Int)], (e, t) :: expr_to_type
  | OpArithm(e1, e2, _) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb e1 Int expr_to_type in
    let u2, j2 = trouvepb e2 Int expr_to_type in
    (t, Int) :: (t1, Int) :: (t2, Int) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  
  (* même type des deux côtés de l'égalité *)
  | OpComp(e1, e2, Eg) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    (t1, t2) :: (t, Bool) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  
  | OpComp(e1, e2, _)  ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb e1 Int expr_to_type in
    let u2, j2 = trouvepb e2 Int expr_to_type in
    (t1, Int) :: (t2, Int) :: (t, Bool) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  
  (* nos types booléens *)
  | Val(VBool(_)) ->
    [(t, Bool)], (e, t) :: expr_to_type
  | Not(e1) ->
    let t1 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    (t, Bool) :: (t1, Bool) :: u1, (e, t) :: j1
  | OpBool(e1, e2, _) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb e1 Bool expr_to_type in
    let u2, j2 = trouvepb e2 Bool expr_to_type in
    (t, Bool) :: (t1, Bool) :: (t2, Bool) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  | Ite(e1, e2, e3) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let t3 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    let u3, j3 = trouvepb e3 t3 expr_to_type in
    (t, t2) :: (t1, Bool) :: (t2, t3) :: u1 @ u2 @ u3, (e, t) :: j1 @ j2 @ j3 @ expr_to_type
  
  (* on veut que le type de l'argument de la fonction corresponde à celui de l'entrée qu'on lui donne *)
  | Apply(e1, e2) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let t3 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    (t, t3) :: (t1, Fleche(t2, t3)) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  | Val(VFun(e1, e2, _)) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    let u3 = all_assoc e1 t1 j2 in (* on veut que tous occurences de notre argument qui apparaît dans le corps de la fonction soit du bon type *)
    (t, Fleche(t1, t2)) :: u1 @ u2 @ u3, (e, t) :: j1 @ j2 @ expr_to_type
  
  | LetIn(e1, e2, e3) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let t3 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    let u3, j3 = trouvepb e3 t3 expr_to_type in
    let u4 = all_assoc e1 t1 j3 in (* comme pour les fonctions *)
    (t1, t2) :: (t, t3) :: u1 @ u2 @ u3 @ u4, (e, t) :: j1 @ j2 @ j3 @ expr_to_type
  | Var(s) ->
    let t1 = new_type () in
    (t1, t) :: [], (e, t) :: expr_to_type
  
  (* notre type pour les couples *)
  | Couple(e1, e2) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    (t, Etoile(t1, t2)) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  | Val(VCouple(v1, v2)) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb (Val(v1)) t1 expr_to_type in
    let u2, j2 = trouvepb (Val(v2)) t2 expr_to_type in
    (t, Etoile(t1, t2)) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  
  (* prInt est de type Int -> Int *)
  | PrInt(e1) ->
    let u1, j1 = trouvepb e1 t expr_to_type in
    (t, Int) :: u1, (e, t) :: j1
  
  | EmptyList -> (* on a une sorte de type polymorphe pour la liste de vide, pour éviter les erreurs *)
    let t1 = new_type () in
    [(t, List(t1))], [(e, t)]
  | Cons(h, q) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb h t1 expr_to_type in
    let u2, j2 = trouvepb q t2 expr_to_type in
    (* on vérifie que tous les éléments de notre liste soient du même type *)
    (t, List(t1)) :: (List(t1), t2) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type
  | Val(VEmptyList) ->
    let t1 = new_type () in
    [(t, List(t1))], [(e, t)]
  | Val(VCons(h, q)) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let u1, j1 = trouvepb (Val(h)) t1 expr_to_type in
    let u2, j2 = trouvepb (Val(q)) t2 expr_to_type in
    (t, List(t1)) :: (List(t1), t2) :: u1 @ u2, (e, t) :: j1 @ j2 @ expr_to_type

  | MatchWith(e1, EndCase) ->
    [], []
  | MatchWith(e1, Case(e2, e3, sc)) ->
    let t1 = new_type () in
    let t2 = new_type () in
    let t3 = new_type () in
    let t4 = new_type () in
    let u1, j1 = trouvepb e1 t1 expr_to_type in
    let u2, j2 = trouvepb e2 t2 expr_to_type in
    let u3, j3 = trouvepb e3 t3 expr_to_type in
    let u4 = all_assoc e2 t2 j3 in (* on a de nouveau une association de variable, comme pour fonction et let in *)
    let u5, j5 = trouvepb (MatchWith(e1, sc)) t4 expr_to_type in
    (* tous les cas doivent renvoyer le même type, ce qu'on match et le pattern doivent être du même type *)
    (t1, t2) :: (t, t3) :: (t3, t4) :: u1 @ u2 @ u3 @ u4 @ u5, (e, t) :: j1 @ j2 @ j3 @ j5 @ expr_to_type