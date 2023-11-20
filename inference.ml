open Expr
open Affichage
open Type
open Exceptions
open Unif
open Unif_tp3

(* ces deux fonctions traduisent notre problème pour être utilisable par unif_tp3.ml *)
let trad (x, y) =
  (transformetype x, transformetype y)
let untrad (x, y) =
  (transformeunif (Var x), transformeunif y)

let infer e = (* on  crée et résout le problème de type pour l'expression e *)
  let pb, expr_to_type = trouvepb e (X(0)) [] in
  let pbtrad = List.map trad pb in
  let correspondance_typetrad = unify pbtrad in
  let correspondance_type = List.map untrad correspondance_typetrad in
  correspondance_type, expr_to_type


let rec cherchecorresp corresp i = match corresp with (* on va chercher à quoi correspondent nos types numérotés, ie Int, Bool, _ -> _, _ * _*)
  | [] -> raise NotUnifyable
  | (a, b) :: q when a = X(i) -> (match b with
    | X(j) -> cherchecorresp q j
    | _ -> b
    )
  | (a, b) :: q -> cherchecorresp q i

let rec print_type t corresp = (* affiche le type t *)
  match t with
  | Int -> print_string "int"
  | Bool -> print_string "bool"
  | Fleche(t1, t2) -> 
    print_string "("; print_type t1 corresp; print_string ") -> "; print_type t2 corresp
  | X(i) ->
    (match cherchecorresp corresp i with
    | X(i) -> print_string "Var "; print_int i
    | t1 -> print_type t1 corresp
    )
  | Etoile(t1, t2) ->
    print_string "("; print_type t1 corresp; print_string ") * ("; print_type t2 corresp; print_string ")"
  | List(t1) ->
    print_string "("; print_type t1 corresp; print_string ") List"


let rec affiche_inference corresp expr_to_type = (* affiche le type de la première expression *)
  match expr_to_type with
  | (e, t) :: q ->
    affiche_expr e; print_string " : "; print_type t corresp; print_string "\n"; affiche_inference corresp q
  | [] -> ()

let rec affiche_corresp corresp =
  match corresp with
  | [] -> ()
  | (a,b) :: q -> 
    print_type a corresp; print_string " : "; print_type b corresp; print_string "\n"; affiche_corresp q
