open Exceptions
open Expr

let rec val_of_var x l = match l with
  | [] -> raise NotExpected
  | (t1,t2)::q -> if x = t1 then t2 else val_of_var x q

let rec dedans x l = List.exists (fun y -> y=x) (List.map fst l)


let aff_parenthese k =
  print_string "("; k (); print_string ")"


let aff_infixe s l fctaffich =
  if l = [] then (print_string "")
  else
  (
    fctaffich (List.hd l);
    List.iter 
    (fun x ->
      (
      print_string s;
      fctaffich x;
      )
    )
    (List.tl l)
  )

let aff_aux s l fctaffich = print_string s; aff_parenthese ( fun _ -> aff_infixe "," l fctaffich )

let rec affiche_list_expr l fctaffich =
  match l with
  | Cons(h, q) ->
    print_string "; "; fctaffich h; affiche_list_expr q fctaffich
  | EmptyList ->
    print_string "]"
  | _ -> raise NotList

let rec affiche_list_val l fctaffich =
  match l with
  | VCons(h, q) ->
    print_string "; "; fctaffich h; affiche_list_val q fctaffich
  | VEmptyList ->
    print_string "]"
  | _ -> raise NotList

let rec affiche_cases c fctaffich =
  match c with
  | Case(e, ec, sc) ->
    print_string "| "; fctaffich e; print_string " -> "; fctaffich ec; affiche_cases sc fctaffich
  | EndCase ->
    ()
  | _ -> raise NotCase