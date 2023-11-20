open Expr
open Exceptions
open Utiles
open FonctionRec

   

(* fonction d'affichage *)

let rec affiche_expr_bis e =
  match e with
  | Val v -> affiche_val_bis v
  | Var s -> print_string s
  | OpArithm(e1,e2, Add) -> aff_aux "Add" [e1;e2] affiche_expr_bis
  | OpArithm(e1,e2, Minus) -> aff_aux "Minus" [e1;e2] affiche_expr_bis
  | OpArithm(e1,e2, Mul) -> aff_aux "Mul" [e1;e2] affiche_expr_bis
  | OpArithm(e1, e2, Div) -> aff_aux "Div" [e1;e2] affiche_expr_bis
  | OpComp(e1, e2, Supeg) -> aff_aux "Supeg" [e1;e2] affiche_expr_bis
  | OpComp(e1, e2, Sup) -> aff_aux "Sup" [e1;e2] affiche_expr_bis
  | OpComp(e1, e2, Infeg) -> aff_aux "Infeg" [e1;e2] affiche_expr_bis
  | OpComp(e1, e2, Inf) -> aff_aux "Inf" [e1;e2] affiche_expr_bis
  | OpComp(e1, e2, Eg) -> aff_aux "Eg" [e1;e2] affiche_expr_bis
  | OpComp(e1, e2, Diff) -> aff_aux "Diff" [e1;e2] affiche_expr_bis
  | Ite(e1, e2, e3) -> aff_aux "Ite" [e1;e2;e3] affiche_expr_bis
  | OpBool(e1, e2, Or) -> aff_aux "Or" [e1;e2] affiche_expr_bis
  | OpBool(e1, e2, And) -> aff_aux "And" [e1;e2] affiche_expr_bis
  | Not(e1) -> aff_aux "Not" [e1] affiche_expr_bis
  | LetIn(e1, e2, e3) -> aff_aux "LetIn" [e1;e2;e3] affiche_expr_bis
  | Apply(e1, e2) -> aff_aux "Apply" [e1;e2] affiche_expr_bis
  | OpRef(e1, Ref) -> aff_aux "Ref" [e1] affiche_expr_bis
  | Maj(e1, e2) -> aff_aux "Maj" [e1;e2] affiche_expr_bis
  | OpRef(e1, Call) -> aff_aux "Call" [e1] affiche_expr_bis
  | For(e1, e2, e3, e4) -> aff_aux "For" [e1;e2;e3;e4] affiche_expr_bis
  | While(e1, e2) -> aff_aux "While" [e1;e2] affiche_expr_bis
  | PrInt e -> aff_aux "PrInt" [e] affiche_expr_bis
  | EmptyVar -> print_string "_";
  | Couple(e1, e2) -> aff_aux "Couple" [e1; e2] affiche_expr_bis
  | EmptyList -> print_string "EmptyList"
  | Cons(h, q) -> aff_aux "Cons" [h; q] affiche_expr_bis
  | MatchWith(e, cases) -> aff_aux "MatchWith" [e; cases] affiche_expr_bis
  | Case(e, ec, sc) -> aff_aux "Case" [e; ec; sc] affiche_expr_bis
  | EndCase -> print_string "EndCase"

and affiche_valvar funcexp funcval (a,b) =
  print_string "("; affiche_expr a; print_string ","; affiche_val b; print_string ")";

and affiche_val_bis v = match v with 
  | VInt x -> print_int x;
  | VBool x -> Printf.printf "%b" x;
  | VFun(x, e, envir) -> affiche_expr_bis x; print_string " ->"; aff_aux "Env" envir (affiche_valvar affiche_expr_bis affiche_val_bis ); print_string " "; affiche_expr_bis e; (* à étendre à plusieurs variables *)
  | VFunRec(f, e, envirrec) -> print_string "Rec."; aff_aux "Envrec" envirrec (affiche_valvar affiche_expr_bis affiche_val_bis); print_string " "; affiche_expr e;
  | VUnit -> print_string "()";
  | VAddress i -> print_string "ref : indice "; print_int i; 
  | Exc i -> aff_aux "Exception" [i] print_int
  | VCouple(v1, v2) -> aff_aux "Couple" [v1; v2] affiche_val_bis
  | VEmptyList -> print_string "VEmptyList"
  | VCons(h, q) -> aff_aux "Cons" [h; q] affiche_val_bis
  | VMemVide -> ()

and affiche_expr e =
  match e with
  | Val v -> affiche_val v
  | Var s -> print_string s
  | OpArithm(e1,e2, Add) ->  aff_parenthese (fun _ -> aff_infixe "+" [e1;e2] affiche_expr)
  | OpArithm(e1,e2, Minus) -> aff_parenthese (fun _ -> aff_infixe "-" [e1;e2] affiche_expr)
  | OpArithm(e1,e2, Mul) -> aff_parenthese (fun _ -> aff_infixe "*" [e1;e2] affiche_expr)
  | OpArithm(e1, e2, Div) -> aff_parenthese (fun _ -> aff_infixe "/" [e1;e2] affiche_expr)
  | OpComp(e1, e2, Supeg) -> aff_parenthese (fun _ -> aff_infixe ">=" [e1;e2] affiche_expr)
  | OpComp(e1, e2, Sup) -> aff_parenthese (fun _ -> aff_infixe ">" [e1;e2] affiche_expr)
  | OpComp(e1, e2, Infeg) -> aff_parenthese (fun _ -> aff_infixe "<=" [e1;e2] affiche_expr)
  | OpComp(e1, e2, Inf) -> aff_parenthese (fun _ -> aff_infixe "<" [e1;e2] affiche_expr)
  | OpComp(e1, e2, Eg) -> aff_parenthese (fun _ -> aff_infixe "=" [e1;e2] affiche_expr)
  | OpComp(e1, e2, Diff) -> aff_parenthese (fun _ -> aff_infixe "!=" [e1;e2] affiche_expr)
  | Ite(e1, e2, e3) -> aff_parenthese (fun _ -> print_string "if "; affiche_expr e1; print_string " then "; affiche_expr e2; print_string " else "; affiche_expr e3;)
  | OpBool(e1, e2, Or) -> aff_parenthese (fun _ -> aff_aux "or" [e1;e2] affiche_expr)
  | OpBool(e1, e2, And) -> aff_parenthese (fun _ -> aff_aux "and" [e1;e2] affiche_expr)
  | Not(e1) -> aff_parenthese (fun _ -> print_string "not "; affiche_expr e1)
  | LetIn(f, Val (VFunRec (x, eint, envirrec)), e) ->  affiche_envirrec envirrec e affiche_expr; 
  | LetIn(e1, e2, e3) -> aff_parenthese (fun _ -> print_string "let "; affiche_expr e1; print_string " = "; affiche_expr e2; print_string " in "; affiche_expr e3;)
  | Apply(e1, e2) -> aff_parenthese (fun _ ->  aff_infixe " " [e1;e2] affiche_expr;)
  | OpRef(e1, Ref) -> aff_parenthese (fun _ -> print_string "ref "; affiche_expr e1;)
  | Maj(e1, e2) -> aff_parenthese (fun _ -> aff_infixe " := " [e1;e2] affiche_expr)
  | OpRef(e1, Call) -> aff_parenthese (fun _ -> print_string "!"; affiche_expr e1;)
  | For(e1, e2, e3, e4) -> aff_parenthese (fun _ -> print_string "for "; affiche_expr e1; print_string " = "; affiche_expr e2; print_string " to "; affiche_expr e3; print_string " do "; affiche_expr e4; print_string " done ")
  | While(e1, e2) -> aff_parenthese (fun _ -> print_string "while "; affiche_expr e1; print_string " do "; affiche_expr e2; print_string " done ")
  | PrInt e -> aff_parenthese (fun _ -> print_string "prInt "; affiche_expr e;)
  | EmptyVar -> print_string "_"
  | Couple(e1, e2) -> aff_parenthese (fun _ -> affiche_expr e1; print_string ", "; affiche_expr e2;)
  | EmptyList -> print_string "[]"
  | Cons(h, q) -> print_string "["; affiche_expr h; affiche_list_expr q affiche_expr
  | MatchWith(e, cases) -> print_string "match "; affiche_expr e; print_string " with "; affiche_cases cases affiche_expr
  | EndCase | Case(_, _, _) -> raise NotExpected

and affiche_val v = match v with
  | VInt x -> print_int x;
  | VBool x -> Printf.printf "%b" x;
  | VFun(x, e, envir) -> aff_parenthese (fun _ -> print_string "fun ";affiche_expr x; print_string " -> "; affiche_expr e;)
  | VFunRec(x, e, envirrec) -> affiche_envirrec envirrec e affiche_expr; 
  | VUnit -> print_string "()";
  | VAddress i -> print_string "adresse "; print_int i; 
  | Exc i -> raise (E i)
  | VCouple(v1, v2) -> aff_parenthese (fun _ -> affiche_val v1; print_string ", "; affiche_val v2;)
  | VEmptyList -> print_string "[]"
  | VCons(h, q) -> print_string "["; affiche_val h; affiche_list_val q affiche_val
  | VMemVide -> ()
