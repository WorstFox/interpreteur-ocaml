open Expr
open Affichage
open Eval
open PrInt
open Inference

(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""

let showsrc = ref false

let debug = ref false

let showtypes = ref false

let speclist = [("-showsrc",Arg.Set showsrc, ""); ("-debug", Arg.Set debug, ""); ("-showtypes", Arg.Set showtypes, "")]

let recupere_entree () =
  Arg.parse speclist (fun s -> nom_fichier := s) "";
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "probl�me de saisie\n"; raise e)

(* mettre � true et recompiler si l'on veut voir l'ex�cution pas � pas de l'automate *)
let trace = ref false
let _ = Stdlib.Parsing.set_trace !trace


      
      
(* le traitement d'une expression en entr�e *)   
let execute e =
  begin
    if (!showsrc) then (
      print_string "-showsrc activé";print_newline ();
      print_string "entrée parsée (en détails) : ";print_newline ();
      affiche_expr_bis e;print_newline ();
      print_newline ();
      print_string "entrée acceptable en OCaml : ";print_newline ();
      affiche_expr e;print_newline ();
    ) 
    else (if (!debug) then 
    (
      print_string "-debug activé";print_newline ();
      print_newline ();
      print_string "entrée parsée (en détails) : ";print_newline ();
      affiche_expr_bis e;print_newline ();
      print_newline ();
      print_string "entrée acceptable en OCaml : ";print_newline ();
      affiche_expr e;print_newline ();
      print_newline ();
      print_string "sortie en détails : ";print_newline ();
      let v =  Eval.eval e Expr.empty_env in
      affiche_val_bis v;print_newline ();
      print_newline ();
      print_string "sortie acceptable en OCaml : ";print_newline ();
      affiche_val v;print_newline ();
    )
    else (if (!showtypes) then
      (
        let tmp = infer e in
        affiche_inference (fst tmp) [(e, X(0))];
      )
    else 
      (
        let _ =  Eval.eval e Expr.empty_env in ()
      )
    ));
  end

(* la boucle principale *)
let calc () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e


let _ = calc()
