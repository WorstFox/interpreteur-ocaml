open Exceptions
open Expr
open Utiles
open Motif

let match_with e cases envir eval = (* limiter aux listes et couples *)
  let rec match_with_aux e cases envir envir_pattern =
    match cases with
    | EndCase -> (* on n'a pas trouvé de pattern correspondant *)
      raise NotMatched
    | Case(c, ec, sc) -> (* de la forme | c -> ec avec sc la suite des cas *)
      (let ve = eval e envir in 
      match ve, c with
      | VCons(h1, q1), Cons(h2, q2) -> (* pour des listes *)
        (
        let (v1, envir1), ver1 = (try match_with_aux (Val(h1)) (Case(h2, Val(VUnit), EndCase)) envir envir_pattern, true with
        | NotMatched -> (* h1 et h2 ne correspondent pas, donc ce n'est pas le bon cas *)
          match_with_aux (Val(ve)) sc envir [], false)
        in (* si h1 et h2 correspondent, on teste q1 et q2 *)
        if ver1 then
          begin
            let (v2, envir2), ver2 = (try match_with_aux (Val(q1)) (Case(q2, Val(VUnit), EndCase)) envir envir_pattern, true with
            | NotMatched -> 
              match_with_aux (Val(ve)) sc envir [], false)
            in (* on a trouvé le bon pattern *)
            if ver2 then
              begin
                let envir_new = envir1 @ envir2 @ envir_pattern in
                eval ec (envir_new@envir), envir_new
              end
            else
              v2, envir2
          end
        else
          v1, envir1    
        )
      | VCouple(v1, v2), Couple(e1, e2) -> (* pour des couples *)
        (
        let (v1, envir1), ver1 = (try match_with_aux (Val(v1)) (Case(e1, Val(VUnit), EndCase)) envir envir_pattern, true with
        | NotMatched -> (* v1 et e1 ne correspondent pas *)
          match_with_aux (Val(ve)) sc envir [], false)
        in (* si v1 et e1 correspondent, on teste v2 et e2 *)
        if ver1 then
          begin
            let (v2, envir2), ver2 = (try match_with_aux (Val(v2)) (Case(e2, Val(VUnit), EndCase)) envir envir_pattern, true with
            | NotMatched -> (* v2 et e2 ne correspondent pas *)
              match_with_aux (Val(ve)) sc envir [], false)
            in
            if ver2 then
              begin
                let envir_new = envir1 @ envir2 @ envir_pattern in
                eval ec (envir_new@envir), envir_new
              end
            else
              v2, envir2
          end
        else
          v1, envir1
        )
      (* on traite ensuite les valeurs reconnaissables *)
      | VEmptyList, EmptyList ->
        eval ec envir, []
      | VInt(n1), Val(VInt(n2)) ->
        if n1 = n2 then
          eval ec envir, envir_pattern
        else
          match_with_aux (Val(ve)) sc envir []
      | VBool(n1), Val(VBool(n2)) ->
        if n1 = n2 then
          eval ec envir, envir_pattern
        else
          match_with_aux (Val(ve)) sc envir []
      | VUnit, Val(VUnit) ->
        eval ec envir, envir_pattern
      (* et on a le cas où on agrandit l'environnement *)
      | _, Var s ->
        eval ec ((Var s, ve) :: envir), (Var s, ve) :: envir_pattern
      | _, EmptyVar ->
        eval ec envir, envir_pattern
      | _, _ -> (* on a pas reconnu de pattern, on passe au suivant *)
        match_with_aux (Val(ve)) sc envir []
      )
    | _ ->
      raise NotExpected
  in
  let v, _ = match_with_aux e cases envir [] in
  v
