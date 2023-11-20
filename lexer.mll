{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | '+'                                               { PLUS }
  | '*'                                               { TIMES }
  | '-'                                               { MINUS }
  | '('                                               { LPAREN }
  | ')'                                               { RPAREN }
  | ['0'-'9']+                                   as s { INT (int_of_string s) }
  | "/"                                               { DIV }
  | ">="                                              { SUPEG }
  | ">"                                               { SUP }
  | "<="                                              { INFEG }
  | "<"                                               { INF }
  | "<>"                                              { DIFF }
  | "!="                                              { DIFF }
  | "="                                               { EG }
  | "if"                                              { IF }
  | "then"                                            { THEN }
  | "else"                                            { ELSE }
  | "true"                                            { TRUE }
  | "false"                                           { FALSE }
  | "||"                                              { OR }
  | "&&"                                              { AND }
  | "not"                                             { NOT }
  | "prInt"                                           { PRINT }
  | "let"                                             { LET }
  | "rec"					                                    { REC }
  | "in"                                              { IN }
  | "fun"                                             { FUN }
  | "->"                                              { FLECHE }
  | "ref"                                             { REF }
  | ":="                                              { MAJ }
  | ";"                                               { PTVIRG }
  | "_"                                               { EMPTY }
  | "!"                                               { PTEXCLM }
  | "for"                                             { FOR }
  | "to"                                              { TO }
  | "do"                                              { DO }
  | "done"                                            { DONE }
  | "while"                                           { WHILE }
  | "and"                                             { ANDREC }
  | "begin"                                           { BEGIN }
  | "end"                                             { END }
  | ","                                               { VIRGULE }
  | "["                                               { LCROCH }
  | "]"                                               { RCROCH }
  | "::"                                              { CONS }
  | "match"                                           { MATCH }
  | "with"                                            { WITH }
  | "|"                                               { PIPE }
  | "function"                                        { FUNCTION }
  | ['a'-'z']+(['a'-'z']*['0'-'9']*['A'-'Z']*)* as x  { VAR x }
  | eof                                               { EOF } (* fin du fichier *)
