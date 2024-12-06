type lexeme = 
  |Etoile_l
  |Texte_l of string 
  |Tiret_l  
  |Espace_l 
  |SautLigne_l 
  |DeuxSautsLigne_l

(*lexèmes après un pré traitement*)
type lexeme_t = 
  | Etoile_t
  | Texte_t of string 
  | Tiret_t
  | DeuxSautsLigne_t
  | SautLigne_t
  | Espace_t 
  | ElementListe_t
  | Gras_t of lexeme_t list
  | Italique_t of lexeme_t list
  
let rec print_lex (lex: lexeme_t): unit =
  match lex with
  | Etoile_t -> print_string "*"
  | Texte_t(t)  -> print_string ("\""^t^"\"")
  | Tiret_t -> print_string "-"
  | DeuxSautsLigne_t -> print_string "deuxsautslignes"
  | SautLigne_t -> print_string "sautligne"
  | Espace_t  -> print_string " "
  | ElementListe_t -> print_string "eltliste"
  | Gras_t(l) -> print_string "gras("; print_lex_list l; print_string ")"
  | Italique_t(l) -> print_string "italique("; print_lex_list l; print_string ")"

and print_lex_list (l: lexeme_t list): unit =
  match l with
  | [] -> print_string "";
  | x :: q -> print_lex x ; print_string";"; print_lex_list q