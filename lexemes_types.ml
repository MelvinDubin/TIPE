type lexeme = 
  |Etoile_l
  |Texte_l of string 
  |Tiret_l  
  |Espace_l
  |Diese_l
  |SautLigne_l 
  |DeuxSautsLigne_l
  |Tab_l
  |Code_l of string

(*lexèmes après un pré traitement*)
type lexeme_t = 
  | Etoile_t
  | Texte_t of string 
  | Tiret_t
  | DeuxSautsLigne_t
  | SautLigne_t
  | Espace_t
  | ElementListe_t
  | Effet_t of (effet)*(lexeme_t list)
  | Gras_t of lexeme_t list
  | Italique_t of lexeme_t list
  | ListePuces_t of (lexeme_t list)
  | Diese_t
  | Titre_t of int*(lexeme_t list) (*l'entier = le niveau du titre*) 
  | Liste_imbriquee_t of lexeme_t list
  | Tab_t
  | Code_t of string

  
let rec print_lex (lex: lexeme_t): unit =
  match lex with
  | Etoile_t -> print_string "*"
  | Texte_t(t)  -> print_string ("\""^t^"\"")
  | Tiret_t -> print_string "-"
  | DeuxSautsLigne_t -> print_string "deuxsautslignes"
  | SautLigne_t -> print_string "sautligne"
  | Espace_t  -> print_string " "
  | ElementListe_t -> print_string "eltliste"
  | Gras_t(l) -> (print_string "gras("; print_lex_list l; print_string ")")
  | Italique_t(l) -> (print_string "italique("; print_lex_list l; print_string ")")
  | Diese_t -> print_string "diese"
  | Titre_t(n, l) -> (print_string "titre(niv:"; print_int n; print_char ','; print_lex_list l; print_string ")")
  | Liste_imbriquee_t(l) -> (print_string "LI("; print_lex_list l; print_string ")")
  | Code_t(t) -> (print_string "Code(";print_string t; print_string ")" )
  | _ -> failwith "pas assigné"

and print_lex_list (l: lexeme_t list): unit =
  match l with
  | [] -> print_string "";
  | x :: q -> print_lex x ; print_string";"; print_lex_list q