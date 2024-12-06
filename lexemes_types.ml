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
  | ListePuces_t of (lexeme_t list list) 