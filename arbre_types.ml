(*fichier à utiliser avec : 
- lexemes_types.ml*)

(*types d'effet appliqués à un texte*)
type effet =
  |Italique
  |Gras
  |EffetVide 
  |A_implementer

(*textes et effets avec du texte*)
type texte = 
  |Texte_nu of string 
  |Texte_effet of (effet*(texte list))

type bloc = 
  |Texte of texte
  |ListeNumerotee of bloc list 
  |ListeAPuces of bloc list
  (*|Tableau
  |Code*) (*a implementer*) 

type titre = (int*texte) (*l'entier correspond au niveau du titre, en markdown, le nombre de # et html <h...>*)
type division =
  |Section of titre*division list
  |Paragraphe of bloc list 
  |Barre