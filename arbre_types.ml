(*fichier à utiliser avec : 
- lexemes_types.ml*)

(*types d'effet appliqués à un texte*)
type effet =
  |Italique
  |Gras
  |Font of (string option*int option* string option) (*premier string : couleur ; entier : taille ; deuxième string : identifiant*)
  |Cliquable of string (*lien sur lequel aller*)
  |EffetVide 
  |A_implementer

(*textes et effets avec du texte, pas de retour à la ligne autorisé*)
type texte = 
  |Texte_nu of string 
  |Texte_effet of (effet*(texte list))

type bloc = 
  |Texte of (texte list) (*retour à la ligne entre chaque élément de liste*)
  |ListeNumerotee of bloc list 
  |ListeAPuces of bloc list
  (*|Tableau
  |Code*) (*a implementer*) 

type titre = (int*texte) (*l'entier correspond au niveau du titre, en markdown, le nombre de # et html <h...>*)
type division =
  |Section of titre*(division list)
  |Paragraphe of bloc list 
  |Barre

type doc = division list 