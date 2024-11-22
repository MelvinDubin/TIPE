(*fichier à utiliser avec : 
- lexemes_types.ml*)

(*types d'effet appliqués à un texte*)
type effet_texte =
  |Italique
  |Gras
  |EffetVide 
  |Liste_puces
  |Liste_numerotee
  |Element_liste
  |Paragraphe 
  |A_implementer

(*noeuds pour un arbre de texte (html ou markdown)*)
type traitement_texte = 
  (*feuilles*)
  |Texte of string   
  (*noeuds internes*)
  |Effet of effet_texte 

(*utilisé pour lire le gras et l'italique : noeuds pour un arbre intermediaire contenant des effets textes mais encore des listes de lexemes*)
type intermediaire = 
  (*feuilles*)
  |Lexeme_list of (lexeme_t list)    
  (*noeuds internes*)
  |Effet_intermediaire of effet_texte 

(* arbre généraux *)
type 'a arbre = 
  |Feuille of 'a 
  |Noeud of 'a*('a arbre list)