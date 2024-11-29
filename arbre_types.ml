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


(* arbre généraux *)
type 'a arbre = 
  |Feuille of 'a 
  |Noeud of 'a*('a arbre list)