(*a ouvrir avec arbre_type.ml*)

(*ajoute la couleur colors[i-1] sur les titres de niveau i dans le document d, None s'il  n'y a pas de couleur*)
let ajoute_couleurs_titres (d : doc) (colors : string array) : doc = 
  let rec traite_division (div : division) : division  = 
    match div with 
    |Section ((i,txt),(sous_divisions : division list)) -> (
      if (colors.(i-1) = "None") then 
        (Section((i, txt), List.map traite_division sous_divisions))
      else 
        (Section((i, Texte_effet (Couleur (colors.(i-1)), [txt])), List.map traite_division sous_divisions))
    )
    |Paragraphe _ -> div 
    |Barre -> div 
  in 
  List.map traite_division d 

