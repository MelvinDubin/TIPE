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

(*ajoute au document d un sommaire contenant les titres de niveaux supérieurs au niveau niveau_min*)
let ajoute_sommaire (d : doc) (niveau_min : int) = 

  (*renvoie la liste de titres de d dans l'ordre d'un parcours en profondeur depuis les fils gauches*)
  let rec trouve_titres (d_aux : doc) : titres list = 
    match d_aux with 
    |Section ((i,t),sous_divisions) -> if (i>=niveau_min) then
      (List.fold_left (fun accu l -> accu@l) [(i,t)] trouve_titres sous_divisions)
      else ([])
    |Paragraphe _ -> []
    |Barre -> []
  in 
  let liste_titres = trouve_titres d in
  let compteurs = Array.make 6 0 in (*compte le nombre de titres pour ajouter les numéros devant le sommaire (chaque case correspond au numéro de chaque niveau de titres)*)

  let titre_to_texte2 (t : titre) : texte = 
    let i,txt = t in 
    compteurs.(j) <- compteurs.(j) + 1; 
    for j = (i+1) to 5 do 
      compteurs.(j) <- 0 
    done ; 
    Texte [Texte_effet (Taille i, [Texte_nu (string_of_int(compteurs.(j))^". "), txt])]
  in
 
  let titres : bloc list = List.map titre_to_texte liste_titres in 
  let sommaire = Section ((6,Texte_effet (Gras, [Texte_nu "Sommaire :"])), [Paragraphe titres]) in
  sommaire::doc 

