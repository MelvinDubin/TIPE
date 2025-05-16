(*a ouvrir avec arbre_type.ml*)

(*ajoute la couleur colors[i-1] sur les titres de niveau i dans le document d, None s'il  n'y a pas de couleur*)
let ajoute_couleurs_titres (d : doc) (colors : (string option) array) : doc =
  
  let rec traite_titre (txt : texte) (i : int) : texte = 
    match txt with 
    |Texte_effet (Font(_,taille,id), l) -> Texte_effet(Font (colors.(i-1),taille,id), l)
    |Texte_effet (e,[x]) -> Texte_effet(e, [traite_titre x i]) (*si on a qu'un seul élément dans les noeuds, on peut toujours trouver un "font" plus tard qui englobe tout le texte brut*)
    |_ -> Texte_effet(Font (colors.(i-1),None,None), [txt])

  in

  let rec traite_division (div : division) : division  = 
    match div with 
    |Section ((i,txt),(sous_divisions : division list)) -> (
      if (colors.(i-1) = None) then 
        (Section((i, txt), List.map traite_division sous_divisions))
      else 
        (Section((i, traite_titre txt i), List.map traite_division sous_divisions))
    )
    |Paragraphe _ -> div 
    |Barre -> div 
  in 
  List.map traite_division d 



(*renvoie la chaîne de caractères contenant 2i fois &nbsp, équivalent à i tab une fois visualisé en html*)
let rec cree_chaine_tab (i : int) : string = 
  match i with 
  |0 -> ""
  |_ -> "&nbsp&nbsp"^(cree_chaine_tab (i-1))

(*enleve tous les effets appliqués au texte t*)
let rec enleve_effet (t : texte) : string =
  match t with 
  |Texte_nu str -> str 
  |Texte_effet (e,l) -> List.fold_left (fun accu x -> accu^(enleve_effet x)) "" l 

(*ajoute au document d un sommaire contenant les titres de niveaux supérieurs au niveau niveau_min*)
let ajoute_sommaire (d : doc) (niveau_min : int) = 

  let profondeur_titre = Array.make (niveau_min) 0 in


  let cree_identifiant (niveau : int) : string =
    (*cree l'identifiant d'un titre de niveau i, en fonction de sa profondeur dans l'arbre de syntaxe : par exemple, le deuxième titre de niveau 2 placé sous le troisième titre de niveau 1 donnera titre_3_2*)
      profondeur_titre.(niveau-1) <- profondeur_titre.(niveau-1) + 1 ; 
      for i = (niveau) to (niveau_min-1) do 
        profondeur_titre.(i) <- 0 
      done ; 
      let identifiant = ref "titre" in 
      for j = 0 to (niveau-1) do 
        identifiant:= !identifiant^"_"^(string_of_int profondeur_titre.(j))
      done ; 
      !identifiant
  in 
  let rec ajoute_ref_titre (i : int) (t : texte) (id : string): texte*texte =
    (*ajoute l'identifiant id au texte t, s'il n'a pas déjà une référence
    renvoie le nouveau texte du titre ainsi créé, ainsi que le texte qui apparaîtra dans le sommaire*)
    match t with 
    |Texte_nu str -> (Texte_effet(Font (None, None, Some id), [t]),Texte_effet(EffetVide, [Texte_nu (cree_chaine_tab (i-1));Texte_effet(Cliquable ("#"^id),[Texte_nu (enleve_effet t)])]))
    |Texte_effet(Font (couleur,taille,None),sous_textes) -> (Texte_effet(Font (couleur, taille, Some id), sous_textes),Texte_effet(EffetVide, [Texte_nu (cree_chaine_tab (i-1));Texte_effet(Cliquable ("#"^id),[Texte_nu (enleve_effet t)])]))
    |Texte_effet(Font (_,_,Some autre_id),sous_textes) -> (t,Texte_effet(EffetVide, [Texte_nu (cree_chaine_tab (i-1));Texte_effet(Cliquable ("#"^autre_id),[Texte_nu (enleve_effet t)])]))
    |Texte_effet(e,l) -> 
      match l with 
      |[x] -> (*s'il n'y a qu'un élément, alors le font qui englobe tout le texte est peut-être plus tard, sinon on peut le placer maintenant*)
        let nouveau_titre,texte_sommaire= ajoute_ref_titre i x id in 
        (Texte_effet(e,[nouveau_titre]),texte_sommaire)
      |_ -> (Texte_effet(Font (None, None, Some id), [t]),Texte_effet(EffetVide, [Texte_nu (cree_chaine_tab (i-1));Texte_effet(Cliquable ("#"^id),[Texte_nu (enleve_effet t)])]))
  in 


  let rec trouve_titres (div : division) : (texte list)*division= 
    (*parcours de l'arbre de syntaxe afin de trouver les titres et de leur ajouter un identifiant s'ils n'en ont pas
    renvoie le nouvel arbre ainsi créé, et la liste des textes correspondant aux titres dans le sommaire*)
    match div with 
    |Section ((i,t),sous_divisions) -> if (i<=niveau_min) then
      (
      let id = cree_identifiant i in 
      let new_t,txt_sommaire = ajoute_ref_titre i t id in 
      let liste_titres, arbre = List.fold_left_map 
        (fun accu x -> 
        let titres_de_x, arbre_de_x = trouve_titres x in 
        (accu@titres_de_x,arbre_de_x))
        [txt_sommaire]
        sous_divisions 
        in 
        (liste_titres,Section((i,new_t),arbre))
      ) else ([],div)
    |Paragraphe _ -> ([],div)
    |Barre -> ([],div)
  in 

  let liste_titres,nouveau_doc= List.fold_left_map 
    (fun accu x -> 
    let titres_de_x, arbre_de_x = trouve_titres x in 
    (accu@titres_de_x,arbre_de_x))
    []
    d
  in 

  let sommaire = Section ((2,Texte_effet (Gras, [Texte_nu "Sommaire :"])), [Paragraphe [Texte liste_titres];Barre]) in
  sommaire::nouveau_doc


(*ajoute le titre contenu dans str (écrit en markdown), au début du document d*)
let ajoute_titre (d : doc) (str : string) = 
  (*on relance une analyse lexicale sur le str*)
  let titre = lexemeliste_to_arbre_syntaxe (let l,_ = pretraitement_lexeme (texte_to_lexeme_list str) in l) in 
  titre@d

(*applique la fonction f sur tous les textes nus de d (but : détecter les raccourcis ajoutés par l'utilisateur et les transformer en effet)*)
let raccourcis_to_effet (d : doc) (f : texte -> texte) = 

  let rec transforme_texte (t : texte) : texte = 
    print_string("boucle dans transforme_texte\n"); 
    match t with 
    |Texte_nu _ -> f t 
    |Texte_effet (e,l) -> Texte_effet(e, List.map transforme_texte l)
  in 
  let rec transforme_bloc (b : bloc) : bloc = 
    print_string("boucle dans transforme_bloc\n"); 
    match b with 
    |Texte l -> Texte (List.map transforme_texte l)
    |ListeNumerotee l -> ListeNumerotee (List.map transforme_bloc l)
    |ListeAPuces l -> ListeAPuces (List.map transforme_bloc l)
  in
  let rec transforme_division (div : division) : division =
    
    print_string("boucle dans transforme_division\n"); 
    match div with 
    |Section ((i,t),l) -> Section((i,transforme_texte t), List.map transforme_division l)
    |Paragraphe l -> Paragraphe (List.map transforme_bloc l)
    |Barre -> Barre
  in 
  List.map transforme_division d