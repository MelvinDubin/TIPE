(* a beseoin de : lexemes_types.ml*)


let etat_to_lexeme (e: int) (w: string): lexeme =
  match e with
  | 1 -> Etoile_l
  | 2 -> Texte_l w
  | 3 -> Tiret_l
  | 4 -> Espace_l
  | 5 -> SautLigne_l
  | 6 -> DeuxSautsLigne_l
  | 9 -> Diese_l
  | 10 -> Tab_l
  | 11 -> Texte_l "`"
  | 12 -> Texte_l "``"
  | 13 -> Code_l (String.sub w 3 ((String.length w) - 3 )) (*On retire les ``` du début*)
  | 14 -> Code_l (String.sub w 3 ((String.length w) - 3 ))
  | 15 -> Code_l (String.sub w 3 ((String.length w) - 3 ))
  | 16 -> Code_l (String.sub w 3 ((String.length w) - 3 ))
  | 17 -> Code_l (String.sub w 3 ((String.length w) - 7 )) (*On retire aussi le \n``` de fin*)
  | _ -> failwith "cet état ne correspond pas à un lexème"
  

(*lit le mot w dans l'automate a à partir de l'indice i et en prenant comme état initial etat_ini, renvoie le
lexeme correspondant au facteur de w qui a été lu, et l'indice de la fin de
la lecture dans w*)
let lit_mot (a: automate) (etat_ini: int) (w: string) (i: int): lexeme*int =
  assert(a.ini.(etat_ini));
  let n = String.length w in

  (*lit le mot w à partir de l'indice ii en commençant à l'état etat_actuel (qui doit être un état initial de a),
    et renvoie le lexeme final et l'indice final, qui sont initialisés en arguments*)
  (*final_ind correspond à l'indice dans le mot tq w[i .. final_ind] a l'exécution
  acceptante la plus récente rencontrée*)
  (*renvoie (lexème lu, indice du dernier caractère du facteur lu) *)
  let rec lit_aux (ii: int) (etat_actuel: int) (final_ind: int option)
      (etat_final_recent: int option) (mot_final_lu: char list) (mot_actuel_lu: char list): lexeme*int =

    if ii >= n then (
      (*on est arrivé au bout de la lecture du mot*)
      assert((final_ind != None) && (etat_final_recent != None)); (*ne doit pas arriver*)
      (etat_to_lexeme (Option.get etat_final_recent)
         (String.of_seq (List.to_seq (List.rev mot_final_lu))) (*mot_final_lu lu en string*)
      , Option.get final_ind)
    )
    else
      let nv_etat = a.transi etat_actuel w.[ii] in
      if nv_etat = None then (
        (*on est arrivé au bout de l'exécution possible dans l'automate*)
        assert((final_ind != None) && (etat_final_recent != None)); (*ne doit pas arriver*)
        (etat_to_lexeme (Option.get etat_final_recent)
           (String.of_seq (List.to_seq (List.rev mot_final_lu))) (*mot_final_lu lu en string*)
        , Option.get final_ind)
      )
      else
        if a.final.(Option.get nv_etat) then
          (*le nouvel état est l'état final le plus récemment rencontré // nv_etat est ici != None*)
          lit_aux (ii+1) (Option.get nv_etat) (Some ii) (nv_etat)
            (w.[ii]::mot_actuel_lu) (w.[ii]::mot_actuel_lu)
        else
          (*on n'a pas de nouvel état final pour mettre à jour le plus récent*)
          lit_aux (ii+1) (Option.get nv_etat) (final_ind) (etat_final_recent)
            mot_final_lu (w.[ii]::mot_actuel_lu)
  in
  lit_aux i etat_ini None None [] []



let texte_to_lexeme_list (t: string): lexeme list =
  let transitions (etat: int) (c:char): int option=
    match (etat, c) with
    | 0, '*' -> Some 1
    | 0, '-' -> Some 3
    | 0, ' ' -> Some 4 
    | 0, '\n' -> Some 5
    | 0, '\r' -> Some 7
    | 0, '#' -> Some 9
    | 0, '\t' -> Some 10
    | 0, '`' -> Some 11
    | 0, _ -> Some 2
    | 2, x when not (List.mem x ['*'; '-'; ' '; '\n'; '\r'; '#']) -> Some 2
    | 4, ' ' -> Some 4 
    | 5, '\n' -> Some 6
    | 5, '\r' -> Some 8
    | 6, '\n' -> Some 6
    | 6, '\r' -> Some 8
    | 7, '\n' -> Some 5
    | 8, '\n' -> Some 6
    | 11, '`' -> Some 12
    | 12, '`' -> Some 13
    | 13, '\n' -> Some 14
    | 13, _ -> Some 13
    | 14, '`' -> Some 15
    | 14, _ -> Some 13
    | 15, '`' -> Some 16
    | 15, _ -> Some 13
    | 16, '`' -> Some 17  
    | _ -> None
  in
  let autom = creer_automate 18 [0] [1;2;3;4;5;6;9;10;11;12;13;14;15;16;17] transitions in
  let n = String.length t in
  let curseur = ref 0 in (*indique à quel caractère du texte t on en est*)
  let lex_list = ref [] in

  while !curseur < n do
    let lexeme_lu, indice_fin_lecture = lit_mot autom 0 t !curseur in
    curseur := indice_fin_lecture + 1;
    lex_list := lexeme_lu :: !lex_list;
  done;
  List.rev !lex_list

(*Renvoie le nombre de lex_compte consécutifs commençant la liste ll (en commençant à compteur),
  et la liste venant après
  lex_compte peut être de n'importe quel type pour pouvoir tout autant compter les lexeme_l que les lexeme_t*)
let rec compte_lexeme (lex_compte: 'a) (ll: 'a list) (compteur: int): int*('a list) =
  match ll with
  | lex_t :: q when lex_t = lex_compte -> compte_lexeme lex_compte q (compteur + 1)
  | _ -> (compteur, ll)

(*Fonction auxiliaire pour traiter les lexèmes bruts de lexlist pour faciliter ensuite la transformation en arbre de syntaxe*)
let pretraitement_lexeme_list_aux (lexlist: lexeme list): (lexeme_t list)*bool =
  let has_code = ref false in (*est à Vrai si il y a un Code_t à au moins un endroit dans la liste de lexèmes*)
  let rec aux_modifie_hascode (l: lexeme list) (l_t: lexeme_t list): lexeme_t list =
    match l with
    | [] -> List.rev l_t

    (*Liste de niveau 0, sans tab devant*)
    | saut_ligne_lex :: marqueur_liste :: Espace_l :: q
      when ((marqueur_liste = Etoile_l) || (marqueur_liste = Tiret_l))
      &&((saut_ligne_lex = SautLigne_l) || (saut_ligne_lex = DeuxSautsLigne_l)) -> aux_modifie_hascode q (ElementListe_t :: l_t)
    | Etoile_l :: q -> aux_modifie_hascode q (Etoile_t :: l_t)
    | Texte_l t :: q -> aux_modifie_hascode q (Texte_t t :: l_t)
    | Tiret_l :: q -> aux_modifie_hascode q (Tiret_t :: l_t)
    | Espace_l :: q -> aux_modifie_hascode q (Espace_t :: l_t)
    | SautLigne_l :: q -> aux_modifie_hascode q (SautLigne_t :: l_t)
    | DeuxSautsLigne_l :: q -> aux_modifie_hascode q (DeuxSautsLigne_t :: l_t)
    | Diese_l :: q -> aux_modifie_hascode q (Diese_t :: l_t)
    | Tab_l :: q -> aux_modifie_hascode q (Tab_t :: l_t)
    | Code_l t :: q -> (has_code:= true; aux_modifie_hascode q (Code_t t :: l_t))
  in (aux_modifie_hascode lexlist [], !has_code)

(*Renvoie la liste de lexèmes traités obtenue en remplaçant les séquences
de # pour faire un titre par le lexème Titre_t(i, sl) où i est le niveau du titre
obtenu avec le nombre de #, et sl les lexèmes lus dans ce titre,
/!\ si ## n'est pas suivi d'un espace ou d'un saut de ligne, ce n'est pas un titre mais du texte*)
let transforme_dieses_titre (l: lexeme_t list): lexeme_t list =
  
  (*Renvoit la liste des lexèmes de ll jusqu'au premier SautLigne, DeuxSautsLigne, ou ElementListe (ou fin de ll si
  il n'y en a aucun), 
  en remplaçant au passage les # par du Texte_t et non des Diese_t (la liste qui sera renvoyée
  et stockée à l'envers par le paramètre ll_t),
  ainsi que la liste restante après la lecture de ceux-ci sans compter le saut de ligne final*)
  let rec lexemes_ligne_sansdiese (ll: lexeme_t list) (ll_t: lexeme_t list): (lexeme_t list)*(lexeme_t list) =
    match ll with
    | SautLigne_t :: q -> (List.rev ll_t, q)
    | DeuxSautsLigne_t :: q -> (List.rev ll_t, q)
    | ElementListe_t :: q -> (List.rev ll_t, ll) (*Si on coupe sur un début de liste, ça veut dire que le paragraphe commence par une liste, et on garde donc l'indicateur*)
    | [] -> (List.rev ll_t, [])
    | Diese_t :: _ ->
      let (n, reste) = compte_lexeme Diese_t ll 0 in
      lexemes_ligne_sansdiese reste (Texte_t (String.make n '#') :: ll_t)
    | x :: q -> lexemes_ligne_sansdiese q (x :: ll_t)
  in

  let rec transfo_diese (ll: lexeme_t list) (ll_t: lexeme_t list): lexeme_t list =
    match ll with
    | z :: Diese_t :: q  when (z=SautLigne_t || z=DeuxSautsLigne_t)->(
      let (niv, reste) = compte_lexeme Diese_t (Diese_t :: q) 0 in
      match reste with
      | Espace_t :: q
      when (niv <= 6) ->
        (*C'est un titre*)
        let (contenu_titre, suite) = lexemes_ligne_sansdiese q [] in
        let vraie_suite = (
          match suite with
          | Diese_t ::_ -> DeuxSautsLigne_t :: suite
          | _ -> suite
        ) in
        transfo_diese (vraie_suite) ((Titre_t (niv, contenu_titre)) :: ll_t)
      | _ ->
        (*Pas un titre*)
        transfo_diese reste (Texte_t (String.make niv '#') :: ll_t)
    )
    | x :: q -> transfo_diese q (x :: ll_t)
    | [] -> List.rev ll_t
  in 
  transfo_diese l []

(*Applique la transformation en lexeme_t (repérage des tirets de Liste valides) ainsi que le groupement sous un lexème
Titre_t des lexèmes formant des titres. Renvoie la liste ainsi formée, ainsi qu'un booléen indiquant si il y a un
bloc de code quelque part (pour savoir si il faut le fichier .css)*)
let pretraitement_lexeme (l: lexeme list): (lexeme_t list) * bool=
  let traitement1, has_code = pretraitement_lexeme_list_aux (DeuxSautsLigne_l :: l) in
  (transforme_dieses_titre traitement1, has_code)
