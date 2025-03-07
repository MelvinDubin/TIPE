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
    | 0, _ -> Some 2
    | 2, x when not (List.mem x ['*'; '-'; ' '; '\n'; '\r'; '#']) -> Some 2
    | 4, ' ' -> Some 4 
    | 5, '\n' -> Some 6
    | 5, '\r' -> Some 8
    | 6, '\n' -> Some 6
    | 6, '\r' -> Some 8
    | 7, '\n' -> Some 5
    | 8, '\n' -> Some 6
    | _ -> None
  in
  let autom = creer_automate 10 [0] [1;2;3;4;5;6;9] transitions in
  (*
    let testi, testf = lit_mot autom t 0 in
    print_int testi; print_string "  "; print_int testf;print_newline ();
  *)
  let n = String.length t in
  let curseur = ref 0 in (*indique à quel caractère du texte t on en est*)
  let lex_list = ref [] in

  while !curseur < n do
    let lexeme_lu, indice_fin_lecture = lit_mot autom 0 t !curseur in
    curseur := indice_fin_lecture + 1;
    lex_list := lexeme_lu :: !lex_list;
  done;
  List.rev !lex_list

(*Fonction auxiliaire pour traiter les lexèmes bruts pour faciliter ensuite la transformation en arbre de syntaxe*)
(*l est la liste de lexèmes d'entrée, et l_t est la liste de sortie renversée, accumulateur*)
let rec pretraitement_lexeme_list_aux (l: lexeme list) (l_t: lexeme_t list): lexeme_t list =
  match l with
  | [] -> List.rev l_t
  | SautLigne_l :: Etoile_l :: Espace_l :: q -> pretraitement_lexeme_list_aux q (ElementListe_t :: l_t)
  | DeuxSautsLigne_l :: Etoile_l :: Espace_l :: q -> pretraitement_lexeme_list_aux q (ElementListe_t :: l_t)
  | Etoile_l :: q -> pretraitement_lexeme_list_aux q (Etoile_t :: l_t)
  | Texte_l t :: q -> pretraitement_lexeme_list_aux q (Texte_t t :: l_t)
  | SautLigne_l :: Tiret_l :: Espace_l :: q -> pretraitement_lexeme_list_aux q (ElementListe_t :: l_t)
  | DeuxSautsLigne_l :: Tiret_l :: Espace_l :: q -> pretraitement_lexeme_list_aux q (ElementListe_t :: l_t)
  | Tiret_l :: q -> pretraitement_lexeme_list_aux q (Tiret_t :: l_t)
  | Espace_l :: q -> pretraitement_lexeme_list_aux q (Espace_t :: l_t)
  | SautLigne_l :: q -> pretraitement_lexeme_list_aux q (SautLigne_t :: l_t)
  | DeuxSautsLigne_l :: q -> pretraitement_lexeme_list_aux q (DeuxSautsLigne_t :: l_t)
  | Diese_l :: q -> pretraitement_lexeme_list_aux q (Diese_t :: l_t)

(*Renvoie la liste de lexèmes traités obtenue en remplaçant les séquences
de # pour faire un titre par le lexème Titre_t(i, sl) où i est le niveau du titre
obtenu avec le nombre de #, et sl les lexèmes lus dans ce titre,
/!\ si ## n'est pas suivi d'un espace ou d'un saut de ligne, ce n'est pas un titre mais du texte*)
let transforme_dieses_titre (l: lexeme_t list): lexeme_t list =
  (*Renvoie le nombre de dièses consécutifs commençant la liste ll (en commençant à compteur),
  et la liste venant après*)
  
  let rec compte_diese (ll: lexeme_t list) (compteur: int): int*(lexeme_t list) =
    match ll with
    | Diese_t :: q -> compte_diese q (compteur + 1)
    | _ -> (compteur, ll)
  in
  (*Renvoit la liste des lexèmes de ll jusqu'au premier SautLigne ou DeuxSautsLigne (ou fin de ll si
  il n'y en a aucun), 
  en remplaçant au passage les # par du Texte_t et non des Diese_t (la liste qui sera renvoyée
  et stockée à l'envers par le paramètre ll_t),
  ainsi que la liste restante après la lecture de ceux-ci sans compter le saut de ligne final*)
  let rec lexemes_ligne_sansdiese (ll: lexeme_t list) (ll_t: lexeme_t list): (lexeme_t list)*(lexeme_t list) =
    match ll with
    | SautLigne_t :: q -> (List.rev ll_t, q)
    | DeuxSautsLigne_t :: q -> (List.rev ll_t, q)
    | [] -> (List.rev ll_t, [])
    | Diese_t :: _ ->
      let (n, reste) = compte_diese ll 0 in
      lexemes_ligne_sansdiese reste (Texte_t (String.make n '#') :: ll_t)
    | x :: q -> lexemes_ligne_sansdiese q (x :: ll_t)
  in

  let rec transfo_diese (ll: lexeme_t list) (ll_t: lexeme_t list): lexeme_t list =
    (*print_string "--\n";
    print_lex_list ll; print_newline ();
    print_lex_list ll_t; print_newline ();*)
    match ll with
    | z :: Diese_t :: q  when (z=SautLigne_t || z=DeuxSautsLigne_t)->
      begin
      let (niv, reste) = compte_diese (Diese_t :: q) 0 in
      match reste with
      | x :: q
      when ((List.mem x [Espace_t; SautLigne_t; DeuxSautsLigne_t]) && niv <= 6) ->
        (*C'est un titre*)
        if x = Espace_t then
          let (contenu_titre, suite) = lexemes_ligne_sansdiese q [] in
          transfo_diese (DeuxSautsLigne_t :: suite) ((Titre_t (niv, contenu_titre)) :: ll_t)
        else
          (*Le titre est vide*)
          transfo_diese (DeuxSautsLigne_t :: q) ((Titre_t (niv, [])) :: ll_t)
      | _ ->
        (*Pas un titre*)
        transfo_diese reste (Texte_t (String.make niv '#') :: ll_t)
      end
    | x :: q -> transfo_diese q (x :: ll_t)
    | [] -> List.rev ll_t
  in 
  transfo_diese l []

let pretraitement_lexeme (l: lexeme list): lexeme_t list =
  transforme_dieses_titre (DeuxSautsLigne_t :: (pretraitement_lexeme_list_aux l []))
