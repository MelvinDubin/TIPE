(* a beseoin de : lexemes_types.ml*)


let etat_to_lexeme (e: int) (w: string): lexeme =
  match e with
  | 1 -> Etoile_l
  | 2 -> Texte_l w
  | 3 -> Tiret_l
  | 4 -> Espace_l
  | 5 -> SautLigne_l
  | 6 -> DeuxSautsLigne_l
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
    | 0, _ -> Some 2
    | 2, x when not (List.mem x ['*'; '-'; ' '; '\n']) -> Some 2
    | 4, ' ' -> Some 4 
    | 5, '\n' -> Some 6
    | 6, '\n' -> Some 6
    | _ -> None
  in
  let autom = creer_automate 7 [0] [1;2;3;4;5;6] transitions in
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


let pretraitement_lexeme (l: lexeme list): lexeme_t list =
  SautLigne_t :: (pretraitement_lexeme_list_aux l [])

(*Lit les lignes de file en modifiant par effet de bord la liste all_lines pour lui ajouter les lignes lues au fur et à mesure,
et ferme le fichier*)
let lit_fichier (filename: string): string =
  let file = open_in filename in
  let taille_tot = in_channel_length file in
  let s = Bytes.create taille_tot in
  really_input file s 0 taille_tot;
  close_in file;
  Bytes.unsafe_to_string s

