
(*

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

  -> (0, '*', 1) :: (0, '-', 3) :: ...


*)

(*Renvoie un tableau d taille nv_taille dont toutes les premières valeurs sont celles de t*)
let changetaille_tableau (t: 'a array) (nv_taille: int) (valeur_init: 'a): 'a array =
  let tableau_retour = Array.make nv_taille valeur_init in
  let n = min nv_taille (Array.length t) in (*Si la nouvelle taille est + petite, il faut remplir que nv_taille cases*)
  for i = 0 to (n-1) do
    tableau_retour.(i) <- t.(i)
  done;
  tableau_retour


(*Renvoie un automate reconnaissant le même langage que a auquel on ajoute le mot dont les lettres sont les éléments de w*)
let ajoute_mot_automate (a: automate) (w: char list): automate = 
  (*Renvoie la fonction de transitions qui comprend : les transitions de nvelles_transitions, et de nouvelles
  transitions permettant la lecture du mot ww à partir de l'état init, ainsi que le nombre d'états en comptant les nouveaux qui ont du être ajoutés, et l'état
  final sur lequel aboutit la lecture de w*)
  (*Si force_creation est à true, oblige à créer une nouvelle transition. C'est utile pour pouvoir ajouter
  des mots dont le début est déjà reconnu, mais la suite est nouvelle. Si la suite du mot contient des lettres
  déjà utilisées dans le début, on ne veut pas*)
  let rec ajoute_mot_transitions (nvelles_transitions: int -> char -> int option)
    (ww: char list) (init: int) (nb_etats: int): (int -> char -> int option)*int*int=
    match ww with
    | [] -> nvelles_transitions, nb_etats, init
    | c :: q -> (
      let etat_suiv = nvelles_transitions init c in
      if etat_suiv <> None then
        (*La transition partant de init avec la première lettre de ww existait déjà, donc on l'emprunte et on va
        chercher à créer les suivantes/voir si elles existent déjà*)
        ajoute_mot_transitions nvelles_transitions q (Option.get etat_suiv) nb_etats
      else ( 
        (*On crée un nouvel état et une nouvelle transition partant de init d'étiquette c*)
        print_string "j'ajoute la transition suivante :\n";
              print_int init; print_string " ";print_char c; print_string " "; print_int (nb_etats);print_newline ();
              
        ajoute_mot_transitions
          (fun etat etiquette ->    (*Fonction de transition qui comprend les transitions précédentes + la nouvelle*)
            match (etat,etiquette) with
            | x, y when (x=init)&&(y=c) -> 
              Some nb_etats
            | _ -> nvelles_transitions etat etiquette
          )
          q (nb_etats) (nb_etats+1)
      )
   )
  in
  (*0 est l'état initial, on considère qu'on lancera toujours cette fonction sur un automate d'état initial 0*)
  let (transitions_update, nb_etats, etat_final) = ajoute_mot_transitions a.transi w 0 (Array.length (a.ini)) in
  let nv_etats_finaux = changetaille_tableau a.final nb_etats false in
  let nv_etats_initiaux = changetaille_tableau a.ini nb_etats false in
  nv_etats_finaux.(etat_final) <- true;
  {
    ini = nv_etats_initiaux;
    transi = transitions_update;
    final = nv_etats_finaux
  }
  

(*Renvoie l'automate reconnaissant les mots reconnus par a et ceux de w_list*)
let rec ajoute_plusieurs_mots_automate (a:automate) (w_list: string list): automate =
  match w_list with
  | [] -> a
  | mot :: autres_mots -> ajoute_plusieurs_mots_automate (ajoute_mot_automate a (char_list_of_string mot)) autres_mots

(*Renvoie un automate reconnaissant tous les mots de la liste langage*)
let creer_automate_langage (langage : string list): automate =
  (*Création de l'automate reconnaissant le langage vide*)
  let autom = creer_automate 1 [0] [] (fun x y -> None) in
  ajoute_plusieurs_mots_automate autom langage


(*----------*)

(*lexème de couleur*)
type couleur_lex =
  | Bleu_l of string
  | Rouge_l of string
  | Blanc_l of string
  (*...*)

(*Les mêmes couleurs que couleur_l mais sans les paramètres*)
type couleur_vide =
  | Bleu_v
  | Rouge_v
  | Blanc_v

type automate_colore = automate * couleur_vide

(*Renvoie le lexème de couleur associé à c ayant comme paramètre w*)
let texte_colore (c: couleur_vide) (w: string): couleur_lex =
  match c,w with
  | Bleu_v, x -> Bleu_l  x
  | Rouge_v, x -> Rouge_l x
  | Blanc_v, x -> Blanc_l x

  
(*Même fonction que dans tipe.ml pour la lecture du markdown, permet de lire un facteur dans un mot w avec 1 automate,
mais renvoie la couleur donnée en argument, et renvoie None si il aucun facteur de w commençant à l'indice i n'était reconnu par a
(hl -> highlight pour les fonctions utilisées dans la coloration syntaxique)

Lit le mot w dans l'automate a à partir de l'indice i et en prenant comme état initial etat_ini, l'indice de la fin de
la lecture dans w
*)
let hl_lit_mot_couleur_fixe (a: automate) (etat_ini: int) (w: string) (i: int) (col: couleur_vide) : (couleur_lex*int) option =
  assert(a.ini.(etat_ini));
  let n = String.length w in

  (*lit le mot w à partir de l'indice ii en commençant à l'état etat_actuel (qui doit être un état initial de a),
    et renvoie l'indice final, qui sont initialisés en arguments*)
  (*final_ind correspond à l'indice dans le mot tq w[i .. final_ind] a l'exécution
  acceptante la plus récente rencontrée*)
  (*renvoie (lexème lu, indice du dernier caractère du facteur lu) *)
  let rec lit_aux (ii: int) (etat_actuel: int) (final_ind: int option)
      (etat_final_recent: int option) (mot_final_lu: char list) (mot_actuel_lu: char list): (couleur_lex*int) option=

    if ii >= n then (
      (*on est arrivé au bout de la lecture du mot*)
      if (final_ind != None) && (etat_final_recent != None) then
        Some (texte_colore col (String.of_seq (List.to_seq (List.rev mot_final_lu))) (*mot_final_lu lu en string*)
        , Option.get final_ind)
      else
        None
      )
    else
      let nv_etat = a.transi etat_actuel w.[ii] in
      if nv_etat = None then (
        (*on est arrivé au bout de l'exécution possible dans l'automate*)
        if (final_ind != None) && (etat_final_recent != None) then (*ne doit pas arriver*)
          Some (texte_colore col (String.of_seq (List.to_seq (List.rev mot_final_lu))) (*mot_final_lu lu en string*)
          , Option.get final_ind)
        else
          None
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


(*Chaque automate coloré présent dans les couples de all_automate va lire le mot w à partir de l'indice i jusqu'à s'arrêter, et on renverra la couleur associée à l'automate
qui a pu lire le plus loin possible dans w (ce lexème couleur prenant comme paramètre le mot lu dans w à partir de l'indice i), et son indice de fin de lecture dans w.
Lève une erreur si aucun mot ne peut être lu dans w à partir de i par les automates de all_automates.
Si plusieurs automates fournissent une couleur de même indice de fin de lecture, c'est celle donnée par le premier automate dans la liste qui est choisie. (priorité
aux automates devant dans la liste)

Ex : si a1 reconnait le mot "abc" et a2 reconnait le mot "efgh", on obtient les résultats-ci dessous pour les appels suivants :
  hl_lit_mot [(a1, Bleu_v); (a2, Rouge_v)] "abcdefgh" 0
  >> (Bleu_l "abc", 2)
  hl_lit_mot [(a1, Bleu_v); (a2, Rouge_v)] "abcdefgh" 4
  >> (Rouge_v "efgh", 7)
  hl_lit_mot [(a1, Bleu_v); (a2, Rouge_v)] "abcdefgh" 3
  >> ERREUR (car aucun mot reconnaissable ne commence par d) -> On s'assurera de ne jamais tomber dans ce cas là en appelant la fonction
*)
let hl_lit_mot (all_automates: automate_colore list) (w: string) (i: int): couleur_lex*int =
  let mots_lus = List.map (fun (a,coul_v) -> hl_lit_mot_couleur_fixe a 0 w i coul_v) all_automates in
  (*Renvoie le lexème de couleur et son indice de fin (dans la lecture du mot w) tel que cet indice est le plus grand parmi les éléments de l,
  les None étant ignorés, et provoque une erreur si il n'y a que des None car aucun mot n'aurait alors été lu.
  Si plusieurs lexèmes de couleur ont le même indice de lecture final, c'est le premier lu dans l qui est considéré
  max_ind et couleur_choisie servent de mémoire au fil des appels récursifs pour savoir quel est le meilleur vu jusqu'à présent*)
  let rec get_plus_long (l: ((couleur_lex*int) option) list) (max_ind: int) (couleur_choisie: couleur_lex option): couleur_lex*int = 
    (*Précondition : Si couleur_choisie = None, max_ind = -1*)
    if couleur_choisie = None then assert (max_ind = -1) else ();
    match l with
    | [] ->
      if couleur_choisie = None then
        failwith "Aucun lexème de couleur n'a été lu"
      else
        (Option.get couleur_choisie, max_ind)
    | None :: q -> get_plus_long q max_ind couleur_choisie
    | (Some (c,i)) :: q ->
      if i > max_ind then
        get_plus_long q i (Some c)
      else
        get_plus_long q max_ind couleur_choisie (*On est sûr que couleur_choisie != None ici car max_ind > i > -1, donc on ne garde pas un None*)
  in 
  get_plus_long mots_lus (-1) None


(*Renvoie la liste des lexèmes de couleurs obtenue en lisant le mot w avec les automates colorés de all_automates*)
let hl_texte_to_lexeme_list (all_automates: automate_colore list) (w: string): couleur_lex list =
  let n = String.length w in
  let rec hl_texte_to_lexeme_list_aux (ind_lecture: int) (output: couleur_lex list): couleur_lex list =
    if ind_lecture < n then
      let (col, ind_fin) = hl_lit_mot all_automates w ind_lecture in
      hl_texte_to_lexeme_list_aux (ind_fin+1) (col :: output)
    else
      List.rev output
  in
  hl_texte_to_lexeme_list_aux 0 []