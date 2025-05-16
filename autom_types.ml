type automate = {
  ini : bool array;
  transi : int -> char -> (int option);
  final : bool array (*final.(i) indique si i est final*)
}


(*Renvoie un automate dont les états sont les entiers de 0 à nb_etats-1,
dont le tableau des états initiaux vaut 1 sur les indices de etats_init,
le tableau des états finaux vaut 1 sur les indices de etats_finaux,
et sa fonction de transitions est delta*)
let creer_automate (nb_etats: int) (etats_init: int list) (etats_finaux: int list) (delta:  int -> char -> (int option)): automate =
  let tab_ini = Array.make nb_etats false in
  let tab_final = Array.make nb_etats false in
  (*met la case i de t à true*)
  let met_a_jour_tab (t: bool array) (i: int): unit =
    t.(i) <- true
  in
  List.iter (met_a_jour_tab tab_ini) etats_init;
  List.iter (met_a_jour_tab tab_final) etats_finaux;
  {
    ini = tab_ini;
    transi = delta;
    final = tab_final
  }
 
(*Renvoie vrai si etat est un état final de a, faux sinon*)
let est_final (a:automate) (etat: int): bool =
  a.final.(etat)


(*Renvoie une liste des caractères de w*)
let char_list_of_string (w: string): char list =
  let l = ref [] in
  let n = String.length w in
  for i = 0 to (n-1) do
    l := w.[i] :: !l
  done;
  List.rev !l






(*Renvoie un tableau d taille nv_taille dont toutes les premières valeurs sont celles de t*)
let changetaille_tableau (t: 'a array) (nv_taille: int) (valeur_init: 'a): 'a array =
  let tableau_retour = Array.make nv_taille valeur_init in
  let n = min nv_taille (Array.length t) in (*Si la nouvelle taille est + petite, il faut remplir que nv_taille cases*)
  for i = 0 to (n-1) do
    tableau_retour.(i) <- t.(i)
  done;
  tableau_retour




(*Renvoie un automate reconnaissant le même langage que a auquel on ajoute le mot dont les lettres sont les éléments de w*)
let ajoute_mot_automate_et_renvoie_son_etat_final (a: automate) (w: char list): automate * int=
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
  ({
    ini = nv_etats_initiaux;
    transi = transitions_update;
    final = nv_etats_finaux
  },etat_final)
 




(*Renvoie l'automate reconnaissant les mots reconnus par a et ceux de w_list,
et une liste des nouveaux états finaux ajoutés à etats_finaux_base*)
let rec ajoute_plusieurs_mots_automate (a:automate) (w_list: string list) (etats_finaux_base: int list): automate * (int list)=
  match w_list with
  | [] -> (a, etats_finaux_base)
  | mot :: autres_mots -> (
    let (nv_autom, nv_etat_f) = ajoute_mot_automate_et_renvoie_son_etat_final a (char_list_of_string mot) in
    ajoute_plusieurs_mots_automate nv_autom autres_mots (nv_etat_f::etats_finaux_base)
  )


(*Renvoie un automate reconnaissant tous les mots de la liste langage*)
let creer_automate_langage (langage : string list): automate =
  (*Création de l'automate reconnaissant le langage vide*)
  let autom = creer_automate 1 [0] [] (fun x y -> None) in
  let a, _ = ajoute_plusieurs_mots_automate autom langage []
  in a


(*L'exécution part de l'état etat_depart.
Renvoie None si l'automate a ne peut pas lire le mot w entre les indices deb et fin inclus, renvoie Some etat si l'exécution de a sur ce
mot arrive sur l'état etat*)
let rec exec_mot (a: automate) (etat_depart: int) (w: string) (deb: int) (fin: int): int option =
  assert(deb>=0 && fin < String.length w);
  if deb>fin then Some etat_depart
  else
    match a.transi etat_depart w.[deb] with
    | None -> None
    | Some nv_etat -> exec_mot a nv_etat w (deb+1) fin

