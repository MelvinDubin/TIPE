(*
let comportement_commandes (s : string) = 
  match s with 
  |"couleur titre" -> ()  (*doit lire un entier puis ":" puis une couleur*)
  |_ -> failwith "n'est pas une commande"
  (*a implementer*)

let cree_tab_couleur_liste (f : in_channel) : string array= 
  let colors = Array.make 6 "None" in 
  for i = 1 to 6 do 
    let line = *)

(*même fonction que dans highlight.ml, modifée pour renvoyer l'etat final ajouté*)
(*Renvoie un automate reconnaissant le même langage que a auquel on ajoute le mot dont les lettres sont les éléments de w, et renvoie son l'etat final du mot ajouté*)
let ajoute_mot_automate_et_renvoie_son_etat_final (a: automate) (w: char list): automate*int = 
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
  ({
    ini = nv_etats_initiaux;
    transi = transitions_update;
    final = nv_etats_finaux
  },etat_final)


(*renvoie l'automate qui lit la liste des commandes, ainsi que le tableau tel que t.(i) donne l'état final dans a si on lit la commande i*)
let cree_autom_commandes (commandes : string array) : automate*(int array) = 
  let n = Array.length commandes in 
  let a = ref (creer_automate 1 [0] [] ((fun x y -> None))) in (*cree un automate réduit à son état initial*)
  let etats_finaux = Array.make n (-1) in 
  for i = 0 to (n-1)do 
    let liste_caracteres = char_list_of_string (commandes.(i)) in 
    let nv_a,q = ajoute_mot_automate_et_renvoie_son_etat_final (!a) (liste_caracteres) in (*ajoute le mot w à l'automate et renvoie le nouvel automate et l'état final sur lequel finit la lecture de w*)
    a := nv_a ;
    etats_finaux.(i) <- q ; 
  done ; 
  (!a,etats_finaux)

(*tente de lire le mot w dans a et renvoie l'état final lorsqu'il est atteint ainsi que la liste des caractères non encore lus*)
let tente_lire_mot (a : automate) (w : string) : (int * char list) option= 
let liste_caracteres = char_list_of_string w in  
(*permet de retenir le dernier état final possible mais de parcourir la liste au cas où on peut lire une autre commande qui admet celle trouvée comme préfixe*) 
  let dernier_etat_final = ref None in 
  let derniere_liste_suite = ref None in  
  (*lit le mot correspondant à la liste l dans a depuis etat*) 
  let rec parcours_sous_liste(l : char list) (etat : int) : (int * char list) option = 
    match l with 
    |[] -> begin match !dernier_etat_final with 
      |None -> None 
      |Some q -> (match !derniere_liste_suite with 
        |None -> failwith "si un état final a été trouvé la liste suivante a été renvoyée avec, ce cas ne doit donc pas arriver"
        |Some liste_suite -> Some (q,liste_suite))
    end
    |c::suite_carac -> 
      (match (a.transi etat c) with 
      |None -> begin 
        match !dernier_etat_final with 
        |None -> None 
        |Some q -> (match !derniere_liste_suite with 
          |None -> failwith "si un état final a été trouvé la liste suivante a été renvoyée avec, ce cas ne doit donc pas arriver"
          |Some liste_suite -> Some (q,liste_suite))
      end  
      |Some etat_suivant ->(
        if (est_final a etat_suivant) then 
          (
            dernier_etat_final := Some etat_suivant ; 
            derniere_liste_suite  := Some suite_carac ; 
          ) 
        else ();
        parcours_sous_liste suite_carac etat_suivant )
    ) 
  in parcours_sous_liste liste_caracteres 0    


(*transforme une liste de caractères en un string*)
let char_list_to_string (lettres : char list) : string = 

    (*ecrit a partir de l'indice i les lettres de fin_lettres dans mot*)
    let rec ecrit_fin_mot (accu : string) (fin_lettres : char list) : string = 
      match fin_lettres with 
      |[] -> accu
      |x::q ->  ecrit_fin_mot (accu^(String.make 1 x)) q
    in ecrit_fin_mot "" lettres

(*lit le fichier preset et renvoie les informations nécessaires pour faire les modifications du fichier : 
- un tableau de 6 cases contenant les couleurs voulues pour chaque titre de niveau correspondant à son indice (None s'il n'y en a pas)*)
let lit_fichier_preset(filename : string) : string array =

  let tableau_commandes = [|"couleur titre"|] in (*commandes qui seront reconnues*)
  let autom_commandes,indices_etats_finaux = cree_autom_commandes tableau_commandes in 
  let f = open_in filename in 
  let n = Array.length tableau_commandes in 

  (*données à trouver*)
  let colors = Array.make 6 "None" in

  (*renseigne les informations données par la commande qui finit sur l'état i dans l'automate appliqué à la suite de caractères suite_carac*)
  let comportement_commande (i : int) (suite_carac : char list) : unit = 
    (*cherche la commande correspondante à l'état final i*)
    let commande = ref (-1) in 
    for j = 0 to (n-1) do 
      if (indices_etats_finaux.(j) = i) then (commande:=j) else ()
    done;
    match !commande with 
    |0 -> begin (*commande couleur_titre*)
      match suite_carac with 
      |' '::numero::' '::':'::couleur -> 
        if (couleur != []) then (
          let indice_tab_titre = int_of_char(numero) - 49 in (*int of char renvoie l'entier ascii, il faut aussi enlever 1 par rapport à l'indice du tableau*)
          colors.(indice_tab_titre) <- char_list_to_string couleur  (*dans un fichier preset, il est possible de ne pas remplir tous les paramètres*)
        ) 
      |_ -> () (*si la syntaxe attendue n'est pas correcte, *)
    end 
    |_ -> failwith "pas d'autres commandes pour le moment"
  in
  let fin_du_doc = ref false in 

  while (not(!fin_du_doc)) do
    try 
    (let line = input_line f in 
    match (tente_lire_mot autom_commandes line) with 
      |None -> ()
      |Some (i,suite_carac) -> comportement_commande i suite_carac
    )
    with 
    |End_of_file -> fin_du_doc := true 
  done;
  colors
