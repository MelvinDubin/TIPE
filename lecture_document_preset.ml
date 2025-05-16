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


type raccourci = 
  |Blank of raccourci
  |Symbole of char*raccourci
  |Fin 

(*transforme une liste de caractères en un raccourci permettant de reconnaître toute chaîne de ce type, où le mot clé "text" permet de reconnaître n'importe quel texte*)
let rec texte_to_raccourci (t : char list) : raccourci = 
  match t with 
  |[] -> Fin 
  |'t'::'e'::'x'::'t'::q -> Blank (texte_to_raccourci q) 
  |s::q -> Symbole (s,texte_to_raccourci q)


(*cherche les endroits contenant le raccourci r dans le texte t pour y ajouter l'effet e*)
let rec tente_construire_arbre (r : raccourci) (e : effet) (t : char list) : texte =

  (*trouve les endroits qui correspondent à la syntaxe attendue par r_aux dans t_aux, en retenant dans accu ce qui a déjà été lu et renvoie, la liste de ce qui a déjà été lu en enlevant les symboles de syntaxe liés au raccourcis, la liste de ce qu'il reste à lire et un booléen indiquant si le symbole a bien été trouvé*)
  let rec trouve_ou_ajouter (r_aux : raccourci) (t_aux: char list) (accu : char list) : (char list)*(char list)*bool = 
    match t_aux with 
    |[] -> begin 
      match r_aux with 
      |Fin |Blank Fin -> (List.rev accu,[],true)
      |Blank (Blank suite) -> trouve_ou_ajouter (Blank suite) t_aux accu (*s'il y a plusieurs Blank, on les considère comme étant un seul*)
      |_ -> ([],[],false)
    end 
    |x::q -> begin match r_aux with 
      |Fin |Blank Fin-> (List.rev accu, t_aux, true)
      |Blank (Blank suite) -> trouve_ou_ajouter (Blank suite) t_aux accu 
      |Blank (Symbole (s, suite)) -> if (s==x) then (
          trouve_ou_ajouter suite q accu 
        ) else (
          trouve_ou_ajouter r_aux q (x::accu)
        )
      |Symbole (s,suite) -> if (s==x) then (
        trouve_ou_ajouter suite q accu 
      ) else (
        ([],[],false)
      )
      end 
  in 
  
  (*selon une liste de caractère t_aux, où l'on a déjà lu accu, sans trouver d'occurence du raccourci, renvoie le texte t correspondant*)
  let rec construction_arbre_effet (t_aux : char list) (accu : char list): texte = 
    
    print_string("boucle dans construction_arbre_effet\n"); 
    match t_aux with
    |[] -> Texte_nu (char_list_to_string (List.rev accu))
    |x::q -> begin 
      match trouve_ou_ajouter r t_aux [] with
      |(_,_,false) -> construction_arbre_effet q (x::accu)
      |(avec_effet,apres,true) -> Texte_effet (EffetVide,
        [Texte_nu (char_list_to_string (List.rev accu));
        Texte_effet (e,[Texte_nu (char_list_to_string avec_effet)]);
        construction_arbre_effet apres []]
        )
      end
  
  in construction_arbre_effet t []

(*pour une liste de raccourcis et d'effets liés à ces raccourcis, transforme le texte t afin qu'il y corresponde*)  
let rec applique_effets_raccourcis (l : (raccourci*effet) list) (t : texte) : texte =
  
    print_string("boucle dans applique_effets_raccourcis\n"); 
  match t with 
  |Texte_nu str -> begin match l with 
    |[] -> t
    |(r,e)::q -> applique_effets_raccourcis q (tente_construire_arbre r e (char_list_of_string str))
  end
  |Texte_effet (e,sous_textes) -> Texte_effet(e, List.map (applique_effets_raccourcis l) sous_textes)
  
(*permet de parse un string (sous la forme "...") présent dans t (ce qui a déjà été parse est stocké dans accu), et le booléen dit si on a déjà ouvert les guillements ou non*)
let rec parse_string (ouvert : bool) (accu : char list) (t : char list) = 
  
    print_string("boucle dans parse_string\n"); 
      match t with 
      |'"'::q -> if ouvert then (
        (char_list_to_string (List.rev accu)),q)
      else 
        parse_string true accu q
      |x::q -> parse_string ouvert (x::accu) q
      |_ -> failwith "ce n'est pas la syntaxe attendue"
     
(*permet de parse un type 'a option contenu au début de o, si on a la fonction de parsing parse_type*) 
let parse_option (o : char list) (parse_type : char list -> ('a)*char list) : ('a option)*char list = 
  print_string("boucle dans parse_option\n"); 
  match o with 
  |'N'::'o'::'n'::'e'::q -> (None,q) 
  |'S'::'o'::'m'::'e'::' '::q -> let x,suite = parse_type q in (Some x, suite)
  |_ -> failwith "ce n'est pas la syntaxe attendue pour un type option"

(*pour un effet écrit dans le texte t (sous forme de la liste de ses caractères), renvoie l'effet associé avec le type effet*)
let parse_effet (t : char list) : effet =
  print_string("boucle dans parse_effet\n"); 
  match t with 
  |'I'::'t'::'a'::'l'::'i'::'q'::'u'::'e'::q -> Italique
  |'G'::'r'::'a'::'s'::q -> Gras 
  |'F'::'o'::'n'::'t'::'('::q -> begin 
    (*pour un string écrit sous la forme "contenu" écrite au début de t, renvoie la chaîne de caractère contenu ainsi que la suite des caractères non lu (ouvert permet de savoir si on a lu le premier '"', accu permet de stocker les caractères de la chaîne avant de la renvoyer *)
      
    let couleur,suite1 = parse_option q (parse_string false []) in 

    match suite1 with 
    |','::suite2 -> let taille,suite3 = parse_option suite2 (parse_string false []) in 
      begin 
      match suite3 with 
      |','::suite3 -> let id,suite4 = parse_option suite3 (parse_string false []) in
        Font(couleur,taille,id)
      |_ -> failwith "ce n'est pas la syntaxe attendue pour la suite de la taille dans un font"
      end 
    |_ -> failwith "ce n'est pas la syntaxe attendue pour la suite de la couleur dans un font"
    end 
  |'C'::'l'::'i'::'q'::'u'::'a'::'b'::'l'::'e'::' '::q -> 
    let lien,_ = parse_string false [] q in 
    (Cliquable lien)  
  |_ -> failwith "ce n'est pas la syntaxe attendue pour l'effet"

(*pour un raccourci écrit dans t sous la forme syntaxe -> effet, renvoie le raccourci contenu dans syntaxe, et l'effet contenu dans effet*)
let rec parse_raccourci (accu : char list) (t : char list) : raccourci*effet  = 

    print_string("boucle dans parse_raccourci\n");
  match t with 
  |' '::'-'::'>'::' '::q -> (texte_to_raccourci (List.rev accu),parse_effet q) 
  |x::q -> parse_raccourci (x::accu) q  
  |_ -> failwith "ce n'est pas la syntaxe attendue pour le raccourci"

(*lit le fichier preset (de nom filename) et renvoie les informations nécessaires pour faire les modifications du fichier : 
- un tableau de 6 cases contenant les couleurs voulues pour chaque titre de niveau correspondant à son indice (None s'il n'y en a pas)
- un numero entre 0 et 6, qui correspond au niveau de précision attendu du sommaire, s'il est à 0, pas de sommaire, s'il est à k>0, le sommaire affichera les titres de niveaux 1, 2, ... k
- l'éventuel titre à écrire au début du document (si la commande de titre est lancé sans renseigner de titres particulier, alors on ajoute le nom du fichier markdown)
- la fonction qui à un texte permet d'ajouter les effets donnés par les raccourcis ajoutés par l'utilisateur
*)

let lit_fichier_preset(filename : string) (filename_md : string): ((string option) array)*int*(string option)*(texte -> texte) =

  let tableau_commandes = [|"couleur titre";"sommaire";"ajout titre";"creer raccourci "|] in (*commandes qui seront reconnues*)
  let autom_commandes,indices_etats_finaux = cree_autom_commandes tableau_commandes in 
  let f = open_in filename in 
  let n = Array.length tableau_commandes in 

  (*données à trouver*)
  let colors = Array.make 6 None in
  let sommaire = ref 0 in 
  let titre = ref None in 
  let liste_raccourcis = ref [] in 

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
          colors.(indice_tab_titre) <- Some (char_list_to_string couleur)  (*dans un fichier preset, il est possible de ne pas remplir tous les paramètres*)
        ) 
      |_ -> () (*si la syntaxe attendue n'est pas correcte, ne fait rien*)
    end 

    |1 -> begin (*commande pour le sommaire*)
      match suite_carac with 
      |' '::numero::_ ->   
        if ('1'<=numero && numero<='6') then (
          sommaire := int_of_char(numero) - 48 
        ) else (
          sommaire := 6 (*si le numero saisi n'est pas correct, alors on décide d'afficher tous les titres*)
        )
      |_ ->  sommaire := 6 (*si on demande juste un sommaire sans préciser où s'arrêter, on part du principe que l'on met tous les titres*)
    end

    |2 -> begin (*commande pour ajouter un titre*)
      match suite_carac with 
      |' '::q -> titre := Some (char_list_to_string q)
      |_ -> (*s'il n'y a pas de titre renseigné, on ajoute le titre fichier en tant que titre de niveau 1, mais il faut d'abord retirer le ".md final"*) 
        titre := Some ("# "^(String.sub filename_md 0 (String.length(filename_md) -3)))
    end

    |3 ->  (*commande pour ajouter une commande*)
      liste_raccourcis := (parse_raccourci [] suite_carac)::!liste_raccourcis

    |_ -> failwith "pas d'autres commandes pour le moment"
  in
  let fin_du_doc = ref false in 

  while (not(!fin_du_doc)) do
    print_string("boucle dans la lecture du doc\n");
    try 
    (let line = input_line f in 
    match (tente_lire_mot autom_commandes line) with 
      |None -> ()
      |Some (i,suite_carac) -> comportement_commande i suite_carac
    )
    with 
    |End_of_file -> fin_du_doc := true 
  done;
  colors,(!sommaire),!titre,(applique_effets_raccourcis !liste_raccourcis)
