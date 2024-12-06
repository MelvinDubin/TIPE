(*but : passer d'une liste de lexèmes traités à un arbre de syntaxe*)
(*à utiliser avec : 
- arbre_types*)

(*cherche un element dans l qui est égal à x, et renvoie, un booléen déterminant si on a trouvé un tel élément, une liste contenant ce qui a déjà été vu et le reste de la liste*)
let extrait_prochain_element_identique (l : 'a list) (x : 'a): bool*'a list*'a list= 
  
  (*retiens les éléments déjà lus avant d'arriver à la fin de la liste*)
  let rec cherche_fin_liste (non_lu : 'a list) (deja_lu : 'a list) : bool*('a list)*('a list) =
    match non_lu with 
    |[] -> (false, (List.rev deja_lu), [])
    |y::q -> if (x=y) then 
        (true, List.rev deja_lu, q) 
      else 
       (cherche_fin_liste q (y::deja_lu))
  
  in cherche_fin_liste l [] 

(*coupe la liste l lorsqu'il y a deux sauts de ligne à la suite, en rajoutant lorsque c'est le cas à une liste contenant Texte "<br> <br>"*)
let rec coupe_deux_sauts_ligne(l : lexeme_t list) : lexeme_t list list = 
  match (extrait_prochain_element_identique l DeuxSautsLigne_t) with 
  |(true, l1, l2) -> l1::coupe_deux_sauts_ligne l2 
  |(false,l1, _) -> [l1] (*il n'y a plus d'autre deux sauts de ligne*)

let rec affiche_liste (l: lexeme_t list): unit =
  match l with
  | [] -> print_newline ()
  | Texte_t(t) :: q -> print_string t; print_string "; "; affiche_liste q
  | Etoile_t :: q -> print_string "*"; print_string "; "; affiche_liste q
  | Gras_t(ll) :: q -> print_string "gras("; affiche_liste ll;print_string ");"; affiche_liste q 
  | Italique_t(ll) :: q -> print_string "ital("; affiche_liste ll;print_string ");"; affiche_liste q 
  | SautLigne_t :: q -> print_string "sautLigne"; affiche_liste q
  | Espace_t :: q -> print_string " "; affiche_liste q
  | _ -> failwith "pas implémenté"

 let effet_gras_italique (lexlist: lexeme_t list): (lexeme_t list) =
  
  (*memoire : 0 si il ne cherchait pas déjà à lire quelque chose, 1 si il lisait de l'italique (à un niveau plus haut)*)
  (*liste renvoyée : liste des lexèmes restants à traiter*)
  (* int renvoyé : réussite ou numéro d'erreur de la fermeture :
  0 -> réussite lecture 
  1 -> aucun lexème lu entre ** et ** (cas de : aa****aa )
  2 -> fermeture d'italique ouvert antérieurement (cas de : aa*aaa**aaaa*aa )
  3 -> fin de liste atteinte sans avoir fermé le gras
  4 -> le gras s'est fermé en faisant de l'italique car il n'y avait rien après une * d'autre (cas de : **aaaa* ) *)

  let rec fermer_gras (l: lexeme_t list) (memoire: int): (lexeme_t list) * (lexeme_t list) * int =

    let lexemes_lus = ref [] in
    let lexemes_a_lire = ref l in

    let sortie = ref ([], [], 0) in

    let fin_boucle = ref false in

    while (not !fin_boucle) do
      match !lexemes_a_lire with
      | Etoile_t :: Etoile_t :: q ->
        begin
          if (!lexemes_lus = []) then
            (*On n'a rien lu depuis les dernières **, donc on lance une sous-lecture de gras*)
            begin
              assert(memoire = 0); (*Si on voulait fermer de l'italique, on aurait lu le groupe *** et l'aurait fermé*)
              let (resultat, liste_restante, etatreussite) = fermer_gras q 0 in
                match etatreussite with
                | 0 -> (
                  (*Le gras a été fermé*)
                  match resultat with
                  | [lex_ital] ->
                    lexemes_lus := lex_ital :: !lexemes_lus;
                    lexemes_a_lire := liste_restante;
                  | _ -> failwith "sous-gras bien fermé mais on n'a pas renvoyé juste Gras_t() dans resultat"
                )
                | 2 -> failwith "impossible, On a voulu fermer de l'italique alors qu'il n'y en avait pas d'ouvert avant"
                | 3 -> (
                  assert(liste_restante = []);
                  
                  sortie := (((List.rev (Etoile_t :: Etoile_t :: !lexemes_lus)) @ resultat), liste_restante, 3);
                  fin_boucle := true
                )
                | 4 -> (
                  (*On a fermé de l'italique avec le sous-gras*)
                  match resultat with
                  | [lexlu] -> (
                    lexemes_lus := lexlu :: Etoile_t :: !lexemes_lus;
                    lexemes_a_lire := liste_restante
                  )
                  | _ -> failwith "La fermeture du sous-gras(en italique, état 4) n'a pas renvoyé que le lexème Italique_t(..)"
                )
                | _ -> failwith "Un état non reconnu a été renvoyé"   
            end
          else
            (*On ferme bien le gras*)
            begin
              sortie := ([Gras_t(List.rev !lexemes_lus)], q, 0);
              fin_boucle := true
            end
        end

      | Etoile_t :: q ->
        begin
          if (memoire = 1) then
            (*On va fermer l'italique qui était ouvert avant ce gras*)
            begin
              sortie := (List.rev !lexemes_lus, q , 2);
              fin_boucle := true
            end
          else
            begin
              (*Deux cas :
              -> Si q = [], on va fermer les ** précédentes avec cet * pour faire de l'italique
              -> Sinon, on va vouloir fermer de l'italique à partir de cet * :*)
              match q with
              | [] -> (
                sortie := ([Italique_t(List.rev !lexemes_lus)], [] , 4);
                fin_boucle := true
              )
              | _ -> (
                let (resultat, liste_restante, etatreussite) = fermer_italique q 1 in
                match etatreussite with
                | 0 -> (
                  (*L'italique a été fermé*)
                  match resultat with
                  | [lex_ital] ->
                    lexemes_lus := lex_ital :: !lexemes_lus;
                    lexemes_a_lire := liste_restante;
                  | _ -> failwith "italique bien fermé mais on n'a pas renvoyé juste Italique_t() dans resultat"
                )
                (*L'état de réussite 1 n'existe pas pour la lecture d'italique car ** aurait été lu comme 2 étoiles directement, pas comme *[rien]* *)
                | 2 -> (
                  (*L'italique n'a pas été fermé mais la fonction s'est arrêté sur une lecture de ** qui peut fermer notre gras*)
                  sortie := ([Gras_t((List.rev (Etoile_t :: !lexemes_lus)) @ resultat)], liste_restante, 0);
                  fin_boucle := true
                )
                | 3 -> (
                  assert(liste_restante = []);
                  (*On n'a pas pu fermer l'italique, et on n'a pas non plus trouvé de quoi fermer le gras d'avant*)
                  sortie := (((List.rev (Etoile_t :: !lexemes_lus)) @ resultat), liste_restante, 3);
                  fin_boucle := true
                )
                | _ -> failwith "Un état non reconnu a été renvoyé"   
              )  
            end 
        end
           
      | x :: q ->
        begin
          lexemes_lus := x :: !lexemes_lus;
          lexemes_a_lire := q
        end

      | [] ->
        (*On le fait ici plutot qu'en condition du while, car on veut mettre à jour sortie si la fin de liste
        est atteinte*)
        begin
          sortie := (List.rev !lexemes_lus, [], 3);
          fin_boucle := true
        end
    
    done;
    (*debug*)
    (*match !sortie with
    | (ret,rest,etat) -> (
      print_string "J'ai fini le gras, mon état est :\n";
      print_string "retour: "; affiche_liste ret;
      print_string "reste: "; affiche_liste rest;
      print_string "etat : "; print_int etat;
      print_string "\n------\n"
    );*)
    !sortie
  
  and fermer_italique (l: lexeme_t list) (memoire: int): (lexeme_t list) * (lexeme_t list) * int =
    (*memoire : 0 si il ne cherchait pas déjà à lire quelque chose, 1 si il lisait du gras (à un niveau plus haut)*)
    (*liste renvoyée : liste des lexèmes restants à traiter*)
    (* int renvoyé : réussite ou numéro d'erreur de la fermeture :
    0 -> réussite lecture
    2 -> fermeture de gras ouvert antérieurement (cas de : aa**aaa*aaaa**aa )
    3 -> fin de liste atteinte sans avoir fermé l'italique
    *)
    let lexemes_lus = ref [] in
    let lexemes_a_lire = ref l in

    let sortie = ref ([], [], 0) in

    let fin_boucle = ref false in

    while (not !fin_boucle) do
      match !lexemes_a_lire with
      | Etoile_t :: Etoile_t :: Etoile_t :: q ->
        (*Si il y a strictement + que 2 *, la 1re est utilisée pour fermer l'italique (ex: *abc***def )*)
        begin
          (*On n'a normalement pas le problème de *[rien]* car si c'était le cas, on aurait lu deux * et donc du gras*)
          assert(!lexemes_lus <> []);
          sortie := ([Italique_t(List.rev !lexemes_lus)], Etoile_t :: Etoile_t :: q, 0);
          fin_boucle := true
        end
        
      | Etoile_t :: Etoile_t :: q ->
        begin
          if (memoire = 1) then
            (*On ferme du gras d'avant*)
            begin
              sortie := (List.rev !lexemes_lus, q , 2);
              fin_boucle := true
            end
          else
            (*Deux cas : q est vide et on ferme l'italique avec la première étoile, sinon on cherche à fermer du gras*)
            match q with
            | [] ->
              begin
                sortie := ([Italique_t(List.rev !lexemes_lus)], [Etoile_t], 0);
                fin_boucle := true
              end
            | _ ->
              (*On veut fermer du gras*)
              begin
                let (resultat, liste_restante, etatreussite) = fermer_gras q 1 in
                match etatreussite with
                | 0 -> (
                  (*Le gras a bien été fermé*)
                  match resultat with
                  | [lex_ital] ->
                    lexemes_lus := lex_ital :: !lexemes_lus;
                    lexemes_a_lire := liste_restante
                  | _ -> failwith "gras bien fermé mais on n'a pas renvoyé juste Gras_t() dans resultat"
                )
                | 1 -> (
                  (*Ne doit pas arriver puisque si il y avait ****, on aurait détecté 3 * et on serait dans le cas d'avannt*)
                  failwith "Fermeture de gras vide dans la lecture de l'italique : l'impossible est arrivé"
                )
                | 2 -> (
                  (*Italique ferrmé dans la tentative de fermer le gras*)
                  begin
                    sortie := ([Italique_t((List.rev (Etoile_t :: Etoile_t :: !lexemes_lus)) @ resultat)], liste_restante, 0);
                    fin_boucle := true
                  end
                )
                | 3 -> (
                  (*Fin de liste atteinte*)
                  assert(liste_restante = []);
                  (*On n'a pas pu fermer le gras, et on n'a pas non plus trouvé de quoi fermer l'italique d'avant*)
                  sortie := (((List.rev (Etoile_t :: Etoile_t :: !lexemes_lus)) @ resultat), liste_restante, 3);

                  fin_boucle := true
                )
                | 4 -> (
                  (*Le gras ne peut pas se fermer en faisant de l'italique lui-même
                  si il a comme mémoire qu'il peut fermer de l'italique antérieur*)
                  failwith "**texte* a donné de l'italique alors que l'* seule aurait du fermer de l'italique ouvert avant"
                )
                | _ -> failwith "Un état non reconnu a été renvoyé"   
              end
        end
      | Etoile_t :: q ->
        begin
          if (!lexemes_lus = []) then
            (*Impossible, sinon on n'aurait pas lu *[rien]* mais ** directement et donc une ouverture de gras*)
            failwith "Italique fermé sans avoir rien lu alors que c'est censé être impossible"
          else
            (*On ferme bien l'italique'*)
            begin
              sortie := ([Italique_t(List.rev !lexemes_lus)], q, 0);
              fin_boucle := true
            end
        end
           
      | x :: q ->
        begin
          lexemes_lus := x :: !lexemes_lus;
          lexemes_a_lire := q
        end

      | [] ->
        begin
          sortie := (List.rev !lexemes_lus, [], 3);
          fin_boucle := true
        end
    
    done;
    (*
    match !sortie with
    | (ret,rest,etat) -> (
      print_string "J'ai fini l'italique, mon état est :\n";
      print_string "retour: "; affiche_liste ret;
      print_string "reste: "; affiche_liste rest;
      print_string "etat : "; print_int etat;
      print_string "\n------\n"
    );*)
    !sortie
  


    
  in

  (*Lecture de la liste lexlist*)
  
  let (lexemes_lus: (lexeme_t list) ref) = ref [] in
  let lexemes_a_lire = ref lexlist in

  let fin_boucle = ref false in

  while (not !fin_boucle) do
    match !lexemes_a_lire with
    | Etoile_t :: Etoile_t :: q ->
      begin
        let (resultat, liste_restante, etatreussite) = fermer_gras q 0 in
        match etatreussite with
        | 0 -> (
          match resultat with
          | [lexlu] -> (
            lexemes_lus := lexlu :: !lexemes_lus;
            lexemes_a_lire := liste_restante
          )
          | _ -> print_lex_list resultat; print_newline ();failwith "La fermeture du gras n'a pas renvoyé que le lexème Gras_t(..)"
        )
        | 1 -> (
          lexemes_lus := Etoile_t :: !lexemes_lus;
          lexemes_a_lire := Etoile_t :: q
        )
        | 2 -> failwith "Le gras a voulu fermer de l'italique ouvert précédemment, impossible car rien n'était ouvert"
        | 3 -> (
          assert(liste_restante = []);
          lexemes_lus := (List.rev resultat) @ (Etoile_t :: Etoile_t :: !lexemes_lus);
          lexemes_a_lire := [];
        )
        | 4 -> (
          match resultat with
          | [lexlu] -> (
            lexemes_lus := lexlu :: Etoile_t :: !lexemes_lus;
            lexemes_a_lire := liste_restante
          )
          | _ -> failwith "La fermeture du gras(en italique, état 4) n'a pas renvoyé que le lexème Italique_t(..)"
          
        )
        | _ -> failwith "Etat non utilisé renvoyé par la fermeture de gras, erreur"
      end

    | Etoile_t :: q ->
      begin
        let (resultat, liste_restante, etatreussite) = fermer_italique q 0 in
        match etatreussite with
        | 0 -> (
          match resultat with
          | [lexlu] -> (
            lexemes_lus := lexlu :: !lexemes_lus;
            lexemes_a_lire := liste_restante
          )
          | _ -> failwith "La fermeture de l'italique n'a pas renvoyé que le lexème Italique_t(..)"
        )
        | 2 -> failwith "L'italique a voulu fermer du gras ouvert précédemment, impossible car rien n'était ouvert"
        | 3 -> (
          assert(liste_restante = []);
          lexemes_lus := (List.rev resultat) @ (Etoile_t :: !lexemes_lus);
          lexemes_a_lire := [];
        )
        | _ -> failwith "Etat non utilisé renvoyé par la fermeture de gras, erreur"
      end
         
    | x :: q ->
      begin
        lexemes_lus := x :: !lexemes_lus;
        lexemes_a_lire := q
      end

    | [] -> fin_boucle := true
  done;
  List.rev (!lexemes_lus)

(*cherche s'il y a une liste à puces dans la liste de lexèmes t (il n'y a forcément qu'une liste au plus, car est utilisée après l'appel des coupe deux sauts de ligne)*)
let liste_puces (l : lexeme_t list) : lexeme_t list = 

  (*sachant que l_aux est la suite du début d'une liste à puces, renvoie la liste des "éléments de liste", sous la forme de listes eux-mêmes*)
  let rec construit_liste (l_aux : lexeme_t list): lexeme_t list list =
    match (extrait_prochain_element_identique l_aux ElementListe_t) with 
    |(true, l1, l2) -> l1::construit_liste l2 
    |(false,l1, _) -> [l1] 
  in 

  (*cherche s'il y a une liste à puces dans la liste de lexèmes, et regroupe les éléments de la liste dans un lexèmes liste à puces*)
  let rec cherche_liste (l_aux : lexeme_t list) : lexeme_t list = 
    match l_aux with 
    |[] -> []
    |ElementListe_t::q -> [ListePuces_t (construit_liste q)]
    |x::q -> x::(cherche_liste q)
  in 
  cherche_liste l 

let cree_arbre(l : lexeme_t list) : traitement_texte arbre =


  let paragraphes = coupe_deux_sauts_ligne l in 

  (*traite le lexème "ElementListe_t"*)
  let paragraphes_listes = List.map liste_puces paragraphes in 

  (*traite le lexème Etoile_t*)
  let paragraphes_gras_italique = List.map effet_gras_italique paragraphes_listes in

  let rec cree_sous_arbres (effet : effet_texte) (l_sous_arbres : lexeme_t list) : traitement_texte arbre = 
    Noeud (Effet effet, List.map traite_lexeme l_sous_arbres)

 
  and traite_lexeme (lex : lexeme_t) : traitement_texte arbre =
    match lex with 
    |Etoile_t ->  Feuille (Texte "*") (*les étoiles qui servent d'effet ont déjà été traitées*)
    |Texte_t str -> Feuille (Texte str)
    |Tiret_t -> Feuille (Texte "-") (*les tirets ne correspondent dans cette implémentation qu'à du texte*)
    |DeuxSautsLigne_t -> failwith "les deux sauts de lignes ne doivent plus être présents"
    |SautLigne_t -> Feuille (Texte "<br>") (*balise html pour le saut de ligne*) 
    |Espace_t -> Feuille (Texte " ")
    |ElementListe_t -> failwith "ces lexèmes doivent déjà avoir été gérés à ce stade"
    |ListePuces_t elements_listes -> Noeud(Effet Liste_puces, List.map (cree_sous_arbres (Element_liste)) elements_listes)
    |Gras_t liste_en_gras -> cree_sous_arbres Gras liste_en_gras
    |Italique_t liste_en_italique -> cree_sous_arbres Italique liste_en_italique

  in Noeud(Effet EffetVide, List.map (cree_sous_arbres Paragraphe) paragraphes_gras_italique) (*cree l'arbre général en mettant une racine vide et des sous arbres correspondant aux paragraphes*)