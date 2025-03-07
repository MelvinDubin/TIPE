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

let rec affiche_liste (l: lexeme_t list): unit =
  match l with
  | [] -> print_newline ()
  | Texte_t(t) :: q -> print_string t; print_string "; "; affiche_liste q
  | Etoile_t :: q -> print_string "*"; print_string "; "; affiche_liste q
  | Effet_t(Gras,(ll)) :: q -> print_string "gras("; affiche_liste ll;print_string ");"; affiche_liste q 
  | Effet_t(Italique, (ll)) :: q -> print_string "ital("; affiche_liste ll;print_string ");"; affiche_liste q 
  | SautLigne_t :: q -> print_string "sautLigne"; affiche_liste q
  | Espace_t :: q -> print_string " "; affiche_liste q
  | _ -> failwith "pas implémenté"

(* Découpe la liste de lexèmes traités en coupant à chaque nouveau titre de même niveau,
et découpe récursivement avec les titres de niveau 1 de moins dans les listes créées, et ainsi de suite.
Toutes les listes créées (sauf la 1ere) commencent donc par un lexème titre*)
let decoupage_sections_titres (l: lexeme_t list): lexeme_t list =

  let titres_presents = [|false; false; false; false; false; false |] in
  (*Pour i entre 1 et 6, si il existe un titre de niveau i dans ll, met à true
  le booléen d'indice i de titres_presents*)
  let rec presence_titres (ll: lexeme_t list): unit =
    if (titres_presents.(0) && titres_presents.(1)
      && titres_presents.(2) && titres_presents.(3)
      && titres_presents.(4) && titres_presents.(5)) then
        (*Tous les niveaux de titre possibles ont été trouvés, pas besoin d'en chercher d'autres*)
        ()
    else (
      match ll with
      | [] -> ()
      | Titre_t(n, _) :: q -> (
        titres_presents.(n-1) <- true;
        presence_titres q
      )
      | _ :: q -> presence_titres q
    )
  in
  presence_titres l;
  let niveaux_presents = (List.filter (fun x -> titres_presents.(x-1)) [1;2;3;4;5;6]) in
  
  (*Renvoie la liste des lexèmes présents avant le premier titre de niveau niv lu (titre exclus, et
  premier lexème de la liste exclus car sinon on ne lirait rien),
  et la liste de ce qui vient ensuite (titre qui a fait le découpagee inclus)
  Elle renvoie donc toute la liste si il n'y a pas de titres de niveau niv lus*)
  
  let coupe_prochain_titre (niv: int) (ll: lexeme_t list): (lexeme_t list)*(lexeme_t list) =
    
    (*Même effet mais en lisant le premier lexème, supposant donc qu'il ne s'agit pas d'un titre sous
    peine de renvoyer ([], _)    
    acc conserve la liste des lexèmes venant avant le premier titre lu*)
    let rec coupe_prochain_titre_avec_premier (lll: lexeme_t list) (acc: lexeme_t list): (lexeme_t list)*(lexeme_t list)=
      match lll with
      | [] -> (List.rev acc, [])
      | Titre_t(n, _) :: _ when (n=niv)-> (List.rev acc, lll) (*Titre inclus dans ce qui reste à lire*)
      | x :: q -> coupe_prochain_titre_avec_premier q (x :: acc)
    in
    match ll with
    | x :: q -> 
      let (a,b) = coupe_prochain_titre_avec_premier q [] in (*On a retiré le 1er lexème*)
      (x :: a, b)
    | [] -> ([], [])
  in 

  (*Découpe ll en liste de listes de lexèmes en coupant à chaque titre de niveau niv,
  niv étant le premier élément de la liste niveaux,
  et renvoie une Liste_imbriquee_t(ce découpage)
  Précondition : les titres de ll sont de niveau niv ou moins*)
  let rec decoupe_divisions_niveau (niveaux: int list) (ll: lexeme_t list): lexeme_t =
    
    match niveaux with 
    | [] -> Liste_imbriquee_t(ll) (*Pas d'autres niveaux de titres repérés au début*)
    | niv :: niveaux_suivants -> 
      begin
        let tous_lexemes = ref ll in
        let decoupage: (lexeme_t list list) ref = ref [] in    
        while (not (!tous_lexemes = [])) do
          let decoupage_actuel, liste_reste = coupe_prochain_titre niv !tous_lexemes in
          decoupage := decoupage_actuel :: !decoupage;
          (*La ligne précédente fait le découpage récursif pour les titres de niveau plus petits,
          et s'arrêtera à ceux de niveau 6 puisque decoupe_divisions_niveau ne fera alors plus d'appels récursifs*)
          tous_lexemes := liste_reste
        done;

        Liste_imbriquee_t (List.map
          (fun lexlist -> decoupe_niveau_ignoretitre niveaux_suivants lexlist)
          (List.rev !decoupage))
      end
  and decoupe_niveau_ignoretitre (niveaux: int list) (lexlist: lexeme_t list): lexeme_t =
    (*Appliqué sur lexlist, va la découper selon les titres de niveau niv (1er élément de niveaux)
    de la façon suivante, et renvoyer Liste_imbriquee_t (cette liste obtenue):
    Si lexlist commencee par un titre de niveau < niv (titre + grand), il sera conservé au début de la liste construite
    Sinon, le premier élément de la liste construite sera la première section de niveau niv*)
      match niveaux with
        | [] -> Liste_imbriquee_t(lexlist)
        | niv :: _ -> 
          begin
            match lexlist with
            | Titre_t(n, t) :: q when n < niv ->
              begin
                match decoupe_divisions_niveau niveaux q with
                | Liste_imbriquee_t(resultat) -> Liste_imbriquee_t(Titre_t(n, t) :: resultat)
                | _ -> failwith "Ne peut pas arriver"
              end
            | _ -> decoupe_divisions_niveau niveaux lexlist
            
          end
  in
  match decoupe_divisions_niveau niveaux_presents l with
  | Liste_imbriquee_t q -> q
  | _ -> failwith "Ne peut pas arriver"



(*Pour chaque occurence d'une Liste_imbriquee_t dans l ayant comme paramètre une
liste de lexèmes qui contient des DeuxSautsLigne_t (n'étant pas le premier élément
de cette liste), decoupe_paragraphes va remplacer cette occurence par une suite de
Liste_imbriquee_t ayant comme paramètre chacune les listes obtenues en coupant la
la liste initiale à chaque "DeuxSautsLigne_t" (sauf le 1er, toujours présent
au début d'un paragraphe*)
let decoupage_paragraphes (l: lexeme_t list): (lexeme_t list) =
  (*Découpe la liste restants au premier DeuxSautsLigne_t trouvé, renvoie un couple contenant
  (liste des lexèmes avant ce DeuxSautsLigne_t, liste des lexèmes après (lui inclus)),
  en utilisant lus comme accumulateur des lexèmes qui formeront la 1ere liste,
  pour être récursive terminale
  Il vaut mieux que restants ne commence pas tout de suite par un DeuxSautsLigne_t, sous risque
  de renvoyer ([], même liste)*)
  let rec coupe_deuxsautsligne (lus: lexeme_t list) (restants: lexeme_t list): (lexeme_t list) * (lexeme_t list) =
    match restants with
    | DeuxSautsLigne_t :: q -> (List.rev lus, restants)
    | [] -> (List.rev lus, [])
    | x :: q -> coupe_deuxsautsligne (x :: lus) q
  in
  (*Renvoie une liste de Liste_imbriquee_t(l_i) où chaque l_i est un morceau de l, étant découpés à chaque
  DeuxSautsLigne_t, de sorte que la concaténation de tous les l_i reformerait l sans les DeuxSautsLigne_t
  listes_decoupees est l'accumulateur qui contient les Liste_imbriquee_t déjà fabriquées*)
  let rec coupe_tous_deuxsautsligne (listes_decoupees: lexeme_t list) (l: lexeme_t list): lexeme_t list =
    match l with
    | [] -> List.rev listes_decoupees
    | DeuxSautsLigne_t :: q -> let (decoupage1, reste) = coupe_deuxsautsligne [] q in
      coupe_tous_deuxsautsligne (Liste_imbriquee_t(decoupage1)::listes_decoupees) reste
    | _ -> List.rev(Liste_imbriquee_t(l) :: listes_decoupees) (*ne commence pas par un DeuxSautsLigne_t -> 1 seul paragraphe*)
  in

  (*Empile les lexèmes de a_ajouter à l'avant de l, à l'envers puisqu'on le fait petit à petit*)
  let rec ajoute_devant_liste (a_ajouter: lexeme_t list) (l: lexeme_t list): lexeme_t list = 
    match a_ajouter with
    | [] -> l
    | lex :: q -> ajoute_devant_liste q (lex :: l)
  in

  let rec modifie_sous_liste (lus: lexeme_t list) (restants: lexeme_t list): lexeme_t list =
    match restants with
    | Liste_imbriquee_t l :: reste ->
      modifie_sous_liste (ajoute_devant_liste (modifie_sous_liste [] l) lus) reste
    | Titre_t (niv, l) :: reste -> (
      assert(lus = []); (*Normalement, on ne lit un Titre_t que si il commence une section et est le 1er elt d'une Liste_imbriquee_t*)
      [Liste_imbriquee_t([Titre_t(niv, l); Liste_imbriquee_t(modifie_sous_liste [] reste)])] (*Représente : le titre et le contenu de la section*)
    )
    | DeuxSautsLigne_t :: _ -> (*On découpe les paragraphes et on ne rencontrera pas
    d'autre titre ou Liste_imbriquee après avoir lu un DeuxSautsLigne_t*)
    coupe_tous_deuxsautsligne [] restants
    | [] -> (List.rev lus)
    | _ -> failwith "Erreur : section qui contient autre chose que des Titre_t, Liste_imbriquee_t, ou qui ne commence
    par par DeuxSautsLigne_t"
  in modifie_sous_liste [] l






(*Là où des lexèmes Etoile_t encadrent ce qu'on veut mettre en gras ou en italique,
les remplace eux et ce qu'ils encadrent par un lexème Effet_t(Gras, l) ou Effet_t(Italique, l)
avec l la liste de lexèmes entre les deux bornes.
Cette fonction traite aussi les cas particuliers où des lexèmes Etoile_t n'ont pas de
correspondant, où si un début d'italique est interrompu par un début de gras
(ex : *abc**def** -> seul "def" sera en gras, et on laissera une Etoile_t au début)*)
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
                  | _ -> failwith "sous-gras bien fermé mais on n'a pas renvoyé juste Effet_t(gras, .) dans resultat"
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
                  | _ -> failwith "La fermeture du sous-gras(en italique, état 4) n'a pas renvoyé que le lexème Effet_t(Italique,(..))"
                )
                | _ -> failwith "Un état non reconnu a été renvoyé"   
            end
          else
            (*On ferme bien le gras*)
            begin
              sortie := ([Effet_t(Gras,(List.rev !lexemes_lus))], q, 0);
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
                sortie := ([Effet_t(Italique,(List.rev !lexemes_lus))], [] , 4);
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
                  | _ -> failwith "italique bien fermé mais on n'a pas renvoyé juste Effet_t(Italique,()) dans resultat"
                )
                (*L'état de réussite 1 n'existe pas pour la lecture d'italique car ** aurait été lu comme 2 étoiles directement, pas comme *[rien]* *)
                | 2 -> (
                  (*L'italique n'a pas été fermé mais la fonction s'est arrêté sur une lecture de ** qui peut fermer notre gras*)
                  sortie := ([Effet_t(Gras,((List.rev (Etoile_t :: !lexemes_lus)) @ resultat))], liste_restante, 0);
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
          sortie := ([Effet_t(Italique,(List.rev !lexemes_lus))], Etoile_t :: Etoile_t :: q, 0);
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
                sortie := ([Effet_t(Italique,(List.rev !lexemes_lus))], [Etoile_t], 0);
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
                  | _ -> failwith "gras bien fermé mais on n'a pas renvoyé juste Effet_t(Gras,.) dans resultat"
                )
                | 1 -> (
                  (*Ne doit pas arriver puisque si il y avait ****, on aurait détecté 3 * et on serait dans le cas d'avannt*)
                  failwith "Fermeture de gras vide dans la lecture de l'italique : l'impossible est arrivé"
                )
                | 2 -> (
                  (*Italique ferrmé dans la tentative de fermer le gras*)
                  begin
                    sortie := ([Effet_t(Italique,((List.rev (Etoile_t :: Etoile_t :: !lexemes_lus)) @ resultat))], liste_restante, 0);
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
              sortie := ([Effet_t(Italique,(List.rev !lexemes_lus))], q, 0);
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
          | _ -> print_lex_list resultat; print_newline ();failwith "La fermeture du gras n'a pas renvoyé que le lexème Effet_t(Gras,(..)"
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
          | _ -> failwith "La fermeture du gras(en italique, état 4) n'a pas renvoyé que le lexème Effet_t(Italique,(..))"
          
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
          | _ -> failwith "La fermeture de l'italique n'a pas renvoyé que le lexème Effet_t(Italique,(..))"
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

(*Renvoie une liste où on été regroupés sous leurs effets correspondant les listes de
lexèmes encadrées par des lexèmes bornes de ces effets (ex: l'effet gras des ** est appliqué,
l'italique des *, etc... pour les effets implémentés )*)
let applique_tous_effets (l: lexeme_t list): lexeme_t list =
  effet_gras_italique (l)



(*Construit l'arbre de syntaxe (plutôt la structure du document) correspondant à la liste de lexèmes traités l,
supposant qu'elle a été fournie par la fonction pretraitement_lexemes*)
let lexemeliste_to_arbre_syntaxe (l: lexeme_t list): doc =
  let l_decoupage = decoupage_paragraphes (decoupage_sections_titres l) in
  (*l_decoupage est une liste de Liste_imbriquee_t, chacune représentant soit un
  Paragraphe, soit une Section commençant par un titre*)

  (*Renvoie la liste des lexemes présents avant le premier SautLigne_t, et la liste
  des lexèmes qui viennent après, en ayant donc retiré ce SautLigne_t*)
  let rec coupe_sautligne (lus: lexeme_t list) (restants: lexeme_t list): (lexeme_t list) * (lexeme_t list) =
    match restants with
    | SautLigne_t :: q -> (List.rev lus, q)
    | [] -> (List.rev lus, [])
    | x :: q -> coupe_sautligne (x :: lus) q
  in
  (*Jusqu'à tomber sur un lexème Effet_t(), fusionne les Texte_t(), les Espace_t, et autres
  lexèmes ne commençant pas un effet spécial de la liste "restants", dans un seul texte (cf arbre_types.ml)
  (ex: Texte_t("Message");Etoile;Espace;Texte_t("test") devient Texte_t("Message* test") )
  "texte_concatene" sert d'accumulateur pour la récursivité terminale,
  et renvoie un couple (string,(liste des lexèmes d'après))*)
  let rec concatene_textes_avanteffet (texte_concatene: string) (restants: lexeme_t list): string * (lexeme_t list) =
    match restants with
    | Texte_t s :: q -> concatene_textes_avanteffet (texte_concatene^s) q
    | Etoile_t :: q -> concatene_textes_avanteffet (texte_concatene^"*") q
    | Espace_t :: q -> concatene_textes_avanteffet (texte_concatene^" ") q
    | Tiret_t :: q -> concatene_textes_avanteffet (texte_concatene^"-") q
    | Diese_t :: q -> concatene_textes_avanteffet (texte_concatene^"#") q
    | [] -> (texte_concatene, [])
    | Effet_t(_, _) :: _ -> (texte_concatene, restants)
    | l :: q -> (print_lex l; failwith "Erreur lors de la concaténation OU lexème non encore implémenté")
  
  in

  (*Précondition: s'applique sur une liste ne contenant pas de SautLigne_t (et pas non plus de lexèmes de + haut
  niveau comme les Liste_Imbriquee_t, DeuxSautsLigne_t, ...)
  tous_texte est l'accumulateur qui stocke les lexèmes créés et forme le miroir de la liste qui sera renvoyée*)
  let rec get_textelist_sanssautligne (effets_lex: lexeme_t list) (tous_textes: texte list): texte list =
    match effets_lex with
    | [] -> List.rev tous_textes
    | Effet_t(eff, contenu) :: q -> 
      get_textelist_sanssautligne q
        (Texte_effet(eff, get_textelist_sanssautligne contenu []) :: tous_textes)
    | _ -> 
      let (str, suite) = concatene_textes_avanteffet "" effets_lex in
      get_textelist_sanssautligne suite (Texte_nu(str) :: tous_textes)
  in

  (*Transforme une liste de lexèmes (du même niveau que les Texte_t, Espace, etc)
  (n'étant pas encore passé par le traitement applique_tous_effets,
  et contenant encore des SautLigne_t)
  en liste de "texte" (cf arbre_types.ml)*)
  let rec get_textelist (ll: lexeme_t list): texte list =
    let texte_list = ref [] in
    let lex_restants = ref ll in
    while (!lex_restants) <> [] do
      let (avant_sautligne, reste) = coupe_sautligne [] (!lex_restants) in
      lex_restants := reste;
      let effets_lex = applique_tous_effets avant_sautligne in
      texte_list := Texte_effet(EffetVide, get_textelist_sanssautligne effets_lex []) :: !texte_list
    done;
    List.rev (!texte_list)
  in


  (*liste_divisions sert d'accumulateur*)
  let rec liste_to_divisionlist (ll: lexeme_t list) (liste_divisions: division list): division list = 
    match ll with
    | Liste_imbriquee_t(q) :: suite ->(
      match q with
      | [Titre_t(niv, titre_contenu); Liste_imbriquee_t(section_contenu)] ->(
        (*On sait qu'il n'y a pas de SautLigne_t ni de DeuxSautLigne_t dans le titre_contenu,
        l'appel sur get_textelist doit donc renvoyer une liste d'un seul élément*)
        
        let titre_section: titre = (niv, Texte_effet(EffetVide, get_textelist titre_contenu)) in
        liste_to_divisionlist (suite) (Section(titre_section, liste_to_divisionlist section_contenu []) :: liste_divisions)        
      )
      | Titre_t(_,_) :: _ -> failwith "Erreur:Un titre ne suit pas la mise en forme [Titre_t;Liste_imbriquee_t]"
      | _ -> (
        (*Ne commence pas par un titre -> pas une Section mais un Paragraphe
        /!\ QUAND ON IMPLEMENTERA LES LISTES A PUCES IL FAUDRA TRAITER CA ICI NOTAMMENT /!\*)
        liste_to_divisionlist (suite) (Paragraphe([Texte(get_textelist q)]) :: liste_divisions)
      )
    )
    | [] -> List.rev(liste_divisions)
    | _ -> failwith "Autre chose qu'une Liste_imbriquee_t a été trouvée dans ll"
  
  in liste_to_divisionlist l_decoupage []
    
  

  (*
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


  *)