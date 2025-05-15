type highlight_lex =
  | Texte_blanc of string
  | Motcle of string
  | Parenthesage of string*int (*niveau de la parenthèse*)
  | Motcle_match of string*int (*niveau du match with*)
  | String of string
  | Caractere of string
  | Nombre of string
  | Booleen of string
  | Type of string
  | Constructeur of string
  | NomVarFun of string
  | Commentaire of string

let tab_classes () = [|"ocaml_motcle";"ocaml_parenthese0";"ocaml_parenthese1";"ocaml_parenthese2";"ocaml_matching0";"ocaml_matching1";"ocaml_matching2";"ocaml_string";
"ocaml_char";"ocaml_nombre";"ocaml_bool";"ocaml_type";"ocaml_constructeur";"ocaml_varfun"; "ocaml_commentaire"|]
let tab_palette () = [|"#E06C75";"#FFD708";"#DA70BD";"#179FE0";"#E06C75";"#87B743";"#8379C6";"#E5C07A";"#E5C07A";"#C678DD";"#52B6C2";"#52B6C2";"#56B6C2";"#98C373";"#5C6370"|]
let nb_couleurs_parentheses () = 3
let nb_couleurs_matching () = 3

(*Renvoie un automate reconnaissant des mots clés d'une syntaxe simplifiée d'ocaml, ainsi qu'un tableau d'entiers t tel que
t.(i) donne le "type" d'un mot qui finit sur l'état final i
Si i = 0, i n'est pas un état final
  i = 1, le mot finissant en i est un mot clé,
  i = 2 : parenthésage,
  i = 3 : matching,
  i = 4 : chaine de caractères,
  i = 5 : caractère
  i = 6 : nombre,
  i = 7 : booléen,
  i = 8 : type
  i = 9 : commentaire
  i = 10 : constructeur*)
let creer_automate_ocaml (): automate * (int array) = 
  let a = ref (creer_automate 1 [0] [] (fun x y -> None)) in

  let motscles1 = ["let";"rec";"in";"if";"then";"else";"while";"do";"done";"not";"for";"to";"downto";"fun"; "function"; "try"; "and"; "new"; "object"; "assert"; "val"; "of"] in  
  let a_temp, etats_f_motscles1 = ajoute_plusieurs_mots_automate (!a) motscles1 [] in
  a := a_temp;
  let motscles2 = ["=";">";"+";"-";",";";";"^";":"; "<"; "|"; "&";".";"!";"*"] in (*On doit les ajouter plus tard*)
  

  let parenthesage1 = ["begin";"end"] in
  let a_temp, etats_f_parenthesage1 = ajoute_plusieurs_mots_automate (!a) parenthesage1 [] in
  a := a_temp;
  let parenthesage2 = [")";"[";"]";"[|";"|]";"{";"}"] in (*On les ajoute aussi plus tard*)
  
  let matching = ["match"; "with"; "when"] in
  let a_temp, etats_f_matching = ajoute_plusieurs_mots_automate (!a) matching [] in
  a := a_temp;

  let booleens = ["true"; "false"] in
  let a_temp, etats_f_bool = ajoute_plusieurs_mots_automate (!a) booleens [] in
  a := a_temp;
  
  let type_defaut = ["int";"char";"string";"float";"bool";"bytes";"unit"; "list"; "array"; "option"] in
  let a_temp, etats_f_types = ajoute_plusieurs_mots_automate (!a) type_defaut [] in
  a := a_temp;
  
  print_endline "OK FIN DE LA PARTIE 1 ICI ?";

  (*Pour ajouter les commentaires, il faut brancher le mot "(*" là où il y avait déjà la parenthèse ouvrante dans l'automate (com_deb : commentaire début)*)
  let a_temp, com_deb = ajoute_mot_automate_et_renvoie_son_etat_final (!a) ['(';'*'] in
  a := a_temp;
  (!a).final.(com_deb) <- false; (*Ce ne sera pas un état final*)

  (*let etiquette_valide_pour_nv_transi1 (e: char): bool =
    let eti = int_of_char e in
    (eti>=int_of_char 'a' && eti <= int_of_char 'z')||
    (eti>=int_of_char 'A' && eti<=int_of_char 'Z')||
    (eti>=int_of_char '0' && eti<=int_of_char '9')||
    (List.mem e ['_'; '\''; '\\'; '\n'; '\r'; '\t'; '.'; '\''; '\"'])
  in*)

  (*Ajout de la lecture des nombres, des caractères, des chaines de caractères, et des commentaires*)
  let nb_etats = Array.length (!a).ini in
  let nv_etats_finaux = changetaille_tableau (!a).final (nb_etats+12) false in
  let nv_etats_initiaux = changetaille_tableau (!a).ini (nb_etats+12) false in 
  (*différents états à ajouter :*)
  let (nb_debut, nb_decimales, char_deb, char_specialchar,
      char_content, char_fin, str_content,str_specialchar, str_fin, nocolor_content, constr_deb, constr_content) =
    (nb_etats, nb_etats+1, nb_etats+2, nb_etats+3, nb_etats+4, nb_etats+5, nb_etats+6,
    nb_etats+7, nb_etats+8, nb_etats+9, nb_etats+10, nb_etats+11) in  
  let nv_transi (etat:int) (etiquette:char): int option =
    Printf.printf "j'entre dans 2eme couche avec etat: %d, etiquette:%c\n" etat etiquette;
    if (*etiquette_valide_pour_nv_transi1 etiquette*) true then (
      match (etat,etiquette) with
      | 0, n when List.mem n ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] -> Some nb_debut
      | etat, '.' when etat=nb_debut -> Some nb_decimales
      | etat, n when ((etat=nb_debut || etat=nb_decimales)
                && (List.mem n ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']))
                -> Some etat
      | 0, '\'' -> Some char_deb
      | etat, '\\' when etat=char_deb -> Some char_specialchar
      | etat, _ when (etat=char_deb || etat=char_specialchar) -> Some char_content
      | etat, '\'' when etat=char_content-> Some char_fin
      | 0, '\"' -> Some str_content
      | etat, '\\' when etat=str_content -> Some str_specialchar
      | etat, _ when etat=str_specialchar -> Some str_content
      | etat, '\"' when etat=str_content -> Some str_fin
      | etat, _ when etat=str_content -> Some str_content
      | _ -> (
        Printf.printf "j'entre dans 3eme couche avec etat: %d, etiquette:%c\n" etat etiquette;
        match (!a).transi etat etiquette with
        | Some i -> Some i
        | None -> (
          Printf.printf "je suis dans le None.\n";
          try (*Si etiquette pose une erreur dans le int_of_char*)
            let eti = int_of_char etiquette in 
            if (eti>=int_of_char 'a' && eti <= int_of_char 'z')||(etiquette='_')||(eti>=int_of_char 'A' && eti<=int_of_char 'Z')||
            (eti>=int_of_char '0' && eti<=int_of_char '9')
              then Some nocolor_content
            else None
          with
          | _ ->None 
        )
      )
    ) else (
      None
    )
  in 
  let nv_a = ref {
    ini = nv_etats_initiaux;
    transi = nv_transi;
    final = nv_etats_finaux
  } in

  print_endline "FIN PARTIE 2 ICI ?";

  (*Ajout des mots clés caractères spéciaux (qui ne peuvent pas commencer un mot blanc,
  ex: leto est coloré en blanc même si let est un mot clé, mais +o est coloré en rouge pour le +, et blanc pour le o)*)
  let a_temp, etats_f_motscles2 = ajoute_plusieurs_mots_automate (!nv_a) motscles2 [] in
  nv_a := a_temp;
  let a_temp, etats_f_parenthesage2 = ajoute_plusieurs_mots_automate (!nv_a) parenthesage2 [] in
  nv_a := a_temp;

  print_endline "test1";
  (*Pour ajouter les commentaires, il faut brancher le mot "(*" là où il y avait déjà la parenthèse ouvrante dans l'automate (com_deb : commentaire début)*)
  let a_temp, com_deb = ajoute_mot_automate_et_renvoie_son_etat_final (!nv_a) ['(';'*'] in
  nv_a := a_temp;
  (!nv_a).final.(com_deb) <- false; (*Ce ne sera pas un état final*)



  print_endline "test2";
  let nb_etats = Array.length (!nv_a).ini in
  let nv_etats_finaux = changetaille_tableau (!nv_a).final (nb_etats+4) false in
  let nv_etats_initiaux = changetaille_tableau (!nv_a).ini (nb_etats+4) false in 
  let (com_deb, com_content, com_fin, com_fin2) = (nb_etats, nb_etats+1, nb_etats+2, nb_etats+3) in
  print_endline "test3";

  let nv_transi_final (etat: int) (etiquette: char): int option =
    Printf.printf "j'entre dans 1ere couche avec etat: %d, etiquette:%c\n" etat etiquette;
    match etat, etiquette with
    | 0, '(' -> Some com_deb
    | etat, '*' when etat=com_deb -> Some com_content
    | etat, _ when etat=com_deb -> None (*C'est alors une parenthèse ouvrante seule*)
    | etat, '*' when etat=com_content -> Some com_fin
    | etat, _ when etat=com_content -> Some com_content
    | etat, ')' when etat=com_fin -> Some com_fin2
    | etat, _ when etat=com_fin -> Some com_content
    | etat, _ when etat=com_fin2 -> None
    | _ -> 
      if (List.mem etat (List.concat [etats_f_motscles2; etats_f_parenthesage2])) then None
      else (!nv_a).transi etat etiquette
    
  in
  print_endline "test4";
  List.iter (fun i -> nv_etats_finaux.(i) <- true) [com_fin2; nb_debut; nb_decimales; char_fin; str_fin; nocolor_content];
  
  print_endline "FIN PARTIE 3 ICI ?";

  (*Ce tableau sera renvoyé, il indique dans la case i la catégorie des mots finissant en i à la lecture*)
  let tableau_types_etatsfinaux = Array.make (Array.length nv_etats_finaux) 0 in
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 1) etats_f_motscles1;
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 1) etats_f_motscles2;
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 2) etats_f_parenthesage1;
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 2) etats_f_parenthesage2;
  tableau_types_etatsfinaux.(com_deb) <- 2;
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 3) etats_f_matching;
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 6) [nb_debut; nb_decimales];
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 7) etats_f_bool;
  List.iter (fun i -> tableau_types_etatsfinaux.(i) <- 8) etats_f_types;
  tableau_types_etatsfinaux.(str_fin) <- 4;
  tableau_types_etatsfinaux.(char_fin) <- 5;
  tableau_types_etatsfinaux.(com_fin) <- 9;
  tableau_types_etatsfinaux.(com_content) <- 9;
  tableau_types_etatsfinaux.(com_fin2) <- 9;
  (*a ajouter : constructeurs = 10*)
  
  print_endline "FIN PARTIE 4 ICI ?";
  ({
    ini = nv_etats_initiaux;
    transi = nv_transi_final;
    final = nv_etats_finaux
  }, tableau_types_etatsfinaux)
 

(*ATTENTION ! En écrivant le code ocaml dans html, il faut le mettre dans une balise <pre> et dans <code>*)




let nouveau_parse_ocaml_highlight (w: string): highlight_lex list =
  let (autom_ocaml, tab_types) = creer_automate_ocaml () in

  let i = ref 0 in
  let n = String.length w in
  let prof_parent = ref 0 in

  let lexeme_list = ref [] in

  let decla_varfun = ref 0 in (*Servira à savoir si on doit considérer un mot inconnu comme du texte blanc ou le colorer
  comme un nom de variable/fonction car il est déclaré par un let. 0->pas de décla, 1->juste un let, 2->let rec. Si on met un 2eme rec, on
  retombe à 0 car la syntaxe let rec rec ne déclare rien*)

  while (!i < n) do
    print_int !i; print_newline ();
    let (nv_i, etat) = get_indice_mot_valide autom_ocaml 0 w (!i) in
    if nv_i = !i then (
      (*Caractère illisible par l'automate, comme un caractère avec accent hors d'un commentaire ou autre*)
      lexeme_list := Texte_blanc(String.sub w !i 1)::(!lexeme_list);
      i := !i + 1
    )
    else (
      let mot_lu = String.sub w !i (nv_i - !i) in
      let lex = match tab_types.(etat) with
        | 0 -> if ((int_of_char mot_lu.[0] >= int_of_char 'A') && (int_of_char mot_lu.[0] <= int_of_char 'Z'))||(mot_lu="_") then (Constructeur mot_lu)
        else (if !decla_varfun > 0 then (decla_varfun := 0; NomVarFun mot_lu) else Texte_blanc mot_lu)
        | 1 -> ( (*Si c'est un let ou un let rec, on est en train de déclarer une variable ou une fonction*)
          if (mot_lu = "let" || (mot_lu="rec"&&(!decla_varfun = 1))) then (
            incr decla_varfun
          ) else (decla_varfun := 0);
          Motcle mot_lu)
        | 2 -> Parenthesage (mot_lu, if List.mem mot_lu ["(";"[";"{";"[|";"begin"] then !prof_parent else (!prof_parent -1))
        | 3 -> Motcle_match (mot_lu, 0)
        | 4 -> String mot_lu
        | 5 -> Caractere mot_lu 
        | 6 -> Nombre mot_lu
        | 7 -> Booleen mot_lu
        | 8 -> Type mot_lu
        | 9 -> Commentaire mot_lu
        | 10 -> Constructeur mot_lu
        | _ -> failwith "ne peut pas arriver"
      in
      (if List.mem mot_lu ["(";"[";"{";"[|";"begin"] then incr prof_parent
      else if List.mem mot_lu [")";"]";"}";"|]";"end"] then decr prof_parent
      else ());
      lexeme_list := lex :: (!lexeme_list);
      i := nv_i
    )
  done;
  List.rev !lexeme_list









(*Renvoie la liste des lexèmes généraux de type highlight_lex obtenue
après analyse lexicale du mot w*)
let parse_ocaml_highlight (w: string): highlight_lex list =
  let (autom_ocaml, tab_types) = creer_automate_ocaml () in

  let decla_varfun = ref 0 in (*Servira à savoir si on doit considérer un mot inconnu comme du texte blanc ou le colorer
  comme un nom de variable/fonction car il est déclaré par un let. 0->pas de décla, 1->juste un let, 2->let rec. Si on met un 2eme rec, on
  retombe à 0 car la syntaxe let rec rec ne déclare rien*)

  (*Renvoie le highlight_lex du mot w lu, en étant dans une profondeur de parenthésage de prof_parent
  et une profondeur de match-with de prof_match (pour la coloration)*)
  let parse_partiel (w: string) (prof_parent: int) (prof_match: int): highlight_lex = 
    match exec_mot autom_ocaml 0 w 0 ((String.length w) - 1) with
    | None -> if ((int_of_char w.[0] >= int_of_char 'A') && (int_of_char w.[0] <= int_of_char 'Z')) then (
          Constructeur  w       
        ) else (Texte_blanc w)
    | Some etat_fin -> (
      if (tab_types.(etat_fin) > 1) then (decla_varfun := 0);
      match tab_types.(etat_fin) with
      | 0 -> if ((int_of_char w.[0] >= int_of_char 'A') && (int_of_char w.[0] <= int_of_char 'Z')) then (
          Constructeur  w       
        ) else (if !decla_varfun > 0 then (decla_varfun := 0; NomVarFun w) else Texte_blanc w)
      | 1 -> ( (*Si c'est un let ou un let rec, on est en train de déclarer une variable ou une fonction*)
        if (w = "let" || (w="rec"&&(!decla_varfun = 1))) then (
          incr decla_varfun
        ) else (decla_varfun := 0);
        Motcle w)
      | 2 -> Parenthesage (w, if w="end" then prof_parent - 1 else prof_parent)
      | 3 -> Motcle_match (w, if w="match" then prof_match + 1 else prof_match)
      | 4 -> String w
      | 5 -> Caractere w 
      | 6 -> Nombre w
      | 7 -> Booleen w
      | 8 -> Type w
      | 9 -> Commentaire w
      | _ -> failwith "ne peut pas arriver"
    )
  in
  let sep_indice = ref (-1) in 
  let prof_parent = ref 0 in
  let prof_match = ref (-1) in (*car le premier match se met à la profondeur supérieure*)
  let indice = ref 0 in
  let lexeme_list = ref [] in

  while (!indice) < (String.length w) do
    if List.mem w.[!indice] [' '; '\n'; '\r'; '\t'] then (
      let sep = (Texte_blanc (String.make 1 w.[!indice])) in
      if (!indice > !sep_indice + 1) then ( (*il y a un mot à lire*)
        let lex = parse_partiel (String.sub w (!sep_indice + 1) (!indice - !sep_indice -1)) !prof_parent !prof_match in
        lexeme_list := lex :: (!lexeme_list); (*:/!\ peut être +1 -1 ici*)
        match lex with
        | Parenthesage (x,_) when x="begin" -> (incr prof_parent; print_string "increment begin\n")
        | Parenthesage (x,_) when x="end" -> (decr prof_parent; print_string "decr end\n")
        | Motcle_match ("match",_) -> (incr prof_match)
        | _ -> ()
      ) else ();
      lexeme_list := sep :: (!lexeme_list);
      sep_indice := !indice
      
    ) else (
      if List.mem w.[!indice] ['+';'-';'=';'&';',';'|';'^';':';'<';'>';'@';'!';';'] then (


        let sep = (Motcle (String.make 1 w.[!indice])) in
        if (!indice > !sep_indice + 1) then ( (*il y a un mot à lire*)
          let lex = parse_partiel (String.sub w (!sep_indice + 1) (!indice - !sep_indice -1)) !prof_parent !prof_match in
          lexeme_list := lex :: (!lexeme_list);
          match lex with
          | Parenthesage (x,_) when x="begin" -> (incr prof_parent; print_string "increment begin\n")
          | Parenthesage (x,_) when x="end" -> (decr prof_parent;print_string "decr end\n";print_int (!prof_parent))
          | Motcle_match ("match",_) -> (incr prof_match)
          | _ -> ()
        ) else ();
        lexeme_list := sep :: (!lexeme_list);
        sep_indice := !indice
        
      ) else (
        if List.mem w.[!indice] ['(';'[';'{'] then (
          let sep = Parenthesage (String.make 1 w.[!indice], !prof_parent) in
          incr prof_parent;
          if (!indice > !sep_indice + 1) then ( (*il y a un mot à lire*)
            let lex = parse_partiel (String.sub w (!sep_indice + 1) (!indice - !sep_indice -1)) !prof_parent !prof_match in
            lexeme_list := lex :: (!lexeme_list); (*:/!\ peut être +1 -1 ici*)
            match lex with
            | Parenthesage (x, _) when x="begin" -> (incr prof_parent; print_string "increment begin\n")
            | Parenthesage (x, _) when x="end" -> (decr prof_parent; print_string "decr end\n";print_int (!prof_parent))
            | Motcle_match ("match", _) -> (incr prof_match)
            | _ -> ()
          ) else ();
          lexeme_list := sep :: (!lexeme_list);
          sep_indice := !indice
        
        ) else (
          if List.mem w.[!indice] [')'; ']'; '}'] then (
            if (!indice > !sep_indice + 1) then ( (*il y a un mot à lire*)

              let lex = parse_partiel (String.sub w (!sep_indice + 1) (!indice - !sep_indice -1)) !prof_parent !prof_match in
              lexeme_list := lex :: (!lexeme_list); (*:/!\ peut être +1 -1 ici*)
              match lex with
              | Parenthesage (x,_) when x="begin" -> (incr prof_parent; print_string "increment begin\n")
              | Parenthesage (x,_) when x="end" -> (decr prof_parent; print_string "decr end\n";print_int (!prof_parent))
              | Motcle_match ("match",_) -> (incr prof_match)
              | _ -> ()
            ) else ();
            let sep = Parenthesage (String.make 1 w.[!indice], !prof_parent - 1) in
            decr prof_parent;
            lexeme_list := sep :: (!lexeme_list);
            sep_indice := !indice
          ) else (
            if (!indice = (String.length w) - 1) then (
              let lex = parse_partiel (String.sub w (!sep_indice + 1) (!indice - !sep_indice)) !prof_parent !prof_match in
              lexeme_list := lex :: (!lexeme_list); (*:/!\ peut être +1 -1 ici*)
            ) else ()
          )
        )
      )
    );
  incr indice
  done;
  List.rev !lexeme_list




(*Transforme chaque lexème général de hlex_l en chaine de caractère contenant les balises html
correspondantes au type du lexème, puis concatène toutes ces chaines de caractères à la suite pour
renvoyer la chaine ainsi formée.
On donne en arguments la profondeur max que peuvent avoir le parenthésage et le matching, les niveaux de profondeurs
au delà boucleront modulo ce nombre maximal*)
let lex_list_to_html (hlex_l: highlight_lex list) ((profmax_parent, profmax_match) : int*int): string =
  (*Ajoute des balises html donnant la classe classe au mot w*)
  let balise_classe (w: string) (classe: string): string =
    "<span class=\""^classe^"\">"^w^"</span>"
  in
  (*Transforme un lexème général en mot entouré de balises html
  lui donnant la bonne classe*)
  let ajoute_balises_lex (hlex: highlight_lex): string =
    match hlex with
    | Texte_blanc w -> w
    | Motcle w -> balise_classe w "ocaml_motcle"
    | Parenthesage (w,p) -> balise_classe w ("ocaml_parenthese"^(string_of_int (p mod profmax_parent)))
    | Motcle_match (w,p) -> balise_classe w ("ocaml_matching"^(string_of_int (p mod profmax_match)))
    | String w -> balise_classe w "ocaml_string"
    | Caractere w -> balise_classe w "ocaml_char"
    | Nombre w -> balise_classe w "ocaml_nombre"
    | Booleen w -> balise_classe w "ocaml_bool"
    | Type w -> balise_classe w "ocaml_type"
    | Constructeur w -> balise_classe w "ocaml_constructeur"
    | NomVarFun w -> balise_classe w "ocaml_varfun"
    | Commentaire w -> balise_classe w "ocaml_commentaire"
  in
  String.concat "" (List.map ajoute_balises_lex hlex_l)



(*A partir d'un texte contenant du code OCaml, renvoie le texte balisé correctement pour affecter à chaque
mot une classe correspondante à ce qu'il est et comment il devra être coloré*)
let colore_code_ocaml (w: string): string =
  lex_list_to_html (nouveau_parse_ocaml_highlight w) (nb_couleurs_parentheses (), nb_couleurs_matching ())

(*Ecrit le fichier css qui affecte la couleur tab_palette.(i) à la classe tab_classes.(i)., ainsi que les attributs par défaut des sections de code ocaml
Précondition: tab_classes a la même taille que tab_palette*)
let ecrit_css (nomfichier: string) (tab_classes: string array) (tab_palette: string array): unit =
  let n = Array.length tab_classes in
  assert(n = Array.length tab_palette);
  let file_out = open_out nomfichier in
  output_string file_out ".ocaml_code{\n    color: white;\n    background-color: #222222;\n    line-height: 2em;\n}";
  for i = 0 to (n - 1) do
    output_string file_out ".";
    output_string file_out tab_classes.(i);
    output_string file_out "{\n\tcolor: ";
    output_string file_out tab_palette.(i);
    output_string file_out ";\n}\n"
  done;
  close_out file_out


let ecrit_fichier (nomfichier: string) (text: string): unit =
  let file_out = open_out nomfichier in output_string file_out text; close_out file_out

  (*fonction pour tester plus rapidement*)
let genere_tout (code_ocaml: string): unit =
  let html_out = open_out "test.html" in
  output_string html_out "<!DOCTYPE html>\n<html>\n<head>\n  <link rel=\"stylesheet\" href=\"test.css\">\n</head>\n<body>\n    <pre class=\"ocaml_code\"><code>";
  output_string html_out (colore_code_ocaml code_ocaml);
  output_string html_out "    </code></pre>\n</body>\n</html>";
  close_out html_out;
  ecrit_css "test.css" (tab_classes()) (tab_palette())