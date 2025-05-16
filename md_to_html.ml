
(*Lit le fichier fileneame et renvoit son contenu dans une chaîne de caractères*)
let lit_fichier (filename: string): string =
  let file = open_in filename in
  let taille_tot = in_channel_length file in
  let s = Bytes.create taille_tot in
  really_input file s 0 taille_tot;
  close_in file;
  Bytes.unsafe_to_string s

let markdown_to_html_with_filters (filename_md : string) (filename_html : string) (filename_preset : string) : unit = 
  let arbre_syntaxe = ref (lexemeliste_to_arbre_syntaxe (pretraitement_lexeme (texte_to_lexeme_list (lit_fichier filename_md)))) in
  
  let colors,sommaire,titre,transforme_raccourcis = lit_fichier_preset filename_preset filename_md in 

  arbre_syntaxe := raccourcis_to_effet !arbre_syntaxe transforme_raccourcis ;
  arbre_syntaxe := ajoute_couleurs_titres (!arbre_syntaxe) colors ;
  if (sommaire >0) then (
    arbre_syntaxe := ajoute_sommaire !arbre_syntaxe sommaire
  );
  if (not(titre = None)) then (
    arbre_syntaxe := ajoute_titre !arbre_syntaxe (Option.get titre)
  );

  let file_out = open_out filename_html in
  ecrit_en_html file_out (!arbre_syntaxe);
  close_out file_out


(*Crée le fichir filename dans lequel est écrit contenu*)
let markdown_to_html (filename_md: string) (filename_html: string): unit =
  let arbre_syntaxe = lexemeliste_to_arbre_syntaxe (pretraitement_lexeme (texte_to_lexeme_list (lit_fichier filename_md))) in
  let file_out = open_out filename_html in
  ecrit_en_html file_out (arbre_syntaxe);
  close_out file_out

