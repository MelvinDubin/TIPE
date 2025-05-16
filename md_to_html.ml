
(*Lit le fichier fileneame et renvoit son contenu dans une chaîne de caractères*)
let lit_fichier (filename: string): string =
  let file = open_in filename in
  let taille_tot = in_channel_length file in
  let s = Bytes.create taille_tot in
  really_input file s 0 taille_tot;
  close_in file;
  Bytes.unsafe_to_string s

let markdown_to_html_with_filters (filename_md : string) (filename_html : string) (filename_preset : string) : unit = 
  let pretraitement_lexemes, doit_ecrire_css = pretraitement_lexeme (texte_to_lexeme_list (lit_fichier filename_md)) in
  
  let arbre_syntaxe = ref (lexemeliste_to_arbre_syntaxe pretraitement_lexemes) in
  
  let colors,sommaire,titre = lit_fichier_preset filename_preset filename_md in 

  arbre_syntaxe := ajoute_couleurs_titres (!arbre_syntaxe) colors ;
  if (sommaire >0) then (
    arbre_syntaxe := ajoute_sommaire !arbre_syntaxe sommaire
  );
  if (not(titre = None)) then (
    arbre_syntaxe := ajoute_titre !arbre_syntaxe (Option.get titre)
  );

  let file_out = open_out filename_html in
  ecrit_en_html file_out (!arbre_syntaxe) doit_ecrire_css;
  close_out file_out;
  if doit_ecrire_css then
    ecrit_css "style.css" (tab_classes ()) (tab_palette ())
  else ()


(*Crée le fichir filename dans lequel est écrit contenu*)
let markdown_to_html (filename_md: string) (filename_html: string): unit =
  let liste_pretraitee, doit_ecrire_css = pretraitement_lexeme (texte_to_lexeme_list (lit_fichier filename_md)) in
  if doit_ecrire_css then print_string "\n\nOUIIIIIIIIIIIII-------------------------------\n\n" else print_string "\n\nNONNNNNNNNNNNNNNN-------------------------------\n\n";
  
  let arbre_syntaxe = lexemeliste_to_arbre_syntaxe liste_pretraitee in
  let file_out = open_out filename_html in
  ecrit_en_html file_out (arbre_syntaxe) doit_ecrire_css;
  close_out file_out;
  if doit_ecrire_css then
    ecrit_css "style.css" (tab_classes ()) (tab_palette ())
  else ()

