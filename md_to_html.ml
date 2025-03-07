
(*Lit le fichier fileneame et renvoit son contenu dans une chaîne de caractères*)
let lit_fichier (filename: string): string =
  let file = open_in filename in
  let taille_tot = in_channel_length file in
  let s = Bytes.create taille_tot in
  really_input file s 0 taille_tot;
  close_in file;
  Bytes.unsafe_to_string s



(*Crée le fichir filename dans lequel est écrit contenu*)
let markdown_to_html (filename_md: string) (filename_html: string): unit =
  let arbre_syntaxe = lexemeliste_to_arbre_syntaxe (pretraitement_lexeme (texte_to_lexeme_list (lit_fichier filename_md))) in
  let file_out = open_out filename_html in
  ecrit_en_html file_out (arbre_syntaxe);
  close_out file_out