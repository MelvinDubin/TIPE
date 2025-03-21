
(*--------ECRITURE EN HTML DEPUIS UN ARBRE DE TRAITEMENT DE TEXTE---------*)
(*a ouvrir avec : 
arbre_types.ml *)


(*renvoie la balise html associée à un effet*)
let balise_html_effet (e : effet) = match e with 
  |Italique -> "I" 
  |Gras -> "B"
  |Couleur _ -> failwith "balise html ouvrante différente de celle fermante"
  |EffetVide -> failwith "pas de balise associée"
  |_ -> failwith "pas encore implémenté"

(*renvoie la balise html associée à un titre de niveau i*)
let balise_html_titre (i : int) = 
  assert(0<i) ; 
  assert(i<=6) ; (*il n'y a que des titres de niveau 1 à 6*)
  "h"^(string_of_int i)


(*écrit la balise html ouvrante contenant str dans le fichier out_channel*)
let ecrit_balise_html_ouvrante (fichier : out_channel) (str : string)  : unit = 
    output_string fichier ("<" ^ str ^ ">")


(*écrit la balise html fermante contenant str dans le fichier out_channel*)
let ecrit_balise_html_fermante (fichier : out_channel) (str : string)  : unit = 
    output_string fichier ("</" ^ str ^ ">")

let rec traite_texte (fichier : out_channel) (t : texte) : unit = 
  match t with 
  |Texte_nu str -> output_string fichier str 
  |Texte_effet (e,sous_textes) -> 
    (match e with 
    |Couleur str -> 
      ecrit_balise_html_ouvrante fichier ("font color =\""^str^"\"") ; 
      List.iter (traite_texte fichier) sous_textes ; 
      ecrit_balise_html_fermante fichier "font"
    |EffetVide -> List.iter (traite_texte fichier) sous_textes
    (*cas général sans arguments*)
    |_ -> let balise = balise_html_effet e in 
      ecrit_balise_html_ouvrante fichier balise ; 
      List.iter (traite_texte fichier) sous_textes ; 
      ecrit_balise_html_fermante fichier balise 
    )

let rec traite_bloc (fichier : out_channel) (b : bloc) : unit = 
  match b with 
  |Texte textes_liste -> List.iter (fun texte -> traite_texte fichier texte ; ecrit_balise_html_ouvrante fichier "bl") textes_liste 
  |ListeNumerotee (elements_liste) ->
    ecrit_balise_html_ouvrante fichier "ol" ; 
    List.iter (fun elt -> 
      ecrit_balise_html_ouvrante fichier "li";
      traite_bloc fichier elt ; 
      ecrit_balise_html_fermante fichier "li" 
      ) elements_liste ; 
    ecrit_balise_html_fermante fichier "ol"
  | ListeAPuces (elements_liste) ->
    ecrit_balise_html_ouvrante fichier "ul" ; 
    List.iter (fun elt -> 
      ecrit_balise_html_ouvrante fichier "li";
      traite_bloc fichier elt ; 
      ecrit_balise_html_fermante fichier "li"  
      ) elements_liste ; 
    ecrit_balise_html_fermante fichier "ul"
  (*à implémenter : code tableaux etc*)

let rec traite_division (fichier : out_channel) (d : division) : unit = 
  match d with 
  |Section ((i,tex), sous_sections) -> 
    (
      (*ecriture du titre*)
      let balise_titre = balise_html_titre i in 
      ecrit_balise_html_ouvrante fichier balise_titre ; 
      traite_texte fichier tex ; 
      ecrit_balise_html_fermante fichier balise_titre ;
      (*traitement des sous sections*)
      ecrit_balise_html_ouvrante fichier "section" ; 
      List.iter (traite_division fichier) sous_sections ;
      ecrit_balise_html_fermante fichier "section" 
    
    )
  |Paragraphe blocs_liste ->
    (ecrit_balise_html_ouvrante fichier "p" ;
    List.iter (traite_bloc fichier) blocs_liste ;
    ecrit_balise_html_fermante fichier "p"
    )
  |Barre -> ecrit_balise_html_ouvrante fichier "hr"


let ecrit_en_html (fichier: out_channel)  (document : doc) : unit = 
  ecrit_balise_html_ouvrante fichier "html" ;
  ecrit_balise_html_ouvrante fichier "head";
  ecrit_balise_html_fermante fichier "head";
  ecrit_balise_html_ouvrante fichier "body";
  List.iter (traite_division fichier) document ;
  ecrit_balise_html_fermante fichier "body";
  ecrit_balise_html_fermante fichier "html"

