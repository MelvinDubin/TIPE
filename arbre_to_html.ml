
(*--------ECRITURE EN HTML DEPUIS UN ARBRE DE TRAITEMENT DE TEXTE---------*)
(*a ouvrir avec : 
arbre_types.ml *)


(*renvoie la balise html associée à un effet*)
let balise_html (effet : effet_texte) : string = match effet with 
  |Italique -> "I" 
  |Gras -> "B"
  |Liste_puces -> "ul"
  |Liste_numerotee -> "ol"
  |Element_liste -> "li"
  |Paragraphe -> "p"
  |EffetVide -> failwith "pas de balise associée"
  |_ -> failwith "pas encore implémenté"


(*écrit la balise html ouvrante associée à l'effet effet_texte dans le fichier out_channel*)
let affiche_balise_html_ouvrante (fichier : out_channel) (effet : effet_texte)  : unit = 
  if not(effet = EffetVide) then (
    output_string fichier ("<" ^ (balise_html effet) ^ ">")
  )


(*écrit la balise fermante html associée à l'effet effet_texte dans le fichier out_channel*)
let affiche_balise_html_fermante (fichier : out_channel) (effet : effet_texte)  : unit = 
  if not(effet = EffetVide) then (
    output_string fichier ("</" ^ (balise_html effet) ^ ">")
)


let rec ecrit_en_html (fichier: out_channel)  (arb : traitement_texte arbre) : unit = 
match arb with 
  |Feuille (Texte contenu) -> output_string fichier contenu 
  |Noeud(Effet nouvel_effet, l) -> (affiche_balise_html_ouvrante fichier nouvel_effet ; List.iter (ecrit_en_html fichier) l ; affiche_balise_html_fermante fichier nouvel_effet )
  |_ -> failwith "erreur de syntaxe"