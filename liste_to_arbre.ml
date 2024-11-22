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
  |(false,_, l1) -> [l1] (*il n'y a plus d'autre deux sauts de ligne*)