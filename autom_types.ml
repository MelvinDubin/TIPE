type automate = {
  ini : bool array;
  transi : int -> char -> (int option);
  final : bool array (*final.(i) indique si i est final*)
}

(*Renvoie un automate dont les états sont les entiers de 0 à nb_etats-1,
dont le tableau des états initiaux vaut 1 sur les indices de etats_init,
le tableau des états finaux vaut 1 sur les indices de etats_finaux,
et sa fonction de transitions est delta*)
let creer_automate (nb_etats: int) (etats_init: int list) (etats_finaux: int list) (delta:  int -> char -> (int option)): automate =
  let tab_ini = Array.make nb_etats false in
  let tab_final = Array.make nb_etats false in
  (*met la case i de t à true*)
  let met_a_jour_tab (t: bool array) (i: int): unit =
    t.(i) <- true
  in
  List.iter (met_a_jour_tab tab_ini) etats_init;
  List.iter (met_a_jour_tab tab_final) etats_finaux;
  {
    ini = tab_ini;
    transi = delta;
    final = tab_final
  }
  
(*Renvoie vrai si etat est un état final de a, faux sinon*)
let est_final (a:automate) (etat: int): bool =
  a.final.(etat)

(*Renvoie une liste des caractères de w*)
let char_list_of_string (w: string): char list =
  let l = ref [] in
  let n = String.length w in 
  for i = 0 to (n-1) do
    l := w.[i] :: !l
  done;
  List.rev !l