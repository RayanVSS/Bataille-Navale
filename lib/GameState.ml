open Bateaux
open Plateaux
open Action
open GameView
open Outils
open IA

(* Fonction pour vérifier si tous les bateaux sont coulés *)

let tous_bateaux_coules plateau =
  let coules = ref true in
  Array.iter (fun ligne ->
      Array.iter (fun case ->
          match case with
          | Navire (_, Intact) -> coules := false
          | Navire (_,Touche) -> coules := false
          | _ -> ()
        ) ligne
    ) plateau;
  !coules

let tours_joueur j plateau_joueur liste_bateaux = 
    print_endline ("Au tour du joueur "^ string_of_int j ^" de jouer");
    print_endline "Entrez les coordonnées de tir (x y) :";
    let coords = read_line () in
    let coords_split = String.split_on_char ' ' coords in
    match coords_split with
    | [x_str; y_str] ->
        let coor = parse_coords [x_str; y_str] in
        let x = fst coor in
        let y = snd coor in
        (match tirer plateau_joueur x y with
          | Some i ->
              if i >= 0 then
                (update_etat (get_coord i !liste_bateaux) plateau_joueur ; true)
              else true (*Cas ou le joueur doit recommencer*)
          | _ -> false;);
    | _ -> 
        (print_endline "Entrée invalide, réessayez.";
        true;) (* Répéter en cas d'erreur *)

let tours_ia plateau_joueur liste_bateaux =
  print_endline "Tour de l'IA";
  afficher_plateau plateau_joueur;
  let i = ia_tirer plateau_joueur in
  if i >= 0 then let list = (get_coord i !liste_bateaux) in (
    if(verif_coule list plateau_joueur) then (coule list plateau_joueur;  reset_tirs () ;print_endline "L'IA a coulé un bateau!";)
    else print_endline "L'IA a touché un bateau!"; true;)
  else false

(* Fonction pour placer tous les bateaux d'un joueur *)

(* Fonction principale du jeu *)
let jeu () =
  let () = print_endline "Bienvenue dans le jeu de bataille navale!" in
  (* Création du plateau *)
  let liste_bateaux_joueur_1 = ref [] in
  let liste_bateaux_joueur_2 = ref [] in
  let plateau_joueur_1 = creer_plateau plateau_taille in
  let plateau_joueur_2 = creer_plateau plateau_taille in
  (* Placement des bateaux *)
  print_endline "Au tour du joueur 1 de placer ces bateaux";
  placer_tous_bateaux plateau_joueur_1 liste_bateaux_joueur_1;
  clearT ();
  print_endline "Au tour du joueur 2 de placer ces bateaux";
  placer_tous_bateaux plateau_joueur_2 liste_bateaux_joueur_2;
  clearT ();

  (* Boucle principale du jeu *)
  let rec boucle x =
    if tous_bateaux_coules plateau_joueur_1 then
      (afficher_plateau_gagner plateau_joueur_1;
      print_endline "Tous les bateaux du joueur 1 ont été coulés! Le joueur 2 a gagne!";)
    else 
      if x==1 then if (afficher_plateau plateau_joueur_2 ; (tours_joueur x plateau_joueur_2 liste_bateaux_joueur_2)) then boucle 1 else boucle 2
        else if x==2 then if (afficher_plateau plateau_joueur_1 ;(tours_joueur x plateau_joueur_1 liste_bateaux_joueur_1))then boucle 2 else boucle 1
  in
  boucle 1;

