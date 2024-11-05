open Bateaux
open Plateaux
open Action
open GameView
open Outils
open IA
open Regle

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
  let i = ia_tirer plateau_joueur in
  if i >= 0 then let list = (get_coord i !liste_bateaux) in (
    if(verif_coule list plateau_joueur) then (coule list plateau_joueur;  reset_tirs () ;afficher_plateau plateau_joueur; print_endline "L'IA a coulé un bateau!";)
    else print_endline "L'IA a touché un bateau!"; true;)
  else (print_endline "L'IA a raté!"; false;)


(* Fonction principale du jeu *)
let rec jeu () =
  (* Affichage du menu *)
  let x = afficher_Menu () in
  (* Initialisation du jeu *)
  let () = print_endline "Bienvenue dans le jeu de bataille navale!" in
  let menu () =
      clearT ();
      if x = "3" then 
       (afficher_regles ();jeu ())
      else ();
  in menu ();
  (* Initialisation du jeu *)
      let joueur1 = "1" in
      let joueur2 = if(int_of_string x == 1)then "IA" else "2" in

      let liste_bateaux_joueur_1 = ref [] in
      let liste_bateaux_joueur_2 = ref [] in
      let plateau_joueur_1 = creer_plateau plateau_taille in
      let plateau_joueur_2 = creer_plateau plateau_taille in
      (* Placement des bateaux *)
      print_endline ("Au tour du joueur " ^ joueur1 ^" de placer ces bateaux");
      placer_tous_bateaux plateau_joueur_1 liste_bateaux_joueur_1;
      clearT ();
      print_endline ("Au tour du joueur "^joueur2^" de placer ces bateaux");
      if joueur2 = "IA" then placer_tous_bateaux_ia plateau_joueur_2 liste_bateaux_joueur_2
      else placer_tous_bateaux plateau_joueur_2 liste_bateaux_joueur_2;
      clearT ();


  
  (* Boucle principale du jeu *)
  let rec boucle j =
    if tous_bateaux_coules plateau_joueur_1 then
      (afficher_plateau_gagner plateau_joueur_1;
      print_endline ("Tous les bateaux du joueur "^joueur1^" ont été coulés! Le joueur "^joueur2^" a gagne!"))
    else if tous_bateaux_coules plateau_joueur_2 then
      (afficher_plateau_gagner plateau_joueur_2;
        print_endline ("Tous les bateaux du joueur "^joueur2^" ont été coulés! Le joueur "^joueur1^" a gagne!"))
    else 
      if j==1 then if (afficher_plateau plateau_joueur_2 ; (tours_joueur j plateau_joueur_2 liste_bateaux_joueur_2)) then boucle 1 else boucle 2
        else if j==2 then if joueur2=="IA" then 
          if (tours_ia plateau_joueur_1 liste_bateaux_joueur_1) then boucle 2 else (afficher_plateau plateau_joueur_1 ; afficher_espace (); boucle 1)
          else
          if (afficher_plateau plateau_joueur_1 ;(tours_joueur j plateau_joueur_1 liste_bateaux_joueur_1))then boucle 2 else boucle 1
  in
  boucle 1;

