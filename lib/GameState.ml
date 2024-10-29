open Bateaux
open Plateaux
open Action
open GameView
open Outils

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

(* Fonction principale du jeu *)
let jeu () =
  let () = print_endline "Bienvenue dans le jeu de bataille navale!" in
  (* Création du plateau *)
  let liste_bateaux = ref [] in
  let plateau = creer_plateau plateau_taille in
  (* Placement des bateaux *)
  placer_tous_bateaux plateau liste_bateaux;

  (* Boucle principale du jeu *)
  let rec boucle () =
    afficher_plateau plateau;
    if tous_bateaux_coules plateau then
      afficher_plateau_gagner plateau
    else 
      print_endline "Entrez les coordonnées de tir (x y) :";
      let coords = read_line () in
      let coords_split = String.split_on_char ' ' coords in
      match coords_split with
      | [x_str; y_str] ->
          let coor = parse_coords [x_str; y_str] in
          let x = fst coor in
          let y = snd coor in
          (match tirer plateau x y with
            | Some i ->
                if i >= 0 then
                  update_etat (get_coord i !liste_bateaux) plateau
                else () (*Cas ou le joueur doit recommencer*)
              | _ -> (););
          boucle () (* Répéter le tour *)
      | _ -> 
          print_endline "Entrée invalide, réessayez.";
          boucle () (* Répéter en cas d'erreur *)
    
  in
  boucle ();