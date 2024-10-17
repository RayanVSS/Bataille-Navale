open Plateaux 
open Regle
open Action
open Bateaux

(* Fonction principale du jeu *)
let jeu () =
  let plateau = creer_plateau taille in

  (* Boucle principale *)
  let rec boucle () =
    afficher_plateau plateau;
    placer_bateaux plateau [(0, 0); (1, 0); (2, 0); (3, 0)];
    if tous_bateaux_coules plateau then
      afficher_plateau_gagner plateau
    else begin
      print_endline "Entrez les coordonnées de tir (x y) :";
      let coords = read_line () in
      let coords_split = String.split_on_char ' ' coords in
      match coords_split with
      | [x_str; y_str] ->
          let x = int_of_string x_str in
          let y = int_of_string y_str in
          tirer plateau x y;
          boucle () (* Répéter le tour *)
      | _ -> 
          print_endline "Entrée invalide, réessayez.";
          boucle () (* Répéter en cas d'erreur *)
    end
  in
  boucle ()

(* Lancer le jeu *)
let () = jeu ()
