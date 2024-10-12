open Plateaux 
open Bateaux
open Regle
open Action

(* Fonction principale du jeu *)
let jeu () =
  let plateau = creer_plateau taille in
  placer_bateaux plateau;

  (* Boucle principale *)
  let rec boucle () =
    afficher_plateau plateau;
    if tous_bateaux_coules plateau then
      print_endline "Tous les bateaux ont été coulés! Victoire!"
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
