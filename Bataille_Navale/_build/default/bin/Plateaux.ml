type case =
  | Vide
  | Bateau
  | Touche

(* Taille du plateau *)
let taille = 8
(* Création d'un plateau vide *)
let creer_plateau taille =
  Array.make_matrix taille taille Vide

(* Affichage du plateau *)
let afficher_plateau plateau =
  let taille = Array.length plateau in
  (* Afficher les indices de colonnes *)
  print_string "  ";
  for i = 0 to taille - 1 do
    Printf.printf "%d " i
  done;
  print_newline ();

  (* Afficher les lignes du plateau *)
  Array.iteri (fun i ligne ->
      Printf.printf "%d " i;
      Array.iter (fun case ->
          match case with
          | Vide -> print_string "- "
          | Bateau -> print_string "- " (* On cache les bateaux *)
          | Touche -> print_string "X "
        ) ligne;
      print_newline ()
    ) plateau
