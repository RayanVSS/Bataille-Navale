(* Type des cases du plateau *)
type case =
  | Vide
  | Bateau
  | Touche
  | Rate
  | Coule


(* Taille du plateau *)
let plateau_taille = 11

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
          | Vide -> print_string "🟦"
          | Bateau -> print_string "🟦"
          | Touche -> print_string "🟨"
          | Coule -> print_string "🟥"
          | Rate -> print_string "⬜"
        ) ligne;
      print_newline ()
    ) plateau

(* Affichage du plateau quand tu a gagner  *)
let afficher_plateau_gagner plateau =
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
          | _ -> print_string "🟩"
        ) ligne;
      print_newline ()
    ) plateau;print_endline "Tous les bateaux ont été coulés! Victoire!";