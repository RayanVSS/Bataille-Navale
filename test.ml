(* Type de case *)
type case =
  | Vide
  | Bateau
  | Touche


let taille = 10
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

(* Placer des bateaux sur le plateau *)
let placer_bateaux plateau =
  (* Placer deux bateaux de manière simple et statique pour l'exemple *)
  if Array.length plateau >= 4 then begin
    plateau.(1).(1) <- Bateau;
    plateau.(3).(3) <- Bateau
  end

(* Vérifier si toutes les cases de bateau sont touchées *)
let tous_bateaux_coules plateau =
  let coules = ref true in
  Array.iter (fun ligne ->
      Array.iter (fun case ->
          if case = Bateau then coules := false
        ) ligne
    ) plateau;
  !coules

(* Fonction pour tirer sur une case *)
let tirer plateau x y =
  let taille = Array.length plateau in
  if x < 0 || x >= taille || y < 0 || y >= taille then
    print_endline "Coordonnées invalides, réessayez."
  else
    match plateau.(x).(y) with
    | Vide ->
        print_endline "Manqué!";
        plateau.(x).(y) <- Vide
    | Bateau ->
        print_endline "Touché!";
        plateau.(x).(y) <- Touche
    | Touche ->
        print_endline "Déjà touché."

(* Fonction principale du jeu *)
let jeu () =
  let plateau = creer_plateau 10 in
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
