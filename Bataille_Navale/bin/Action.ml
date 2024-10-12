open Plateaux

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
