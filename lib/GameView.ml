open Bateaux
open Plateaux

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
          | Rate -> print_string "⬜"
          | Coule -> print_string "🟥"
          | Navire (_,e)-> match e with
                    | Touche -> print_string "🟨"
                    | _ -> print_string "🟦"
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
          | Coule-> print_string "🟩"
          | _ -> print_string "⬜"
        ) ligne;
      print_newline ()
    ) plateau;print_endline "Tous les bateaux ont été coulés! Victoire!"