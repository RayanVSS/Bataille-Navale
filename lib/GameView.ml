open Bateaux
open Plateaux
open Outils

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
      Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
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

let afficher_espace () =
  print_endline "-----------------------------------"
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
      Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
      Array.iter (fun case ->
          match case with
          | Coule-> print_string "🟩"
          | _ -> print_string "⬜"
        ) ligne;
      print_newline ()
    ) plateau;print_endline "Tous les bateaux ont été coulés! Victoire!"

let afficher_plateau_placement plateau =
  let taille = Array.length plateau in
  print_string "  ";
  for i = 0 to taille - 1 do
    Printf.printf "%d " i
  done;
  print_newline ();

  (* Afficher les lignes du plateau *)
  Array.iteri (fun i ligne ->
      Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
      Array.iter (fun case ->
          match case with
          | Navire (_,_)-> print_string "🟪"
          | _ -> print_string "🟦"
        ) ligne;
      print_newline ()
    ) plateau

let afficher_Menu () =
  clearT ();
  print_endline"";
  print_endline"                           ⬛​⬜";
  print_endline"                ⬛​         ⬛​⬜⬜";
  print_endline"                ⬛​⬜       ⬛​⬜⬜⬜⬜";
  print_endline"                ⬛​⬜⬜     ⬛​";
  print_endline"        ⬛​      ⬛​         ⬛​⬜⬜";
  print_endline"        ⬛​⬜​    ⬛​⬜​       ⬛​⬜⬜⬜";
  print_endline"        ⬛​⬜​    ⬛​⬜​⬜     ⬛​⬜⬜⬜⬜";
  print_endline"        ⬛​⬜​⬜​  ⬛​⬜​⬜​⬜​   ​⬛​⬜⬜⬜⬜⬜";
  print_endline"        ⬛​      ⬛​         ⬛​";
  print_endline"    ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​";
  print_endline"        ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​";
  print_endline"          ⬛​⬛​🟨⬛​🟨⬛​⬛​🟨⬛​🟨⬛​⬛​🟨⬛​⬛​​";
  print_endline"            ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​​";
  print_endline"            ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​";
  print_endline"🟦🟦🔵​🟦🟦🔵​🟦🔵​🟦🟦🟦🟦🟦🟦🟦🟦🔵​🟦🔵​🟦​🟦​🟦";
  print_endline"🟦🟦🔵​🟦🟦🔵​🟦🔵​🟦🟦🟦🟦🟦🟦🟦🟦🔵​🟦🔵​🟦​🟦​🟦";
  print_endline"🔲​⬛​⬛​​⬛​​​⬛​⬛​⬛​​​⬛​⬛​​⬛​​​⬛​⬛​​⬛​⬛​⬛​​⬛​​​⬛​⬛​⬛​​​⬛​⬛​🔲​";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ 0️⃣  :1 vs 1​​                             ⬛";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ 1️⃣​​  :1 vs IA​                            ⬛";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ ​2️⃣​  :1 vs 1 rapide​ 🔒                   ⬛";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ 3️⃣​  :regle​                              ⬛"; 
  print_endline"⬛                                        ​⬛";
  print_endline"🔲​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​🔲";
  
  (*
  print_endline "🚢​ 🛥️ ​⛴️ ​🛳️ ​🚤 ​⛵ ​🛶​";
  print_endline "Bienvenue dans la bataille navale!";
  print_endline "0️⃣​ Jouer";
  print_endline "1️⃣ Règles";
  print_endline "2️⃣​ Quitter";
  print_string "Choix: ";
  *)
  read_line ();
  

