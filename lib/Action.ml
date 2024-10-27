open Plateaux
open Bateaux
open Outils

(* Fonction pour tirer sur une case *)
let tirer plateau x y =
  let taille = Array.length plateau in
  if x < 0 || x >= taille || y < 0 || y >= taille then
    (print_endline "Coordonnées invalides, réessayez.";Some(-1)) (*Cas ou le joueur doit recommencer*)
  else
    match plateau.(x).(y) with
    | Vide ->
        print_endline "Manqué!";
        plateau.(x).(y) <- Rate;
        None
    | Coule ->
        print_endline "Déjà coulé.";
        Some(-1) (*Cas ou le joueur doit recommencer*)
    | Rate ->
        print_endline "Déjà tiré ici.";
        Some(-1) (*Cas ou le joueur doit recommencer*)
    | Navire (id, etat) ->
        match etat with
        | Intact ->
            print_endline "Touché!";
            plateau.(x).(y) <- Navire (id, Touche);
            Some(id)
        | Touche ->
            print_endline "Déjà touché.";
            Some(-1) (*Cas ou le joueur doit recommencer*)

            

  let demander_placement nom taille plateau =
    let rec demander_valides () =
      print_endline (Printf.sprintf "Placer le %s (taille: %d)" nom taille);
      print_endline "Entrez les coordonnées de départ (x y) :";
      let coords = read_line () in
      let coords_split = String.split_on_char ' ' coords in
      match coords_split with
      | [x_str; y_str] -> 
          let x = int_of_string x_str in
          let y = int_of_string y_str in
          print_endline "Entrez l'orientation (h pour horizontal, v pour vertical) :";
          let orientation = read_line () in
          if coordonnees_valides x y taille orientation plateau_taille then
            let positions = make_pos_list x y taille orientation in
            let coords = List.map (fun (x, y) -> (x, y)) positions in
            if verif_coord coords plateau then
              positions (* Coordonnées valides et bateau peut être placé *)
            else begin
              print_endline "Les coordonnées sont valides mais il y a déjà un bateau à cet endroit.";
              demander_valides () (* Redemander placement *)
            end
          else begin
            print_endline "Les coordonnées ou l'orientation sont invalides, veuillez réessayer.";
            demander_valides () (* Redemander placement *)
          end
      | _ ->
          print_endline "Entrée invalide, réessayez.";
          demander_valides () (* Redemander placement *)
    in
    demander_valides ()


(* Placer tous les bateaux *)
let placer_tous_bateaux plateau list_bateaux=
    let navires = [
      ("Cuirassé", 1);
      ("Croisseur", 3);
    ] in
    List.iter (fun (nom, taille)->
      let coords = demander_placement nom taille plateau in
      let coords = List.map (fun (x, y) -> (x, y)) coords in
      ((placer_bateaux plateau coords !list_bateaux);list_bateaux:=((make_navire nom ((length !list_bateaux)+1) coords))::!list_bateaux)
    ) navires

  
  