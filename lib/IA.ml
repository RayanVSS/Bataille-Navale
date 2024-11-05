open Regle
open Bateaux
open Outils
open Plateaux

let rec ajoute_bateau_alea nom taille plateau liste_navire = 
  let x = Random.int plateau_taille in
  let y = Random.int plateau_taille in
  let orientation = if Random.int 2 = 0 then "h" else "v" in
  let coords = make_pos_list x y taille orientation in
    if coordonnees_valides x y taille orientation plateau_taille then
      let coords = List.map (fun (x, y) -> (x, y)) coords in
      if verif_coord coords plateau then
        ((placer_bateaux plateau coords !liste_navire);liste_navire:=((make_navire nom ((length !liste_navire)+1) coords)::!liste_navire))
      else ajoute_bateau_alea nom taille plateau  liste_navire
    else ajoute_bateau_alea nom taille plateau  liste_navire

    
let placer_tous_bateaux_ia plateau liste_navire = 
  let navires = navire_plateau_1 in
  List.iter (fun (nom, nb, taille) -> 
    let rec place n = 
      match n with 
      |0 ->()
      |_-> ajoute_bateau_alea nom taille plateau liste_navire; place (n-1)
    in place nb;
  ) navires;()


  let tirs_effectues = ref []
  let dernier_touche = ref None

  let reset_tirs () =
    dernier_touche := None
  
  let rec tir_aleatoire () =
    let x = Random.int plateau_taille in
    let y = Random.int plateau_taille in
    if List.mem (x, y) !tirs_effectues then tir_aleatoire ()
    else (x, y)

  let ia_tirer plateau =
    let x, y =
      match !dernier_touche with
      | Some (dx, dy) ->
          let adjacents = [(dx+1, dy); (dx-1, dy); (dx, dy+1); (dx, dy-1)] in
          let valid_adj = List.filter (fun (x, y) ->
            x >= 0 && x < plateau_taille &&
            y >= 0 && y < plateau_taille &&
            not (List.mem (x, y) !tirs_effectues)
          ) adjacents in
          if valid_adj = [] then (
            dernier_touche := None;
            tir_aleatoire ();
          ) else
            get (Random.int (List.length valid_adj)) valid_adj 
      | None -> tir_aleatoire ()
    in
    tirs_effectues := (x, y) :: !tirs_effectues;
    let resultat =
      match plateau.(x).(y) with
      | Vide -> plateau.(x).(y) <- Rate;-1
      | Coule -> -1
      | Rate -> -1
      | Navire (id, etat) ->
          match etat with
          | Intact -> plateau.(x).(y) <- Navire (id, Touche);id
          | Touche -> -1
    in
    if resultat != -1 then (dernier_touche := Some (x, y);print_endline ("L'IA a tiré en " ^ string_of_int x ^ " " ^ string_of_int y ^ " et a touche .");resultat)
    else -1;
    
