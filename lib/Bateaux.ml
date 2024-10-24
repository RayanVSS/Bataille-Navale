open Plateaux

type nom_navire = Cuirasse | Croisseur | Torpilleur | SousMarin | PorteAvion

type etat_navire = Intact | Touche | Coule
type navire = {nom:nom_navire; coord : (int*int*etat_navire) list}
type list_navire = navire list


(* Vérifie si on peut positionner le bateau a cette position *)
let rec verif_coord list_c plateau= 
    match list_c with
    | [] -> true
    | (x, y)::q -> if plateau.(x).(y) == Vide  then verif_coord q plateau else false

(* Placer des bateaux sur le plateau *)
let placer_bateaux plateau list_coords  = 
  let rec place l = 
    match l with
    | [] -> ()
    | (x, y)::q -> plateau.(x).(y) <- Bateau; place q
  in if not (verif_coord list_coords plateau) then print_endline "Impossible de placer le bateau" else place list_coords;print_endline "Bateau placé"

let rec make_pos_list x y taille orientation = 
  if taille=0 then [] else  if orientation="h" then (x, y,Intact)::(make_pos_list (x+1) y (taille-1) orientation) else (x, y,Intact)::(make_pos_list x (y+1) (taille-1 ) orientation)

  let make_navire nom taille x y orientation = {nom=nom; coord=make_pos_list x y taille orientation}