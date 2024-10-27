type nom_navire = Cuirasse | Croisseur | Torpilleur | SousMarin | PorteAvion
type etat_navire = Intact | Touche 
type navire = {nom:nom_navire; id:int; coord : (int*int) list ;}
type list_navire = navire list


(* Vérifie si on peut positionner le bateau a cette position *)

let make_navire nom id coord = 
  let name = 
    match nom with 
      |"Cuirassé"->Cuirasse
      |"Croisseur"->Croisseur
      |"Torpilleur"->Torpilleur
      |"SousMarin"->SousMarin
      |"PorteAvion"->PorteAvion
      |_->failwith "Nom de navire invalide"
    in {nom=name;id=id; coord=coord;}


(* Créer une liste de coordonnées pour un bateau *)
let rec make_pos_list x y taille orientation = 
  if taille=0 then [] 
  else if orientation="h" || orientation="H" then (x, y)::(make_pos_list x (y+1) (taille-1) orientation) 
  else if orientation="v" || orientation="V" then (x, y)::(make_pos_list (x+1) y (taille-1 ) orientation)
  else failwith "Orientation invalide"

let rec get_coord id liste = 
  match liste with
  | [] -> failwith "Navire inexistant"
  | h::q -> if h.id==id then h.coord else get_coord id q