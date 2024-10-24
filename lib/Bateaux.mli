type nom_navire = Cuirasse | Croisseur | Torpilleur | SousMarin | PorteAvion
type etat_navire = Intact | Touche | Coule
type navire = { nom : nom_navire; coord : (int * int * etat_navire) list; }
type list_navire = navire list
val verif_coord : (int * int) list -> Plateaux.case array array -> bool
val placer_bateaux : Plateaux.case array array -> (int * int) list -> unit
val make_pos_list :
  int -> int -> int -> string -> (int * int * etat_navire) list
val make_navire : nom_navire -> int -> int -> int -> string -> navire
