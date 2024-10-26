type case = Vide | Navire of int * Bateaux.etat_navire | Rate | Coule
val plateau_taille : int
val creer_plateau : int -> case array array
val verif_coord : (int * int) list -> case array array -> bool
val placer_bateaux : case array array -> (int * int) list -> 'a list -> unit
val coule : (int * int) list -> case array array -> unit
val update_etat : (int * int) list -> case array array -> unit
