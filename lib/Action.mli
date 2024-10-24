val tirer : Plateaux.case array array -> int -> int -> unit
val demander_placement :
  string ->
  int -> Plateaux.case array array -> (int * int * Bateaux.etat_navire) list
val placer_tous_bateaux : Plateaux.case array array -> unit
