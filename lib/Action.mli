val tirer : Plateaux.case array array -> int -> int -> int option
val demander_placement :
  string -> int -> Plateaux.case array array -> (int * int) list
val placer_tous_bateaux :
  Plateaux.case array array -> Bateaux.navire list ref -> unit
