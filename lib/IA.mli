val dernier_tir : (int * int * bool) ref
val ajoute_bateau_alea :
  string ->
  int -> Plateaux.case array array -> Bateaux.navire list ref -> unit
val ajoute_bateaux_aleatoire :
  Bateaux.navire list ref -> Plateaux.case array array -> unit
val touche_environ : Plateaux.case array array -> int -> int -> unit
