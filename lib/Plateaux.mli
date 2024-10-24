type case = Vide | Bateau | Touche | Rate | Coule
val plateau_taille : int
val creer_plateau : int -> case array array
val afficher_plateau : case array array -> unit
val afficher_plateau_gagner : 'a array array -> unit
