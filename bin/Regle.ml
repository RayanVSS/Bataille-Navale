open Plateaux

(* Vérifier si toutes les cases de bateau sont touchées *)
let tous_bateaux_coules plateau =
  let coules = ref true in
  Array.iter (fun ligne ->
      Array.iter (fun case ->
          if case = Bateau then coules := false
        ) ligne
    ) plateau;
  !coules


  let nombre_navire = [1,2,3]
