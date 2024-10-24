
(*
let rec ajoute_navire_aleatoire liste_navire taille = 
  if taille = 0 then liste_navire else 
  let x = Random.int 10 in
  let y = Random.int 10 in
  let orientation = if Random.int 2 = 0 then "h" else "v" in
  let nom = match Random.int 5 with
    | 0 -> Cuirasse
    | 1 -> Croisseur
    | 2 -> Torpilleur
    | 3 -> SousMarin
    | _ -> PorteAvion
  in
  let navire = make_navire nom taille x y orientation in
  ajoute_navire_aleatoire (navire::liste_navire) (taille-1)
  *)
