open Regle
open Bateaux
open Outils
open Plateaux


let dernier_tir = ref (0,0,true)

let rec ajoute_bateau_alea nom taille plateau liste_navire = 
  let x = Random.int plateau_taille in
  let y = Random.int plateau_taille in
  let orientation = if Random.int 2 = 0 then "h" else "v" in
  let coords = make_pos_list x y taille orientation in
    if coordonnees_valides x y taille orientation plateau_taille then
      let coords = List.map (fun (x, y) -> (x, y)) coords in
      if verif_coord coords plateau then
        ((placer_bateaux plateau coords !liste_navire);liste_navire:=((make_navire nom ((length !liste_navire)+1) coords)::!liste_navire))
      else ajoute_bateau_alea nom taille plateau  liste_navire
    else ajoute_bateau_alea nom taille plateau  liste_navire

    
let ajoute_bateaux_aleatoire liste_navire plateau = 
  let navires = navire_plateau_1 in
  List.iter (fun (nom, nb, taille) -> 
    let rec place n = 
      match n with 
      |0 ->()
      |_-> ajoute_bateau_alea nom taille plateau liste_navire; place (n-1)
    in place nb;
  ) navires;()


let touche_environ plateau x y =
  let rec touche l = 
    match l with
    |[]->()
    |(x,y)::q -> match plateau.(x).(y) with
                |Vide -> plateau.(x).(y) <- Rate 
                |Rate -> touche q
                |Coule -> touche q
                |Navire (id,etat) -> match etat with
                                    |Touche -> touche q
                                    |Intact -> plateau.(x).(y) <- Navire (id,Touche)
  in let list = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)] in touche list

  (*
let rec touche_aleatoire plateau  = 
  match !dernier_tir with
  |(x,y,true) -> 
    let alea_x = if (Random.int 2)==1 then 1 else -1 in 
    let alea_y = if
    if (Random.int 2)==1 then 

  |(x,y,false)->    
    let newx = Random.int plateau_taille in
    let newy = Random.int plateau_taille in
    match plateau.(x).(y) with
    |Vide -> plateau.(x).(y) <- Rate
    |Rate -> touche_aleatoire plateau plateau_taille
    |Coule -> touche_aleatoire plateau plateau_taille
    |Navire (id,etat) -> match etat with
                        |Touche -> touche_aleatoire plateau plateau_taille
                        |Intact -> plateau.(x).(y) <- Navire (id,Touche)
          *)

    
