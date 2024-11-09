open Bateaux
open Outils

(* Type des cases du plateau *)
type case =
  | Vide
  | Navire of int * etat_navire
  | Rate
  | Coule


(* Taille du plateau *)
let plateau_taille = 11

(* Création d'un plateau vide *)
let creer_plateau taille =
  Array.make_matrix taille taille Vide

let rec verif_coord list_c plateau= 
  match list_c with
  | [] -> true
  | (x, y)::q -> let verif =
                  match plateau.(x).(y) with
                  | Vide->verif_coord q plateau 
                  |_->false
  in verif

let placer_bateaux plateau list_coords list_bateaux = 
let rec place l = 
  match l with
  | [] -> ()
  | (x, y)::q -> plateau.(x).(y) <- Navire (((length list_bateaux)+1,Intact)); place q
in  place list_coords

let rec coule list plateau =
  match list with
  |(x,y)::q -> plateau.(x).(y) <- Coule ; coule q plateau
  |_->()

let rec verif_coule l plateau=
  match l with
  |(x,y)::q-> (match plateau.(x).(y) with
                |Navire (_,Touche)-> verif_coule q plateau
                |_->false)
  |[]-> true

let update_etat list plateau=  
  if(verif_coule list plateau) then ((coule list plateau);true) else false

  let reset_plateaux plateau list_navire =
    let rec reset l = 
      match l with
      |[]->()
      |n1::n -> let rec reset_coord l =
        match l  with
        |(x,y)::q -> plateau.(x).(y) <- Vide; reset_coord q
        |[]->()
      in reset_coord n1.coord; reset n
    in reset !list_navire; list_navire:= [];