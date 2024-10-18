open Plateaux

let rec get i liste = 
  match liste with
  | [] -> failwith "Sortie de liste"
  | h::q -> if i = 0 then h else get (i-1) q

let add x liste = x::liste


let coordonnees_valides x y taille_bateau orientation =
  print_int x; print_string ", "; print_int y; print_string ", "; print_int taille_bateau; print_string ", "; print_int plateau_taille; print_newline();
  if x < 0 || y < 0 || x >= plateau_taille || y >= plateau_taille then false
  else if orientation = "h" && (x + taille_bateau > plateau_taille) then false
  else if orientation = "v" && (y + taille_bateau > plateau_taille) then false
  else true