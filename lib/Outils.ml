
let rec get i liste = 
  match liste with
  | [] -> failwith "Sortie de liste"
  | h::q -> if i = 0 then h else get (i-1) q

let rec remove i liste =
  match liste with
  | [] -> []
  | h::q -> if i = 0 then q else h::(remove (i-1) q)

let add x liste = x::liste

let rec length liste =
  match liste with
  |[]->0
  |_::q->1+(length q)

let coordonnees_valides x y taille_bateau orientation plateau_taille =
  if x < 0 || y < 0 || x >= plateau_taille || y >= plateau_taille then false
  else if orientation = "h" || orientation="H" then  (y + taille_bateau < plateau_taille)
  else if orientation= "v" || orientation="V" then (x + taille_bateau < plateau_taille ) 
  else false

  let clearT () =
    let command =
      if Sys.os_type = "Win32" then "cls"
      else "clear"
    in
    ignore (Sys.command command)
    

let parse_coord coord_str =
  print_endline "Parsed a coord";
  try
    int_of_string coord_str  
  with Failure _ ->
    if String.length coord_str = 1 then
      let c = coord_str.[0] in
      if c >= 'a' && c <= 'z' then
        (Char.code c) - (Char.code 'a') 
      else if c >= 'A' && c <= 'Z' then
        (Char.code c) - (Char.code 'A')  
      else
        failwith "Coordonnée invalide"
    else
      failwith "Coordonnée invalide"

let parse_coords coords_split =
  let est_lettre c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  let est_chiffre s =
    try
      ignore (int_of_string s); true
    with Failure _ -> false
  in
  let verifier_et_inverser coords_split =
    print_endline "Vérification";
    match coords_split with
    | [a; b] when est_lettre a.[0] && est_chiffre b -> print_endline "Lettre - Chiffre 1";coords_split
    | [b; a] when est_lettre a.[0] && est_chiffre b -> print_endline "Lettre - Chiffre 2";[a; b]
    | [a; b] when est_chiffre a && est_chiffre b -> print_endline "Chiffre - Chiffre ";coords_split
    | [a; b] when est_lettre a.[0] && est_lettre b.[0] -> print_endline "Lettre - Lettre ";coords_split
    | _ -> failwith "Format de coordonnées incorrect"
  in
  let coords_split = verifier_et_inverser coords_split in
  match List.map parse_coord coords_split with
  | [a; b] when a >= 0 && a < 26 && b >= 0 -> (a, b)
  | _ -> failwith "Format de coordonnées incorrect"

  let iscoordonee coords = 
    match coords with
    | [x_str; y_str] ->String.length x_str == 1 && (String.length y_str==1 ||String.length y_str==2) && x_str.[0] >= 'A' && x_str.[0] <= 'Z' && (y_str.[0] >= '0' && y_str.[0] <= '9' || y_str= "10")
    | _ -> false