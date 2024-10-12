open Plateaux

(* Placer des bateaux sur le plateau *)
let placer_bateaux plateau =
  (* Placer deux bateaux de manière simple et statique pour l'exemple *)
  if Array.length plateau >= 4 then begin
    plateau.(1).(1) <- Bateau;
    plateau.(3).(3) <- Bateau
  end
