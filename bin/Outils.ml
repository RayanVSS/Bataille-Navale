
let rec get i liste = 
  match liste with
  | [] -> failwith "Sortie de liste"
  | h::q -> if i = 0 then h else get (i-1) q

let add x liste = x::liste