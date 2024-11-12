let rec aux chars = match chars with
  |[] -> (' ',[])
  |' '::tail -> aux (tail) 
  |l::tail ->(l,tail)


let next_letter (str : string) = aux (List.init (String.length str) (String.get str))

    
let match_letter c = match c with
  | 'A' | 'a' -> 0
  | 'B' | 'b' -> 1
  | 'C' | 'c' -> 2
  | 'D' | 'd' -> 3
  | 'E' | 'e' -> 4
  | 'F' | 'f' -> 5
  | 'G' | 'g' -> 6
  | 'H' | 'h' -> 7
  | 'I' | 'i' -> 8
  | 'J' | 'j' -> 9
  | 'K' | 'k' -> 10
  | _ -> raise(Invalid_argument("NaL"))
           
           
let match_number c1 c2 = match c1 with 
  | '1' when (c2 = '0') -> 10
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | '0' -> 0
  | _ -> raise(Invalid_argument("NaN"))
  
let find_coords string = 
  let (c1,tail) = (next_letter string) in
  let (c2,tail) = (aux tail) in
  let (c3,_) = (aux tail) in 
  try 
    let num = (match_letter c1) in
    try 
      let lt = (match_number c2 c3) in (num,lt)
    with Invalid_argument(_) -> raise(Invalid_argument("Wrong input"))
  with Invalid_argument(_) -> 
    try let num = (match_number c1 c2) in
      let lt = if (num = 10) 
        then (match_letter c3)
        else (match_letter c2) in (lt,num) 
    with Invalid_argument(_) -> raise(Invalid_argument("Wrong input"))  