val aux : char list -> char * char list
val next_letter : string -> char * char list
val match_letter : char -> int           (* Can raise Invalid_argument("NaL") *)
val match_number : char -> char -> int   (* Can raise Invalid_argument("NaN") *)
val find_coords : string -> (int * int)  (* Can raise Invalid_argument("Wrong input") *)