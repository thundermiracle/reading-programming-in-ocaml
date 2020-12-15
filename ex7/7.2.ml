(* 7.2 *)
exception Multiple_zero;;

let prod_list l =
  let rec prod_list' = function
    [] -> 1
    | a::rest when a = 0 -> raise Multiple_zero
    | a::rest -> a * (prod_list' rest)
  in
  try prod_list' l with Multiple_zero -> 0;;

prod_list [2;4;5;];;
(* 40 *)
prod_list [2;4;0;5;6;7;8;];
(* 0 *)
